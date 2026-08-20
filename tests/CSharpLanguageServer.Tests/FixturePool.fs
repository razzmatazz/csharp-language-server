/// A pool of warm, reusable `LspTestClient` instances for read-only test fixtures.
///
/// Motivation: many tests (e.g. `InlayHintTests`) each spin up their own server process
/// and load the same, unchanging fixture solution just to issue a handful of read-only
/// requests. That solution-load cost dominates their runtime. Since these tests never
/// mutate documents, many of them can safely take turns using a small number of shared,
/// already-loaded server instances instead of each paying for their own.
///
/// Implementation note: pool bookkeeping (idle instances, pending waiters, per-fixture
/// created counts) lives as plain immutable state inside a `MailboxProcessor` actor,
/// `poolManager` — the same pattern `LspTestClient` itself already uses internally (see
/// `Tooling.fs`). Because it processes messages strictly one at a time, no
/// `SemaphoreSlim`, `Interlocked`, or explicit locks are needed for that bookkeeping.
///
/// Booting a fresh instance, however, is *not* done inline inside `poolManager` — it's
/// handed off to a second, dedicated actor, `bootWorker`. Booting blocks on
/// `activeClientsSemaphore` (see `Tooling.fs`), which can legitimately take a while if
/// other, unrelated ad-hoc `activateFixture` tests currently hold all the free slots. If
/// that wait happened inside `poolManager`'s own message loop, the *entire* pool would
/// freeze for its duration — no `CheckIn` could be processed, not even to return an
/// already-idle instance of the very fixture something else is waiting to boot. Since
/// `bootWorker` has its own single-threaded loop, it still serializes actual boots one at
/// a time (preserving the "don't thundering-herd multiple solution loads at once" goal),
/// but a slow boot only stalls `bootWorker`, never `poolManager`, which keeps servicing
/// every other fixture and every check-in throughout.
///
/// This module is deliberately narrow in scope:
///   - `PooledLspTestClient` / `PooledLspDocumentHandle` deliberately do not expose
///     `Change`/`Save` — pooled fixtures must stay read-only, since the same instance is
///     handed to many tests in sequence.
///   - `PooledLspTestClient.Request` only accepts LSP methods on the `readOnlyRequestMethods`
///     allow-list (default-deny) — this is the actual mutation boundary that matters, since
///     `Request` is otherwise a generic escape hatch that could reach a mutating method
///     (e.g. `workspace/executeCommand`, which can alter files as a side effect of running
///     a command) even with `Change`/`Save` gone. A fixture-name allow-list wouldn't guard
///     against that, so there deliberately isn't one — any fixture name can be leased;
///     typos are still caught for free by `LoadSolution`'s existing "no such test data dir"
///     check the first time an instance for that name is booted.
module CSharpLanguageServer.Tests.FixturePool

// `poolManager` and `bootWorker` below are defined as a mutually-recursive pair of
// MailboxProcessor *values* (not functions), which triggers FS0040 ("recursive
// references ... checked for initialization-soundness at runtime through a delayed
// reference"). This is the standard, sound pattern for actors that post to each other:
// neither actor's body touches the other until a message is actually processed, which
// can only happen after both are fully constructed (nothing posts to either during
// module initialization) — so the delayed-reference check FS0040 warns about always
// succeeds here.
#nowarn "40"

open System
open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling

/// LSP request methods considered safe to issue through a pooled client: read-only
/// queries whose server-side handling doesn't mutate documents or workspace state.
/// Deliberately an allow-list rather than a block-list of known-dangerous methods — an
/// unrecognized method fails immediately instead of silently being let through. Extend
/// this as more read-only test files opt into pooling.
let private readOnlyRequestMethods =
    Set.ofList
        [ "textDocument/hover"
          "textDocument/definition"
          "textDocument/declaration"
          "textDocument/typeDefinition"
          "textDocument/implementation"
          "textDocument/references"
          "textDocument/documentHighlight"
          "textDocument/documentSymbol"
          "textDocument/foldingRange"
          "textDocument/selectionRange"
          "textDocument/semanticTokens/full"
          "textDocument/semanticTokens/range"
          "textDocument/inlayHint"
          "textDocument/inlineValue"
          "textDocument/moniker"
          "textDocument/prepareCallHierarchy"
          "callHierarchy/incomingCalls"
          "callHierarchy/outgoingCalls"
          "textDocument/prepareTypeHierarchy"
          "typeHierarchy/supertypes"
          "typeHierarchy/subtypes"
          "textDocument/signatureHelp"
          "textDocument/completion"
          "completionItem/resolve"
          "textDocument/diagnostic"
          "workspace/diagnostic"
          "workspace/symbol"
          "workspaceSymbol/resolve"
          "textDocument/documentColor"
          "textDocument/colorPresentation"
          "textDocument/linkedEditingRange"
          "csharp/metadata"
          // `rename` only computes a `WorkspaceEdit` description; the server never applies
          // it (that would require a separate `workspace/applyEdit` round trip, which
          // pooled fixtures can't reach — only `PooledLspTestClient.Request` is exposed,
          // and it isn't on this list). Actually applying edits to a document still
          // requires `Change`/`Save`, which pooled document handles don't expose at all.
          "textDocument/prepareRename"
          "textDocument/rename"
          // Same reasoning as `rename` above: `codeAction` only returns `CodeAction[]`
          // descriptions (each possibly carrying an unapplied `WorkspaceEdit`), and
          // `formatting` only returns `TextEdit[]` — neither handler mutates the document
          // or workspace itself, and pooled fixtures have no way to apply the result back
          // (no `Change`/`Save`, no `workspace/applyEdit`, no `workspace/executeCommand`).
          "textDocument/codeAction"
          "textDocument/formatting" ]

/// Deliberately a minority share of `activeClientsSemaphore`'s total budget (see
/// `Tooling.fs`) — pooled instances stay alive for the whole test run once booted, so
/// this must leave headroom for every other file's ad-hoc `activateFixture` calls still
/// running concurrently. Floor is 1, not 2: on a small CI runner (e.g. 4 cores) a floor of
/// 2 would force the pool to permanently claim half the semaphore's entire budget, which
/// starved the rest of the suite badly enough to hang in CI (4 cores, 2 NUnit workers).
let private maxPoolSize () =
    max 1 (min 4 (Environment.ProcessorCount / 4))

let private isHealthy (client: LspTestClient) =
    match client.GetState().ServerProcess with
    | Some p ->
        try
            not p.HasExited
        with _ ->
            false
    | None -> false

/// Reply payload for a `Rent` request: either a checked-out client, or the exception that
/// occurred while trying to boot one (so a boot failure surfaces on the caller's thread
/// instead of hanging `PostAndReply` forever or killing the pool actor).
type private RentReply = Result<LspTestClient, exn>

type private PoolMessage =
    | Rent of fixtureName: string * AsyncReplyChannel<RentReply>
    | CheckIn of fixtureName: string * LspTestClient
    /// Reported by `bootWorker` once a boot it was asked to perform finishes (success or
    /// failure). `poolManager` is the sole owner of `CreatedCount`/`Idle`/`Waiters`, so
    /// even though `bootWorker` did the actual (slow) work, only `poolManager` applies its
    /// outcome to state and replies to the original waiter.
    | BootCompleted of fixtureName: string * AsyncReplyChannel<RentReply> * RentReply
    | DisposeAll of AsyncReplyChannel<unit>

/// A single boot request handed off to `bootWorker`: which fixture to load, and the
/// original `Rent` caller's reply channel to eventually satisfy (via `BootCompleted`).
type private BootMessage = BootRequest of fixtureName: string * AsyncReplyChannel<RentReply>

/// Per-fixture-name pool state. `CreatedCount` tracks how many live instances exist (idle
/// + currently leased, *including* ones a `BootRequest` has been reserved for but hasn't
/// finished booting yet) so we never kick off more than `MaxSize` concurrent boots for
/// this fixture. `Waiters` holds reply channels for `Rent` calls that arrived while the
/// pool had no idle instance and no room to grow — satisfied in order as instances become
/// available.
type private FixturePoolState =
    { Idle: LspTestClient list
      Waiters: AsyncReplyChannel<RentReply> list
      CreatedCount: int }

let private emptyFixturePoolState =
    { Idle = []
      Waiters = []
      CreatedCount = 0 }

let private tryBoot fixtureName : RentReply =
    try
        Ok(activateFixture fixtureName)
    with ex ->
        Error ex

/// After any change that might free up capacity or add an idle instance, satisfies as
/// many pending waiters as possible from `Idle`. If none are idle but there's still room
/// to grow (`CreatedCount < MaxSize`), reserves the slot immediately (so a burst of
/// concurrent `Rent` calls can't over-request boots) and hands the actual boot off to
/// `bootWorker` — this function itself never blocks, regardless of how long that boot
/// ends up taking.
let rec private drainWaiters fixtureName (fps: FixturePoolState) : FixturePoolState =
    match fps.Waiters, fps.Idle with
    | rc :: restWaiters, client :: restIdle ->
        rc.Reply(Ok client)

        drainWaiters
            fixtureName
            { fps with
                Waiters = restWaiters
                Idle = restIdle }
    | rc :: restWaiters, [] when fps.CreatedCount < maxPoolSize () ->
        bootWorker.Post(BootRequest(fixtureName, rc))

        drainWaiters
            fixtureName
            { fps with
                Waiters = restWaiters
                CreatedCount = fps.CreatedCount + 1 }
    | _ -> fps

and private processMessage (state: Map<string, FixturePoolState>) (msg: PoolMessage) : Map<string, FixturePoolState> =
    match msg with
    | Rent(fixtureName, rc) ->
        let fps =
            state |> Map.tryFind fixtureName |> Option.defaultValue emptyFixturePoolState

        let fps =
            { fps with
                Waiters = fps.Waiters @ [ rc ] }
            |> drainWaiters fixtureName

        state |> Map.add fixtureName fps

    | CheckIn(fixtureName, client) ->
        let fps =
            state |> Map.tryFind fixtureName |> Option.defaultValue emptyFixturePoolState

        let fps =
            if isHealthy client then
                client.ResetAccumulatedState()
                { fps with Idle = client :: fps.Idle }
            else
                (client :> IDisposable).Dispose()

                { fps with
                    CreatedCount = fps.CreatedCount - 1 }

        state |> Map.add fixtureName (fps |> drainWaiters fixtureName)

    | BootCompleted(fixtureName, rc, result) ->
        rc.Reply(result)

        match result with
        | Ok _ ->
            // The slot was already reserved (CreatedCount bumped) when the boot was
            // kicked off in `drainWaiters` — nothing further to update.
            state
        | Error _ ->
            // The reserved slot never materialized; free it back up and let another
            // still-pending waiter (if any) trigger a fresh boot attempt.
            let fps =
                state |> Map.tryFind fixtureName |> Option.defaultValue emptyFixturePoolState

            let fps =
                { fps with
                    CreatedCount = fps.CreatedCount - 1 }
                |> drainWaiters fixtureName

            state |> Map.add fixtureName fps

    | DisposeAll rc ->
        for KeyValue(_, fps) in state do
            for client in fps.Idle do
                (client :> IDisposable).Dispose()

        rc.Reply(())
        Map.empty

and private poolManager =
    MailboxProcessor<PoolMessage>.Start(fun inbox ->
        let rec loop (state: Map<string, FixturePoolState>) = async {
            let! msg = inbox.Receive()

            let newState =
                try
                    processMessage state msg
                with ex ->
                    // Keep the actor alive even on an unexpected bug here: make sure a
                    // pending caller doesn't hang forever, then carry on with the
                    // pre-message state.
                    match msg with
                    | Rent(_, rc) -> rc.Reply(Error ex)
                    | BootCompleted(_, rc, _) -> rc.Reply(Error ex)
                    | DisposeAll rc -> rc.Reply(())
                    | CheckIn _ -> ()

                    state

            return! loop newState
        }

        loop Map.empty)

/// Dedicated single-worker actor that performs the actual (slow, semaphore-gated) boot
/// work requested by `drainWaiters`, strictly one at a time, entirely off `poolManager`'s
/// thread. See the module doc comment for why this separation matters.
///
/// Explicit type annotation: `drainWaiters` (defined above, in the same `let rec ... and
/// ...` group) calls `bootWorker.Post` before the compiler has otherwise inferred this
/// binding's type from its own definition below — without the annotation that lookup is
/// of indeterminate type (FS0072).
and private bootWorker: MailboxProcessor<BootMessage> =
    MailboxProcessor<BootMessage>.Start(fun inbox ->
        let rec loop () = async {
            let! BootRequest(fixtureName, rc) = inbox.Receive()

            let result =
                try
                    tryBoot fixtureName
                with ex ->
                    // tryBoot already catches everything from activateFixture; this is
                    // just an extra safety net so bootWorker's loop can never die and
                    // silently stop accepting future boot requests.
                    Error ex

            poolManager.Post(BootCompleted(fixtureName, rc, result))
            return! loop ()
        }

        loop ())

let private checkIn fixtureName (client: LspTestClient) =
    poolManager.Post(CheckIn(fixtureName, client))

/// A document handle leased from a `PooledLspTestClient`. Deliberately narrower than
/// `LspDocumentHandle` — no `Change`/`Save` — since pooled fixtures are shared by many
/// tests in sequence and must stay read-only.
type PooledLspDocumentHandle internal (inner: LspDocumentHandle, onDispose: unit -> unit) =
    let mutable disposed = false

    member __.FileName = inner.FileName
    member __.Uri = inner.Uri

    /// The document's in-memory content as last opened/changed on this handle. Plain
    /// read-only snapshot, not a mutation vector.
    member __.GetFileContents() = inner.GetFileContents()

    /// Applies `TextEdit`s to the in-memory content locally (a pure string transform, not
    /// a server call) — used to check a computed edit (e.g. from `textDocument/rename`)
    /// against an expected result without ever sending `Change`/`Save`.
    member __.GetFileContentsWithTextEditsApplied(tes: TextEdit[]) =
        inner.GetFileContentsWithTextEditsApplied(tes)

    interface IDisposable with
        member __.Dispose() =
            if not disposed then
                disposed <- true
                onDispose ()
                (inner :> IDisposable).Dispose()

/// A leased, read-only view onto a warm, pooled `LspTestClient`. Disposing returns the
/// underlying instance to the pool (or discards it if unhealthy) instead of tearing the
/// server down, and force-closes any documents the lease opened but didn't close itself.
///
/// A given lease is only ever used by the one test that rented it (that exclusivity is the
/// whole point of the pool), so tracking the handles it opens needs no locking here either
/// — it's plain, single-threaded, per-lease bookkeeping.
type PooledLspTestClient internal (fixtureName: string, client: LspTestClient) =
    let openHandles = ResizeArray<LspDocumentHandle>()
    let mutable disposed = false

    /// The temp directory the pooled fixture's solution was loaded from. Plain read-only
    /// string, safe to pass through as-is — unlike `GetState()` (not exposed here), it
    /// carries no mutable/live handles (e.g. the server `Process`) a test could abuse.
    member __.SolutionDir = client.SolutionDir

    /// The server's advertised capabilities, snapshotted at `initialize` time. A narrow
    /// slice of `GetState()` rather than the full record — deliberately doesn't expose the
    /// live server `Process` (or other mutable-ish fields) the way `GetState()` would.
    member __.ServerCapabilities = client.GetState().ServerCapabilities

    member __.Request<'Request, 'Response>(method: string, request: 'Request) : 'Response =
        if not (readOnlyRequestMethods.Contains method) then
            failwithf
                "PooledLspTestClient.Request: \"%s\" is not on the pooled read-only allow-list; pooled fixtures must never mutate server state. Allow-listed: %s"
                method
                (String.concat ", " readOnlyRequestMethods)

        client.Request<'Request, 'Response>(method, request)

    member __.Open(filename: string) : PooledLspDocumentHandle =
        let handle = client.Open(filename)
        openHandles.Add handle
        new PooledLspDocumentHandle(handle, (fun () -> openHandles.Remove handle |> ignore))

    /// Opens `filename` with synthetic `text` instead of its on-disk contents — never
    /// written to disk (no `.Save`), so the shared fixture's on-disk state stays
    /// untouched; the server's in-memory view reverts to disk once this handle is closed
    /// (tracked/force-closed the same way `Open` is).
    member __.OpenWithText(filename: string, text: string) : PooledLspDocumentHandle =
        let handle = client.OpenWithText(filename, text)
        openHandles.Add handle
        new PooledLspDocumentHandle(handle, (fun () -> openHandles.Remove handle |> ignore))

    interface IDisposable with
        member __.Dispose() =
            if not disposed then
                disposed <- true

                for handle in List.ofSeq openHandles do
                    (handle :> IDisposable).Dispose()

                openHandles.Clear()
                checkIn fixtureName client

/// Leases a warm `LspTestClient` for `fixtureName` from its pool, booting one if needed
/// (bounded and staggered — see `drainWaiters` / `maxPoolSize`). Any fixture name that
/// `LoadSolution` accepts can be leased; mutation-safety is enforced structurally on the
/// returned `PooledLspTestClient` instead (see module doc comment), not by curating
/// fixture names here.
let rentFixture (fixtureName: string) : PooledLspTestClient =
    match poolManager.PostAndReply(fun rc -> Rent(fixtureName, rc)) with
    | Ok client -> new PooledLspTestClient(fixtureName, client)
    | Error ex -> raise ex

/// Disposes every idle pooled instance across all fixture pools. Called once from
/// `PoolTeardown.OneTimeTearDown` after the whole assembly's tests have finished; by that
/// point every `PooledLspTestClient` lease has already been disposed and checked its
/// instance back in, so nothing should still be checked out.
let disposeAll () =
    poolManager.PostAndReply(fun rc -> DisposeAll rc)

/// Namespace-scoped `SetUpFixture` — NUnit applies it to every test in the
/// `CSharpLanguageServer.Tests` namespace (i.e. every test module in this project, all of
/// which are declared as `module CSharpLanguageServer.Tests.<Name>`). Runs once after the
/// whole assembly's tests finish.
[<SetUpFixture>]
type PoolTeardown() =
    [<OneTimeTearDown>]
    member __.TearDown() = disposeAll ()
