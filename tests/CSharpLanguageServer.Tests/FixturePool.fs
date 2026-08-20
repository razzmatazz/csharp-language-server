/// A pool of warm, reusable `LspTestClient` instances for read-only test fixtures.
///
/// Motivation: many tests (e.g. `InlayHintTests`) each spin up their own server process
/// and load the same, unchanging fixture solution just to issue a handful of read-only
/// requests. That solution-load cost dominates their runtime. Since these tests never
/// mutate documents, many of them can safely take turns using a small number of shared,
/// already-loaded server instances instead of each paying for their own.
///
/// Implementation note: all pool bookkeeping (idle instances, pending waiters, per-fixture
/// created counts) lives as plain immutable state inside a single `MailboxProcessor` actor
/// — the same pattern `LspTestClient` itself already uses internally (see `Tooling.fs`).
/// Because the actor processes messages strictly one at a time, no `SemaphoreSlim`,
/// `Interlocked`, or explicit locks are needed anywhere: booting a fresh instance inline
/// inside a message handler *is* the cross-fixture "don't boot two servers at once"
/// serialization, for free, and handing an idle/newly-booted instance to the oldest
/// pending renter is just sequential list manipulation.
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

open System
open NUnit.Framework

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
          "textDocument/linkedEditingRange" ]

/// Deliberately a minority share of `activeClientsSemaphore`'s total budget (see
/// `Tooling.fs`) — pooled instances stay alive for the whole test run once booted, so
/// this must leave headroom for every other file's ad-hoc `activateFixture` calls still
/// running concurrently.
let private maxPoolSize () =
    max 2 (min 4 (Environment.ProcessorCount / 4))

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
    | DisposeAll of AsyncReplyChannel<unit>

/// Per-fixture-name pool state. `CreatedCount` tracks how many live instances exist (idle
/// + currently leased) so we never boot more than `MaxSize` concurrently for this fixture.
/// `Waiters` holds reply channels for `Rent` calls that arrived while the pool was at
/// capacity with no idle instance — satisfied in order as instances become available.
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
/// many pending waiters as possible: first from `Idle`, then by booting fresh instances
/// (bounded by `MaxSize`) — this recursion is what actually boots a fresh server, and
/// since it only ever runs on the single pool-actor thread, at most one boot is ever in
/// flight at a time, across *all* fixture names.
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
        match tryBoot fixtureName with
        | Ok client ->
            rc.Reply(Ok client)

            drainWaiters
                fixtureName
                { fps with
                    Waiters = restWaiters
                    CreatedCount = fps.CreatedCount + 1 }
        | Error ex ->
            rc.Reply(Error ex)
            drainWaiters fixtureName { fps with Waiters = restWaiters }
    | _ -> fps

let private processMessage (state: Map<string, FixturePoolState>) (msg: PoolMessage) : Map<string, FixturePoolState> =
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

    | DisposeAll rc ->
        for KeyValue(_, fps) in state do
            for client in fps.Idle do
                (client :> IDisposable).Dispose()

        rc.Reply(())
        Map.empty

let private poolManager =
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
                    | DisposeAll rc -> rc.Reply(())
                    | CheckIn _ -> ()

                    state

            return! loop newState
        }

        loop Map.empty)

let private checkIn fixtureName (client: LspTestClient) =
    poolManager.Post(CheckIn(fixtureName, client))

/// A document handle leased from a `PooledLspTestClient`. Deliberately narrower than
/// `LspDocumentHandle` — no `Change`/`Save` — since pooled fixtures are shared by many
/// tests in sequence and must stay read-only.
type PooledLspDocumentHandle internal (inner: LspDocumentHandle, onDispose: unit -> unit) =
    let mutable disposed = false

    member __.FileName = inner.FileName
    member __.Uri = inner.Uri

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
