/// A pool of warm, reusable `LspTestClient` instances for read-only test fixtures.
///
/// Motivation: many tests (e.g. `InlayHintTests`) each spin up their own server process
/// and load the same, unchanging fixture solution just to issue a handful of read-only
/// requests. That solution-load cost dominates their runtime. Since these tests never
/// mutate documents, many of them can safely take turns using a small number of shared,
/// already-loaded server instances instead of each paying for their own.
///
/// Ownership: this module owns `activeClientsSemaphore`, the sole gate on how many
/// `LspTestClient` server processes may be alive (booting, leased out, or idle-pooled) at
/// once. `bootClientAsync` below is the *only* place in the whole test assembly that
/// constructs an `LspTestClient` (see its `releaseSlot` callback, which is how `Dispose`
/// -- in `Tooling.fs` -- reports a slot free again without `Tooling.fs` needing to know
/// what a "slot" is). Crucially, *every* boot goes through this same admission path --
/// including plain, single-use, ad-hoc activations (`activateFixture`/`activateFixtureExt`,
/// used e.g. by tests that mutate documents and so can never be pooled). Previously those
/// bypassed the pool entirely and raced it for semaphore permits with zero visibility into
/// each other; since pooled instances are designed to sit alive+idle for arbitrarily long
/// stretches, that let the pool alone starve every ad-hoc caller permanently once it had
/// claimed enough distinct fixtures to exhaust the semaphore (a real deadlock: nothing
/// would ever `Release()` again until the whole assembly's teardown, which itself can't
/// run until the stuck ad-hoc test finishes). Funnelling both paths through the same actor
/// closes that gap: it can see every idle instance across every fixture at once and evict
/// one on demand to make room, something a stuck caller blocked on a raw `SemaphoreSlim`
/// never could.
///
/// Idle instances are freed (semaphore permit released) two ways:
///   - Reactively, right before any new boot is committed to (`evictIfSaturated`, called
///     from `drainWaiters`'s boot branch and from `AdHocBoot`) if the semaphore currently
///     has zero free permits: the globally-oldest idle instance (any fixture) is evicted
///     to guarantee the upcoming boot won't wait forever. Also checked once more, from the
///     other direction, right when an instance would otherwise become newly idle
///     (`CheckIn`) -- closes the gap for a caller *already* blocked inside
///     `activeClientsSemaphore.WaitAsync` at that exact moment, which the boot-time check
///     above can't reach after the fact.
///   - Proactively, on a TTL: a `System.Threading.Timer` (`PoolState.SweepTimer`) posts
///     `EvictExpiredIdle` every `idleSweepInterval`, disposing any instance that's been
///     idle longer than `idleTtl` regardless of contention, so long idle stretches don't
///     just hold processes/memory open for nothing. The timer's whole lifecycle --
///     creation and disposal -- is itself owned by `poolManager`'s own state machine, via
///     the `Initialize` (posted once, right after the actor starts) and `Shutdown`
///     (posted once, from `disposeAll`, at the very end of the assembly's test run)
///     messages, rather than a freestanding loop/resource outside the actor.
///
/// Implementation note: pool bookkeeping (idle instances + their idle-since timestamp,
/// pending waiters, per-fixture created counts, and now the sweep timer handle -- all of
/// it, see `PoolState`) lives as plain immutable state inside a `MailboxProcessor` actor,
/// `poolManager`. Because it processes messages strictly one at a time, no `SemaphoreSlim`,
/// `Interlocked`, or explicit locks are needed for that bookkeeping. Booting itself is
/// *not* awaited inline inside `poolManager`'s message loop (that would block every other
/// fixture's `Rent`/`CheckIn` for as long as the boot takes, e.g. while waiting on a scarce
/// semaphore permit) -- instead `startPooledBoot`/`AdHocBoot` fire the boot as a plain
/// `Async.StartWithContinuations` and let `poolManager` continue servicing other messages;
/// the boot reports its own outcome back via a `BootCompleted`/`AdHocBootCompleted`
/// message once it finishes, whenever that ends up being. There's deliberately no separate
/// actor/thread for this (an earlier version had one, `bootWorker`) -- a self-contained
/// fire-and-post-back `Async` keeps all pool state in exactly one place or MailboxProcessor,
/// easier to reason about than coordinating two.
///
/// This module is deliberately narrow in scope beyond that:
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
///   - Ad-hoc (`activateFixture`/`activateFixtureExt`) instances are never added to any
///     fixture's `Idle` list — they're leased exactly once, to exactly one caller, and torn
///     down for good (never checked back in) by that caller's own `Dispose`, same as before
///     this module took over booting them. Only their *admission* (getting a semaphore
///     permit, possibly evicting an idle pooled instance to get one) goes through the pool.
module CSharpLanguageServer.Tests.Fixtures

// `drainWaiters`/`processMessage`/`poolManager`/`startPooledBoot` below are a mutually
// recursive group of `let rec ... and ...` bindings (some of them MailboxProcessor
// *values*, not functions), which triggers FS0040 ("recursive references ... checked for
// initialization-soundness at runtime through a delayed reference"). This is the standard,
// sound pattern for actors/helpers that post to each other: none of their bodies touch
// `poolManager` until a message is actually processed, which can only happen after the
// whole group is fully constructed (nothing posts to `poolManager` during module
// initialization) — so the delayed-reference check FS0040 warns about always succeeds here.
#nowarn "40"

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open System.Xml.Linq

open NUnit.Framework
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Tests.Tooling

/// No-op fixture-dir patch, for use when `activateFixtureExt` doesn't need to modify the
/// fixture's temp-copied files before the server starts.
let emptyFixturePatch _ = ()

/// Returns a `patchFixtureDir` callback (see `activateFixtureExt`) that rewrites
/// `<TargetFramework>` in every `.csproj` under the fixture — used to run the same
/// fixture against multiple target framework monikers.
let patchFixtureWithTfm newTfm =
    let updateTfmInSubdir (rootDir: string) =
        let csprojs = Directory.GetFiles(rootDir, "*.csproj", SearchOption.AllDirectories)

        for file in csprojs do
            let doc = file |> XDocument.Load

            let tfm =
                doc.Descendants() |> Seq.tryFind (fun e -> e.Name.LocalName = "TargetFramework")

            match tfm with
            | Some elem ->
                elem.Value <- newTfm
                doc.Save file
            | None -> ()

    updateTfmInSubdir

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

/// Deliberately a minority share of `activeClientsSemaphore`'s total budget — pooled
/// instances stay alive for a while once booted, so this must leave headroom for every
/// other file's ad-hoc `activateFixture` calls still running concurrently. Floor is 1, not
/// 2: on a small CI runner (e.g. 4 cores) a floor of 2 would force the pool to permanently
/// claim half the semaphore's entire budget for just one fixture. Note this is no longer
/// the only thing standing between the pool and starving the rest of the suite — see the
/// module doc comment's "Idle instances are freed" section for the eviction machinery that
/// backstops it even if this cap is exceeded across several distinct fixtures at once.
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

/// How long a pooled instance may sit idle before the periodic sweep (`EvictExpiredIdle`)
/// disposes it, releasing its `activeClientsSemaphore` permit. Reactive eviction
/// (`evictIfSaturated`/the `CheckIn` check) already rules out outright deadlock regardless
/// of this value; the TTL exists to free processes/memory sooner during long idle
/// stretches, not just once something else is actively contending for a permit.
let private idleTtl = TimeSpan.FromMinutes 2.0
let private idleSweepInterval = TimeSpan.FromSeconds 30.0

/// The sole admission gate for every `LspTestClient` this assembly ever constructs — both
/// pooled boots (`startPooledBoot`) and ad-hoc ones (`activateFixtureExt`, via `AdHocBoot`)
/// go through `bootClientAsync` below, which is the only place that calls `new LspTestClient`.
let private activeClientsSemaphore =
    // Analyzers are disabled by default in tests (see buildConfigurationResponse), so the
    // per-test CPU cost is low enough to run one server per logical core safely.
    let concurrency = Environment.ProcessorCount
    new SemaphoreSlim(concurrency, concurrency)

/// Reply payload for a boot: either a fresh client, or the exception that occurred while
/// trying to boot one (so a boot failure surfaces on the caller's thread instead of
/// hanging `PostAndReply` forever or killing the pool actor).
type private RentReply = Result<LspTestClient, exn>

/// Acquires a permit, then constructs and loads a fresh `LspTestClient` — the only place
/// in this assembly that does either. On failure (including a `LoadSolution` failure after
/// the client was already constructed) the client's own `Dispose` releases the permit via
/// `releaseSlot`, so callers never need to release it themselves.
let private bootClientAsync
    (fixtureName: string)
    (clientProfile: LspClientProfile)
    (patchFixtureDir: string -> unit)
    (initializeParamsUpdate: InitializeParams -> InitializeParams)
    : Async<RentReply> =
    async {
        do! activeClientsSemaphore.WaitAsync() |> Async.AwaitTask

        let client =
            new LspTestClient(clientProfile, (fun () -> activeClientsSemaphore.Release() |> ignore))

        try
            client.LoadSolution(fixtureName, patchFixtureDir, initializeParamsUpdate)
            return Ok client
        with ex ->
            (client :> IDisposable).Dispose()
            return Error ex
    }

/// Tears a client down off the calling thread — used for eviction, where we want to free
/// the semaphore permit for someone else without making `poolManager`'s single-threaded
/// message loop wait on the teardown IPC (server stop request, process kill, etc.).
let private disposeInBackground (client: LspTestClient) =
    Async.Start(async { (client :> IDisposable).Dispose() })

/// Per-fixture-name pool state. `CreatedCount` tracks how many live instances exist (idle
/// + currently leased, *including* ones a boot has been reserved for but hasn't finished
/// yet) so we never kick off more than `maxPoolSize ()` concurrent boots for this fixture.
/// `Idle` instances carry the `DateTime` they became idle at, used both by TTL eviction and
/// to pick the globally-oldest instance to evict under pressure (`evictOldestIdle`).
/// `Waiters` holds reply channels for `Rent` calls that arrived while the pool had no idle
/// instance and no room to grow — satisfied in order as instances become available.
type private FixturePoolState =
    { Idle: (LspTestClient * DateTime) list
      Waiters: AsyncReplyChannel<RentReply> list
      CreatedCount: int }

let private emptyFixturePoolState =
    { Idle = []
      Waiters = []
      CreatedCount = 0 }

/// Evicts the single globally-oldest idle instance across *every* fixture's pool (not just
/// one particular fixture) — frees exactly one `activeClientsSemaphore` permit, eventually
/// (`disposeInBackground` doesn't block on it). No-op if nothing is currently idle anywhere.
let private evictOldestIdle (state: Map<string, FixturePoolState>) : Map<string, FixturePoolState> =
    let oldest =
        state
        |> Map.toSeq
        |> Seq.collect (fun (name, fps) -> fps.Idle |> Seq.map (fun (client, since) -> name, client, since))
        |> Seq.sortBy (fun (_, _, since) -> since)
        |> Seq.tryHead

    match oldest with
    | None -> state
    | Some(fixtureName, client, _) ->
        disposeInBackground client

        state
        |> Map.change
            fixtureName
            (Option.map (fun fps ->
                { fps with
                    Idle = fps.Idle |> List.filter (fun (c, _) -> not (obj.ReferenceEquals(c, client)))
                    CreatedCount = fps.CreatedCount - 1 }))

/// Evicts one idle instance (any fixture) iff `activeClientsSemaphore` currently has zero
/// free permits — called right before committing to any new boot, pooled or ad-hoc, so
/// that boot's `WaitAsync` is guaranteed not to wait forever on a semaphore that's fully
/// (and, absent this, permanently) claimed by other idle pooled instances.
let private evictIfSaturated (state: Map<string, FixturePoolState>) : Map<string, FixturePoolState> =
    if activeClientsSemaphore.CurrentCount = 0 then
        evictOldestIdle state
    else
        state

/// All state `poolManager` owns: the per-fixture pools plus the idle-TTL sweep timer's
/// handle. `SweepTimer` is `None` until `Initialize` runs (right after the actor starts)
/// and disposed/cleared again by `Shutdown` (at assembly teardown, via `disposeAll`) --
/// see the module doc comment.
type private PoolState =
    { Fixtures: Map<string, FixturePoolState>
      SweepTimer: Timer option }

let private emptyPoolState = { Fixtures = Map.empty; SweepTimer = None }

/// Every message `poolManager` handles. Ad-hoc boots (`AdHocBoot`/`AdHocBootCompleted`)
/// are deliberately not `Rent`/`CheckIn`: a booted ad-hoc instance is never tracked in any
/// `FixturePoolState` — it's leased exactly once and torn down for good by its caller, so
/// there's nothing to check back in and no per-fixture bookkeeping needed for it, only the
/// shared admission gate (`bootClientAsync`, possibly preceded by `evictIfSaturated`).
/// `Initialize`/`Shutdown` own the sweep timer's setup/teardown (see `PoolState`).
[<NoComparison; NoEquality>]
type private PoolMessage =
    | Initialize
    | Rent of fixtureName: string * AsyncReplyChannel<RentReply>
    | CheckIn of fixtureName: string * LspTestClient
    | BootCompleted of fixtureName: string * AsyncReplyChannel<RentReply> * RentReply
    | AdHocBoot of
        fixtureName: string *
        clientProfile: LspClientProfile *
        patchFixtureDir: (string -> unit) *
        initializeParamsUpdate: (InitializeParams -> InitializeParams) *
        AsyncReplyChannel<RentReply>
    | AdHocBootCompleted of AsyncReplyChannel<RentReply> * RentReply
    | EvictExpiredIdle
    | Shutdown of AsyncReplyChannel<unit>

/// After any change that might free up capacity or add an idle instance, satisfies as
/// many pending waiters as possible from `Idle`. If none are idle but there's still room
/// to grow (`CreatedCount < maxPoolSize ()`), reserves the slot immediately (so a burst of
/// concurrent `Rent` calls can't over-request boots), evicts an idle instance elsewhere if
/// the semaphore looks saturated, and fires the actual boot off as a background `Async`
/// that reports back via `BootCompleted` whenever it finishes — this function itself never
/// blocks, regardless of how long that boot ends up taking.
let rec private drainWaiters (fixtureName: string) (state: Map<string, FixturePoolState>) : Map<string, FixturePoolState> =
    let fps = state |> Map.tryFind fixtureName |> Option.defaultValue emptyFixturePoolState

    match fps.Waiters, fps.Idle with
    | rc :: restWaiters, (client, _since) :: restIdle ->
        rc.Reply(Ok client)

        drainWaiters
            fixtureName
            (state
             |> Map.add
                 fixtureName
                 { fps with
                     Waiters = restWaiters
                     Idle = restIdle })
    | rc :: restWaiters, [] when fps.CreatedCount < maxPoolSize () ->
        // Evict here, right before committing to the boot -- not any earlier -- so it can
        // never cannibalize an idle instance this very fixture could otherwise have handed
        // straight to `rc` in the branch above.
        let state = evictIfSaturated state
        startPooledBoot fixtureName rc

        drainWaiters
            fixtureName
            (state
             |> Map.add
                 fixtureName
                 { fps with
                     Waiters = restWaiters
                     CreatedCount = fps.CreatedCount + 1 })
    | _ -> state

and private processMessage (state: PoolState) (msg: PoolMessage) : PoolState =
    match msg with
    | Initialize ->
        match state.SweepTimer with
        | Some _ -> state // already initialized; stay idempotent rather than leak a second timer
        | None ->
            let timer =
                new Timer((fun _ -> poolManager.Post EvictExpiredIdle), null, idleSweepInterval, idleSweepInterval)

            { state with
                SweepTimer = Some timer }

    | Rent(fixtureName, rc) ->
        let fps =
            state.Fixtures |> Map.tryFind fixtureName |> Option.defaultValue emptyFixturePoolState

        { state with
            Fixtures =
                drainWaiters
                    fixtureName
                    (state.Fixtures
                     |> Map.add
                         fixtureName
                         { fps with
                             Waiters = fps.Waiters @ [ rc ] }) }

    | CheckIn(fixtureName, client) ->
        let fps =
            state.Fixtures |> Map.tryFind fixtureName |> Option.defaultValue emptyFixturePoolState

        let fps =
            if isHealthy client then
                client.ResetAccumulatedState()

                if activeClientsSemaphore.CurrentCount = 0 then
                    // Fully saturated right now: don't let this instance sit idle holding
                    // a permit nobody else can get at -- free it immediately instead. A
                    // caller already blocked inside `WaitAsync` at this exact moment isn't
                    // reachable by `evictIfSaturated` (which only runs when *starting* a
                    // new boot) or by the TTL sweep (which only runs periodically) -- this
                    // closes that specific gap.
                    disposeInBackground client

                    { fps with
                        CreatedCount = fps.CreatedCount - 1 }
                else
                    { fps with
                        Idle = (client, DateTime.UtcNow) :: fps.Idle }
            else
                disposeInBackground client

                { fps with
                    CreatedCount = fps.CreatedCount - 1 }

        { state with
            Fixtures = drainWaiters fixtureName (state.Fixtures |> Map.add fixtureName fps) }

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
                state.Fixtures |> Map.tryFind fixtureName |> Option.defaultValue emptyFixturePoolState

            { state with
                Fixtures =
                    drainWaiters
                        fixtureName
                        (state.Fixtures
                         |> Map.add
                             fixtureName
                             { fps with
                                 CreatedCount = fps.CreatedCount - 1 }) }

    | AdHocBoot(fixtureName, clientProfile, patchFixtureDir, initializeParamsUpdate, rc) ->
        let fixtures = evictIfSaturated state.Fixtures

        Async.StartWithContinuations(
            bootClientAsync fixtureName clientProfile patchFixtureDir initializeParamsUpdate,
            (fun result -> poolManager.Post(AdHocBootCompleted(rc, result))),
            (fun ex -> poolManager.Post(AdHocBootCompleted(rc, Error ex))),
            (fun _ -> ())
        )

        { state with Fixtures = fixtures }

    | AdHocBootCompleted(rc, result) ->
        rc.Reply(result)
        state

    | EvictExpiredIdle ->
        let now = DateTime.UtcNow

        { state with
            Fixtures =
                state.Fixtures
                |> Map.map (fun _ fps ->
                    let expired, fresh =
                        fps.Idle |> List.partition (fun (_, since) -> now - since > idleTtl)

                    for client, _ in expired do
                        disposeInBackground client

                    { fps with
                        Idle = fresh
                        CreatedCount = fps.CreatedCount - expired.Length }) }

    | Shutdown rc ->
        state.SweepTimer |> Option.iter (fun t -> t.Dispose())

        for KeyValue(_, fps) in state.Fixtures do
            for client, _ in fps.Idle do
                (client :> IDisposable).Dispose()

        rc.Reply(())
        emptyPoolState

and private poolManager: MailboxProcessor<PoolMessage> =
    let mp =
        MailboxProcessor<PoolMessage>.Start(fun inbox ->
            let rec loop (state: PoolState) = async {
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
                        | AdHocBoot(_, _, _, _, rc) -> rc.Reply(Error ex)
                        | AdHocBootCompleted(rc, _) -> rc.Reply(Error ex)
                        | Shutdown rc -> rc.Reply(())
                        | Initialize
                        | CheckIn _
                        | EvictExpiredIdle -> ()

                        state

                return! loop newState
            }

            loop emptyPoolState)

    // Kicks off the idle-TTL sweep timer (see `PoolState`/`Initialize` above) once the
    // actor is up and processing messages.
    mp.Post Initialize
    mp

/// Fires a pooled boot for `fixtureName` off `poolManager`'s thread and reports the
/// outcome back via `BootCompleted` once it finishes (success or failure), whenever that
/// ends up being — see the module doc comment for why this must never block the caller.
/// Explicit type annotation: `drainWaiters` (defined above, in the same `let rec ... and
/// ...` group) calls this before the compiler has otherwise inferred its type from its own
/// definition here — without the annotation that lookup is of indeterminate type (FS0072).
and private startPooledBoot (fixtureName: string) (rc: AsyncReplyChannel<RentReply>) : unit =
    Async.StartWithContinuations(
        bootClientAsync fixtureName defaultClientProfile emptyFixturePatch id,
        (fun result -> poolManager.Post(BootCompleted(fixtureName, rc, result))),
        (fun ex -> poolManager.Post(BootCompleted(fixtureName, rc, Error ex))),
        (fun _ -> ())
    )

let private checkIn fixtureName (client: LspTestClient) =
    poolManager.Post(CheckIn(fixtureName, client))

/// Full control: custom profile, fixture dir patch callback, InitializeParams transform.
/// Routed through `poolManager` (`AdHocBoot`) exactly like a pooled boot is — see the
/// module doc comment for why ad-hoc, single-use activations need to share the pool's
/// admission control (and its eviction machinery) rather than racing it independently for
/// `activeClientsSemaphore` permits. The returned instance is never added to any fixture's
/// `Idle` list; disposing it (a plain `LspTestClient.Dispose`, not routed back through the
/// pool) tears it down and releases its slot for good — it's a "spoiled", single-use lease.
let activateFixtureExt
    fixtureName
    clientProfile
    (patchFixtureDir: string -> unit)
    (initializeParamsUpdate: InitializeParams -> InitializeParams)
    : LspTestClient =
    match
        poolManager.PostAndReply(fun rc ->
            AdHocBoot(fixtureName, clientProfile, patchFixtureDir, initializeParamsUpdate, rc))
    with
    | Ok client -> client
    | Error ex -> raise ex

/// Simple: default profile, no fixture patching, no InitializeParams customization.
let activateFixture fixtureName =
    activateFixtureExt fixtureName defaultClientProfile emptyFixturePatch id

/// Shorthand for `activateFixtureExt` with `LoggingEnabled = true`, for debugging a
/// failing test — see AGENTS.md's "Debugging server-side data flow from tests" section.
let activateFixtureWithLoggingEnabled fixtureName =
    activateFixtureExt
        fixtureName
        { defaultClientProfile with
            LoggingEnabled = true }
        emptyFixturePatch
        id

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

    interface ILspRequestClient with
        member this.Request<'Request, 'Response>(method: string, request: 'Request) : 'Response =
            this.Request<'Request, 'Response>(method, request)

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

/// Disposes every idle pooled instance across all fixture pools, and the idle-TTL sweep
/// timer (`Shutdown`, see `PoolState`). Called once from `PoolTeardown.OneTimeTearDown`
/// after the whole assembly's tests have finished; by that point every
/// `PooledLspTestClient` lease has already been disposed and checked its instance back
/// in, so nothing should still be checked out.
let disposeAll () =
    poolManager.PostAndReply(fun rc -> Shutdown rc)

/// Namespace-scoped `SetUpFixture` — NUnit applies it to every test in the
/// `CSharpLanguageServer.Tests` namespace (i.e. every test module in this project, all of
/// which are declared as `module CSharpLanguageServer.Tests.<Name>`). Runs once after the
/// whole assembly's tests finish.
[<SetUpFixture>]
type PoolTeardown() =
    [<OneTimeTearDown>]
    member __.TearDown() = disposeAll ()
