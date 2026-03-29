module CSharpLanguageServer.Tests.RequestSchedulingTests

open System
open NUnit.Framework

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Types
open CSharpLanguageServer.Runtime
open CSharpLanguageServer.Runtime.RequestScheduling
open CSharpLanguageServer.Runtime.ServerStateLoop
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Tests.Tooling

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Creates a dummy AsyncReplyChannel<obj> using a MailboxProcessor.
/// The `onReply` callback receives the value sent through the channel.
let private makeDummyReplyChannel (onReply: obj -> unit) : AsyncReplyChannel<RequestContext> =
    let mbox =
        MailboxProcessor<AsyncReplyChannel<obj>>.Start(fun inbox -> async {
            let! rc = inbox.Receive()
            // We never actually use the channel here; the test grabs it
            // from the PostAndAsyncReply continuation below.
            ignore rc
        })

    // We use PostAndAsyncReply solely to obtain a real AsyncReplyChannel.
    // The reply value is forwarded to `onReply`.
    let _task =
        mbox.PostAndAsyncReply(fun rc ->
            // Wrap the real channel so that Reply goes to onReply.
            rc)
        |> Async.Ignore
        |> Async.Start

    // Build a channel whose Reply delegates to `onReply`.
    // FSharp.Core doesn't expose a public ctor for AsyncReplyChannel, so we
    // create a thin wrapper via an object expression that calls onReply.
    // Unfortunately AsyncReplyChannel is a sealed class, not an interface.
    //
    // Workaround: use a MailboxProcessor.PostAndAsyncReply to capture a real
    // AsyncReplyChannel and store it; then in the test we don't care about
    // the reply value — we only need a valid record field.
    //
    // Simplest approach: spin up a tiny mailbox just to produce the channel.
    let mutable captured: AsyncReplyChannel<RequestContext> option = None

    let agent =
        MailboxProcessor.Start(fun inbox -> async {
            let! _msg = inbox.Receive()
            return ()
        })

    let replyTask =
        agent.PostAndAsyncReply(fun (rc: AsyncReplyChannel<RequestContext>) ->
            captured <- Some rc
            ())

    // Give the agent a moment to process so `captured` is set.
    Async.Sleep(50) |> Async.RunSynchronously

    match captured with
    | Some rc -> rc
    | None -> failwith "Failed to capture AsyncReplyChannel"

/// A simpler helper: produce a ServerRequest record suitable for testing.
/// The ActivationRC is a real channel but we don't exercise it in pure tests.
let private makeTestRequest ordinal name mode phase =
    let rc = makeDummyReplyChannel ignore

    { Phase = phase
      Mode = mode
      Name = name
      RpcOrdinal = ordinal
      Registered = DateTime.UtcNow
      ActivationRC = rc
      RunningSince = (if phase = Running then Some DateTime.UtcNow else None)
      BufferedServerEvents = [] }

let private defaultSettings: CSharpConfiguration = CSharpConfiguration.Default

/// Minimal ILspClient stub for constructing ServerRequestContext in tests.
type private NoOpLspClient() =
    interface System.IDisposable with
        member _.Dispose() = ()

    interface ILspClient with
        member _.WindowWorkDoneProgressCreate(_) = async { return LspResult.Ok() }
        member _.Progress(_) = async { return () }
        member _.WindowShowMessage(_) = async { return () }
        member _.WindowLogMessage(_) = async { return () }
        member _.TelemetryEvent(_) = async { return () }
        member _.TextDocumentPublishDiagnostics(_) = async { return () }
        member _.LogTrace(_) = async { return () }
        member _.CancelRequest(_) = async { return () }
        member _.WorkspaceWorkspaceFolders() = async { return LspResult.Ok None }
        member _.WorkspaceConfiguration(_) = async { return LspResult.Ok [||] }
        member _.WorkspaceSemanticTokensRefresh() = async { return LspResult.Ok() }
        member _.WindowShowDocument(_) = async { return LspResult.Ok { Success = false } }
        member _.WorkspaceInlineValueRefresh() = async { return LspResult.Ok() }
        member _.WorkspaceInlayHintRefresh() = async { return LspResult.Ok() }
        member _.WorkspaceDiagnosticRefresh() = async { return LspResult.Ok() }
        member _.ClientRegisterCapability(_) = async { return LspResult.Ok() }
        member _.ClientUnregisterCapability(_) = async { return LspResult.Ok() }
        member _.WindowShowMessageRequest(_) = async { return LspResult.Ok None }
        member _.WorkspaceCodeLensRefresh() = async { return LspResult.Ok() }

        member _.WorkspaceApplyEdit(_) = async {
            return
                LspResult.Ok
                    { Applied = false
                      FailureReason = None
                      FailedChange = None }
        }

let private makeTestServerRequestContext (mode: RequestMode) =
    RequestContext(
        mode,
        new NoOpLspClient() :> ILspClient,
        CSharpConfiguration.Default,
        LspWorkspace.Empty,
        emptyClientCapabilities,
        false
    )

// ---------------------------------------------------------------------------
// registerRequest
// ---------------------------------------------------------------------------

[<Test>]
let ``registerRequest adds a Pending request to the queue`` () =
    let rc = makeDummyReplyChannel ignore

    let queue =
        RequestQueue.Empty |> registerRequest 1L "textDocument/hover" ReadOnly rc

    Assert.AreEqual(1, queue.Requests.Count)

    let req = queue.Requests.[1L]
    Assert.AreEqual(Pending, req.Phase)
    Assert.AreEqual("textDocument/hover", req.Name)
    Assert.AreEqual(ReadOnly, req.Mode)
    Assert.AreEqual(1L, req.RpcOrdinal)

[<Test>]
let ``registerRequest preserves existing requests`` () =
    let rc1 = makeDummyReplyChannel ignore
    let rc2 = makeDummyReplyChannel ignore

    let queue =
        RequestQueue.Empty
        |> registerRequest 1L "textDocument/hover" ReadOnly rc1
        |> registerRequest 2L "textDocument/rename" ReadWrite rc2

    Assert.AreEqual(2, queue.Requests.Count)
    Assert.IsTrue(queue.Requests.ContainsKey(1L))
    Assert.IsTrue(queue.Requests.ContainsKey(2L))

// ---------------------------------------------------------------------------
// finishRequest
// ---------------------------------------------------------------------------

[<Test>]
let ``finishRequest transitions request to Finished with buffered events`` () =
    let rc = makeDummyReplyChannel ignore

    let queue =
        RequestQueue.Empty |> registerRequest 1L "textDocument/hover" ReadOnly rc

    let events = [ ProcessRequestQueue :> obj ]
    let updated = queue |> finishRequest 1L events

    let req = updated.Requests.[1L]
    Assert.AreEqual(Finished, req.Phase)
    Assert.AreEqual(1, req.BufferedServerEvents.Length)

// ---------------------------------------------------------------------------
// processRequestQueue — Retired cases
// ---------------------------------------------------------------------------

[<Test>]
let ``processRequestQueue returns Retired for lowest-ordinal finished request`` () =
    let queue =
        { RequestQueue.Empty with
            WatermarkRpcOrdinal = 2L
            Requests =
                Map.ofList
                    [ (1L, makeTestRequest 1L "a" ReadOnly Finished)
                      (2L, makeTestRequest 2L "b" ReadOnly Pending) ] }

    let result =
        processRequestQueue defaultSettings makeTestServerRequestContext queue
        |> Async.RunSynchronously

    match result with
    | Retired(retiredRequest, updatedQueue) ->
        Assert.AreEqual("a", retiredRequest.Name)
        Assert.AreEqual(1, updatedQueue.Requests.Count)
        Assert.IsTrue(updatedQueue.Requests.ContainsKey(2L))
    | other -> Assert.Fail($"Expected Retired but got {other}")

[<Test>]
let ``processRequestQueue returns Waiting when head is still Running`` () =
    let queue =
        { RequestQueue.Empty with
            WatermarkRpcOrdinal = 2L
            Requests =
                Map.ofList
                    [ (1L, makeTestRequest 1L "a" ReadOnly Running)
                      (2L, makeTestRequest 2L "b" ReadOnly Finished) ] }

    let result =
        processRequestQueue defaultSettings makeTestServerRequestContext queue
        |> Async.RunSynchronously

    match result with
    | Waiting -> ()
    | other -> Assert.Fail($"Expected Waiting but got {other}")

[<Test>]
let ``processRequestQueue returns Retired skipping running ReadOnlyBackground`` () =
    let queue =
        { RequestQueue.Empty with
            WatermarkRpcOrdinal = 2L
            Requests =
                Map.ofList
                    [ (1L, makeTestRequest 1L "bg" ReadOnlyBackground Running)
                      (2L, makeTestRequest 2L "normal" ReadOnly Finished) ] }

    let result =
        processRequestQueue defaultSettings makeTestServerRequestContext queue
        |> Async.RunSynchronously

    match result with
    | Retired(retiredRequest, _) -> Assert.AreEqual("normal", retiredRequest.Name)
    | other -> Assert.Fail($"Expected Retired but got {other}")

// ---------------------------------------------------------------------------
// enterDrainingMode / enterDispatchingMode
// ---------------------------------------------------------------------------

[<Test>]
let ``enterDrainingMode sets DrainingUpTo with max ordinal`` () =
    let queue =
        { RequestQueue.Empty with
            Requests =
                Map.ofList
                    [ (5L, makeTestRequest 5L "a" ReadOnly Pending)
                      (10L, makeTestRequest 10L "b" ReadOnly Pending) ] }

    let result = enterDrainingMode queue

    Assert.IsTrue(result.IsSome)

    match result.Value.Mode with
    | DrainingUpTo ord -> Assert.AreEqual(10L, ord)
    | _ -> Assert.Fail("Expected DrainingUpTo mode")

[<Test>]
let ``enterDrainingMode returns None when already draining`` () =
    let queue =
        { RequestQueue.Empty with
            Mode = DrainingUpTo 5L }

    let result = enterDrainingMode queue
    Assert.IsTrue(result.IsNone)

[<Test>]
let ``enterDispatchingMode resets to Dispatching`` () =
    let queue =
        { RequestQueue.Empty with
            Mode = DrainingUpTo 5L }

    let result = enterDispatchingMode queue
    Assert.AreEqual(Dispatching, result.Mode)

// ---------------------------------------------------------------------------
// processRequestQueue
// ---------------------------------------------------------------------------

[<Test>]
let ``processRequestQueue returns Waiting on empty queue`` () =
    let result =
        processRequestQueue defaultSettings makeTestServerRequestContext RequestQueue.Empty
        |> Async.RunSynchronously

    match result with
    | Waiting -> ()
    | other -> Assert.Fail($"Expected Waiting but got {other}")

[<Test>]
let ``processRequestQueue returns Waiting when no request can be activated`` () =
    // A RO request is pending but a RW request is already running — blocks it.
    let queue =
        { RequestQueue.Empty with
            WatermarkRpcOrdinal = 2L
            Requests =
                Map.ofList
                    [ (1L, makeTestRequest 1L "rw" ReadWrite Running)
                      (2L, makeTestRequest 2L "ro" ReadOnly Pending) ] }

    let result =
        processRequestQueue defaultSettings makeTestServerRequestContext queue
        |> Async.RunSynchronously

    match result with
    | Waiting -> ()
    | other -> Assert.Fail($"Expected Waiting but got {other}")

[<Test>]
let ``processRequestQueue activates a pending ReadOnly request`` () =
    let rc = makeDummyReplyChannel ignore

    let queue =
        RequestQueue.Empty |> registerRequest 1L "textDocument/hover" ReadOnly rc

    let result =
        processRequestQueue defaultSettings makeTestServerRequestContext queue
        |> Async.RunSynchronously

    match result with
    | Activated(activatedRequest, _updatedQueue) ->
        Assert.AreEqual("textDocument/hover", activatedRequest.Name)
        Assert.AreEqual(ReadOnly, activatedRequest.Mode)
        Assert.AreEqual(Running, activatedRequest.Phase)
        Assert.IsTrue(activatedRequest.RunningSince.IsSome)
    | other -> Assert.Fail($"Expected Activated but got {other}")

[<Test>]
let ``processRequestQueue activates a pending ReadWrite request when queue is idle`` () =
    let rc = makeDummyReplyChannel ignore

    let queue =
        RequestQueue.Empty |> registerRequest 1L "textDocument/rename" ReadWrite rc

    let result =
        processRequestQueue defaultSettings makeTestServerRequestContext queue
        |> Async.RunSynchronously

    match result with
    | Activated(activatedRequest, _updatedQueue) ->
        Assert.AreEqual("textDocument/rename", activatedRequest.Name)
        Assert.AreEqual(ReadWrite, activatedRequest.Mode)
        Assert.AreEqual(Running, activatedRequest.Phase)
        Assert.IsTrue(activatedRequest.RunningSince.IsSome)
    | other -> Assert.Fail($"Expected Activated but got {other}")

// ---------------------------------------------------------------------------
// Gap-in-sequence tests
//
// Requests may arrive out of order and there may be gaps in the ordinal
// sequence. A gap represents an ordinal whose request has not yet been
// registered. Since we do not know the mode of the missing request, we must
// assume it *could* be ReadWrite and therefore block activation of any
// request past the gap. These tests verify that behaviour.
// ---------------------------------------------------------------------------

[<Test>]
let ``processRequestQueue does not activate ReadOnly request when there is a gap before it`` () =
    // Ordinals 1 and 3 are registered, but ordinal 2 is missing (not yet arrived).
    // The missing ordinal 2 could be a ReadWrite request, so ordinal 3 (ReadOnly)
    // must NOT be activated.
    let rc1 = makeDummyReplyChannel ignore
    let rc3 = makeDummyReplyChannel ignore

    let queue =
        RequestQueue.Empty
        |> registerRequest 1L "textDocument/completion" ReadOnly rc1
        |> registerRequest 3L "textDocument/hover" ReadOnly rc3

    // Simulate ordinal 1 already running — ordinal 3 should still wait because
    // ordinal 2 is missing.
    let queue =
        { queue with
            Requests =
                queue.Requests
                |> Map.add
                    1L
                    { queue.Requests.[1L] with
                        Phase = Running
                        RunningSince = Some DateTime.UtcNow } }

    let result =
        processRequestQueue defaultSettings makeTestServerRequestContext queue
        |> Async.RunSynchronously

    match result with
    | Waiting -> ()
    | other -> Assert.Fail($"Expected Waiting (gap at ordinal 2 could be RW) but got {other}")

[<Test>]
let ``processRequestQueue does not activate ReadWrite request when there is a gap before it`` () =
    // Ordinal 2 is registered as ReadWrite but ordinal 1 has not yet arrived.
    // The missing ordinal 1 could be ReadWrite, so ordinal 2 must not start
    // until we know what ordinal 1 is.
    let rc = makeDummyReplyChannel ignore

    let queue =
        RequestQueue.Empty |> registerRequest 2L "textDocument/rename" ReadWrite rc

    let result =
        processRequestQueue defaultSettings makeTestServerRequestContext queue
        |> Async.RunSynchronously

    match result with
    | Waiting -> ()
    | other -> Assert.Fail($"Expected Waiting (gap at ordinal 1 could be RW) but got {other}")

[<Test>]
let ``processRequestQueue does not activate ReadOnly past a gap even when earlier requests are running`` () =
    // Ordinals 1, 2, 5 are registered. Ordinals 3 and 4 are missing.
    // Ordinal 1 is Running (ReadOnly), ordinal 2 is Running (ReadOnly),
    // ordinal 5 is Pending (ReadOnly).
    // Even though all registered requests before 5 are running RO, the gap
    // at ordinals 3-4 means we cannot activate ordinal 5 — the missing
    // requests could be ReadWrite.
    let queue =
        { RequestQueue.Empty with
            Requests =
                Map.ofList
                    [ (1L, makeTestRequest 1L "textDocument/hover" ReadOnly Running)
                      (2L, makeTestRequest 2L "textDocument/completion" ReadOnly Running)
                      (5L, makeTestRequest 5L "textDocument/references" ReadOnly Pending) ] }

    let result =
        processRequestQueue defaultSettings makeTestServerRequestContext queue
        |> Async.RunSynchronously

    match result with
    | Waiting -> ()
    | other -> Assert.Fail($"Expected Waiting (gap at ordinals 3-4 could be RW) but got {other}")

[<Test>]
let ``canActivateRequest returns false for ReadOnly request past a gap`` () =
    // Direct unit test for canActivateRequest: ordinals 1 (Running) and 3 (Pending)
    // exist, ordinal 2 is missing. canActivateRequest for ordinal 3 should return false.
    let queue =
        { RequestQueue.Empty with
            Requests =
                Map.ofList
                    [ (1L, makeTestRequest 1L "a" ReadOnly Running)
                      (3L, makeTestRequest 3L "b" ReadOnly Pending) ] }

    let pendingRequests = eligiblePendingRequests queue
    let request3 = queue.Requests.[3L]

    let canActivate = canActivateRequest queue pendingRequests (3L, request3)

    Assert.IsFalse(canActivate, "Should not activate a request past a gap in the ordinal sequence")

[<Test>]
let ``canActivateRequest returns false for ReadWrite request with gap before it`` () =
    // Only ordinal 5 is registered as ReadWrite, ordinals 1-4 have not arrived.
    // It must not be activated because we don't know what ordinals 1-4 are.
    let queue =
        { RequestQueue.Empty with
            Requests = Map.ofList [ (5L, makeTestRequest 5L "textDocument/rename" ReadWrite Pending) ] }

    let pendingRequests = eligiblePendingRequests queue
    let request5 = queue.Requests.[5L]

    let canActivate = canActivateRequest queue pendingRequests (5L, request5)

    Assert.IsFalse(canActivate, "Should not activate RW request when earlier ordinals have not arrived")

[<Test>]
let ``processRequestQueue returns Drained when all requests up to drain ordinal are retired`` () =
    // DrainingUpTo 0 with no requests at or below ordinal 0 → immediately drained.
    let queue =
        { RequestQueue.Empty with
            Mode = DrainingUpTo 0L
            Requests = Map.ofList [ (5L, makeTestRequest 5L "later" ReadOnly Pending) ] }

    let result =
        processRequestQueue defaultSettings makeTestServerRequestContext queue
        |> Async.RunSynchronously

    match result with
    | Drained -> ()
    | other -> Assert.Fail($"Expected Drained but got {other}")

[<Test>]
let ``processRequestQueue activates ReadWrite request when only ReadOnlyBackground is running`` () =
    // A running ReadOnlyBackground request must not block a pending ReadWrite request.
    let queue =
        { RequestQueue.Empty with
            WatermarkRpcOrdinal = 2L
            Requests =
                Map.ofList
                    [ (1L, makeTestRequest 1L "background" ReadOnlyBackground Running)
                      (2L, makeTestRequest 2L "textDocument/rename" ReadWrite Pending) ] }

    let result =
        processRequestQueue defaultSettings makeTestServerRequestContext queue
        |> Async.RunSynchronously

    match result with
    | Activated(activatedRequest, _updatedQueue) ->
        Assert.AreEqual("textDocument/rename", activatedRequest.Name)
        Assert.AreEqual(ReadWrite, activatedRequest.Mode)
        Assert.AreEqual(Running, activatedRequest.Phase)
        Assert.IsTrue(activatedRequest.RunningSince.IsSome)
    | other -> Assert.Fail($"Expected Activated but got {other}")
