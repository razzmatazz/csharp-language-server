module CSharpLanguageServer.Tests.JsonRpcTests

open System
open System.IO
open System.Text
open System.Text.Json
open System.Threading

open NUnit.Framework

open CSharpLanguageServer.Runtime.JsonRpc

/// Local copy of CSharpLanguageServer.Util.nonNull — kept independent of the server's
/// internals so this test file's null-safety plumbing doesn't depend on production code.
/// NOTE: uses the `'T | null` union annotation rather than the classic `'T when 'T: null`
/// constraint, since only the former actually clears the nullability of the return type
/// under F#'s nullness checker (the constraint form still infers 'T as nullable downstream).
let private nonNull name (value: 'T | null) : 'T =
    match value |> Option.ofObj with
    | Some v -> v
    | None -> raise (Exception(sprintf "A non-null value was expected: %s" name))

/// Helper: encode a JSON-RPC message with Content-Length framing into bytes.
let private encodeMessage (json: string) =
    let body = Encoding.UTF8.GetBytes(json)

    let header =
        sprintf "Content-Length: %d\r\n\r\n" body.Length |> Encoding.ASCII.GetBytes

    Array.append header body

/// Helper: create an anonymous pipe whose write end stays open for the lifetime of the test.
/// Use this when the transport must not see EOF during the test (e.g. when sending multiple
/// outbound messages).  Dispose the returned write-end stream when the test is done.
let private makeOpenStdin () =
    let writeEnd = new IO.Pipes.AnonymousPipeServerStream(IO.Pipes.PipeDirection.Out)

    let readEnd =
        new IO.Pipes.AnonymousPipeClientStream(IO.Pipes.PipeDirection.In, writeEnd.ClientSafePipeHandle)

    writeEnd, readEnd :> Stream

/// Helper: write a JSON-RPC request into a stream and return a MemoryStream positioned at 0 for reading.
let private makeInputStream (messages: string list) =
    let ms = new MemoryStream()

    for json in messages do
        let bytes = encodeMessage json
        ms.Write(bytes, 0, bytes.Length)

    ms.Position <- 0L
    ms

/// Helper: poll until a condition is true, with timeout. Returns true if condition was met.
let private waitUntil (timeoutMs: int) (condition: unit -> bool) =
    let deadline = DateTime.UtcNow.AddMilliseconds(float timeoutMs)

    while not (condition ()) && DateTime.UtcNow < deadline do
        Thread.Sleep 50

    condition ()

/// Helper: count the number of complete Content-Length-framed messages in a MemoryStream.
let private countMessages (stdout: MemoryStream) =
    let snapshot = stdout.ToArray()
    let tempStream = new MemoryStream(snapshot)
    let mutable count = 0
    let mutable keepReading = true

    while keepReading do
        let msg = readMessage tempStream |> Async.RunSynchronously

        match msg with
        | Some _ -> count <- count + 1
        | None -> keepReading <- false

    count

/// Helper: wait until exactly `n` messages have been written to stdout, then read and return them all.
let private waitForMessages (stdout: MemoryStream) (n: int) (timeoutMs: int) =
    waitUntil timeoutMs (fun () -> countMessages stdout >= n) |> ignore
    // Small extra delay to let the last write fully flush
    Thread.Sleep 100
    stdout.Position <- 0L

    [ for _ in 1..n do
          let msgBytes = readMessage stdout |> Async.RunSynchronously

          match msgBytes with
          | None -> ()
          | Some msgBytes ->
              use doc = JsonDocument.Parse(ReadOnlyMemory<byte>(msgBytes))
              yield doc.RootElement.Clone() ]

/// Helper: wait for the output stream to contain a complete response (with timeout).
let private waitForResponse (stdout: MemoryStream) (timeoutMs: int) = async {
    waitUntil timeoutMs (fun () -> stdout.Length > 0L) |> ignore
    // Give a bit more time for the full response to be written
    do! Async.Sleep 100
    stdout.Position <- 0L
    let! msgBytes = readMessage stdout

    match msgBytes with
    | None -> return None
    | Some msgBytes ->
        use doc = JsonDocument.Parse(ReadOnlyMemory<byte>(msgBytes))
        return Some(doc.RootElement.Clone())
}

[<Test>]
let testBasicMethodInvocationReturnsResponse () =
    let request =
        JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               id = 1
               ``method`` = "test/echo"
               ``params`` = {| message = "hello" |} |}
        )

    let handler _ctx = async { return Ok(JsonSerializer.SerializeToElement({| echo = "hello" |})) }

    let requestHandlings = Map.ofList [ "test/echo", handler ]

    let writeEnd, stdin = makeOpenStdin ()
    use writeEnd = writeEnd
    let stdout = new MemoryStream()

    let _server =
        startJsonRpcTransport stdin stdout None (fun _ -> requestHandlings, Map.empty)

    let requestBytes = encodeMessage request
    writeEnd.Write(requestBytes, 0, requestBytes.Length)
    writeEnd.Flush()

    let responseOpt = waitForResponse stdout 5000 |> Async.RunSynchronously

    Assert.IsTrue(responseOpt.IsSome, "Expected a response but got None")

    let response = responseOpt.Value
    Assert.AreEqual("2.0", response.GetProperty("jsonrpc").GetString())
    Assert.AreEqual(1, response.GetProperty("id").GetInt32())
    Assert.AreEqual("hello", response.GetProperty("result").GetProperty("echo").GetString())

    // After the handler has completed and the response is written the transport
    // should be quiescent: no running inbound requests and no queued writes.
    let stats = getJsonRpcStats _server |> Async.RunSynchronously
    Assert.AreEqual("Active", stats.Phase)

    Assert.AreEqual(
        0,
        stats.RunningInboundRequestCount,
        "No inbound requests should be running after handler completes"
    )

    Assert.AreEqual(0, stats.WriteQueueLength, "Write queue should be empty after response is flushed")

    shutdownJsonRpcTransport _server |> Async.RunSynchronously

[<Test>]
let testHandlerReturningEmptyResultProducesResponse () =
    let request =
        JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               id = 2
               ``method`` = "test/silent"
               ``params`` = {| |} |}
        )

    let handler _ctx = async { return Ok(JsonSerializer.SerializeToElement({| |})) }

    let requestHandlings = Map.ofList [ "test/silent", handler ]

    let stdin = makeInputStream [ request ]
    let stdout = new MemoryStream()

    let _server =
        startJsonRpcTransport stdin stdout None (fun _ -> requestHandlings, Map.empty)

    let responseOpt = waitForResponse stdout 5000 |> Async.RunSynchronously
    Assert.IsTrue(responseOpt.IsSome, "Expected a response")
    Assert.AreEqual(2, responseOpt.Value.GetProperty("id").GetInt32())

[<Test>]
let testUnregisteredMethodReturnsMethodNotFoundError () =
    let request =
        JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               id = 3
               ``method`` = "test/unknown"
               ``params`` = {| |} |}
        )

    // No handlers registered for "test/unknown"
    let stdin = makeInputStream [ request ]
    let stdout = new MemoryStream()

    let _server =
        startJsonRpcTransport stdin stdout None (fun _ -> Map.empty, Map.empty)

    let responseOpt = waitForResponse stdout 5000 |> Async.RunSynchronously

    Assert.IsTrue(responseOpt.IsSome, "Expected an error response")

    let response = responseOpt.Value
    Assert.AreEqual(3, response.GetProperty("id").GetInt32())
    Assert.AreEqual(-32601, response.GetProperty("error").GetProperty("code").GetInt32())

    Assert.IsTrue(
        response.GetProperty("error").GetProperty("message").GetString()
        |> nonNull "error message"
        |> _.Contains("Method not found")
    )

[<Test>]
let testMultipleRequestsAreHandledSequentially () =
    let handler ctx = async {
        let id = ctx.WireId.Value.GetInt32()
        return Ok(JsonSerializer.SerializeToElement({| doubled = id * 2 |}))
    }

    let requestHandlings = Map.ofList [ "test/double", handler ]

    let request1 =
        JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               id = 10
               ``method`` = "test/double"
               ``params`` = {| |} |}
        )

    let request2 =
        JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               id = 20
               ``method`` = "test/double"
               ``params`` = {| |} |}
        )

    let stdin = makeInputStream [ request1; request2 ]
    let stdout = new MemoryStream()

    let _server =
        startJsonRpcTransport stdin stdout None (fun _ -> requestHandlings, Map.empty)

    let responses = waitForMessages stdout 2 5000

    Assert.AreEqual(2, responses.Length, "Expected 2 responses")

    Assert.AreEqual(10, responses.[0].GetProperty("id").GetInt32())
    Assert.AreEqual(20, responses.[0].GetProperty("result").GetProperty("doubled").GetInt32())

    Assert.AreEqual(20, responses.[1].GetProperty("id").GetInt32())
    Assert.AreEqual(40, responses.[1].GetProperty("result").GetProperty("doubled").GetInt32())

[<Test>]
let testContentLengthFramingIsCorrect () =
    let request =
        JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               id = 42
               ``method`` = "test/ping"
               ``params`` = {| |} |}
        )

    let handler _ctx = async { return Ok(JsonSerializer.SerializeToElement("pong")) }

    let requestHandlings = Map.ofList [ "test/ping", handler ]

    let stdin = makeInputStream [ request ]
    let stdout = new MemoryStream()

    let _server =
        startJsonRpcTransport stdin stdout None (fun _ -> requestHandlings, Map.empty)

    waitUntil 5000 (fun () -> stdout.Length > 0L) |> ignore
    Thread.Sleep 100

    // Read raw output to verify Content-Length header
    stdout.Position <- 0L
    let rawBytes = stdout.ToArray()
    let rawString = Encoding.UTF8.GetString(rawBytes)

    Assert.IsTrue(rawString.StartsWith("Content-Length: "), "Response should start with Content-Length header")
    Assert.IsTrue(rawString.Contains("\r\n\r\n"), "Response should contain header/body separator")

    // Verify the Content-Length value matches the actual body length
    let headerEnd = rawString.IndexOf("\r\n\r\n")
    let headerLine = rawString.Substring(0, headerEnd)
    let clValue = headerLine.Replace("Content-Length: ", "") |> int
    let bodyString = rawString.Substring(headerEnd + 4)
    let bodyBytes = Encoding.UTF8.GetBytes(bodyString)

    Assert.AreEqual(clValue, bodyBytes.Length, "Content-Length should match actual body byte length")

    // Verify body parses as valid JSON with expected content
    let parsed = JsonDocument.Parse(bodyString).RootElement
    Assert.AreEqual(42, parsed.GetProperty("id").GetInt32())
    Assert.AreEqual("pong", parsed.GetProperty("result").GetString())

[<Test>]
let testMessageWithIdButNoMethodIsTreatedAsResponseAndDroppedIfUnmatched () =
    // A message with "id" but no "method" is now classified as a response to an
    // outbound request. Since there are no outstanding outbound requests, it is
    // silently logged and dropped (no output).
    let malformed = JsonSerializer.Serialize({| jsonrpc = "2.0"; id = 99 |})

    let stdin = makeInputStream [ malformed ]
    let stdout = new MemoryStream()

    let _server =
        startJsonRpcTransport stdin stdout None (fun _ -> Map.empty, Map.empty)

    Async.Sleep 500 |> Async.RunSynchronously
    Assert.AreEqual(0L, stdout.Length, "Expected no output for unmatched response")

[<Test>]
let testMessageWithNoMethodOrIdIsIgnored () =
    // A message with neither "method" nor "id" — not a valid request or notification
    let malformed = JsonSerializer.Serialize({| jsonrpc = "2.0" |})

    let stdin = makeInputStream [ malformed ]
    let stdout = new MemoryStream()

    let _server =
        startJsonRpcTransport stdin stdout None (fun _ -> Map.empty, Map.empty)

    Async.Sleep 500 |> Async.RunSynchronously
    Assert.AreEqual(0L, stdout.Length, "Expected no output for message with no method or id")

// ---- Notification tests ----

[<Test>]
let testNotificationIsDispatchedToHandler () =
    let received = ref false
    let receivedMethod = ref ""

    let notification =
        JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               ``method`` = "test/notify"
               ``params`` = {| data = "abc" |} |}
        )
    // No "id" — this is a notification

    let handler (ctx: JsonRpcRequestContext) = async {
        received.Value <- true
        receivedMethod.Value <- ctx.MethodName
    }

    let notificationHandlings = Map.ofList [ "test/notify", handler ]

    let stdin = makeInputStream [ notification ]
    let stdout = new MemoryStream()

    let _server =
        startJsonRpcTransport stdin stdout None (fun _ -> Map.empty, notificationHandlings)

    // Wait for the notification to be processed
    waitUntil 5000 (fun () -> received.Value) |> ignore

    Assert.IsTrue(received.Value, "Notification handler should have been invoked")
    Assert.AreEqual("test/notify", receivedMethod.Value)
    Assert.AreEqual(0L, stdout.Length, "Notifications should produce no output on stdout")

[<Test>]
let testNotificationHandlerReceivesCorrectParams () =
    let capturedParams = ref ""

    let notification =
        JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               ``method`` = "test/log"
               ``params`` =
                {| level = "info"
                   message = "hello world" |} |}
        )

    let handler ctx = async {
        capturedParams.Value <- ctx.Params.Value.GetProperty("message").GetString() |> nonNull "message"
    }

    let notificationHandlings = Map.ofList [ "test/log", handler ]

    let stdin = makeInputStream [ notification ]
    let stdout = new MemoryStream()

    let _server =
        startJsonRpcTransport stdin stdout None (fun _ -> Map.empty, notificationHandlings)

    waitUntil 5000 (fun () -> capturedParams.Value <> "") |> ignore

    Assert.AreEqual("hello world", capturedParams.Value)

[<Test>]
let testUnregisteredNotificationDoesNotCrash () =
    let notification =
        JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               ``method`` = "test/unknown_notif"
               ``params`` = {| |} |}
        )

    let stdin = makeInputStream [ notification ]
    let stdout = new MemoryStream()

    // No notification handlers registered
    let _server =
        startJsonRpcTransport stdin stdout None (fun _ -> Map.empty, Map.empty)

    Async.Sleep 500 |> Async.RunSynchronously
    Assert.AreEqual(0L, stdout.Length, "Expected no output for unregistered notification")

[<Test>]
let testMixedRequestsAndNotificationsWork () =
    let notificationReceived = ref false

    let request =
        JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               id = 1
               ``method`` = "test/echo"
               ``params`` = {| value = "hi" |} |}
        )

    let notification =
        JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               ``method`` = "test/ping"
               ``params`` = {| |} |}
        )

    let requestHandler _ctx = async { return Ok(JsonSerializer.SerializeToElement("ok")) }

    let notificationHandler _ctx = async { notificationReceived.Value <- true }

    let requestHandlings = Map.ofList [ "test/echo", requestHandler ]
    let notificationHandlings = Map.ofList [ "test/ping", notificationHandler ]

    // Send request first, then notification
    let stdin = makeInputStream [ request; notification ]
    let stdout = new MemoryStream()

    let _server =
        startJsonRpcTransport stdin stdout None (fun _ -> requestHandlings, notificationHandlings)

    // Wait for the request response
    let responseOpt = waitForResponse stdout 5000 |> Async.RunSynchronously
    Assert.IsTrue(responseOpt.IsSome, "Expected a response for the request")
    Assert.AreEqual(1, responseOpt.Value.GetProperty("id").GetInt32())
    Assert.AreEqual("ok", responseOpt.Value.GetProperty("result").GetString())

    // Wait for the notification to be processed
    waitUntil 5000 (fun () -> notificationReceived.Value) |> ignore

    Assert.IsTrue(notificationReceived.Value, "Notification handler should have been invoked")

[<Test>]
let testNotificationWithIdIsRoutedAsRequest () =
    // A message with both "id" and "method" should be treated as a request, not a notification
    let notificationHandlerCalled = ref false
    let requestHandlerCalled = ref false

    let msg =
        JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               id = 5
               ``method`` = "test/ambiguous"
               ``params`` = {| |} |}
        )

    let requestHandler _ctx = async {
        requestHandlerCalled.Value <- true
        return Ok(JsonSerializer.SerializeToElement(null: obj | null))
    }

    let notificationHandler _ctx = async { notificationHandlerCalled.Value <- true }

    let requestHandlings = Map.ofList [ "test/ambiguous", requestHandler ]
    let notificationHandlings = Map.ofList [ "test/ambiguous", notificationHandler ]

    let stdin = makeInputStream [ msg ]
    let stdout = new MemoryStream()

    let _server =
        startJsonRpcTransport stdin stdout None (fun _ -> requestHandlings, notificationHandlings)

    waitUntil 5000 (fun () -> requestHandlerCalled.Value) |> ignore

    Assert.IsTrue(requestHandlerCalled.Value, "Request handler should be called for message with 'id'")
    Assert.IsFalse(notificationHandlerCalled.Value, "Notification handler should NOT be called for message with 'id'")

[<Test>]
let testWriteQueueDrainsAllResponses () =
    // Send many requests at once to exercise the write queue — multiple responses
    // will be enqueued while the first write is still in progress.
    let requestCount = 10

    let handler ctx = async {
        let id = ctx.WireId.Value.GetInt32()
        return Ok(JsonSerializer.SerializeToElement({| seq = id |}))
    }

    let requestHandlings = Map.ofList [ "test/seq", handler ]

    let requests =
        [ for i in 1..requestCount ->
              JsonSerializer.Serialize(
                  {| jsonrpc = "2.0"
                     id = i
                     ``method`` = "test/seq"
                     ``params`` = {| |} |}
              ) ]

    let writeEnd, stdin = makeOpenStdin ()
    use writeEnd = writeEnd
    let stdout = new MemoryStream()

    let server =
        startJsonRpcTransport stdin stdout None (fun _ -> requestHandlings, Map.empty)

    // Write all requests into the pipe at once so the transport sees them back-to-back.
    for req in requests do
        let bytes = encodeMessage req
        writeEnd.Write(bytes, 0, bytes.Length)

    writeEnd.Flush()

    // Snapshot stats while responses are still being written — the write queue should
    // be non-empty at some point while 10 responses are being serialised sequentially.
    // We poll until we catch at least one moment where the queue is non-empty, which
    // confirms the write-queue path is exercised.  (If the machine is very fast the
    // queue may drain before we sample, so we only assert the post-drain state.)
    let mutable midFlightStats = getJsonRpcStats server |> Async.RunSynchronously
    let deadline = DateTime.UtcNow.AddSeconds(5.0)

    while midFlightStats.Phase = "Active"
          && midFlightStats.WriteQueueLength = 0
          && midFlightStats.RunningInboundRequestCount = 0
          && DateTime.UtcNow < deadline do
        Thread.Sleep 5
        midFlightStats <- getJsonRpcStats server |> Async.RunSynchronously

    let responses = waitForMessages stdout requestCount 10000

    Assert.AreEqual(
        requestCount,
        responses.Length,
        sprintf "Expected %d responses but got %d" requestCount responses.Length
    )

    let receivedIds =
        responses |> List.map (fun r -> r.GetProperty("id").GetInt32()) |> Set.ofList

    for i in 1..requestCount do
        Assert.IsTrue(receivedIds.Contains(i), sprintf "Missing response for id %d" i)

    // After all responses are flushed the transport should be fully quiescent.
    let finalStats = getJsonRpcStats server |> Async.RunSynchronously
    Assert.AreEqual("Active", finalStats.Phase)
    Assert.AreEqual(0, finalStats.RunningInboundRequestCount, "No handlers should still be running")
    Assert.AreEqual(0, finalStats.WriteQueueLength, "Write queue should be empty after all responses are sent")
    Assert.AreEqual(0, finalStats.PendingOutboundCallCount, "No pending outbound calls expected")

    shutdownJsonRpcTransport server |> Async.RunSynchronously

// ---- SendNotification tests ----

[<Test>]
let testSendNotificationWritesProperJsonRpcNotification () =
    // Use an open pipe so stdin never hits EOF and the transport doesn't shut down
    // before the notification is enqueued.  An empty MemoryStream would EOF
    // immediately and — under a loaded test runner — the transport could enter
    // ShuttingDown before SendNotification is processed, causing a silent drop.
    let writeEnd, stdin = makeOpenStdin ()
    use writeEnd = writeEnd
    let stdout = new MemoryStream()

    let server = startJsonRpcTransport stdin stdout None (fun _ -> Map.empty, Map.empty)

    let body = JsonSerializer.SerializeToElement({| message = "hello from server" |})

    // sendJsonRpcNotification only completes after MessageWriteComplete fires,
    // which is after the bytes have been flushed to stdout.  No polling needed —
    // we can rewind and read directly.
    sendJsonRpcNotification server "window/logMessage" body
    |> Async.RunSynchronously

    let responseOpt = waitForResponse stdout 5000 |> Async.RunSynchronously

    Assert.IsTrue(responseOpt.IsSome, "Expected a notification to be written to stdout")

    let msg = responseOpt.Value
    Assert.AreEqual("2.0", msg.GetProperty("jsonrpc").GetString())
    Assert.AreEqual("window/logMessage", msg.GetProperty("method").GetString())
    Assert.AreEqual("hello from server", msg.GetProperty("params").GetProperty("message").GetString())
    Assert.IsFalse(msg.TryGetProperty("id") |> fst, "Notification should not have an 'id' field")

[<Test>]
let testSendMultipleNotificationsAllWritten () =
    // Use an open pipe so stdin never hits EOF while we are sending notifications.
    let writeEnd, stdin = makeOpenStdin ()
    use writeEnd = writeEnd
    let stdout = new MemoryStream()

    let server = startJsonRpcTransport stdin stdout None (fun _ -> Map.empty, Map.empty)

    let notifCount = 5

    for i in 1..notifCount do
        let body = JsonSerializer.SerializeToElement({| index = i |})

        sendJsonRpcNotification server "test/notif" body |> Async.RunSynchronously

    let messages = waitForMessages stdout notifCount 5000

    Assert.AreEqual(
        notifCount,
        messages.Length,
        sprintf "Expected %d notifications but got %d" notifCount messages.Length
    )

    writeEnd.Dispose()

// ---- SendRequest tests ----

[<Test>]
let testSendRequestWritesRequestAndResolvesOnResponse () =
    // We need stdin to stay open so the server doesn't shut down before we can
    // interact with it. Use a pipe so we can write the client's response into it.
    use clientToServer =
        new IO.Pipes.AnonymousPipeServerStream(IO.Pipes.PipeDirection.Out)

    use serverStdin =
        new IO.Pipes.AnonymousPipeClientStream(IO.Pipes.PipeDirection.In, clientToServer.ClientSafePipeHandle)

    use stdout = new MemoryStream()

    let server =
        startJsonRpcTransport serverStdin stdout None (fun _ -> Map.empty, Map.empty)

    let body = JsonSerializer.SerializeToElement({| items = [| "a"; "b" |] |})

    // Post request asynchronously and capture the reply future
    let replyTask =
        sendJsonRpcCall server "workspace/configuration" body |> Async.StartAsTask

    // Wait for the outbound request to appear on stdout
    let outboundMessages = waitForMessages stdout 1 5000

    Assert.AreEqual(1, outboundMessages.Length, "Expected an outbound request on stdout")

    let outbound = outboundMessages.[0]
    Assert.AreEqual("2.0", outbound.GetProperty("jsonrpc").GetString())
    Assert.AreEqual("workspace/configuration", outbound.GetProperty("method").GetString())
    Assert.IsTrue(outbound.TryGetProperty("id") |> fst, "Request should have an 'id' field")

    let outboundId = outbound.GetProperty("id").GetInt32()

    // While the call is still in flight, stats must show exactly one pending outbound
    // call and no timer (sendJsonRpcCall carries no timeout).
    let pendingStats = getJsonRpcStats server |> Async.RunSynchronously
    Assert.AreEqual("Active", pendingStats.Phase)
    Assert.AreEqual(1, pendingStats.PendingOutboundCallCount, "One outbound call should be pending")
    Assert.AreEqual(false, pendingStats.TimerArmed, "Timer must not be armed for an untimed call")

    // Now simulate the client sending a response back through stdin
    let clientResponse =
        JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               id = outboundId
               result = [| "config1"; "config2" |] |}
        )

    let responseBytes = encodeMessage clientResponse
    clientToServer.Write(responseBytes, 0, responseBytes.Length)
    clientToServer.Flush()

    // Wait for the reply to come back
    let completed = replyTask.Wait(TimeSpan.FromSeconds(5.0))
    Assert.IsTrue(completed, "SendRequest should have received a reply")

    match replyTask.Result with
    | Ok reply ->
        let arr = reply.EnumerateArray() |> Seq.toArray
        Assert.AreEqual("config1", arr.[0].GetString())
        Assert.AreEqual("config2", arr.[1].GetString())
    | Error err -> Assert.Fail(sprintf "Expected Ok response but got Error: %s" (string err))

    // Pending call resolved — count must drop back to zero before we shut down.
    let resolvedStats = getJsonRpcStats server |> Async.RunSynchronously

    Assert.AreEqual(
        0,
        resolvedStats.PendingOutboundCallCount,
        "Pending call count should be zero after response received"
    )

    shutdownJsonRpcTransport server |> Async.RunSynchronously

    // Transport must be in the Stopped phase after a clean shutdown.
    let stoppedStats = getJsonRpcStats server |> Async.RunSynchronously
    Assert.AreEqual("Stopped", stoppedStats.Phase)

[<Test>]
let testActorRemainsHealthyAfterTimeout () =
    // After a timed-out call, the actor must still process a subsequent call that
    // receives a real response — proves PendingOutboundCalls was cleaned up properly.
    use clientToServer =
        new IO.Pipes.AnonymousPipeServerStream(IO.Pipes.PipeDirection.Out)

    use serverStdin =
        new IO.Pipes.AnonymousPipeClientStream(IO.Pipes.PipeDirection.In, clientToServer.ClientSafePipeHandle)

    use stdout = new MemoryStream()

    let server =
        startJsonRpcTransport serverStdin stdout None (fun _ -> Map.empty, Map.empty)

    // First call — will time out because no response is sent.
    let timedOutTask =
        sendJsonRpcCallWithTimeout
            server
            "test/hang"
            (JsonSerializer.SerializeToElement({| |}))
            (Some(TimeSpan.FromMilliseconds 200.0))
        |> Async.StartAsTask

    let timedOutCompleted = timedOutTask.Wait(TimeSpan.FromMilliseconds 500.0)
    Assert.IsTrue(timedOutCompleted, "First call should have timed out")

    match timedOutTask.Result with
    | Error err -> Assert.AreEqual(-32000, err.GetProperty("code").GetInt32())
    | Ok _ -> Assert.Fail("Expected Error(-32000) for first (timed-out) call")

    // Second call — this time we feed a real response back.
    // Wait for the outbound request to appear on stdout first.
    let outboundMessages = waitForMessages stdout 1 3000
    Assert.AreEqual(1, outboundMessages.Length, "Expected 1 outbound message (first call's request)")

    // NOTE: the first call's outbound request is already on stdout. Send a second call
    // and immediately respond to it.
    let healthyTask =
        sendJsonRpcCallWithTimeout
            server
            "test/healthy"
            (JsonSerializer.SerializeToElement({| |}))
            (Some(TimeSpan.FromSeconds 5.0))
        |> Async.StartAsTask

    // Wait for the second request to appear on stdout
    waitUntil 3000 (fun () -> countMessages stdout >= 2) |> ignore
    Thread.Sleep 50
    stdout.Position <- 0L
    let allOutbound = waitForMessages stdout 2 1000
    Assert.AreEqual(2, allOutbound.Length, "Expected 2 outbound requests total")

    let secondRequestId = allOutbound.[1].GetProperty("id").GetInt32()

    // Feed back a success response for the second call
    let successResponse =
        JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               id = secondRequestId
               result = {| alive = true |} |}
        )

    let responseBytes = encodeMessage successResponse
    clientToServer.Write(responseBytes, 0, responseBytes.Length)
    clientToServer.Flush()

    let healthyCompleted = healthyTask.Wait(TimeSpan.FromSeconds 5.0)
    Assert.IsTrue(healthyCompleted, "Second call should have received a reply")

    match healthyTask.Result with
    | Ok reply -> Assert.AreEqual(true, reply.GetProperty("alive").GetBoolean())
    | Error err -> Assert.Fail(sprintf "Expected Ok for second call but got Error: %s" (string err))

    shutdownJsonRpcTransport server |> Async.RunSynchronously

[<Test>]
let testReplyBeforeDeadlineReturnsOk () =
    // A call with a 5 s timeout that gets a real response in ~50 ms must return Ok,
    // and the actor's Timer must be disarmed (no spurious CheckTimeouts side-effects).
    use clientToServer =
        new IO.Pipes.AnonymousPipeServerStream(IO.Pipes.PipeDirection.Out)

    use serverStdin =
        new IO.Pipes.AnonymousPipeClientStream(IO.Pipes.PipeDirection.In, clientToServer.ClientSafePipeHandle)

    use stdout = new MemoryStream()

    let server =
        startJsonRpcTransport serverStdin stdout None (fun _ -> Map.empty, Map.empty)

    let replyTask =
        sendJsonRpcCallWithTimeout
            server
            "test/fast"
            (JsonSerializer.SerializeToElement({| |}))
            (Some(TimeSpan.FromSeconds 5.0))
        |> Async.StartAsTask

    // Wait for the outbound request then reply quickly
    let outboundMessages = waitForMessages stdout 1 3000
    let outboundId = outboundMessages.[0].GetProperty("id").GetInt32()

    Thread.Sleep 50 // simulate a 50 ms round trip — well before the 5 s deadline

    let successResponse =
        JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               id = outboundId
               result = {| value = 99 |} |}
        )

    let responseBytes = encodeMessage successResponse
    clientToServer.Write(responseBytes, 0, responseBytes.Length)
    clientToServer.Flush()

    let completed = replyTask.Wait(TimeSpan.FromSeconds 3.0)
    Assert.IsTrue(completed, "Call should resolve before the 5 s deadline")

    match replyTask.Result with
    | Ok reply -> Assert.AreEqual(99, reply.GetProperty("value").GetInt32())
    | Error err -> Assert.Fail(sprintf "Expected Ok but got Error: %s" (string err))

    shutdownJsonRpcTransport server |> Async.RunSynchronously

[<Test>]
let testTwoConcurrentCallsWithNearEqualDeadlinesBothTimeout () =
    // Two calls with 200 ms / 220 ms timeouts against a peer that never replies.
    // Both should be failed with Error(-32000); the second deadline is close enough
    // that a single CheckTimeouts sweep should catch both.
    let writeEnd, stdin = makeOpenStdin ()
    use writeEnd = writeEnd
    use stdout = new MemoryStream()

    let server = startJsonRpcTransport stdin stdout None (fun _ -> Map.empty, Map.empty)

    let task1 =
        sendJsonRpcCallWithTimeout
            server
            "test/hang1"
            (JsonSerializer.SerializeToElement({| |}))
            (Some(TimeSpan.FromMilliseconds 200.0))
        |> Async.StartAsTask

    let task2 =
        sendJsonRpcCallWithTimeout
            server
            "test/hang2"
            (JsonSerializer.SerializeToElement({| |}))
            (Some(TimeSpan.FromMilliseconds 220.0))
        |> Async.StartAsTask

    // While both calls are still pending the stats must show two pending outbound calls
    // and the shared timer armed for the earlier of the two deadlines.
    waitForMessages stdout 2 3000 |> ignore // ensure both requests hit the wire first
    let pendingStats = getJsonRpcStats server |> Async.RunSynchronously
    Assert.AreEqual(2, pendingStats.PendingOutboundCallCount, "Both outbound calls should be pending")
    Assert.AreEqual(true, pendingStats.TimerArmed, "Timer must be armed when timed calls are pending")

    // Both must complete well within 1 s
    let completed1 = task1.Wait(TimeSpan.FromSeconds 1.0)
    let completed2 = task2.Wait(TimeSpan.FromSeconds 1.0)

    Assert.IsTrue(completed1, "First call should have timed out within 1 s")
    Assert.IsTrue(completed2, "Second call should have timed out within 1 s")

    for task, label in [ task1, "first"; task2, "second" ] do
        match task.Result with
        | Error err ->
            Assert.AreEqual(-32000, err.GetProperty("code").GetInt32(), sprintf "Expected -32000 for %s call" label)

            Assert.AreEqual("Call timed out", err.GetProperty("message").GetString())
        | Ok _ -> Assert.Fail(sprintf "Expected Error(-32000) for %s call, got Ok" label)

    // After both timeouts fire: no pending calls, timer disarmed, and both IDs stamped
    // into RecentlyTimedOutCalls so that late responses can be distinguished from
    // genuine unknown-id violations.
    let afterStats = getJsonRpcStats server |> Async.RunSynchronously
    Assert.AreEqual(0, afterStats.PendingOutboundCallCount, "All pending calls should be gone after timeout")
    Assert.AreEqual(false, afterStats.TimerArmed, "Timer should be disarmed once no timed calls remain")
    Assert.AreEqual(2, afterStats.RecentlyTimedOutCallCount, "Both timed-out IDs should be in RecentlyTimedOutCalls")

    shutdownJsonRpcTransport server |> Async.RunSynchronously

[<Test>]
let testEarlierDeadlineArrivingMidArmFiresAtCorrectTime () =
    // Schedule a call with a 5 s deadline, then a second call with a 200 ms deadline.
    // The second (shorter) deadline must cause the timer to re-arm via Change() and
    // fire at ~200 ms, not at 5 s.  We assert the 200 ms call times out within 500 ms.
    let writeEnd, stdin = makeOpenStdin ()
    use writeEnd = writeEnd
    use stdout = new MemoryStream()

    let server = startJsonRpcTransport stdin stdout None (fun _ -> Map.empty, Map.empty)

    // Long-deadline call — should still be pending when the short one fires
    let longTask =
        sendJsonRpcCallWithTimeout
            server
            "test/long"
            (JsonSerializer.SerializeToElement({| |}))
            (Some(TimeSpan.FromSeconds 5.0))
        |> Async.StartAsTask

    // Short-deadline call — must fire at ~200 ms regardless of the 5 s timer already armed
    let shortTask =
        sendJsonRpcCallWithTimeout
            server
            "test/short"
            (JsonSerializer.SerializeToElement({| |}))
            (Some(TimeSpan.FromMilliseconds 200.0))
        |> Async.StartAsTask

    let sw = System.Diagnostics.Stopwatch.StartNew()
    let shortCompleted = shortTask.Wait(TimeSpan.FromMilliseconds 500.0)
    sw.Stop()

    Assert.IsTrue(
        shortCompleted,
        sprintf "Short-deadline call should have timed out within 500 ms but took %d ms" sw.ElapsedMilliseconds
    )

    match shortTask.Result with
    | Error err -> Assert.AreEqual(-32000, err.GetProperty("code").GetInt32())
    | Ok _ -> Assert.Fail("Expected Error(-32000) for the short-deadline call")

    // Long call should still be pending (not yet timed out after ~200 ms wall time)
    Assert.IsFalse(longTask.IsCompleted, "Long-deadline call should still be pending after ~200 ms")

    // Clean up — shutdown will fail the long call with -32099
    shutdownJsonRpcTransport server |> Async.RunSynchronously

    let longCompleted = longTask.Wait(TimeSpan.FromSeconds 3.0)
    Assert.IsTrue(longCompleted, "Long-deadline call should be failed on shutdown")

    match longTask.Result with
    | Error err -> Assert.AreEqual(-32099, err.GetProperty("code").GetInt32())
    | Ok _ -> Assert.Fail("Expected Error(-32099) for long-deadline call after shutdown")

[<Test>]
let testShutdownDisposesTimerAndFailsPendingTimedCall () =
    // Start a transport with a 30 s timed call (deadline never fires during the test).
    // Shut the transport down; the call must be failed with -32099 (not -32000),
    // proving that shutdown wins over the timer.
    use clientToServer =
        new IO.Pipes.AnonymousPipeServerStream(IO.Pipes.PipeDirection.Out)

    use serverStdin =
        new IO.Pipes.AnonymousPipeClientStream(IO.Pipes.PipeDirection.In, clientToServer.ClientSafePipeHandle)

    use stdout = new MemoryStream()

    let server =
        startJsonRpcTransport serverStdin stdout None (fun _ -> Map.empty, Map.empty)

    let replyTask =
        sendJsonRpcCallWithTimeout
            server
            "test/long-hang"
            (JsonSerializer.SerializeToElement({| |}))
            (Some(TimeSpan.FromSeconds 30.0))
        |> Async.StartAsTask

    // Wait for the outbound request so we know the call is registered
    waitForMessages stdout 1 3000 |> ignore

    // Shut down — timer should be disposed; pending call failed with -32099
    shutdownJsonRpcTransport server |> Async.RunSynchronously

    let completed = replyTask.Wait(TimeSpan.FromSeconds 3.0)
    Assert.IsTrue(completed, "Pending timed call should be unblocked by shutdown")

    match replyTask.Result with
    | Error err ->
        Assert.AreEqual(
            -32099,
            err.GetProperty("code").GetInt32(),
            "Shutdown must return -32099, not the timer's -32000"
        )

        Assert.AreEqual("Transport shut down", err.GetProperty("message").GetString())
    | Ok _ -> Assert.Fail("Expected Error(-32099) after shutdown, got Ok")

[<Test>]
let testLateResponseAfterTimeoutLogsRpcWarnNotRpcError () =
    // Call with a 100 ms timeout; wait for it to expire; then feed the response back.
    // The rpc-log callback must receive an RpcWarn("late response") and must NOT
    // receive an RpcError for that same id.
    let logEntries = System.Collections.Generic.List<JsonRpcLogEntry>()

    let logCallback (entry: JsonRpcLogEntry) =
        lock logEntries (fun () -> logEntries.Add(entry))

    use clientToServer =
        new IO.Pipes.AnonymousPipeServerStream(IO.Pipes.PipeDirection.Out)

    use serverStdin =
        new IO.Pipes.AnonymousPipeClientStream(IO.Pipes.PipeDirection.In, clientToServer.ClientSafePipeHandle)

    use stdout = new MemoryStream()

    let server =
        startJsonRpcTransport serverStdin stdout (Some logCallback) (fun _ -> Map.empty, Map.empty)

    let replyTask =
        sendJsonRpcCallWithTimeout
            server
            "test/late-peer"
            (JsonSerializer.SerializeToElement({| |}))
            (Some(TimeSpan.FromMilliseconds 100.0))
        |> Async.StartAsTask

    // Capture the outbound id before the timeout fires
    let outboundMessages = waitForMessages stdout 1 3000
    let outboundId = outboundMessages.[0].GetProperty("id").GetInt32()

    // Let the 100 ms deadline fire and the call be failed
    let timedOut = replyTask.Wait(TimeSpan.FromMilliseconds 500.0)
    Assert.IsTrue(timedOut, "Call should have timed out")

    match replyTask.Result with
    | Error err -> Assert.AreEqual(-32000, err.GetProperty("code").GetInt32())
    | Ok _ -> Assert.Fail("Expected Error(-32000)")

    // Now feed back the late response (300 ms after the timeout fired)
    Thread.Sleep 300

    let lateResponse =
        JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               id = outboundId
               result = {| late = true |} |}
        )

    let responseBytes = encodeMessage lateResponse
    clientToServer.Write(responseBytes, 0, responseBytes.Length)
    clientToServer.Flush()

    // Give the actor time to process the late response
    Thread.Sleep 200

    let entries = lock logEntries (fun () -> logEntries |> Seq.toList)
    let idStr = string outboundId

    let lateWarn =
        entries
        |> List.exists (fun e ->
            match e with
            | RpcWarn msg -> msg.Contains("late") && msg.Contains(idStr)
            | _ -> false)

    let spuriousError =
        entries
        |> List.exists (fun e ->
            match e with
            | RpcError msg -> msg.Contains(idStr) && msg.Contains("unknown")
            | _ -> false)

    Assert.IsTrue(lateWarn, "Expected an RpcWarn entry about a late response for the timed-out call id")
    Assert.IsFalse(spuriousError, "Must NOT receive an RpcError for the late response (id is in RecentlyTimedOutCalls)")

    shutdownJsonRpcTransport server |> Async.RunSynchronously

[<Test>]
let testUntimedCallDoesNotTimeOut () =
    // sendJsonRpcCall (no timeout) against a peer that never replies must NOT complete
    // on its own — the infinite-wait behaviour must be preserved.
    // We bound the test itself with a short Task.Wait and assert the call is still pending.
    let writeEnd, stdin = makeOpenStdin ()
    use writeEnd = writeEnd
    use stdout = new MemoryStream()

    let server = startJsonRpcTransport stdin stdout None (fun _ -> Map.empty, Map.empty)

    let replyTask =
        sendJsonRpcCall server "test/untimed-hang" (JsonSerializer.SerializeToElement({| |}))
        |> Async.StartAsTask

    // Give 500 ms — more than enough for a timer-based timeout to fire if one were armed
    let completedEarly = replyTask.Wait(TimeSpan.FromMilliseconds 500.0)

    Assert.IsFalse(completedEarly, "An untimed call against a non-responding peer must not complete on its own")

    // Clean up: shut down the transport so the call is released
    shutdownJsonRpcTransport server |> Async.RunSynchronously

    let completedAfterShutdown = replyTask.Wait(TimeSpan.FromSeconds 3.0)
    Assert.IsTrue(completedAfterShutdown, "Untimed call should be released by shutdown")

    match replyTask.Result with
    | Error err -> Assert.AreEqual(-32099, err.GetProperty("code").GetInt32())
    | Ok _ -> Assert.Fail("Expected Error(-32099) from shutdown, got Ok")

/// Helper: write a framed JSON-RPC message into a writable pipe stream.
let private writeMessageToPipe (pipe: IO.Pipes.AnonymousPipeServerStream) (msg: string) =
    let bytes = encodeMessage msg
    pipe.Write(bytes, 0, bytes.Length)
    pipe.Flush()

// ---- Shutdown drain tests ----

[<Test>]
let testShutdownWaitsForRunningHandlerToFinish () =
    // A slow inbound handler is running when Shutdown arrives.
    // shutdownJsonRpcTransport must not return until the handler has finished.
    use handlerStarted = new ManualResetEventSlim(false)
    use handlerMayFinish = new ManualResetEventSlim(false)
    let handlerFinished = ref false

    let handler _ctx = async {
        handlerStarted.Set()
        handlerMayFinish.Wait() |> ignore
        handlerFinished.Value <- true
        return Ok(JsonSerializer.SerializeToElement(null: obj | null))
    }

    use clientToServer =
        new IO.Pipes.AnonymousPipeServerStream(IO.Pipes.PipeDirection.Out)

    use serverStdin =
        new IO.Pipes.AnonymousPipeClientStream(IO.Pipes.PipeDirection.In, clientToServer.ClientSafePipeHandle)

    use stdout = new MemoryStream()

    let server =
        startJsonRpcTransport serverStdin stdout None (fun _ -> Map.ofList [ "test/slow", handler ], Map.empty)

    writeMessageToPipe
        clientToServer
        (JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               id = 1
               ``method`` = "test/slow"
               ``params`` = {| |} |}
        ))

    Assert.IsTrue(handlerStarted.Wait(5000), "Handler should have started")

    // Kick off shutdown asynchronously — it must block until the handler completes
    let shutdownTask = shutdownJsonRpcTransport server |> Async.StartAsTask

    // Shutdown should not complete while the handler is still blocked
    Assert.IsFalse(
        shutdownTask.Wait(TimeSpan.FromMilliseconds 500.0),
        "Shutdown should not complete while handler is still running"
    )

    Assert.IsFalse(handlerFinished.Value, "Handler should not have finished yet")

    // Let the handler proceed
    handlerMayFinish.Set()

    let completed = shutdownTask.Wait(TimeSpan.FromSeconds 5.0)
    Assert.IsTrue(completed, "Shutdown should complete after handler finishes")
    Assert.IsTrue(handlerFinished.Value, "Handler should have finished before shutdown returned")

[<Test>]
let testShutdownCancelsRunningHandlerAndThenReturns () =
    // A handler that sleeps indefinitely (respecting cancellation) should be
    // cancelled by Shutdown, and shutdownJsonRpcTransport should then return.
    use handlerStarted = new ManualResetEventSlim(false)

    let handler _ctx = async {
        handlerStarted.Set()
        do! Async.Sleep 60_000 // would block forever without cancellation
        return Ok(JsonSerializer.SerializeToElement(null: obj | null))
    }

    use clientToServer =
        new IO.Pipes.AnonymousPipeServerStream(IO.Pipes.PipeDirection.Out)

    use serverStdin =
        new IO.Pipes.AnonymousPipeClientStream(IO.Pipes.PipeDirection.In, clientToServer.ClientSafePipeHandle)

    use stdout = new MemoryStream()

    let server =
        startJsonRpcTransport serverStdin stdout None (fun _ -> Map.ofList [ "test/inf", handler ], Map.empty)

    writeMessageToPipe
        clientToServer
        (JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               id = 1
               ``method`` = "test/inf"
               ``params`` = {| |} |}
        ))

    Assert.IsTrue(handlerStarted.Wait(5000), "Handler should have started")

    // Shutdown should cancel the handler and return within a few seconds
    let shutdownTask = shutdownJsonRpcTransport server |> Async.StartAsTask
    let completed = shutdownTask.Wait(TimeSpan.FromSeconds 5.0)
    Assert.IsTrue(completed, "Shutdown should return after cancelling the in-flight handler")

    // The response on the wire should be the -32800 cancellation error
    let msgs = waitForMessages stdout 1 1000
    Assert.AreEqual(1, msgs.Length, "Expected exactly one response (the cancellation error)")
    Assert.AreEqual(-32800, msgs.[0].GetProperty("error").GetProperty("code").GetInt32())

[<Test>]
let testShutdownWithNoRunningHandlersReturnsImmediately () =
    // When there are no in-flight handlers, Shutdown should return without waiting.
    let stdin = makeInputStream []
    use stdout = new MemoryStream()

    let server = startJsonRpcTransport stdin stdout None (fun _ -> Map.empty, Map.empty)

    let shutdownTask = shutdownJsonRpcTransport server |> Async.StartAsTask
    let completed = shutdownTask.Wait(TimeSpan.FromSeconds 5.0)
    Assert.IsTrue(completed, "Shutdown with no running handlers should return immediately")

[<Test>]
let testShutdownDrainsAllConcurrentHandlers () =
    // Three handlers running concurrently when Shutdown arrives.
    // All three must finish (or be cancelled) before shutdownJsonRpcTransport returns.
    let count = 3
    use allStarted = new CountdownEvent(count)
    // Use a TCS so we can release handlers from outside without blocking a thread
    let mayFinishTcs = System.Threading.Tasks.TaskCompletionSource<unit>()
    let finishedCount = ref 0

    let handler _ctx = async {
        allStarted.Signal() |> ignore
        do! mayFinishTcs.Task |> Async.AwaitTask
        System.Threading.Interlocked.Increment(finishedCount) |> ignore
        return Ok(JsonSerializer.SerializeToElement(null: obj | null))
    }

    use clientToServer =
        new IO.Pipes.AnonymousPipeServerStream(IO.Pipes.PipeDirection.Out)

    use serverStdin =
        new IO.Pipes.AnonymousPipeClientStream(IO.Pipes.PipeDirection.In, clientToServer.ClientSafePipeHandle)

    use stdout = new MemoryStream()

    let server =
        startJsonRpcTransport serverStdin stdout None (fun _ -> Map.ofList [ "test/h", handler ], Map.empty)

    for i in 1..count do
        writeMessageToPipe
            clientToServer
            (JsonSerializer.Serialize(
                {| jsonrpc = "2.0"
                   id = i
                   ``method`` = "test/h"
                   ``params`` = {| |} |}
            ))

    Assert.IsTrue(allStarted.Wait(10000), "All handlers should have started")

    let shutdownTask = shutdownJsonRpcTransport server |> Async.StartAsTask

    Assert.IsFalse(
        shutdownTask.Wait(TimeSpan.FromMilliseconds 500.0),
        "Shutdown should not complete while handlers are running"
    )

    // During the drain the transport must report ShuttingDown and all three handlers
    // still occupying RunningInboundRequests.
    let drainingStats = getJsonRpcStats server |> Async.RunSynchronously
    Assert.AreEqual("ShuttingDown", drainingStats.Phase)

    Assert.AreEqual(
        count,
        drainingStats.RunningInboundRequestCount,
        "All handlers should be listed as running during drain"
    )

    mayFinishTcs.SetResult(())

    let completed = shutdownTask.Wait(TimeSpan.FromSeconds 5.0)
    Assert.IsTrue(completed, "Shutdown should complete after all handlers finish")
    Assert.AreEqual(count, finishedCount.Value, "All handlers should have finished")

    // Once fully stopped, the phase must flip to Stopped and the map be empty.
    let stoppedStats = getJsonRpcStats server |> Async.RunSynchronously
    Assert.AreEqual("Stopped", stoppedStats.Phase)
    Assert.AreEqual(0, stoppedStats.RunningInboundRequestCount, "No handlers should remain after shutdown completes")

// ---- Late sendJsonRpc* guard tests ----

[<Test>]
let testSendNotificationAfterShutdownIsDropped () =
    // After shutdown, sendJsonRpcNotification should return immediately without
    // writing anything to the wire.
    let stdin = makeInputStream []
    use stdout = new MemoryStream()

    let server = startJsonRpcTransport stdin stdout None (fun _ -> Map.empty, Map.empty)

    shutdownJsonRpcTransport server |> Async.RunSynchronously

    let bytesBeforeSend = stdout.Length

    // This must return and must not block or raise
    sendJsonRpcNotification server "window/logMessage" (JsonSerializer.SerializeToElement({| message = "late" |}))
    |> Async.RunSynchronously

    Assert.AreEqual(bytesBeforeSend, stdout.Length, "No bytes should be written for a post-shutdown notification")

[<Test>]
let testSendCallAfterShutdownReturnsError () =
    // After shutdown, sendJsonRpcCall should return Error -32099 immediately.
    let stdin = makeInputStream []
    use stdout = new MemoryStream()

    let server = startJsonRpcTransport stdin stdout None (fun _ -> Map.empty, Map.empty)

    shutdownJsonRpcTransport server |> Async.RunSynchronously

    let result =
        sendJsonRpcCall server "workspace/configuration" (JsonSerializer.SerializeToElement({| |}))
        |> Async.RunSynchronously

    match result with
    | Error err ->
        Assert.AreEqual(-32099, err.GetProperty("code").GetInt32(), "Expected error code -32099")
        Assert.AreEqual("Transport shut down", err.GetProperty("message").GetString())
    | Ok _ -> Assert.Fail("Expected Error -32099 for post-shutdown sendJsonRpcCall, got Ok")

[<Test>]
let testSendCallDuringShutdownDrainReturnsError () =
    // A sendJsonRpcCall that arrives while the transport is in ShuttingDown (draining
    // a slow handler) should be rejected immediately rather than being parked forever
    // in PendingOutboundCalls.
    use handlerStarted = new ManualResetEventSlim(false)
    // Use a TCS so the handler can be released without blocking a thread pool thread;
    // this also means the CancellationToken is observable (not masked by a blocking Wait).
    let mayFinishTcs = System.Threading.Tasks.TaskCompletionSource<unit>()

    let handler (_ctx: JsonRpcRequestContext) = async {
        handlerStarted.Set()
        // Await the TCS — this yields the thread so F# async cancellation can fire.
        do! mayFinishTcs.Task |> Async.AwaitTask
        return Ok(JsonSerializer.SerializeToElement(null: obj | null))
    }

    use clientToServer =
        new IO.Pipes.AnonymousPipeServerStream(IO.Pipes.PipeDirection.Out)

    use serverStdin =
        new IO.Pipes.AnonymousPipeClientStream(IO.Pipes.PipeDirection.In, clientToServer.ClientSafePipeHandle)

    use stdout = new MemoryStream()

    let server =
        startJsonRpcTransport serverStdin stdout None (fun _ -> Map.ofList [ "test/slow", handler ], Map.empty)

    writeMessageToPipe
        clientToServer
        (JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               id = 1
               ``method`` = "test/slow"
               ``params`` = {| |} |}
        ))

    Assert.IsTrue(handlerStarted.Wait(5000), "Handler should have started")

    // Begin shutdown — the actor will immediately fire cancellations on all handlers
    let shutdownTask = shutdownJsonRpcTransport server |> Async.StartAsTask

    // The handler awaits a Task (not a blocking Wait), so the actor's thread is free
    // to process the Shutdown event. Give it a short beat to do so.
    Thread.Sleep 200

    // This call arrives while the drain is in progress; it must return Error immediately
    let lateCallTask =
        sendJsonRpcCall server "workspace/configuration" (JsonSerializer.SerializeToElement({| |}))
        |> Async.StartAsTask

    let lateCompleted = lateCallTask.Wait(TimeSpan.FromSeconds 5.0)
    Assert.IsTrue(lateCompleted, "Late sendJsonRpcCall should return immediately during drain")

    match lateCallTask.Result with
    | Error err ->
        Assert.AreEqual(-32099, err.GetProperty("code").GetInt32(), "Expected error code -32099")
        Assert.AreEqual("Transport shut down", err.GetProperty("message").GetString())
    | Ok _ -> Assert.Fail("Expected Error -32099 for call made during shutdown drain")

    // Unblock the handler so the drain completes and shutdown returns
    mayFinishTcs.SetResult(())

    Assert.IsTrue(shutdownTask.Wait(TimeSpan.FromSeconds 5.0), "Shutdown should complete after handler finishes")

// ---- Pending outbound call shutdown tests ----

[<Test>]
let testShutdownFailsPendingOutboundCall () =
    // Start a single sendJsonRpcCall that will never receive a response, then shut down
    // the transport. The call should unblock immediately with Error -32099.
    use clientToServer =
        new IO.Pipes.AnonymousPipeServerStream(IO.Pipes.PipeDirection.Out)

    use serverStdin =
        new IO.Pipes.AnonymousPipeClientStream(IO.Pipes.PipeDirection.In, clientToServer.ClientSafePipeHandle)

    use stdout = new MemoryStream()

    let server =
        startJsonRpcTransport serverStdin stdout None (fun _ -> Map.empty, Map.empty)

    // Fire an outbound call and leave it hanging (no response sent)
    let replyTask =
        sendJsonRpcCall server "test/hanging" (JsonSerializer.SerializeToElement({| |}))
        |> Async.StartAsTask

    // Wait for the request to appear on stdout so we know it was sent
    waitForMessages stdout 1 5000 |> ignore

    // Shut down — this should immediately fail the pending call
    shutdownJsonRpcTransport server |> Async.RunSynchronously

    let completed = replyTask.Wait(TimeSpan.FromSeconds(5.0))
    Assert.IsTrue(completed, "Pending sendJsonRpcCall should have unblocked after shutdown")

    match replyTask.Result with
    | Error err ->
        Assert.AreEqual(-32099, err.GetProperty("code").GetInt32(), "Expected error code -32099")
        Assert.AreEqual("Transport shut down", err.GetProperty("message").GetString())
    | Ok _ -> Assert.Fail("Expected Error -32099 but got Ok")

[<Test>]
let testShutdownFailsMultiplePendingOutboundCalls () =
    // Two concurrent sendJsonRpcCalls, neither receiving a response — both should
    // be failed with Error -32099 on shutdown.
    use clientToServer =
        new IO.Pipes.AnonymousPipeServerStream(IO.Pipes.PipeDirection.Out)

    use serverStdin =
        new IO.Pipes.AnonymousPipeClientStream(IO.Pipes.PipeDirection.In, clientToServer.ClientSafePipeHandle)

    use stdout = new MemoryStream()

    let server =
        startJsonRpcTransport serverStdin stdout None (fun _ -> Map.empty, Map.empty)

    let reply1Task =
        sendJsonRpcCall server "test/hang1" (JsonSerializer.SerializeToElement({| |}))
        |> Async.StartAsTask

    let reply2Task =
        sendJsonRpcCall server "test/hang2" (JsonSerializer.SerializeToElement({| |}))
        |> Async.StartAsTask

    // Wait for both requests to be written before shutting down
    waitForMessages stdout 2 5000 |> ignore

    shutdownJsonRpcTransport server |> Async.RunSynchronously

    let completed1 = reply1Task.Wait(TimeSpan.FromSeconds(5.0))
    let completed2 = reply2Task.Wait(TimeSpan.FromSeconds(5.0))

    Assert.IsTrue(completed1, "First pending call should have unblocked after shutdown")
    Assert.IsTrue(completed2, "Second pending call should have unblocked after shutdown")

    for replyTask in [ reply1Task; reply2Task ] do
        match replyTask.Result with
        | Error err ->
            Assert.AreEqual(-32099, err.GetProperty("code").GetInt32(), "Expected error code -32099")
            Assert.AreEqual("Transport shut down", err.GetProperty("message").GetString())
        | Ok _ -> Assert.Fail("Expected Error -32099 but got Ok")

[<Test>]
let testEofFailsPendingOutboundCall () =
    // A pending sendJsonRpcCall should also be failed when the transport detects EOF
    // on stdin (i.e. the client disconnects) rather than an explicit Shutdown event.
    use clientToServer =
        new IO.Pipes.AnonymousPipeServerStream(IO.Pipes.PipeDirection.Out)

    use serverStdin =
        new IO.Pipes.AnonymousPipeClientStream(IO.Pipes.PipeDirection.In, clientToServer.ClientSafePipeHandle)

    use stdout = new MemoryStream()

    let server =
        startJsonRpcTransport serverStdin stdout None (fun _ -> Map.empty, Map.empty)

    let replyTask =
        sendJsonRpcCall server "test/hanging" (JsonSerializer.SerializeToElement({| |}))
        |> Async.StartAsTask

    // Wait for the request to be written
    waitForMessages stdout 1 5000 |> ignore

    // Close the write end of the pipe — this causes the server to read EOF on stdin
    clientToServer.Dispose()

    let completed = replyTask.Wait(TimeSpan.FromSeconds(5.0))
    Assert.IsTrue(completed, "Pending sendJsonRpcCall should have unblocked after EOF")

    match replyTask.Result with
    | Error err ->
        Assert.AreEqual(-32099, err.GetProperty("code").GetInt32(), "Expected error code -32099")
        Assert.AreEqual("Transport shut down", err.GetProperty("message").GetString())
    | Ok _ -> Assert.Fail("Expected Error -32099 but got Ok")

[<Test>]
let testEofCancelsRunningHandler () =
    // A handler that is running when EOF is detected should be cancelled.
    // The transport should send a -32800 cancellation error response.
    use handlerStarted = new ManualResetEventSlim(false)

    let handler _ctx = async {
        handlerStarted.Set()
        do! Async.Sleep 60_000 // would block forever without cancellation
        return Ok(JsonSerializer.SerializeToElement(null: obj | null))
    }

    use clientToServer =
        new IO.Pipes.AnonymousPipeServerStream(IO.Pipes.PipeDirection.Out)

    use serverStdin =
        new IO.Pipes.AnonymousPipeClientStream(IO.Pipes.PipeDirection.In, clientToServer.ClientSafePipeHandle)

    use stdout = new MemoryStream()

    let server =
        startJsonRpcTransport serverStdin stdout None (fun _ -> Map.ofList [ "test/slow", handler ], Map.empty)

    writeMessageToPipe
        clientToServer
        (JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               id = 1
               ``method`` = "test/slow"
               ``params`` = {| |} |}
        ))

    Assert.IsTrue(handlerStarted.Wait(5000), "Handler should have started")

    // Close the write end of the pipe — triggers EOF on stdin
    clientToServer.Dispose()

    // The handler should be cancelled and the -32800 response written
    let msgs = waitForMessages stdout 1 5000
    Assert.AreEqual(1, msgs.Length, "Expected exactly one response (the cancellation error)")
    Assert.AreEqual(1, msgs.[0].GetProperty("id").GetInt32())
    Assert.AreEqual(-32800, msgs.[0].GetProperty("error").GetProperty("code").GetInt32())

[<Test>]
let testEofLateCallReturnsError () =
    // After EOF, the transport is in ShuttingDown/Stopped — sendJsonRpcCall should
    // return Error -32099 immediately rather than hanging.
    use handlerStarted = new ManualResetEventSlim(false)
    let mayFinishTcs = System.Threading.Tasks.TaskCompletionSource<unit>()

    let handler _ctx = async {
        handlerStarted.Set()
        do! mayFinishTcs.Task |> Async.AwaitTask
        return Ok(JsonSerializer.SerializeToElement(null: obj | null))
    }

    use clientToServer =
        new IO.Pipes.AnonymousPipeServerStream(IO.Pipes.PipeDirection.Out)

    use serverStdin =
        new IO.Pipes.AnonymousPipeClientStream(IO.Pipes.PipeDirection.In, clientToServer.ClientSafePipeHandle)

    use stdout = new MemoryStream()

    let server =
        startJsonRpcTransport serverStdin stdout None (fun _ -> Map.ofList [ "test/slow", handler ], Map.empty)

    writeMessageToPipe
        clientToServer
        (JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               id = 1
               ``method`` = "test/slow"
               ``params`` = {| |} |}
        ))

    Assert.IsTrue(handlerStarted.Wait(5000), "Handler should have started")

    // Trigger EOF; give the actor a moment to process it and move to ShuttingDown
    clientToServer.Dispose()
    Thread.Sleep 200

    // A call made now must be rejected immediately
    let lateCallTask =
        sendJsonRpcCall server "workspace/configuration" (JsonSerializer.SerializeToElement({| |}))
        |> Async.StartAsTask

    let completed = lateCallTask.Wait(TimeSpan.FromSeconds 5.0)
    Assert.IsTrue(completed, "Late sendJsonRpcCall after EOF should return immediately")

    match lateCallTask.Result with
    | Error err ->
        Assert.AreEqual(-32099, err.GetProperty("code").GetInt32(), "Expected error code -32099")
        Assert.AreEqual("Transport shut down", err.GetProperty("message").GetString())
    | Ok _ -> Assert.Fail("Expected Error -32099 for call made after EOF")

    // Unblock the handler so the drain completes cleanly
    mayFinishTcs.SetResult(())

[<Test>]
let testEofDrainsAllConcurrentHandlers () =
    // Multiple handlers running concurrently when EOF arrives — all should be
    // cancelled and the transport should drain before AwaitShutdown returns.
    let count = 3
    use allStarted = new CountdownEvent(count)
    let mayFinishTcs = System.Threading.Tasks.TaskCompletionSource<unit>()
    let finishedCount = ref 0

    let handler _ctx = async {
        allStarted.Signal() |> ignore
        do! mayFinishTcs.Task |> Async.AwaitTask
        System.Threading.Interlocked.Increment(finishedCount) |> ignore
        return Ok(JsonSerializer.SerializeToElement(null: obj | null))
    }

    use clientToServer =
        new IO.Pipes.AnonymousPipeServerStream(IO.Pipes.PipeDirection.Out)

    use serverStdin =
        new IO.Pipes.AnonymousPipeClientStream(IO.Pipes.PipeDirection.In, clientToServer.ClientSafePipeHandle)

    use stdout = new MemoryStream()

    let server =
        startJsonRpcTransport serverStdin stdout None (fun _ -> Map.ofList [ "test/h", handler ], Map.empty)

    for i in 1..count do
        writeMessageToPipe
            clientToServer
            (JsonSerializer.Serialize(
                {| jsonrpc = "2.0"
                   id = i
                   ``method`` = "test/h"
                   ``params`` = {| |} |}
            ))

    Assert.IsTrue(allStarted.Wait(10000), "All handlers should have started")

    // Register a shutdown waiter before triggering EOF
    let awaitTask = awaitJsonRpcTransportShutdown server |> Async.StartAsTask

    // Trigger EOF — all handlers should be cancelled
    clientToServer.Dispose()

    // awaitShutdown should not return while handlers are still blocked
    Assert.IsFalse(
        awaitTask.Wait(TimeSpan.FromMilliseconds 500.0),
        "awaitJsonRpcTransportShutdown should not return while handlers are running"
    )

    // Unblock all handlers
    mayFinishTcs.SetResult(())

    let completed = awaitTask.Wait(TimeSpan.FromSeconds 5.0)
    Assert.IsTrue(completed, "awaitJsonRpcTransportShutdown should complete after all handlers finish")

[<Test>]
let testAlreadyResolvedCallIsUnaffectedByShutdown () =
    // A call that already received its Ok response before shutdown should retain that
    // result — shutdown must not double-resolve or corrupt it.
    use clientToServer =
        new IO.Pipes.AnonymousPipeServerStream(IO.Pipes.PipeDirection.Out)

    use serverStdin =
        new IO.Pipes.AnonymousPipeClientStream(IO.Pipes.PipeDirection.In, clientToServer.ClientSafePipeHandle)

    use stdout = new MemoryStream()

    let server =
        startJsonRpcTransport serverStdin stdout None (fun _ -> Map.empty, Map.empty)

    let replyTask =
        sendJsonRpcCall server "test/will-respond" (JsonSerializer.SerializeToElement({| |}))
        |> Async.StartAsTask

    // Wait for the outbound request, capture its id
    let outboundMessages = waitForMessages stdout 1 5000
    let outboundId = outboundMessages.[0].GetProperty("id").GetInt32()

    // Send a successful response back — call is now resolved
    let successResponse =
        JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               id = outboundId
               result = {| value = 42 |} |}
        )

    let responseBytes = encodeMessage successResponse
    clientToServer.Write(responseBytes, 0, responseBytes.Length)
    clientToServer.Flush()

    let resolvedBeforeShutdown = replyTask.Wait(TimeSpan.FromSeconds(5.0))
    Assert.IsTrue(resolvedBeforeShutdown, "Call should have resolved before shutdown")

    match replyTask.Result with
    | Ok reply -> Assert.AreEqual(42, reply.GetProperty("value").GetInt32())
    | Error _ -> Assert.Fail("Expected Ok response, not an error")

    // Now shut down — the already-resolved call must not be touched
    shutdownJsonRpcTransport server |> Async.RunSynchronously

    // Result must still be Ok with the original value
    match replyTask.Result with
    | Ok reply ->
        Assert.AreEqual(42, reply.GetProperty("value").GetInt32(), "Result should be unchanged after shutdown")
    | Error _ -> Assert.Fail("Shutdown should not have overwritten an already-resolved Ok result")

[<Test>]
let testSendRequestAssignsIncrementingIds () =
    // Use an open pipe so stdin never hits EOF while we are checking outbound IDs.
    let writeEnd, stdin = makeOpenStdin ()
    use writeEnd = writeEnd
    let stdout = new MemoryStream()

    let server = startJsonRpcTransport stdin stdout None (fun _ -> Map.empty, Map.empty)

    // Fire two requests (we won't resolve them — just check the outbound IDs)
    let _reply1 =
        sendJsonRpcCall server "test/req1" (JsonSerializer.SerializeToElement({| |}))
        |> Async.StartAsTask

    let _reply2 =
        sendJsonRpcCall server "test/req2" (JsonSerializer.SerializeToElement({| |}))
        |> Async.StartAsTask

    let requests = waitForMessages stdout 2 5000

    writeEnd.Dispose()

    Assert.AreEqual(2, requests.Length, "Expected 2 outbound requests")

    let req1 = requests.[0]
    let req2 = requests.[1]

    let id1 = req1.GetProperty("id").GetInt32()
    let id2 = req2.GetProperty("id").GetInt32()

    Assert.AreEqual(id1 + 1, id2, "Outbound request IDs should be sequential")

[<Test>]
let testInboundResponseForUnknownIdIsHandledGracefully () =
    // Send a response with an id that doesn't match any outstanding request
    let unknownResponse =
        JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               id = 9999
               result = "orphan" |}
        )

    let stdin = makeInputStream [ unknownResponse ]
    let stdout = new MemoryStream()

    let _server =
        startJsonRpcTransport stdin stdout None (fun _ -> Map.empty, Map.empty)

    // Should not crash and should produce no output
    Async.Sleep 500 |> Async.RunSynchronously
    Assert.AreEqual(0L, stdout.Length, "Expected no output for response with unknown id")

[<Test>]
let testSendCallReturnsErrorOnMethodNotFound () =
    use clientToServer =
        new IO.Pipes.AnonymousPipeServerStream(IO.Pipes.PipeDirection.Out)

    use serverStdin =
        new IO.Pipes.AnonymousPipeClientStream(IO.Pipes.PipeDirection.In, clientToServer.ClientSafePipeHandle)

    use stdout = new MemoryStream()

    let server =
        startJsonRpcTransport serverStdin stdout None (fun _ -> Map.empty, Map.empty)

    let replyTask =
        sendJsonRpcCall server "test/unknown" (JsonSerializer.SerializeToElement({| |}))
        |> Async.StartAsTask

    // Wait for the outbound request
    let outboundMessages = waitForMessages stdout 1 5000
    let outboundId = outboundMessages.[0].GetProperty("id").GetInt32()

    // Simulate client responding with MethodNotFound error
    let errorResponse =
        JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               id = outboundId
               error =
                {| code = -32601
                   message = "Method not found: 'test/unknown'" |} |}
        )

    let responseBytes = encodeMessage errorResponse
    clientToServer.Write(responseBytes, 0, responseBytes.Length)
    clientToServer.Flush()

    let completed = replyTask.Wait(TimeSpan.FromSeconds(5.0))
    Assert.IsTrue(completed, "SendCall should have received a reply")

    match replyTask.Result with
    | Error err ->
        Assert.AreEqual(-32601, err.GetProperty("code").GetInt32())

        Assert.IsTrue(
            err.GetProperty("message").GetString()
            |> nonNull "error message"
            |> _.Contains("Method not found")
        )
    | Ok _ -> Assert.Fail("Expected Error but got Ok")

    shutdownJsonRpcTransport server |> Async.RunSynchronously

[<Test>]
let testSendCallReturnsErrorWithData () =
    use clientToServer =
        new IO.Pipes.AnonymousPipeServerStream(IO.Pipes.PipeDirection.Out)

    use serverStdin =
        new IO.Pipes.AnonymousPipeClientStream(IO.Pipes.PipeDirection.In, clientToServer.ClientSafePipeHandle)

    use stdout = new MemoryStream()

    let server =
        startJsonRpcTransport serverStdin stdout None (fun _ -> Map.empty, Map.empty)

    let replyTask =
        sendJsonRpcCall server "test/failing" (JsonSerializer.SerializeToElement({| |}))
        |> Async.StartAsTask

    let outboundMessages = waitForMessages stdout 1 5000
    let outboundId = outboundMessages.[0].GetProperty("id").GetInt32()

    // Simulate client responding with an error that includes data
    let errorResponse =
        JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               id = outboundId
               error =
                {| code = -32603
                   message = "Internal error"
                   data =
                    {| details = "something went wrong"
                       retry = false |} |} |}
        )

    let responseBytes = encodeMessage errorResponse
    clientToServer.Write(responseBytes, 0, responseBytes.Length)
    clientToServer.Flush()

    let completed = replyTask.Wait(TimeSpan.FromSeconds(5.0))
    Assert.IsTrue(completed, "SendCall should have received a reply")

    match replyTask.Result with
    | Error err ->
        Assert.AreEqual(-32603, err.GetProperty("code").GetInt32())
        Assert.AreEqual("Internal error", err.GetProperty("message").GetString())
        Assert.AreEqual("something went wrong", err.GetProperty("data").GetProperty("details").GetString())
        Assert.AreEqual(false, err.GetProperty("data").GetProperty("retry").GetBoolean())
    | Ok _ -> Assert.Fail("Expected Error but got Ok")

    shutdownJsonRpcTransport server |> Async.RunSynchronously

[<Test>]
let testSendCallSuccessAndErrorForDifferentRequests () =
    // Send two outbound calls; one succeeds, one fails — verify each gets the right result
    use clientToServer =
        new IO.Pipes.AnonymousPipeServerStream(IO.Pipes.PipeDirection.Out)

    use serverStdin =
        new IO.Pipes.AnonymousPipeClientStream(IO.Pipes.PipeDirection.In, clientToServer.ClientSafePipeHandle)

    use stdout = new MemoryStream()

    let server =
        startJsonRpcTransport serverStdin stdout None (fun _ -> Map.empty, Map.empty)

    let reply1Task =
        sendJsonRpcCall server "test/ok" (JsonSerializer.SerializeToElement({| |}))
        |> Async.StartAsTask

    let reply2Task =
        sendJsonRpcCall server "test/fail" (JsonSerializer.SerializeToElement({| |}))
        |> Async.StartAsTask

    let outboundMessages = waitForMessages stdout 2 5000
    Assert.AreEqual(2, outboundMessages.Length, "Expected 2 outbound requests")

    let id1 = outboundMessages.[0].GetProperty("id").GetInt32()
    let id2 = outboundMessages.[1].GetProperty("id").GetInt32()

    // Send success for first, error for second
    let successResponse =
        JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               id = id1
               result = {| status = "ok" |} |}
        )

    let errorResponse =
        JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               id = id2
               error =
                {| code = -32600
                   message = "Invalid request" |} |}
        )

    // Send both responses
    for resp in [ successResponse; errorResponse ] do
        let bytes = encodeMessage resp
        clientToServer.Write(bytes, 0, bytes.Length)

    clientToServer.Flush()

    let completed1 = reply1Task.Wait(TimeSpan.FromSeconds(5.0))
    let completed2 = reply2Task.Wait(TimeSpan.FromSeconds(5.0))
    Assert.IsTrue(completed1 && completed2, "Both calls should have received replies")

    match reply1Task.Result with
    | Ok reply -> Assert.AreEqual("ok", reply.GetProperty("status").GetString())
    | Error _ -> Assert.Fail("Expected Ok for first call")

    match reply2Task.Result with
    | Error err ->
        Assert.AreEqual(-32600, err.GetProperty("code").GetInt32())
        Assert.AreEqual("Invalid request", err.GetProperty("message").GetString())
    | Ok _ -> Assert.Fail("Expected Error for second call")

    shutdownJsonRpcTransport server |> Async.RunSynchronously

[<Test>]
let testMixedInboundRequestsAndOutboundNotifications () =
    // Server handles an inbound request while also sending outbound notifications
    let handler _ctx = async { return Ok(JsonSerializer.SerializeToElement("handled")) }

    let inboundRequest =
        JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               id = 1
               ``method`` = "test/handle"
               ``params`` = {| |} |}
        )

    let requestHandlings = Map.ofList [ "test/handle", handler ]

    let writeEnd, stdin = makeOpenStdin ()
    use writeEnd = writeEnd
    let stdout = new MemoryStream()

    let server =
        startJsonRpcTransport stdin stdout None (fun _ -> requestHandlings, Map.empty)

    // Write the inbound request into the open pipe
    let requestBytes = encodeMessage inboundRequest
    writeEnd.Write(requestBytes, 0, requestBytes.Length)
    writeEnd.Flush()

    // Also send a notification from the server side
    let notifBody = JsonSerializer.SerializeToElement({| kind = "info" |})

    sendJsonRpcNotification server "window/logMessage" notifBody
    |> Async.RunSynchronously

    // Wait for both messages to be written (response + notification)
    let allMsgs = waitForMessages stdout 2 5000

    Assert.AreEqual(2, allMsgs.Length, "Expected 2 messages on stdout (1 response + 1 notification)")

    let responses = allMsgs |> List.filter (fun m -> m.TryGetProperty("result") |> fst)

    let notifications =
        allMsgs |> List.filter (fun m -> m.TryGetProperty("method") |> fst)

    Assert.AreEqual(1, responses.Length, "Expected 1 response")
    Assert.AreEqual(1, notifications.Length, "Expected 1 notification")

    Assert.AreEqual(1, responses.[0].GetProperty("id").GetInt32())
    Assert.AreEqual("handled", responses.[0].GetProperty("result").GetString())

    Assert.AreEqual("window/logMessage", notifications.[0].GetProperty("method").GetString())
    Assert.AreEqual("info", notifications.[0].GetProperty("params").GetProperty("kind").GetString())

// ---- $/cancelRequest tests ----

[<Test>]
let testCancelRequestCancelsRunningHandler () =
    // Start a request with a slow handler, send $/cancelRequest, verify cancellation response
    use handlerStarted = new ManualResetEventSlim(false)

    let handler _ctx = async {
        handlerStarted.Set()
        // Simulate a long-running operation that respects cancellation
        do! Async.Sleep 30000
        // Should not reach here if cancelled
        return Ok(JsonSerializer.SerializeToElement("should-not-see"))
    }

    let requestHandlings = Map.ofList [ "test/slow", handler ]

    use clientToServer =
        new IO.Pipes.AnonymousPipeServerStream(IO.Pipes.PipeDirection.Out)

    use serverStdin =
        new IO.Pipes.AnonymousPipeClientStream(IO.Pipes.PipeDirection.In, clientToServer.ClientSafePipeHandle)

    use stdout = new MemoryStream()

    let server =
        startJsonRpcTransport serverStdin stdout None (fun _ -> requestHandlings, Map.empty)

    // Send the slow request
    let request =
        JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               id = 1
               ``method`` = "test/slow"
               ``params`` = {| |} |}
        )

    writeMessageToPipe clientToServer request

    // Wait for the handler to start
    Assert.IsTrue(handlerStarted.Wait(5000), "Handler should have started")

    // Send $/cancelRequest
    let cancelNotification =
        JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               ``method`` = "$/cancelRequest"
               ``params`` = {| id = 1 |} |}
        )

    writeMessageToPipe clientToServer cancelNotification

    // Wait for the cancellation error response
    let responseOpt = waitForResponse stdout 5000 |> Async.RunSynchronously

    Assert.IsTrue(responseOpt.IsSome, "Expected a cancellation error response")

    let response = responseOpt.Value
    Assert.AreEqual(1, response.GetProperty("id").GetInt32())

    Assert.AreEqual(
        -32800,
        response.GetProperty("error").GetProperty("code").GetInt32(),
        "Expected RequestCancelled error code (-32800)"
    )

    Assert.IsTrue(
        response.GetProperty("error").GetProperty("message").GetString()
        |> nonNull "error message"
        |> _.Contains("cancel", StringComparison.OrdinalIgnoreCase)
    )

    shutdownJsonRpcTransport server |> Async.RunSynchronously

[<Test>]
let testCancelRequestForUnknownIdIsIgnored () =
    // Send $/cancelRequest for an ID that was never sent — should not crash or produce output
    use clientToServer =
        new IO.Pipes.AnonymousPipeServerStream(IO.Pipes.PipeDirection.Out)

    use serverStdin =
        new IO.Pipes.AnonymousPipeClientStream(IO.Pipes.PipeDirection.In, clientToServer.ClientSafePipeHandle)

    use stdout = new MemoryStream()

    let server =
        startJsonRpcTransport serverStdin stdout None (fun _ -> Map.empty, Map.empty)

    let cancelNotification =
        JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               ``method`` = "$/cancelRequest"
               ``params`` = {| id = 999 |} |}
        )

    writeMessageToPipe clientToServer cancelNotification

    Async.Sleep 500 |> Async.RunSynchronously
    Assert.AreEqual(0L, stdout.Length, "Expected no output for cancel of unknown request id")

    shutdownJsonRpcTransport server |> Async.RunSynchronously

[<Test>]
let testCancelRequestForAlreadyCompletedRequestIsIgnored () =
    // Send a request, wait for it to complete, then send $/cancelRequest for same ID
    let handler _ctx = async { return Ok(JsonSerializer.SerializeToElement("done")) }

    let requestHandlings = Map.ofList [ "test/fast", handler ]

    use clientToServer =
        new IO.Pipes.AnonymousPipeServerStream(IO.Pipes.PipeDirection.Out)

    use serverStdin =
        new IO.Pipes.AnonymousPipeClientStream(IO.Pipes.PipeDirection.In, clientToServer.ClientSafePipeHandle)

    use stdout = new MemoryStream()

    let server =
        startJsonRpcTransport serverStdin stdout None (fun _ -> requestHandlings, Map.empty)

    // Send the request
    let request =
        JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               id = 42
               ``method`` = "test/fast"
               ``params`` = {| |} |}
        )

    writeMessageToPipe clientToServer request

    // Wait for the response
    let responseOpt = waitForResponse stdout 5000 |> Async.RunSynchronously
    Assert.IsTrue(responseOpt.IsSome, "Expected a response")
    Assert.AreEqual(42, responseOpt.Value.GetProperty("id").GetInt32())
    Assert.AreEqual("done", responseOpt.Value.GetProperty("result").GetString())

    // Now send $/cancelRequest for the same ID — should be silently ignored
    let cancelNotification =
        JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               ``method`` = "$/cancelRequest"
               ``params`` = {| id = 42 |} |}
        )

    writeMessageToPipe clientToServer cancelNotification

    // Give time for the cancel to be processed
    Async.Sleep 500 |> Async.RunSynchronously

    // Should still only have the one original response
    stdout.Position <- 0L
    let allMessages = waitForMessages stdout 1 1000

    Assert.AreEqual(
        1,
        allMessages.Length,
        "Expected exactly 1 message (the original response, not a cancellation error)"
    )

    shutdownJsonRpcTransport server |> Async.RunSynchronously

[<Test>]
let testHandlerObservesCancellationToken () =
    // Verify that a handler's ambient CancellationToken gets cancelled when $/cancelRequest arrives.
    // We use CancellationToken.Register to observe cancellation directly, since F# async
    // cancellation bypasses try/catch and goes through the cancellation continuation.
    use handlerStarted = new ManualResetEventSlim(false)
    use tokenWasCancelled = new ManualResetEventSlim(false)

    let handler
        (_ctx: JsonRpcRequestContext)
        : Async<Result<System.Text.Json.JsonElement, System.Text.Json.JsonElement>> =
        async {
            let! ct = Async.CancellationToken
            ct.Register(fun () -> tokenWasCancelled.Set()) |> ignore
            handlerStarted.Set()

            // Block until cancelled
            do! Async.Sleep 30000
            return Ok(JsonSerializer.SerializeToElement(null: obj | null))
        }

    let requestHandlings = Map.ofList [ "test/cancellable", handler ]

    use clientToServer =
        new IO.Pipes.AnonymousPipeServerStream(IO.Pipes.PipeDirection.Out)

    use serverStdin =
        new IO.Pipes.AnonymousPipeClientStream(IO.Pipes.PipeDirection.In, clientToServer.ClientSafePipeHandle)

    use stdout = new MemoryStream()

    let server =
        startJsonRpcTransport serverStdin stdout None (fun _ -> requestHandlings, Map.empty)

    // Send the request
    let request =
        JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               id = 7
               ``method`` = "test/cancellable"
               ``params`` = {| |} |}
        )

    writeMessageToPipe clientToServer request

    Assert.IsTrue(handlerStarted.Wait(5000), "Handler should have started")

    // Cancel
    let cancelNotification =
        JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               ``method`` = "$/cancelRequest"
               ``params`` = {| id = 7 |} |}
        )

    writeMessageToPipe clientToServer cancelNotification

    // Wait for the cancellation error response
    let responseOpt = waitForResponse stdout 5000 |> Async.RunSynchronously

    Assert.IsTrue(responseOpt.IsSome, "Expected a cancellation error response")
    Assert.AreEqual(-32800, responseOpt.Value.GetProperty("error").GetProperty("code").GetInt32())

    // Verify the handler's CancellationToken was actually cancelled
    Assert.IsTrue(tokenWasCancelled.Wait(2000), "Handler's CancellationToken should have been cancelled")

    shutdownJsonRpcTransport server |> Async.RunSynchronously

[<Test>]
let testOtherRequestsStillWorkAfterCancellation () =
    // Cancel one request, then send another — second should complete normally
    use handlerStarted = new ManualResetEventSlim(false)

    let slowHandler _ctx = async {
        handlerStarted.Set()
        do! Async.Sleep 30000
        return Ok(JsonSerializer.SerializeToElement(null: obj | null))
    }

    let fastHandler _ctx = async { return Ok(JsonSerializer.SerializeToElement("ok")) }

    let requestHandlings =
        Map.ofList [ "test/slow", slowHandler; "test/fast", fastHandler ]

    use clientToServer =
        new IO.Pipes.AnonymousPipeServerStream(IO.Pipes.PipeDirection.Out)

    use serverStdin =
        new IO.Pipes.AnonymousPipeClientStream(IO.Pipes.PipeDirection.In, clientToServer.ClientSafePipeHandle)

    use stdout = new MemoryStream()

    let server =
        startJsonRpcTransport serverStdin stdout None (fun _ -> requestHandlings, Map.empty)

    // Send a slow request (will be cancelled)
    let slowRequest =
        JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               id = 1
               ``method`` = "test/slow"
               ``params`` = {| |} |}
        )

    writeMessageToPipe clientToServer slowRequest

    Assert.IsTrue(handlerStarted.Wait(5000), "Slow handler should have started")

    // Cancel the slow request
    let cancelNotification =
        JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               ``method`` = "$/cancelRequest"
               ``params`` = {| id = 1 |} |}
        )

    writeMessageToPipe clientToServer cancelNotification

    // Wait for the cancellation response
    let cancelResponse = waitForResponse stdout 5000 |> Async.RunSynchronously
    Assert.IsTrue(cancelResponse.IsSome, "Expected cancellation response")
    Assert.AreEqual(-32800, cancelResponse.Value.GetProperty("error").GetProperty("code").GetInt32())

    // Now send a fast request — should complete normally
    let fastRequest =
        JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               id = 2
               ``method`` = "test/fast"
               ``params`` = {| |} |}
        )

    // Reset stdout position tracking — need to read from current position
    let stdoutBefore = stdout.Length
    writeMessageToPipe clientToServer fastRequest

    // Wait for the second response
    waitUntil 5000 (fun () -> countMessages stdout >= 2) |> ignore
    Thread.Sleep 100

    let allMessages = waitForMessages stdout 2 1000

    // Find the fast response (id=2)
    let fastResponse =
        allMessages
        |> List.tryFind (fun m -> m.TryGetProperty("id") |> fst && m.GetProperty("id").GetInt32() = 2)

    Assert.IsTrue(fastResponse.IsSome, "Expected a response for the fast request (id=2)")
    Assert.AreEqual("ok", fastResponse.Value.GetProperty("result").GetString())

    shutdownJsonRpcTransport server |> Async.RunSynchronously

// ---- jsonrpc version field validation tests ----

[<Test>]
let testRequestWithMissingJsonRpcFieldReturnsInvalidRequest () =
    // A request without "jsonrpc": "2.0" must be rejected with -32600 Invalid Request.
    // The id is recoverable so must be echoed in the error response.
    let request =
        JsonSerializer.Serialize(
            {| id = 1
               ``method`` = "test/echo"
               ``params`` = {| |} |}
        )
    // Deliberately no "jsonrpc" field

    let stdin = makeInputStream [ request ]
    let stdout = new MemoryStream()

    let _server =
        startJsonRpcTransport stdin stdout None (fun _ -> Map.empty, Map.empty)

    let responseOpt = waitForResponse stdout 5000 |> Async.RunSynchronously

    Assert.IsTrue(responseOpt.IsSome, "Expected an error response for missing jsonrpc field")

    let response = responseOpt.Value
    Assert.AreEqual(1, response.GetProperty("id").GetInt32(), "Error response must echo the request id")

    Assert.AreEqual(
        -32600,
        response.GetProperty("error").GetProperty("code").GetInt32(),
        "Expected -32600 Invalid Request"
    )

[<Test>]
let testRequestWithWrongJsonRpcVersionReturnsInvalidRequest () =
    // A request with "jsonrpc": "1.0" (wrong version) must be rejected with -32600.
    let request =
        JsonSerializer.Serialize(
            {| jsonrpc = "1.0"
               id = 2
               ``method`` = "test/echo"
               ``params`` = {| |} |}
        )

    let stdin = makeInputStream [ request ]
    let stdout = new MemoryStream()

    let _server =
        startJsonRpcTransport stdin stdout None (fun _ -> Map.empty, Map.empty)

    let responseOpt = waitForResponse stdout 5000 |> Async.RunSynchronously

    Assert.IsTrue(responseOpt.IsSome, "Expected an error response for wrong jsonrpc version")

    let response = responseOpt.Value
    Assert.AreEqual(2, response.GetProperty("id").GetInt32(), "Error response must echo the request id")

    Assert.AreEqual(
        -32600,
        response.GetProperty("error").GetProperty("code").GetInt32(),
        "Expected -32600 Invalid Request"
    )

[<Test>]
let testNotificationWithMissingJsonRpcFieldIsSilentlyDropped () =
    // A notification (no id) with a missing "jsonrpc" field must be silently dropped.
    // No response is ever sent for notifications, so the only observable effect is
    // that the handler must NOT be called.
    let handlerCalled = ref false

    let notification =
        JsonSerializer.Serialize(
            {| ``method`` = "test/notify"
               ``params`` = {| |} |}
        )
    // Deliberately no "jsonrpc" field

    let handler _ctx = async { handlerCalled.Value <- true }

    let stdin = makeInputStream [ notification ]
    let stdout = new MemoryStream()

    let _server =
        startJsonRpcTransport stdin stdout None (fun _ -> Map.empty, Map.ofList [ "test/notify", handler ])

    Async.Sleep 500 |> Async.RunSynchronously

    Assert.AreEqual(0L, stdout.Length, "Dropped notification must produce no output")
    Assert.IsFalse(handlerCalled.Value, "Handler must not be called for invalid notification")

// ---- Per-call timeout tests ----

[<Test>]
let testSingleCallTimeoutWithNoResponseReturnsError () =
    // A call made with a 200 ms timeout against a peer that never replies must
    // return Error(-32000, "Call timed out") well before the 5 s wall-clock guard.
    let writeEnd, stdin = makeOpenStdin ()
    use writeEnd = writeEnd
    use stdout = new MemoryStream()

    let server = startJsonRpcTransport stdin stdout None (fun _ -> Map.empty, Map.empty)

    let sw = System.Diagnostics.Stopwatch.StartNew()

    let result =
        sendJsonRpcCallWithTimeout
            server
            "test/hang"
            (JsonSerializer.SerializeToElement({| |}))
            (Some(TimeSpan.FromMilliseconds 200.0))
        |> Async.StartAsTask

    // The call must complete within 500 ms of its deadline firing
    let completed = result.Wait(TimeSpan.FromMilliseconds 500.0)

    sw.Stop()

    Assert.IsTrue(completed, sprintf "Call should have timed out within 500 ms but took %d ms" sw.ElapsedMilliseconds)

    match result.Result with
    | Error err ->
        Assert.AreEqual(-32000, err.GetProperty("code").GetInt32(), "Expected error code -32000 (Call timed out)")

        Assert.AreEqual("Call timed out", err.GetProperty("message").GetString())
    | Ok _ -> Assert.Fail("Expected Error(-32000) for a timed-out call, got Ok")

    shutdownJsonRpcTransport server |> Async.RunSynchronously

// ---- getJsonRpcStats tests ----

[<Test>]
let testGetRpcStatsIdleTransportIsActiveWithEmptyCounters () =
    // A freshly started transport with no messages in flight must report Active phase
    // and zero counts across all counters.
    let writeEnd, stdin = makeOpenStdin ()
    use writeEnd = writeEnd
    use stdout = new MemoryStream()

    let server = startJsonRpcTransport stdin stdout None (fun _ -> Map.empty, Map.empty)

    let stats = getJsonRpcStats server |> Async.RunSynchronously

    Assert.AreEqual("Active", stats.Phase)
    Assert.AreEqual(0, stats.WriteQueueLength, "Idle transport should have empty write queue")
    Assert.AreEqual(0, stats.PendingOutboundCallCount, "Idle transport should have no pending outbound calls")
    Assert.AreEqual(0, stats.RunningInboundRequestCount, "Idle transport should have no running inbound requests")
    Assert.AreEqual(false, stats.TimerArmed, "Timer should not be armed when there are no timed calls")
    Assert.AreEqual(0, stats.RecentlyTimedOutCallCount, "No recently timed-out calls on a fresh transport")

    shutdownJsonRpcTransport server |> Async.RunSynchronously

[<Test>]
let testGetRpcStatsRunningHandlerIsVisible () =
    // While a slow inbound handler is executing, RunningInboundRequestCount must be 1.
    // Once it completes and the response is flushed, the count must drop back to 0.
    use handlerStarted = new ManualResetEventSlim(false)
    use handlerMayFinish = new ManualResetEventSlim(false)

    let handler _ctx = async {
        handlerStarted.Set()
        // Use AwaitWaitHandle so the thread-pool thread is released while waiting,
        // leaving the pool free for the actor mailbox and getJsonRpcStats to run.
        do! Async.AwaitWaitHandle(handlerMayFinish.WaitHandle) |> Async.Ignore
        return Ok(JsonSerializer.SerializeToElement(null: obj | null))
    }

    use clientToServer =
        new IO.Pipes.AnonymousPipeServerStream(IO.Pipes.PipeDirection.Out)

    use serverStdin =
        new IO.Pipes.AnonymousPipeClientStream(IO.Pipes.PipeDirection.In, clientToServer.ClientSafePipeHandle)

    use stdout = new MemoryStream()

    let server =
        startJsonRpcTransport serverStdin stdout None (fun _ -> Map.ofList [ "test/slow", handler ], Map.empty)

    writeMessageToPipe
        clientToServer
        (JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               id = 1
               ``method`` = "test/slow"
               ``params`` = {| |} |}
        ))

    Assert.IsTrue(handlerStarted.Wait(5000), "Handler should have started")

    let duringStats = getJsonRpcStats server |> Async.RunSynchronously
    Assert.AreEqual("Active", duringStats.Phase)
    Assert.AreEqual(1, duringStats.RunningInboundRequestCount, "Exactly one handler should be running")
    Assert.AreEqual(0, duringStats.PendingOutboundCallCount)

    // Unblock the handler, then wait for its response to appear in stdout before
    // checking the counters — this ensures HandlerCompleted has been processed.
    handlerMayFinish.Set()
    let gotResponse = waitForMessages stdout 1 5000
    Assert.AreEqual(1, gotResponse.Length, "Handler should have produced a response")
    Thread.Sleep 50 // let the actor process HandlerCompleted

    let afterStats = getJsonRpcStats server |> Async.RunSynchronously
    Assert.AreEqual(0, afterStats.RunningInboundRequestCount, "Running count should be zero after handler finishes")

    shutdownJsonRpcTransport server |> Async.RunSynchronously

[<Test>]
let testGetRpcStatsTimedCallArmsAndDisarmsTimer () =
    // Issuing a timed outbound call must arm the timer (TimerArmed = true).
    // Receiving the response before the deadline must disarm it (TimerArmed = false).
    use clientToServer =
        new IO.Pipes.AnonymousPipeServerStream(IO.Pipes.PipeDirection.Out)

    use serverStdin =
        new IO.Pipes.AnonymousPipeClientStream(IO.Pipes.PipeDirection.In, clientToServer.ClientSafePipeHandle)

    use stdout = new MemoryStream()

    let server =
        startJsonRpcTransport serverStdin stdout None (fun _ -> Map.empty, Map.empty)

    let replyTask =
        sendJsonRpcCallWithTimeout
            server
            "test/timed"
            (JsonSerializer.SerializeToElement({| |}))
            (Some(TimeSpan.FromSeconds 30.0))
        |> Async.StartAsTask

    // Wait for the outbound request so the call is registered in PendingOutboundCalls.
    let outbound = waitForMessages stdout 1 3000
    Assert.AreEqual(1, outbound.Length)

    let pendingStats = getJsonRpcStats server |> Async.RunSynchronously
    Assert.AreEqual(1, pendingStats.PendingOutboundCallCount, "One timed call should be pending")
    Assert.AreEqual(true, pendingStats.TimerArmed, "Timer must be armed for a timed call")

    // Feed back a success response — the call resolves and the timer should be disarmed.
    let outboundId = outbound.[0].GetProperty("id").GetInt32()

    let response =
        JsonSerializer.Serialize(
            {| jsonrpc = "2.0"
               id = outboundId
               result = "ok" |}
        )

    writeMessageToPipe clientToServer response

    let completed = replyTask.Wait(TimeSpan.FromSeconds 5.0)
    Assert.IsTrue(completed, "Call should have resolved")

    match replyTask.Result with
    | Ok v -> Assert.AreEqual("ok", v.GetString())
    | Error e -> Assert.Fail(sprintf "Expected Ok but got Error: %s" (string e))

    // Give the actor a beat to process the response and reschedule the timer.
    Thread.Sleep 50

    let resolvedStats = getJsonRpcStats server |> Async.RunSynchronously
    Assert.AreEqual(0, resolvedStats.PendingOutboundCallCount, "Call should be gone from PendingOutboundCalls")
    Assert.AreEqual(false, resolvedStats.TimerArmed, "Timer must be disarmed after all timed calls resolve")

    shutdownJsonRpcTransport server |> Async.RunSynchronously
