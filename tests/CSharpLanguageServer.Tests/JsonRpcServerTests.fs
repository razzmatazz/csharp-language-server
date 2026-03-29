module CSharpLanguageServer.Tests.JsonRpcServerTests

open System
open System.IO
open System.Text
open System.Threading

open Microsoft.Extensions.Logging
open NUnit.Framework
open Newtonsoft.Json.Linq

open CSharpLanguageServer.Logging
open CSharpLanguageServer.Runtime.JsonRpcServer

/// Helper: encode a JSON-RPC message with Content-Length framing into bytes.
let private encodeMessage (json: string) =
    let body = Encoding.UTF8.GetBytes(json)

    let header =
        sprintf "Content-Length: %d\r\n\r\n" body.Length |> Encoding.ASCII.GetBytes

    Array.append header body

/// Helper: read a full Content-Length-framed response from a stream (positioned at the start).
let private readResponse (stream: Stream) = async {
    let! msgOpt = readMessage stream
    return msgOpt
}

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
          let msg = readMessage stdout |> Async.RunSynchronously

          if msg.IsSome then
              yield msg.Value ]

/// Helper: wait for the output stream to contain a complete response (with timeout).
let private waitForResponse (stdout: MemoryStream) (timeoutMs: int) = async {
    waitUntil timeoutMs (fun () -> stdout.Length > 0L) |> ignore
    // Give a bit more time for the full response to be written
    do! Async.Sleep 100
    stdout.Position <- 0L
    return! readResponse stdout
}

[<Test>]
let testBasicMethodInvocationReturnsResponse () =
    let request =
        JObject(
            JProperty("jsonrpc", "2.0"),
            JProperty("id", 1),
            JProperty("method", "test/echo"),
            JProperty("params", JObject(JProperty("message", "hello")))
        )

    let handler _ctx = async { return Ok(JObject(JProperty("echo", "hello")) :> JToken) }

    let requestHandlings = Map.ofList [ "test/echo", handler ]

    let stdin = makeInputStream [ string request ]
    let stdout = new MemoryStream()

    let _server =
        startJsonRpcServer stdin stdout None (fun _ -> requestHandlings, Map.empty)

    let responseOpt = waitForResponse stdout 5000 |> Async.RunSynchronously

    Assert.IsTrue(responseOpt.IsSome, "Expected a response but got None")

    let response = responseOpt.Value
    Assert.AreEqual("2.0", string response.["jsonrpc"])
    Assert.AreEqual(1, int response.["id"])
    Assert.AreEqual("hello", string (response.SelectToken("result.echo")))

[<Test>]
let testHandlerReturningEmptyResultProducesResponse () =
    let request =
        JObject(
            JProperty("jsonrpc", "2.0"),
            JProperty("id", 2),
            JProperty("method", "test/silent"),
            JProperty("params", JObject())
        )

    let handler _ctx = async { return Ok(JObject() :> JToken) }

    let requestHandlings = Map.ofList [ "test/silent", handler ]

    let stdin = makeInputStream [ string request ]
    let stdout = new MemoryStream()

    let _server =
        startJsonRpcServer stdin stdout None (fun _ -> requestHandlings, Map.empty)

    let responseOpt = waitForResponse stdout 5000 |> Async.RunSynchronously
    Assert.IsTrue(responseOpt.IsSome, "Expected a response")
    Assert.AreEqual(2, int responseOpt.Value.["id"])

[<Test>]
let testUnregisteredMethodReturnsMethodNotFoundError () =
    let request =
        JObject(
            JProperty("jsonrpc", "2.0"),
            JProperty("id", 3),
            JProperty("method", "test/unknown"),
            JProperty("params", JObject())
        )

    // No handlers registered for "test/unknown"
    let stdin = makeInputStream [ string request ]
    let stdout = new MemoryStream()

    let _server = startJsonRpcServer stdin stdout None (fun _ -> Map.empty, Map.empty)

    let responseOpt = waitForResponse stdout 5000 |> Async.RunSynchronously

    Assert.IsTrue(responseOpt.IsSome, "Expected an error response")

    let response = responseOpt.Value
    Assert.AreEqual(3, int response.["id"])
    Assert.AreEqual(-32601, int (response.SelectToken("error.code")))
    Assert.IsTrue((string (response.SelectToken("error.message"))).Contains("Method not found"))

[<Test>]
let testMultipleRequestsAreHandledSequentially () =
    let handler ctx = async {
        let id = int ctx.WireId.Value
        return Ok(JObject(JProperty("doubled", id * 2)) :> JToken)
    }

    let requestHandlings = Map.ofList [ "test/double", handler ]

    let request1 =
        JObject(
            JProperty("jsonrpc", "2.0"),
            JProperty("id", 10),
            JProperty("method", "test/double"),
            JProperty("params", JObject())
        )

    let request2 =
        JObject(
            JProperty("jsonrpc", "2.0"),
            JProperty("id", 20),
            JProperty("method", "test/double"),
            JProperty("params", JObject())
        )

    let stdin = makeInputStream [ string request1; string request2 ]
    let stdout = new MemoryStream()

    let _server =
        startJsonRpcServer stdin stdout None (fun _ -> requestHandlings, Map.empty)

    let responses = waitForMessages stdout 2 5000

    Assert.AreEqual(2, responses.Length, "Expected 2 responses")

    Assert.AreEqual(10, int responses.[0].["id"])
    Assert.AreEqual(20, int (responses.[0].SelectToken("result.doubled")))

    Assert.AreEqual(20, int responses.[1].["id"])
    Assert.AreEqual(40, int (responses.[1].SelectToken("result.doubled")))

[<Test>]
let testContentLengthFramingIsCorrect () =
    let request =
        JObject(
            JProperty("jsonrpc", "2.0"),
            JProperty("id", 42),
            JProperty("method", "test/ping"),
            JProperty("params", JObject())
        )

    let handler _ctx = async { return Ok(JValue("pong") :> JToken) }

    let requestHandlings = Map.ofList [ "test/ping", handler ]

    let stdin = makeInputStream [ string request ]
    let stdout = new MemoryStream()

    let _server =
        startJsonRpcServer stdin stdout None (fun _ -> requestHandlings, Map.empty)

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
    let parsed = JObject.Parse(bodyString)
    Assert.AreEqual(42, int parsed.["id"])
    Assert.AreEqual("pong", string parsed.["result"])

[<Test>]
let testMessageWithIdButNoMethodIsTreatedAsResponseAndDroppedIfUnmatched () =
    // A message with "id" but no "method" is now classified as a response to an
    // outbound request. Since there are no outstanding outbound requests, it is
    // silently logged and dropped (no output).
    let malformed = JObject(JProperty("jsonrpc", "2.0"), JProperty("id", 99))

    let stdin = makeInputStream [ string malformed ]
    let stdout = new MemoryStream()

    let _server = startJsonRpcServer stdin stdout None (fun _ -> Map.empty, Map.empty)

    Async.Sleep 500 |> Async.RunSynchronously
    Assert.AreEqual(0L, stdout.Length, "Expected no output for unmatched response")

[<Test>]
let testMessageWithNoMethodOrIdIsIgnored () =
    // A message with neither "method" nor "id" — not a valid request or notification
    let malformed = JObject(JProperty("jsonrpc", "2.0"))

    let stdin = makeInputStream [ string malformed ]
    let stdout = new MemoryStream()

    let _server = startJsonRpcServer stdin stdout None (fun _ -> Map.empty, Map.empty)

    Async.Sleep 500 |> Async.RunSynchronously
    Assert.AreEqual(0L, stdout.Length, "Expected no output for message with no method or id")

// ---- Notification tests ----

[<Test>]
let testNotificationIsDispatchedToHandler () =
    let received = ref false
    let receivedMethod = ref ""

    let notification =
        JObject(
            JProperty("jsonrpc", "2.0"),
            JProperty("method", "test/notify"),
            JProperty("params", JObject(JProperty("data", "abc")))
        )
    // No "id" — this is a notification

    let handler ctx = async {
        received.Value <- true
        receivedMethod.Value <- ctx.MethodName
    }

    let notificationHandlings = Map.ofList [ "test/notify", handler ]

    let stdin = makeInputStream [ string notification ]
    let stdout = new MemoryStream()

    let _server =
        startJsonRpcServer stdin stdout None (fun _ -> Map.empty, notificationHandlings)

    // Wait for the notification to be processed
    waitUntil 5000 (fun () -> received.Value) |> ignore

    Assert.IsTrue(received.Value, "Notification handler should have been invoked")
    Assert.AreEqual("test/notify", receivedMethod.Value)
    Assert.AreEqual(0L, stdout.Length, "Notifications should produce no output on stdout")

[<Test>]
let testNotificationHandlerReceivesCorrectParams () =
    let capturedParams = ref ""

    let notification =
        JObject(
            JProperty("jsonrpc", "2.0"),
            JProperty("method", "test/log"),
            JProperty("params", JObject(JProperty("level", "info"), JProperty("message", "hello world")))
        )

    let handler ctx = async { capturedParams.Value <- string (ctx.Params.Value.SelectToken("message")) }

    let notificationHandlings = Map.ofList [ "test/log", handler ]

    let stdin = makeInputStream [ string notification ]
    let stdout = new MemoryStream()

    let _server =
        startJsonRpcServer stdin stdout None (fun _ -> Map.empty, notificationHandlings)

    waitUntil 5000 (fun () -> capturedParams.Value <> "") |> ignore

    Assert.AreEqual("hello world", capturedParams.Value)

[<Test>]
let testUnregisteredNotificationDoesNotCrash () =
    let notification =
        JObject(JProperty("jsonrpc", "2.0"), JProperty("method", "test/unknown_notif"), JProperty("params", JObject()))

    let stdin = makeInputStream [ string notification ]
    let stdout = new MemoryStream()

    // No notification handlers registered
    let _server = startJsonRpcServer stdin stdout None (fun _ -> Map.empty, Map.empty)

    Async.Sleep 500 |> Async.RunSynchronously
    Assert.AreEqual(0L, stdout.Length, "Expected no output for unregistered notification")

[<Test>]
let testMixedRequestsAndNotificationsWork () =
    let notificationReceived = ref false

    let request =
        JObject(
            JProperty("jsonrpc", "2.0"),
            JProperty("id", 1),
            JProperty("method", "test/echo"),
            JProperty("params", JObject(JProperty("value", "hi")))
        )

    let notification =
        JObject(JProperty("jsonrpc", "2.0"), JProperty("method", "test/ping"), JProperty("params", JObject()))

    let requestHandler _ctx = async { return Ok(JValue("ok") :> JToken) }

    let notificationHandler _ctx = async { notificationReceived.Value <- true }

    let requestHandlings = Map.ofList [ "test/echo", requestHandler ]
    let notificationHandlings = Map.ofList [ "test/ping", notificationHandler ]

    // Send request first, then notification
    let stdin = makeInputStream [ string request; string notification ]
    let stdout = new MemoryStream()

    let _server =
        startJsonRpcServer stdin stdout None (fun _ -> requestHandlings, notificationHandlings)

    // Wait for the request response
    let responseOpt = waitForResponse stdout 5000 |> Async.RunSynchronously
    Assert.IsTrue(responseOpt.IsSome, "Expected a response for the request")
    Assert.AreEqual(1, int responseOpt.Value.["id"])
    Assert.AreEqual("ok", string responseOpt.Value.["result"])

    // Wait for the notification to be processed
    waitUntil 5000 (fun () -> notificationReceived.Value) |> ignore

    Assert.IsTrue(notificationReceived.Value, "Notification handler should have been invoked")

[<Test>]
let testNotificationWithIdIsRoutedAsRequest () =
    // A message with both "id" and "method" should be treated as a request, not a notification
    let notificationHandlerCalled = ref false
    let requestHandlerCalled = ref false

    let msg =
        JObject(
            JProperty("jsonrpc", "2.0"),
            JProperty("id", 5),
            JProperty("method", "test/ambiguous"),
            JProperty("params", JObject())
        )

    let requestHandler _ctx = async {
        requestHandlerCalled.Value <- true
        return Ok(JValue.CreateNull() :> JToken)
    }

    let notificationHandler _ctx = async { notificationHandlerCalled.Value <- true }

    let requestHandlings = Map.ofList [ "test/ambiguous", requestHandler ]
    let notificationHandlings = Map.ofList [ "test/ambiguous", notificationHandler ]

    let stdin = makeInputStream [ string msg ]
    let stdout = new MemoryStream()

    let _server =
        startJsonRpcServer stdin stdout None (fun _ -> requestHandlings, notificationHandlings)

    waitUntil 5000 (fun () -> requestHandlerCalled.Value) |> ignore

    Assert.IsTrue(requestHandlerCalled.Value, "Request handler should be called for message with 'id'")
    Assert.IsFalse(notificationHandlerCalled.Value, "Notification handler should NOT be called for message with 'id'")

[<Test>]
let testWriteQueueDrainsAllResponses () =
    // Send many requests at once to exercise the write queue — multiple responses
    // will be enqueued while the first write is still in progress.
    let requestCount = 10

    let handler ctx = async {
        let id = int ctx.WireId.Value
        return Ok(JObject(JProperty("seq", id)) :> JToken)
    }

    let requestHandlings = Map.ofList [ "test/seq", handler ]

    let requests =
        [ for i in 1..requestCount ->
              let req =
                  JObject(
                      JProperty("jsonrpc", "2.0"),
                      JProperty("id", i),
                      JProperty("method", "test/seq"),
                      JProperty("params", JObject())
                  )

              string req ]

    let stdin = makeInputStream requests
    let stdout = new MemoryStream()

    let _server =
        startJsonRpcServer stdin stdout None (fun _ -> requestHandlings, Map.empty)

    let responses = waitForMessages stdout requestCount 10000

    Assert.AreEqual(
        requestCount,
        responses.Length,
        sprintf "Expected %d responses but got %d" requestCount responses.Length
    )

    let receivedIds = responses |> List.map (fun r -> int r.["id"]) |> Set.ofList

    for i in 1..requestCount do
        Assert.IsTrue(receivedIds.Contains(i), sprintf "Missing response for id %d" i)

// ---- SendNotification tests ----

[<Test>]
let testSendNotificationWritesProperJsonRpcNotification () =
    // Start server with no inbound messages (empty stdin that immediately EOFs)
    let stdin = makeInputStream []
    let stdout = new MemoryStream()

    let server = startJsonRpcServer stdin stdout None (fun _ -> Map.empty, Map.empty)

    let body = JObject(JProperty("message", "hello from server"))

    sendJsonRpcNotification server "window/logMessage" body

    let responseOpt = waitForResponse stdout 5000 |> Async.RunSynchronously

    Assert.IsTrue(responseOpt.IsSome, "Expected a notification to be written to stdout")

    let msg = responseOpt.Value
    Assert.AreEqual("2.0", string msg.["jsonrpc"])
    Assert.AreEqual("window/logMessage", string msg.["method"])
    Assert.AreEqual("hello from server", string (msg.SelectToken("params.message")))
    Assert.IsNull(msg.["id"], "Notification should not have an 'id' field")

[<Test>]
let testSendMultipleNotificationsAllWritten () =
    let stdin = makeInputStream []
    let stdout = new MemoryStream()

    let server = startJsonRpcServer stdin stdout None (fun _ -> Map.empty, Map.empty)

    let notifCount = 5

    for i in 1..notifCount do
        let body = JObject(JProperty("index", i))
        sendJsonRpcNotification server "test/notif" body

    let messages = waitForMessages stdout notifCount 5000

    Assert.AreEqual(
        notifCount,
        messages.Length,
        sprintf "Expected %d notifications but got %d" notifCount messages.Length
    )

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
        startJsonRpcServer serverStdin stdout None (fun _ -> Map.empty, Map.empty)

    let body = JObject(JProperty("items", JArray("a", "b")))

    // Post request asynchronously and capture the reply future
    let replyTask =
        sendJsonRpcCall server "workspace/configuration" body |> Async.StartAsTask

    // Wait for the outbound request to appear on stdout
    let outboundMessages = waitForMessages stdout 1 5000

    Assert.AreEqual(1, outboundMessages.Length, "Expected an outbound request on stdout")

    let outbound = outboundMessages.[0]
    Assert.AreEqual("2.0", string outbound.["jsonrpc"])
    Assert.AreEqual("workspace/configuration", string outbound.["method"])
    Assert.IsNotNull(outbound.["id"], "Request should have an 'id' field")

    let outboundId = int outbound.["id"]

    // Now simulate the client sending a response back through stdin
    let clientResponse =
        JObject(
            JProperty("jsonrpc", "2.0"),
            JProperty("id", outboundId),
            JProperty("result", JArray("config1", "config2"))
        )

    let responseBytes = encodeMessage (string clientResponse)
    clientToServer.Write(responseBytes, 0, responseBytes.Length)
    clientToServer.Flush()

    // Wait for the reply to come back
    let completed = replyTask.Wait(TimeSpan.FromSeconds(5.0))
    Assert.IsTrue(completed, "SendRequest should have received a reply")

    match replyTask.Result with
    | Ok reply ->
        Assert.AreEqual("config1", string reply.[0])
        Assert.AreEqual("config2", string reply.[1])
    | Error err -> Assert.Fail(sprintf "Expected Ok response but got Error: %s" (string err))

    server.Post(Shutdown)

[<Test>]
let testSendRequestAssignsIncrementingIds () =
    let stdin = makeInputStream []
    let stdout = new MemoryStream()

    let server = startJsonRpcServer stdin stdout None (fun _ -> Map.empty, Map.empty)

    // Fire two requests (we won't resolve them — just check the outbound IDs)
    let _reply1 = sendJsonRpcCall server "test/req1" (JObject()) |> Async.StartAsTask

    let _reply2 = sendJsonRpcCall server "test/req2" (JObject()) |> Async.StartAsTask

    let requests = waitForMessages stdout 2 5000

    Assert.AreEqual(2, requests.Length, "Expected 2 outbound requests")

    let req1 = requests.[0]
    let req2 = requests.[1]

    let id1 = int req1.["id"]
    let id2 = int req2.["id"]

    Assert.AreEqual(id1 + 1, id2, "Outbound request IDs should be sequential")

[<Test>]
let testInboundResponseForUnknownIdIsHandledGracefully () =
    // Send a response with an id that doesn't match any outstanding request
    let unknownResponse =
        JObject(JProperty("jsonrpc", "2.0"), JProperty("id", 9999), JProperty("result", "orphan"))

    let stdin = makeInputStream [ string unknownResponse ]
    let stdout = new MemoryStream()

    let _server = startJsonRpcServer stdin stdout None (fun _ -> Map.empty, Map.empty)

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
        startJsonRpcServer serverStdin stdout None (fun _ -> Map.empty, Map.empty)

    let replyTask =
        sendJsonRpcCall server "test/unknown" (JObject()) |> Async.StartAsTask

    // Wait for the outbound request
    let outboundMessages = waitForMessages stdout 1 5000
    let outboundId = int outboundMessages.[0].["id"]

    // Simulate client responding with MethodNotFound error
    let errorResponse =
        JObject(
            JProperty("jsonrpc", "2.0"),
            JProperty("id", outboundId),
            JProperty(
                "error",
                JObject(JProperty("code", -32601), JProperty("message", "Method not found: 'test/unknown'"))
            )
        )

    let responseBytes = encodeMessage (string errorResponse)
    clientToServer.Write(responseBytes, 0, responseBytes.Length)
    clientToServer.Flush()

    let completed = replyTask.Wait(TimeSpan.FromSeconds(5.0))
    Assert.IsTrue(completed, "SendCall should have received a reply")

    match replyTask.Result with
    | Error err ->
        Assert.AreEqual(-32601, int (err.SelectToken("code")))
        Assert.IsTrue((string (err.SelectToken("message"))).Contains("Method not found"))
    | Ok _ -> Assert.Fail("Expected Error but got Ok")

    server.Post(Shutdown)

[<Test>]
let testSendCallReturnsErrorWithData () =
    use clientToServer =
        new IO.Pipes.AnonymousPipeServerStream(IO.Pipes.PipeDirection.Out)

    use serverStdin =
        new IO.Pipes.AnonymousPipeClientStream(IO.Pipes.PipeDirection.In, clientToServer.ClientSafePipeHandle)

    use stdout = new MemoryStream()

    let server =
        startJsonRpcServer serverStdin stdout None (fun _ -> Map.empty, Map.empty)

    let replyTask =
        sendJsonRpcCall server "test/failing" (JObject()) |> Async.StartAsTask

    let outboundMessages = waitForMessages stdout 1 5000
    let outboundId = int outboundMessages.[0].["id"]

    // Simulate client responding with an error that includes data
    let errorResponse =
        JObject(
            JProperty("jsonrpc", "2.0"),
            JProperty("id", outboundId),
            JProperty(
                "error",
                JObject(
                    JProperty("code", -32603),
                    JProperty("message", "Internal error"),
                    JProperty("data", JObject(JProperty("details", "something went wrong"), JProperty("retry", false)))
                )
            )
        )

    let responseBytes = encodeMessage (string errorResponse)
    clientToServer.Write(responseBytes, 0, responseBytes.Length)
    clientToServer.Flush()

    let completed = replyTask.Wait(TimeSpan.FromSeconds(5.0))
    Assert.IsTrue(completed, "SendCall should have received a reply")

    match replyTask.Result with
    | Error err ->
        Assert.AreEqual(-32603, int (err.SelectToken("code")))
        Assert.AreEqual("Internal error", string (err.SelectToken("message")))
        Assert.AreEqual("something went wrong", string (err.SelectToken("data.details")))
        Assert.AreEqual(false, System.Convert.ToBoolean(err.SelectToken("data.retry")))
    | Ok _ -> Assert.Fail("Expected Error but got Ok")

    server.Post(Shutdown)

[<Test>]
let testSendCallSuccessAndErrorForDifferentRequests () =
    // Send two outbound calls; one succeeds, one fails — verify each gets the right result
    use clientToServer =
        new IO.Pipes.AnonymousPipeServerStream(IO.Pipes.PipeDirection.Out)

    use serverStdin =
        new IO.Pipes.AnonymousPipeClientStream(IO.Pipes.PipeDirection.In, clientToServer.ClientSafePipeHandle)

    use stdout = new MemoryStream()

    let server =
        startJsonRpcServer serverStdin stdout None (fun _ -> Map.empty, Map.empty)

    let reply1Task = sendJsonRpcCall server "test/ok" (JObject()) |> Async.StartAsTask

    let reply2Task = sendJsonRpcCall server "test/fail" (JObject()) |> Async.StartAsTask

    let outboundMessages = waitForMessages stdout 2 5000
    Assert.AreEqual(2, outboundMessages.Length, "Expected 2 outbound requests")

    let id1 = int outboundMessages.[0].["id"]
    let id2 = int outboundMessages.[1].["id"]

    // Send success for first, error for second
    let successResponse =
        JObject(
            JProperty("jsonrpc", "2.0"),
            JProperty("id", id1),
            JProperty("result", JObject(JProperty("status", "ok")))
        )

    let errorResponse =
        JObject(
            JProperty("jsonrpc", "2.0"),
            JProperty("id", id2),
            JProperty("error", JObject(JProperty("code", -32600), JProperty("message", "Invalid request")))
        )

    // Send both responses
    for resp in [ successResponse; errorResponse ] do
        let bytes = encodeMessage (string resp)
        clientToServer.Write(bytes, 0, bytes.Length)

    clientToServer.Flush()

    let completed1 = reply1Task.Wait(TimeSpan.FromSeconds(5.0))
    let completed2 = reply2Task.Wait(TimeSpan.FromSeconds(5.0))
    Assert.IsTrue(completed1 && completed2, "Both calls should have received replies")

    match reply1Task.Result with
    | Ok reply -> Assert.AreEqual("ok", string (reply.SelectToken("status")))
    | Error _ -> Assert.Fail("Expected Ok for first call")

    match reply2Task.Result with
    | Error err ->
        Assert.AreEqual(-32600, int (err.SelectToken("code")))
        Assert.AreEqual("Invalid request", string (err.SelectToken("message")))
    | Ok _ -> Assert.Fail("Expected Error for second call")

    server.Post(Shutdown)

[<Test>]
let testMixedInboundRequestsAndOutboundNotifications () =
    // Server handles an inbound request while also sending outbound notifications
    let handler _ctx = async { return Ok(JValue("handled") :> JToken) }

    let inboundRequest =
        JObject(
            JProperty("jsonrpc", "2.0"),
            JProperty("id", 1),
            JProperty("method", "test/handle"),
            JProperty("params", JObject())
        )

    let requestHandlings = Map.ofList [ "test/handle", handler ]

    let stdin = makeInputStream [ string inboundRequest ]
    let stdout = new MemoryStream()

    let server =
        startJsonRpcServer stdin stdout None (fun _ -> requestHandlings, Map.empty)

    // Also send a notification from the server side
    let notifBody = JObject(JProperty("kind", "info"))
    sendJsonRpcNotification server "window/logMessage" notifBody

    // Wait for both messages to be written (response + notification)
    let allMsgs = waitForMessages stdout 2 5000

    Assert.AreEqual(2, allMsgs.Length, "Expected 2 messages on stdout (1 response + 1 notification)")

    let responses = allMsgs |> List.filter (fun m -> m.["result"] <> null)
    let notifications = allMsgs |> List.filter (fun m -> m.["method"] <> null)

    Assert.AreEqual(1, responses.Length, "Expected 1 response")
    Assert.AreEqual(1, notifications.Length, "Expected 1 notification")

    Assert.AreEqual(1, int responses.[0].["id"])
    Assert.AreEqual("handled", string responses.[0].["result"])

    Assert.AreEqual("window/logMessage", string notifications.[0].["method"])
    Assert.AreEqual("info", string (notifications.[0].SelectToken("params.kind")))

// ---- $/cancelRequest tests ----

/// Helper: write a framed JSON-RPC message into a writable pipe stream.
let private writeMessageToPipe (pipe: IO.Pipes.AnonymousPipeServerStream) (msg: JObject) =
    let bytes = encodeMessage (string msg)
    pipe.Write(bytes, 0, bytes.Length)
    pipe.Flush()

[<Test>]
let testCancelRequestCancelsRunningHandler () =
    // Start a request with a slow handler, send $/cancelRequest, verify cancellation response
    use handlerStarted = new ManualResetEventSlim(false)

    let handler _ctx = async {
        handlerStarted.Set()
        // Simulate a long-running operation that respects cancellation
        do! Async.Sleep 30000
        // Should not reach here if cancelled
        return Ok(JValue("should-not-see") :> JToken)
    }

    let requestHandlings = Map.ofList [ "test/slow", handler ]

    use clientToServer =
        new IO.Pipes.AnonymousPipeServerStream(IO.Pipes.PipeDirection.Out)

    use serverStdin =
        new IO.Pipes.AnonymousPipeClientStream(IO.Pipes.PipeDirection.In, clientToServer.ClientSafePipeHandle)

    use stdout = new MemoryStream()

    let server =
        startJsonRpcServer serverStdin stdout None (fun _ -> requestHandlings, Map.empty)

    // Send the slow request
    let request =
        JObject(
            JProperty("jsonrpc", "2.0"),
            JProperty("id", 1),
            JProperty("method", "test/slow"),
            JProperty("params", JObject())
        )

    writeMessageToPipe clientToServer request

    // Wait for the handler to start
    Assert.IsTrue(handlerStarted.Wait(5000), "Handler should have started")

    // Send $/cancelRequest
    let cancelNotification =
        JObject(
            JProperty("jsonrpc", "2.0"),
            JProperty("method", "$/cancelRequest"),
            JProperty("params", JObject(JProperty("id", 1)))
        )

    writeMessageToPipe clientToServer cancelNotification

    // Wait for the cancellation error response
    let responseOpt = waitForResponse stdout 5000 |> Async.RunSynchronously

    Assert.IsTrue(responseOpt.IsSome, "Expected a cancellation error response")

    let response = responseOpt.Value
    Assert.AreEqual(1, int response.["id"])
    Assert.AreEqual(-32800, int (response.SelectToken("error.code")), "Expected RequestCancelled error code (-32800)")

    Assert.IsTrue(
        (string (response.SelectToken("error.message"))).Contains("cancel", StringComparison.OrdinalIgnoreCase)
    )

    server.Post(Shutdown)

[<Test>]
let testCancelRequestForUnknownIdIsIgnored () =
    // Send $/cancelRequest for an ID that was never sent — should not crash or produce output
    use clientToServer =
        new IO.Pipes.AnonymousPipeServerStream(IO.Pipes.PipeDirection.Out)

    use serverStdin =
        new IO.Pipes.AnonymousPipeClientStream(IO.Pipes.PipeDirection.In, clientToServer.ClientSafePipeHandle)

    use stdout = new MemoryStream()

    let server =
        startJsonRpcServer serverStdin stdout None (fun _ -> Map.empty, Map.empty)

    let cancelNotification =
        JObject(
            JProperty("jsonrpc", "2.0"),
            JProperty("method", "$/cancelRequest"),
            JProperty("params", JObject(JProperty("id", 999)))
        )

    writeMessageToPipe clientToServer cancelNotification

    Async.Sleep 500 |> Async.RunSynchronously
    Assert.AreEqual(0L, stdout.Length, "Expected no output for cancel of unknown request id")

    server.Post(Shutdown)

[<Test>]
let testCancelRequestForAlreadyCompletedRequestIsIgnored () =
    // Send a request, wait for it to complete, then send $/cancelRequest for same ID
    let handler _ctx = async { return Ok(JValue("done") :> JToken) }

    let requestHandlings = Map.ofList [ "test/fast", handler ]

    use clientToServer =
        new IO.Pipes.AnonymousPipeServerStream(IO.Pipes.PipeDirection.Out)

    use serverStdin =
        new IO.Pipes.AnonymousPipeClientStream(IO.Pipes.PipeDirection.In, clientToServer.ClientSafePipeHandle)

    use stdout = new MemoryStream()

    let server =
        startJsonRpcServer serverStdin stdout None (fun _ -> requestHandlings, Map.empty)

    // Send the request
    let request =
        JObject(
            JProperty("jsonrpc", "2.0"),
            JProperty("id", 42),
            JProperty("method", "test/fast"),
            JProperty("params", JObject())
        )

    writeMessageToPipe clientToServer request

    // Wait for the response
    let responseOpt = waitForResponse stdout 5000 |> Async.RunSynchronously
    Assert.IsTrue(responseOpt.IsSome, "Expected a response")
    Assert.AreEqual(42, int responseOpt.Value.["id"])
    Assert.AreEqual("done", string responseOpt.Value.["result"])

    // Now send $/cancelRequest for the same ID — should be silently ignored
    let cancelNotification =
        JObject(
            JProperty("jsonrpc", "2.0"),
            JProperty("method", "$/cancelRequest"),
            JProperty("params", JObject(JProperty("id", 42)))
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

    server.Post(Shutdown)

[<Test>]
let testHandlerObservesCancellationToken () =
    // Verify that a handler's ambient CancellationToken gets cancelled when $/cancelRequest arrives.
    // We use CancellationToken.Register to observe cancellation directly, since F# async
    // cancellation bypasses try/catch and goes through the cancellation continuation.
    use handlerStarted = new ManualResetEventSlim(false)
    use tokenWasCancelled = new ManualResetEventSlim(false)

    let handler (_ctx: JsonRpcRequestContext) : Async<Result<JToken, JToken>> = async {
        let! ct = Async.CancellationToken
        ct.Register(fun () -> tokenWasCancelled.Set()) |> ignore
        handlerStarted.Set()

        // Block until cancelled
        do! Async.Sleep 30000
        return Ok(JValue.CreateNull() :> JToken)
    }

    let requestHandlings = Map.ofList [ "test/cancellable", handler ]

    use clientToServer =
        new IO.Pipes.AnonymousPipeServerStream(IO.Pipes.PipeDirection.Out)

    use serverStdin =
        new IO.Pipes.AnonymousPipeClientStream(IO.Pipes.PipeDirection.In, clientToServer.ClientSafePipeHandle)

    use stdout = new MemoryStream()

    let server =
        startJsonRpcServer serverStdin stdout None (fun _ -> requestHandlings, Map.empty)

    // Send the request
    let request =
        JObject(
            JProperty("jsonrpc", "2.0"),
            JProperty("id", 7),
            JProperty("method", "test/cancellable"),
            JProperty("params", JObject())
        )

    writeMessageToPipe clientToServer request

    Assert.IsTrue(handlerStarted.Wait(5000), "Handler should have started")

    // Cancel
    let cancelNotification =
        JObject(
            JProperty("jsonrpc", "2.0"),
            JProperty("method", "$/cancelRequest"),
            JProperty("params", JObject(JProperty("id", 7)))
        )

    writeMessageToPipe clientToServer cancelNotification

    // Wait for the cancellation error response
    let responseOpt = waitForResponse stdout 5000 |> Async.RunSynchronously

    Assert.IsTrue(responseOpt.IsSome, "Expected a cancellation error response")
    Assert.AreEqual(-32800, int (responseOpt.Value.SelectToken("error.code")))

    // Verify the handler's CancellationToken was actually cancelled
    Assert.IsTrue(tokenWasCancelled.Wait(2000), "Handler's CancellationToken should have been cancelled")

    server.Post(Shutdown)

[<Test>]
let testOtherRequestsStillWorkAfterCancellation () =
    // Cancel one request, then send another — second should complete normally
    use handlerStarted = new ManualResetEventSlim(false)

    let slowHandler _ctx = async {
        handlerStarted.Set()
        do! Async.Sleep 30000
        return Ok(JValue.CreateNull() :> JToken)
    }

    let fastHandler _ctx = async { return Ok(JValue("ok") :> JToken) }

    let requestHandlings =
        Map.ofList [ "test/slow", slowHandler; "test/fast", fastHandler ]

    use clientToServer =
        new IO.Pipes.AnonymousPipeServerStream(IO.Pipes.PipeDirection.Out)

    use serverStdin =
        new IO.Pipes.AnonymousPipeClientStream(IO.Pipes.PipeDirection.In, clientToServer.ClientSafePipeHandle)

    use stdout = new MemoryStream()

    let server =
        startJsonRpcServer serverStdin stdout None (fun _ -> requestHandlings, Map.empty)

    // Send a slow request (will be cancelled)
    let slowRequest =
        JObject(
            JProperty("jsonrpc", "2.0"),
            JProperty("id", 1),
            JProperty("method", "test/slow"),
            JProperty("params", JObject())
        )

    writeMessageToPipe clientToServer slowRequest

    Assert.IsTrue(handlerStarted.Wait(5000), "Slow handler should have started")

    // Cancel the slow request
    let cancelNotification =
        JObject(
            JProperty("jsonrpc", "2.0"),
            JProperty("method", "$/cancelRequest"),
            JProperty("params", JObject(JProperty("id", 1)))
        )

    writeMessageToPipe clientToServer cancelNotification

    // Wait for the cancellation response
    let cancelResponse = waitForResponse stdout 5000 |> Async.RunSynchronously
    Assert.IsTrue(cancelResponse.IsSome, "Expected cancellation response")
    Assert.AreEqual(-32800, int (cancelResponse.Value.SelectToken("error.code")))

    // Now send a fast request — should complete normally
    let fastRequest =
        JObject(
            JProperty("jsonrpc", "2.0"),
            JProperty("id", 2),
            JProperty("method", "test/fast"),
            JProperty("params", JObject())
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
        allMessages |> List.tryFind (fun m -> m.["id"] <> null && int m.["id"] = 2)

    Assert.IsTrue(fastResponse.IsSome, "Expected a response for the fast request (id=2)")
    Assert.AreEqual("ok", string fastResponse.Value.["result"])

    server.Post(Shutdown)

// ---- Write priority tests ----

/// A stream wrapper that blocks writes until signalled, allowing us to keep
/// the writer busy while we queue up messages at different priorities.
type private GatedStream(inner: MemoryStream) =
    inherit Stream()
    let gate = new ManualResetEventSlim(true)
    member _.Block() = gate.Reset()
    member _.Unblock() = gate.Set()
    override _.CanRead = inner.CanRead
    override _.CanSeek = inner.CanSeek
    override _.CanWrite = inner.CanWrite
    override _.Length = inner.Length

    override _.Position
        with get () = inner.Position
        and set v = inner.Position <- v

    override _.Flush() = inner.Flush()
    override _.Read(buffer, offset, count) = inner.Read(buffer, offset, count)
    override _.Seek(offset, origin) = inner.Seek(offset, origin)
    override _.SetLength(value) = inner.SetLength(value)

    override _.Write(buffer, offset, count) =
        gate.Wait()
        inner.Write(buffer, offset, count)

    override _.WriteAsync(buffer, offset, count, ct) =
        gate.Wait(ct)
        inner.WriteAsync(buffer, offset, count, ct)

    override _.FlushAsync(ct) = inner.FlushAsync(ct)

[<Test>]
let testNormalPriorityNotificationsSentBeforeLowPriority () =
    // 1. Start server, send an initial notification to start a write
    // 2. Block stdout so the write stays in progress
    // 3. Queue low-priority notifications, then normal-priority ones
    // 4. Unblock stdout — normal should come out before low
    let inner = new MemoryStream()
    let stdout = new GatedStream(inner)

    let stdin = makeInputStream []

    let server =
        startJsonRpcServer stdin (stdout :> Stream) None (fun _ -> Map.empty, Map.empty)

    // Send the first notification — this starts the first write
    sendJsonRpcNotification server "initial" (JObject(JProperty("seq", 0)))

    // Wait for the first write to begin
    waitUntil 5000 (fun () -> inner.Length > 0L) |> ignore

    // Now block the stream — subsequent writes will queue
    stdout.Block()

    // Send another normal notification to saturate the pending write
    // (the WriteComplete for the first write will try to write this one, and block)
    sendJsonRpcNotification server "filler" (JObject(JProperty("seq", 1)))

    // Give the actor time to process the first WriteComplete and start writing the filler
    Thread.Sleep 200

    // Now queue low-priority notifications
    for i in 1..3 do
        sendJsonRpcNotificationWithLowPriority server "low" (JObject(JProperty("seq", 10 + i)))

    // Then queue normal-priority notifications
    for i in 1..3 do
        sendJsonRpcNotification server "normal" (JObject(JProperty("seq", 20 + i)))

    // Give the actor time to process all the SendNotification events
    Thread.Sleep 200

    // Unblock — all queued messages should now drain
    stdout.Unblock()

    // initial + filler + 3 low + 3 normal = 8
    let totalExpected = 8
    let messages = waitForMessages inner totalExpected 5000

    Assert.AreEqual(
        totalExpected,
        messages.Length,
        sprintf "Expected %d messages but got %d" totalExpected messages.Length
    )

    // First two are the initial and filler (already written/in-progress before queueing)
    // After that, the 3 normal-priority should come before the 3 low-priority
    let queuedMessages = messages |> List.skip 2
    let queuedMethods = queuedMessages |> List.map (fun m -> string m.["method"])

    Assert.AreEqual(
        [ "normal"; "normal"; "normal"; "low"; "low"; "low" ],
        queuedMethods,
        "Normal-priority notifications should be written before low-priority ones"
    )

[<Test>]
let testLowPriorityNotificationsAreEventuallyWritten () =
    // Verify low-priority messages are not dropped — they arrive after normal ones drain
    let stdin = makeInputStream []
    let stdout = new MemoryStream()

    let server = startJsonRpcServer stdin stdout None (fun _ -> Map.empty, Map.empty)

    // Send only low-priority notifications
    for i in 1..3 do
        sendJsonRpcNotificationWithLowPriority server "$/progress" (JObject(JProperty("index", i)))

    let messages = waitForMessages stdout 3 5000

    Assert.AreEqual(3, messages.Length, "Expected all 3 low-priority notifications to be written")

    for i in 0..2 do
        Assert.AreEqual("$/progress", string messages.[i].["method"])
        Assert.AreEqual(i + 1, int (messages.[i].SelectToken("params.index")))

[<Test>]
let testNormalNotificationsPreemptQueuedLowPriority () =
    // Queue a mix of low and normal notifications while a write is in progress,
    // then verify normal ones are written before low ones.
    let inner = new MemoryStream()
    let stdout = new GatedStream(inner)

    let stdin = makeInputStream []

    let server =
        startJsonRpcServer stdin (stdout :> Stream) None (fun _ -> Map.empty, Map.empty)

    // Send the first notification to start a write
    sendJsonRpcNotification server "initial" (JObject(JProperty("seq", 0)))

    // Wait for the first write to begin
    waitUntil 5000 (fun () -> inner.Length > 0L) |> ignore

    // Block the stream so writes queue up
    stdout.Block()

    // Send another normal one that will saturate the pending write slot
    sendJsonRpcNotification server "filler" (JObject(JProperty("seq", 1)))

    // Give the actor time to process the first WriteComplete and start the filler write (which blocks)
    Thread.Sleep 300

    // Now queue: low, low, normal, normal, low
    sendJsonRpcNotificationWithLowPriority server "low" (JObject(JProperty("seq", 10)))
    sendJsonRpcNotificationWithLowPriority server "low" (JObject(JProperty("seq", 11)))
    sendJsonRpcNotification server "normal" (JObject(JProperty("seq", 20)))
    sendJsonRpcNotification server "normal" (JObject(JProperty("seq", 21)))
    sendJsonRpcNotificationWithLowPriority server "low" (JObject(JProperty("seq", 12)))

    // Give the actor time to process all SendNotification events
    Thread.Sleep 300

    // Unblock — all queued messages should now drain
    stdout.Unblock()

    // initial + filler + 2 normal + 3 low = 7
    let totalExpected = 7
    let messages = waitForMessages inner totalExpected 5000

    Assert.AreEqual(
        totalExpected,
        messages.Length,
        sprintf "Expected %d messages but got %d" totalExpected messages.Length
    )

    // Skip initial and filler (already written/in-progress before queueing)
    let queuedMethods =
        messages |> List.skip 2 |> List.map (fun m -> string m.["method"])

    // Normal-priority should drain first, then low-priority
    Assert.AreEqual(
        [ "normal"; "normal"; "low"; "low"; "low" ],
        queuedMethods,
        "Normal-priority notifications should be written before low-priority ones"
    )
