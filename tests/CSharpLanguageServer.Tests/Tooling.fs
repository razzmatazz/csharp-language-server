module CSharpLanguageServer.Tests.Tooling

open System
open System.Collections.Generic
open System.IO
open System.Diagnostics
open System.Text
open System.Threading.Tasks
open System.Threading

open NUnit.Framework
open Newtonsoft.Json.Linq


let writeProjectDir (fileMap: Map<string, string>) : string =
    let tempDir = Path.Combine(
        Path.GetTempPath(),
        "CSharpLanguageServer.Tests." + System.DateTime.Now.Ticks.ToString())

    Directory.CreateDirectory(tempDir) |> ignore

    for kv in fileMap do
        let filename = Path.Combine(tempDir, kv.Key)

        if filename.Contains("/") then
            let parts = kv.Key.Split("/")
            if parts.Length > 2 then
               failwith "more than 1 subdir is not supported"

            let fileDir = Path.Combine(tempDir, parts[0])

            if not (Directory.Exists(fileDir)) then
                Directory.CreateDirectory(fileDir) |> ignore

        use fileStream = File.Create(filename)
        fileStream.Write(Encoding.UTF8.GetBytes(kv.Value))

    tempDir


let rec deleteDirectory (path: string) =
    if Directory.Exists(path) then
        Directory.GetFileSystemEntries(path)
        |> Array.iter (fun item ->
            if File.Exists(item) then
                File.Delete(item)
            else
                deleteDirectory item)
        Directory.Delete(path)


type ClientServerRpcCallInfo =
    {
        Method: string
        RpcCallMsg: JObject
    }


type RpcEndpoint = Server | Client

type RpcMessageLogEntry =
    {
        Source: RpcEndpoint
        TimestampMS: int
        Message: JObject
    }

type ClientState =
    {
        LoggingEnabled: bool
        ProjectDir: string option
        InitializeReplyChannel: AsyncReplyChannel<unit> option
        ProcessStartInfo: ProcessStartInfo option
        ServerProcess: Process option
        ServerStderrLineReadTask: Task
        ServerStdoutJsonRpcMessageRead: Task
        NextRpcCallId: int
        OutstandingServerRpcCalls: Map<int, ClientServerRpcCallInfo>
        SetupTimestamp: DateTime
        RpcLog: RpcMessageLogEntry list
    }
    static member Default = {
        LoggingEnabled = false
        ProjectDir = None
        InitializeReplyChannel = None
        ProcessStartInfo = None
        ServerProcess = None
        ServerStderrLineReadTask = Task.CompletedTask
        ServerStdoutJsonRpcMessageRead = Task.CompletedTask
        NextRpcCallId = 1
        OutstandingServerRpcCalls = Map.empty
        SetupTimestamp = DateTime.MinValue
        RpcLog = []
    }


type ClientEvent =
    | Setup
    | ServerStartRequest of Map<string, string>
    | ServerStopRequest of AsyncReplyChannel<unit>
    | InitializeRequest of AsyncReplyChannel<unit>
    | ShutdownRequest
    | ExitRequest
    | ServerStderrLineRead of string option
    | RpcMessageReceived of Result<JObject option, Exception>
    | SendServerRpcCall of string * JObject
    | ServerRpcCallResult of JObject
    | ClientRpcCall of JValue * string * JObject
    | SendClientRpcCallResult of JToken * JToken
    | SendRpcMessage of JObject
    | EmitLogMessage of DateTime * string * string
    | GetRpcLog of AsyncReplyChannel<RpcMessageLogEntry list>


let makeServerProcessInfo projectTempDir =
    let serverExe = Path.Combine(Environment.CurrentDirectory)
    let tfm = Path.GetFileName(serverExe)
    let buildMode = Path.GetFileName(Path.GetDirectoryName(serverExe))

    let baseDir =
        serverExe
        |> Path.GetDirectoryName
        |> Path.GetDirectoryName
        |> Path.GetDirectoryName
        |> Path.GetDirectoryName
        |> Path.GetDirectoryName

    let baseServerFileName =
        Path.Combine(baseDir, "src", "CSharpLanguageServer", "bin", buildMode, tfm, "CSharpLanguageServer")

    let serverFileName =
        match System.Environment.OSVersion.Platform with
        | PlatformID.Win32NT -> baseServerFileName + ".exe"
        | _ -> baseServerFileName

    Assert.IsTrue(File.Exists(serverFileName))

    let processStartInfo = new ProcessStartInfo()
    processStartInfo.FileName <- serverFileName
    processStartInfo.RedirectStandardInput <- true
    processStartInfo.RedirectStandardOutput <- true
    processStartInfo.RedirectStandardError <- true
    processStartInfo.UseShellExecute <- false
    processStartInfo.CreateNoWindow <- true
    processStartInfo.WorkingDirectory <- projectTempDir

    processStartInfo


let processClientEvent (state: ClientState) (post: ClientEvent -> unit) msg : Async<ClientState> = async {

    let logMessage logger msg =
        post (EmitLogMessage (DateTime.Now, logger, msg))

    match msg with
    | Setup ->
        return { state with SetupTimestamp = DateTime.Now }

    | ServerStartRequest fileMap ->
        let projectTempDir = writeProjectDir fileMap
        let processStartInfo = makeServerProcessInfo projectTempDir

        let p = new Process()
        p.StartInfo <- processStartInfo

        logMessage "ServerStartRequest" "StartServer: p.Start().."
        let startResult = p.Start()
        logMessage "ServerStartRequest" (String.Format("StartServer: p.Start(): {0}, {1}", startResult, p.Id))

        post (RpcMessageReceived (Ok None))
        post (ServerStderrLineRead None)

        return { state with ProjectDir = Some projectTempDir
                            ProcessStartInfo = Some processStartInfo
                            ServerProcess = Some p }

    | ServerStopRequest rc ->
        let p = state.ServerProcess.Value
        logMessage "StopServer" "p.Kill().."
        p.Kill()
        logMessage "StopServer" "p.WaitForExit().."
        p.WaitForExit()
        logMessage "StopServer" "p.WaitForExit(): OK"

        deleteDirectory state.ProjectDir.Value

        logMessage "StopServer" (sprintf "exit code=%d" p.ExitCode)

        rc.Reply(())

        return state

    | InitializeRequest rc ->
        logMessage "InitializeRequest" "sending 'initialize'.."

        let initParams = JObject()
        initParams["processId"] <- int (System.Diagnostics.Process.GetCurrentProcess().Id)
        initParams["rootUri"] <- (sprintf "file://%s" state.ProjectDir.Value)
        initParams["capabilities"] <- JObject()
        initParams["trace"] <- JValue "off"

        post (SendServerRpcCall ("initialize", initParams))

        return { state with InitializeReplyChannel = Some rc }

    | ShutdownRequest ->
        logMessage "ShutdownRequest" "sending 'shutdown'.."
        let initParams = JObject()
        post (SendServerRpcCall ("shutdown", initParams))
        return state

    | ExitRequest ->
        logMessage "ExitRequest" "sending 'exit'.."
        let initParams = JObject()
        post (SendServerRpcCall ("exit", initParams))
        return state

    | ServerStderrLineRead line ->
        match line with
        | Some line -> logMessage "StdErr" line
        | None -> ()

        let readNextStdErrLine () =
            if not state.ServerProcess.Value.HasExited then
                let line = state.ServerProcess.Value.StandardError.ReadLine()
                post (ServerStderrLineRead (Some line))

        let nextStdErrLineReadTask = Task.Run(readNextStdErrLine)

        return { state with ServerStderrLineReadTask = nextStdErrLineReadTask }

    | RpcMessageReceived result ->
        let readJsonRpcHeader (stdout: Stream) =
            let headerBytes = List<byte>()

            let terminatorReached () =
                headerBytes.Count >= 4
                    && headerBytes[headerBytes.Count - 4] = (byte '\r')
                    && headerBytes[headerBytes.Count - 3] = (byte '\n')
                    && headerBytes[headerBytes.Count - 2] = (byte '\r')
                    && headerBytes[headerBytes.Count - 1] = (byte '\n')

            while not (terminatorReached()) do
                let b: int = stdout.ReadByte()
                if b < 0 then
                   let failureMessage = (sprintf "readJsonRpcHeader: EOF, b=%d; bytes=%d; \"%s\"" (int b) (headerBytes.Count) (headerBytes.ToArray() |> Encoding.UTF8.GetString))
                   failwith failureMessage
                else
                   headerBytes.Add(byte b)
                ()

            let headers =
                Encoding.UTF8.GetString(headerBytes.ToArray())
                |> _.Split("\r\n")
                |> Seq.filter (fun s -> s.Length > 0)
                |> Seq.map (_.Split(":"))

            let contentLength =
                headers
                |> Seq.filter (fun a -> a[0].ToLower().Trim() = "content-length")
                |> Seq.map (fun a -> a[1] |> Int32.Parse)
                |> Seq.head

            {| ContentLength = contentLength; ContentType = None |}

        let readNextJsonRpcMessage () =
            try
                let stdout = state.ServerProcess.Value.StandardOutput.BaseStream
                if not stdout.CanRead then
                    failwith "stdout.CanRead = false"
                let header = readJsonRpcHeader stdout
                let content: byte[] = Array.zeroCreate header.ContentLength
                let bytesRead = stdout.Read(content)
                if bytesRead <> header.ContentLength then
                    failwith "readNextJsonRpcMessage: could not read full content"

                let msg = Encoding.UTF8.GetString(content) |> JObject.Parse

                post (RpcMessageReceived (Ok (Some msg)))
            with ex ->
                post (RpcMessageReceived (Error ex))

        match result with
        | Error e ->
            logMessage "RpcMessageReceived" (sprintf "e=%s" (string e))
            return state

        | Ok rpcMsgMaybe ->
            match rpcMsgMaybe with
            | Some rpcMsg ->
                if rpcMsg.ContainsKey("result") then
                    post (ServerRpcCallResult rpcMsg)
                else if rpcMsg.ContainsKey("method") then
                    post (ClientRpcCall (rpcMsg["id"] :?> JValue, string rpcMsg["method"], rpcMsg["params"] :?> JObject))
                else
                    failwith (sprintf "RpcMessageReceived: unknown json rpc msg type: %s" (string rpcMsg))
            | None ->
                ()

            let nextJsonRpcMessageReadTask = Task.Run(readNextJsonRpcMessage)

            let newRpcLog =
                match rpcMsgMaybe with
                | Some rpcMsg -> state.RpcLog @ [{ Source = Server; TimestampMS = 0; Message = rpcMsg }]
                | None -> state.RpcLog

            return { state with ServerStdoutJsonRpcMessageRead = nextJsonRpcMessageReadTask
                                RpcLog = newRpcLog }

    | ClientRpcCall (id, m, p) ->
        match m with
        | "window/showMessage" ->
            logMessage "windows/showMessage" (String.Format("[{0}] \"{1}\"", p["type"], p["message"]))
        | "$/progress" ->
            logMessage "$/progress" (String.Format("({0}) \"{1}\"", p["value"]["kind"], p["value"]["message"]))
        | "client/registerCapability" ->
            logMessage "ClientRpcCall" (String.Format("client/registerCapability: registrations={0}", p["registrations"]))
            post (SendClientRpcCallResult (id, null))
        | "workspace/configuration" ->
            logMessage "ClientRpcCall" (String.Format("workspace/configuration: params={0}", p))
            post (SendClientRpcCallResult (id, new JArray()))
        | "window/workDoneProgress/create" ->
            logMessage "ClientRpcCall" (String.Format("window/workDoneProgress/create: params={0}", p))
            post (SendClientRpcCallResult (id, null))
        | _ ->
            logMessage "ClientRpcCall" (String.Format("ClientRpcCall: unhandled method call: \"{0}\"", m))

        return state

    | SendServerRpcCall (m, p) ->
        let rpcCallId = state.NextRpcCallId
        let msg = JObject()
        msg["jsonrpc"] <- JValue "2.0"
        msg["id"] <- JValue rpcCallId
        msg["method"] <- JValue m
        msg["params"] <- p

        post (SendRpcMessage msg)

        let newOutstandingServerRpcCalls =
            state.OutstandingServerRpcCalls
            |> Map.add rpcCallId { Method = m; RpcCallMsg = msg }

        return { state with OutstandingServerRpcCalls = newOutstandingServerRpcCalls
                            NextRpcCallId = state.NextRpcCallId + 1 }

    | ServerRpcCallResult result ->
        let rpcCallId = int result["id"]
        let rpcCall = state.OutstandingServerRpcCalls |> Map.find rpcCallId

        let newOutstandingServerRpcCalls =
            state.OutstandingServerRpcCalls |> Map.remove rpcCallId

        // hack
        match rpcCall.Method with
        | "initialize" ->
            let initializedParams = JObject()
            post (SendServerRpcCall ("initialized", initializedParams))
        | "initialized" ->
            state.InitializeReplyChannel.Value.Reply()
        | _ -> ()

        return { state with OutstandingServerRpcCalls = newOutstandingServerRpcCalls }

    | SendClientRpcCallResult (id, result) ->
        let msg = JObject()
        msg["jsonrpc"] <- JValue "2.0"
        msg["id"] <- id
        msg["result"] <- result
        post (SendRpcMessage msg)

        return state

    | SendRpcMessage rpcMsg ->
        let rpcMsgJson = (string rpcMsg)

        let serverStdin = state.ServerProcess.Value.StandardInput
        let formattedMessage = String.Format("Content-Length: {0}\r\n\r\n{1}", rpcMsgJson.Length, rpcMsgJson)
        serverStdin.Write(formattedMessage)
        serverStdin.Flush()

        let newRpcLog = state.RpcLog @ [{ Source = Client; TimestampMS = 0; Message = rpcMsg }]

        return { state with RpcLog = newRpcLog }

    | EmitLogMessage (timestamp, logger, msg) ->
        let offsetMs = int (timestamp - state.SetupTimestamp).TotalMilliseconds
        let timestampFmt = if offsetMs >= 0 then (sprintf "+%06i" offsetMs) else (sprintf "%07i" offsetMs)

        if state.LoggingEnabled then
            Console.Error.WriteLine("[{0} {1}] {2}", timestampFmt,  logger, msg)

        return state

    | GetRpcLog rc ->
        rc.Reply(state.RpcLog)
        return state
}


let clientEventLoop (initialState: ClientState) (inbox: MailboxProcessor<ClientEvent>) =
    let rec loop state = async {
        let! msg = inbox.Receive()
        let! newState = processClientEvent state inbox.Post msg
        return! loop newState
    }

    async {
        try
            do! loop initialState
        with ex ->
            Console.Error.WriteLine("clientEventLoop: failed {0}", ex)
            raise ex
    }


type ClientController (client: MailboxProcessor<ClientEvent>, projectFiles: Map<string, string>) =
    let logMessage m msg =
        client.Post (EmitLogMessage (DateTime.Now, (sprintf "ClientActorController.%s" m), msg))

    interface IDisposable with
        member __.Dispose() =
            logMessage "Dispose" "sending ServerStopRequest.."
            client.PostAndReply(fun rc -> ServerStopRequest rc)
            logMessage "Dispose" "OK, ServerStopRequest has finished"

    member __.Start () =
        logMessage "Start" "sending ServerStartRequest"
        client.Post(ServerStartRequest projectFiles)

    member __.Initialize () =
        logMessage "Initialize" "sending InitializeRequest.."
        client.PostAndReply(fun rc -> InitializeRequest rc)
        logMessage "Initialize" "OK, InitializeRequest complete"

    member __.WaitForProgressEnd(message: string) =
        let timeoutMS = 10 * 1000
        let start = DateTime.Now

        let progressEndMatch m =
            m.Source = Server
            && (m.Message.["method"] |> string) = "$/progress"
            && (m.Message.["params"].["value"].["kind"] |> string) = "end"
            && (m.Message.["params"].["value"].["message"] |> string) = message

        let mutable haveProgressEnd = false
        while (not haveProgressEnd) do
            let rpcLog = client.PostAndReply(fun rc -> GetRpcLog rc)
            haveProgressEnd <- rpcLog |> Seq.exists progressEndMatch

            if (DateTime.Now - start).TotalMilliseconds > timeoutMS then
               failwith (sprintf "WaitForProgressEnd: no $/progress[end] received in %dms"
                                 timeoutMS)
            Thread.Sleep(25)

    member __.Shutdown () =
        client.Post(ShutdownRequest)
        client.Post(ExitRequest)

    member __.Stop () =
        client.PostAndReply(fun rc -> ServerStopRequest rc)

    member __.DumpRpcLog () =
        let rpcLog = client.PostAndReply(fun rc -> GetRpcLog rc)
        for m in rpcLog do
            logMessage "RpcLog" (sprintf "%s; %s" (string m.Source) (string m.Message))

    member __.ServerDidInvoke (rpcMethod: string) =
        let rpcLog = client.PostAndReply(fun rc -> GetRpcLog rc)
        rpcLog |> Seq.exists (fun m -> m.Source = Server && (string m.Message["method"]) = rpcMethod)

    member __.ServerDidRespondTo (rpcMethod: string) =
        let rpcLog = client.PostAndReply(fun rc -> GetRpcLog rc)
        let invocation = rpcLog |> Seq.find (fun m -> m.Source = Client && (string m.Message["method"]) = rpcMethod)
        rpcLog
        |> Seq.exists (fun m -> m.Source = Client
                                && (m.Message.["id"] |> string) = (invocation.Message.["id"] |> string)
                                && (m.Message.["method"] |> string) = rpcMethod)

let startAndMountServer (projectFiles: Map<string, string>) (enableLogging: bool) =
    let initialState = { ClientState.Default with LoggingEnabled = enableLogging }
    let clientActor = MailboxProcessor.Start(clientEventLoop initialState)
    clientActor.Post(Setup)
    new ClientController (clientActor, projectFiles)
