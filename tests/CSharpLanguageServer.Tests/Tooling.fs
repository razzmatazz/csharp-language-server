module CSharpLanguageServer.Tests.Tooling

open System
open System.Collections.Generic
open System.IO
open System.Diagnostics
open System.Text
open System.Threading.Tasks
open System.Threading
open System.Runtime.InteropServices

open NUnit.Framework
open Newtonsoft.Json.Linq
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Server


let emptyClientCapabilities: ClientCapabilities = {
    Workspace = None
    TextDocument = None
    NotebookDocument = None
    Window = None
    General = None
    Experimental = None
}

type ClientProfile = {
    LoggingEnabled: bool
    ClientCapabilities: ClientCapabilities
}

let defaultClientProfile = {
    LoggingEnabled = false
    ClientCapabilities = emptyClientCapabilities
}

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


type ClientServerRpcRequestInfo =
    {
        Method: string
        RpcRequestMsg: JObject
        ResultReplyChannel: option<AsyncReplyChannel<JObject>>
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
        ClientProfile: ClientProfile
        LoggingEnabled: bool
        ProjectDir: string option
        ProcessStartInfo: ProcessStartInfo option
        ServerProcess: Process option
        ServerStderrLineReadTask: Task
        ServerStdoutJsonRpcMessageRead: Task
        NextRpcRequestId: int
        OutstandingServerRpcReqs: Map<int, ClientServerRpcRequestInfo>
        SetupTimestamp: DateTime
        RpcLog: RpcMessageLogEntry list
        ServerInfo: InitializeResultServerInfo option
        ServerCapabilities: ServerCapabilities option
        PushDiagnostics: Map<string, int option * Diagnostic[]>
    }

let defaultClientState = {
    ClientProfile = { LoggingEnabled = false; ClientCapabilities = emptyClientCapabilities }
    LoggingEnabled = false
    ProjectDir = None
    ProcessStartInfo = None
    ServerProcess = None
    ServerStderrLineReadTask = Task.CompletedTask
    ServerStdoutJsonRpcMessageRead = Task.CompletedTask
    NextRpcRequestId = 1
    OutstandingServerRpcReqs = Map.empty
    SetupTimestamp = DateTime.MinValue
    RpcLog = []
    ServerInfo = None
    ServerCapabilities = None
    PushDiagnostics = Map.empty
}

type ClientEvent =
    | SetupWithProfile of ClientProfile
    | UpdateState of (ClientState -> ClientState)
    | ServerStartRequest of string * AsyncReplyChannel<ClientState>
    | ServerStopRequest of AsyncReplyChannel<unit>
    | GetState of AsyncReplyChannel<ClientState>
    | ServerStderrLineRead of string option
    | RpcMessageReceived of Result<JObject option, Exception>
    | SendServerRpcRequest of string * JToken * option<AsyncReplyChannel<JObject>>
    | ServerRpcCallResult of JObject
    | SendServerRpcNotification of string * JToken
    | ClientRpcCall of JValue * string * JObject
    | SendClientRpcCallResult of JToken * JToken
    | SendRpcMessage of JObject
    | EmitLogMessage of DateTime * string * string
    | GetRpcLog of AsyncReplyChannel<RpcMessageLogEntry list>

let processClientEvent (state: ClientState) (post: ClientEvent -> unit) msg : Async<ClientState> = async {

    let logMessage logger msg =
        post (EmitLogMessage (DateTime.Now, logger, msg))

    match msg with
    | SetupWithProfile clientProfile ->
        return { state with SetupTimestamp = DateTime.Now
                            ClientProfile = clientProfile }

    | UpdateState stateUpdateFn ->
        return stateUpdateFn(state)

    | ServerStartRequest (projectDir, rc) ->
        let processStartInfo = makeServerProcessInfo projectDir

        let p = new Process()
        p.StartInfo <- processStartInfo
        logMessage "ServerStartRequest" "StartServer: p.Start().."
        let startResult = p.Start()
        logMessage "ServerStartRequest" (String.Format("StartServer: p.Start(): {0}, {1}", startResult, p.Id))

        post (RpcMessageReceived (Ok None))
        post (ServerStderrLineRead None)

        let newState = { state with ProjectDir = Some projectDir
                                    ProcessStartInfo = Some processStartInfo
                                    ServerProcess = Some p }

        rc.Reply(newState)

        return newState

    | ServerStopRequest rc ->
        let p = state.ServerProcess.Value
        logMessage "StopServer" "p.Kill().."
        p.Kill()
        logMessage "StopServer" "p.WaitForExit().."
        p.WaitForExit()
        logMessage "StopServer" "p.WaitForExit(): OK"

        logMessage "StopServer" (sprintf "exit code=%d" p.ExitCode)

        rc.Reply(())

        return state

    | GetState rc ->
        rc.Reply(state)
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
        let newState =
            match m with
            | "window/showMessage" ->
                let p = p |> deserialize<ShowMessageParams>
                logMessage "windows/showMessage" (String.Format("[{0}] \"{1}\"", p.Type, p.Message))
                state
            | "$/progress" ->
                logMessage "$/progress" (String.Format("({0}) \"{1}\"", p["value"]["kind"], p["value"]["message"]))
                state
            | "client/registerCapability" ->
                logMessage "ClientRpcCall" (String.Format("client/registerCapability: registrations={0}", p["registrations"]))
                post (SendClientRpcCallResult (id, null))
                state
            | "workspace/configuration" ->
                logMessage "ClientRpcCall" (String.Format("workspace/configuration: params={0}", p))
                post (SendClientRpcCallResult (id, new JArray()))
                state
            | "window/workDoneProgress/create" ->
                logMessage "ClientRpcCall" (String.Format("window/workDoneProgress/create: params={0}", p))
                post (SendClientRpcCallResult (id, null))
                state
            | "textDocument/publishDiagnostics" ->
                logMessage "ClientRpcCall" (sprintf "textDocument/publishDiagnostics: %s" (string p))
                let p = p |> deserialize<PublishDiagnosticsParams>
                let newPushDiagnostics = state.PushDiagnostics |> Map.add p.Uri (p.Version, p.Diagnostics)
                { state with PushDiagnostics = newPushDiagnostics }
            | _ ->
                logMessage "ClientRpcCall" (String.Format("ClientRpcCall: unhandled method call: \"{0}\"", m))
                state

        return newState

    | SendServerRpcRequest (m, p, rc) ->
        let rpcRequestId = state.NextRpcRequestId
        let msg = JObject()
        msg["jsonrpc"] <- JValue "2.0"
        msg["id"] <- JValue rpcRequestId
        msg["method"] <- JValue m
        msg["params"] <- p

        post (SendRpcMessage msg)

        let newOutstandingServerRpcReqs =
            state.OutstandingServerRpcReqs
            |> Map.add rpcRequestId { Method = m; RpcRequestMsg = msg; ResultReplyChannel = rc }

        return { state with OutstandingServerRpcReqs = newOutstandingServerRpcReqs
                            NextRpcRequestId = state.NextRpcRequestId + 1 }

    | ServerRpcCallResult result ->
        let rpcCallId = int result["id"]
        let rpcRequest = state.OutstandingServerRpcReqs |> Map.find rpcCallId

        let newOutstandingServerRpcReqs =
            state.OutstandingServerRpcReqs |> Map.remove rpcCallId

        match rpcRequest.ResultReplyChannel with
        | Some rc ->
            rc.Reply(result["result"].ToObject())
        | None -> ()

        return { state with OutstandingServerRpcReqs = newOutstandingServerRpcReqs }

    | SendServerRpcNotification (m, p) ->
        let msg = JObject()
        msg["jsonrpc"] <- JValue "2.0"
        msg["method"] <- JValue m
        msg["params"] <- p

        post (SendRpcMessage msg)
        return state

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


let writeProjectDir (fileMap: Map<string, string>) : string =
    let tempDir = Path.Combine(
        Path.GetTempPath(),
        "CSharpLanguageServer.Tests." + System.DateTime.Now.Ticks.ToString())

    // a hack for macOS
    let tempDir =
        match RuntimeInformation.IsOSPlatform(OSPlatform.OSX), tempDir.StartsWith("/private") with
        | true, false -> "/private" + tempDir
        | _ -> tempDir

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


type FileController (client: MailboxProcessor<ClientEvent>, filename: string, uri: string, fileText: string) =
    member __.Filename = filename
    member __.Uri = uri
    member __.FileText = fileText

    interface IDisposable with
        member __.Dispose() =
            let didCloseParams: DidCloseTextDocumentParams =
                { TextDocument = { Uri = uri } }

            client.Post(SendServerRpcNotification ("textDocument/didClose", serialize didCloseParams))
            ()

    member __.Request<'Request, 'Response>(method: string, request: 'Request): 'Response =
        let requestJObject = request |> serialize
        let responseJObject = client.PostAndReply<JObject>(fun rc -> SendServerRpcRequest (method, requestJObject, Some rc))
        responseJObject |> deserialize<'Response>

    member __.DidChange(text: string) =
        let didChangeParams: DidChangeTextDocumentParams =
            {
                TextDocument = { Uri = uri; Version = 2 }
                ContentChanges =
                    [|
                        { Text = text } |> U2.C2
                     |]
            }

        client.Post(SendServerRpcNotification ("textDocument/didChange", serialize didChangeParams))
        ()


type ClientController (client: MailboxProcessor<ClientEvent>, projectFiles: Map<string, string>) =
    let mutable projectDir: string option = None

    let logMessage m msg =
        client.Post (EmitLogMessage (DateTime.Now, (sprintf "ClientActorController.%s" m), msg))

    interface IDisposable with
        member __.Dispose() =
            logMessage "Dispose" "sending ServerStopRequest.."
            client.PostAndReply(fun rc -> ServerStopRequest rc)
            logMessage "Dispose" "OK, ServerStopRequest has finished"

            match projectDir with
            | Some projectDir ->
                logMessage "Dispose" (sprintf "Removing files on project dir \"%s\".." projectDir)
                deleteDirectory projectDir
            | _ -> ()

    member this.StartAndWaitForSolutionLoad() =
        projectDir <- Some (writeProjectDir projectFiles)

        let log = logMessage "StartInitializeAndWaitForSolutionLoad"

        log "sending ServerStartRequest"
        let state = client.PostAndReply(fun rc -> ServerStartRequest (projectDir.Value, rc))

        let initializeParams: InitializeParams =
            {
                RootPath = None
                ProcessId = (System.Diagnostics.Process.GetCurrentProcess().Id) |> int |> Some
                RootUri = (sprintf "file://%s" projectDir.Value) |> Some
                Capabilities = state.ClientProfile.ClientCapabilities
                WorkDoneToken = None
                ClientInfo = None
                Locale = None
                InitializationOptions = None
                Trace = None
                WorkspaceFolders = None
            }

        let initializeResult =
            client.PostAndReply<JObject>(fun rc -> SendServerRpcRequest ("initialize", serialize initializeParams, Some rc))
            |> deserialize<InitializeResult>

        client.Post(UpdateState (fun s -> { s with ServerInfo = initializeResult.ServerInfo
                                                   ServerCapabilities = Some initializeResult.Capabilities }))

        log "OK, 'initialize' request complete"

        let _ = client.PostAndReply<JObject>(fun rc -> SendServerRpcRequest ("initialized", JObject(), Some rc))
        log "OK, 'initialized' request complete"

        this.WaitForProgressEnd("OK, 1 project file(s) loaded")

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
        let _ = client.PostAndReply<JObject>(fun rc -> SendServerRpcRequest ("shutdown", JObject(), Some rc))
        client.Post(SendServerRpcRequest ("exit", JObject(), None))

    member __.Stop () =
        client.PostAndReply(fun rc -> ServerStopRequest rc)

    member __.GetState () =
        client.PostAndReply(fun rc -> GetState rc)

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

    member __.Open(filename: string): FileController =
        let uri = "file://" + projectDir.Value + "/" + filename

        let fileText =
            Path.Combine(projectDir.Value, filename)
            |> File.ReadAllBytes
            |> Encoding.UTF8.GetString

        let textDocument = { Uri = uri
                             LanguageId = "csharp"
                             Version = 1
                             Text = fileText }

        let didOpenParams: DidOpenTextDocumentParams =
            { TextDocument = textDocument }

        client.Post(SendServerRpcNotification ("textDocument/didOpen", serialize didOpenParams))

        new FileController(client, filename, uri, fileText)


let setupServerClient (clientProfile: ClientProfile) (projectFiles: Map<string, string>) =
    let initialState = { defaultClientState with LoggingEnabled = clientProfile.LoggingEnabled }
    let clientActor = MailboxProcessor.Start(clientEventLoop initialState)
    clientActor.Post(SetupWithProfile clientProfile)
    new ClientController (clientActor, projectFiles)
