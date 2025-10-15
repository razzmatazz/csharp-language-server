module CSharpLanguageServer.Tests.Tooling

open System
open System.Collections.Generic
open System.IO
open System.Diagnostics
open System.Text
open System.Threading.Tasks
open System.Threading
open System.Runtime.InteropServices
open System.Reflection

open Newtonsoft.Json.Linq
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Server
open System.Text.RegularExpressions

let indexJToken (name: string) (jobj: option<JToken>) : option<JToken> =
    jobj |> Option.bind (fun p -> p[name] |> Option.ofObj)

let emptyClientCapabilities: ClientCapabilities =
    { Workspace = None
      TextDocument = None
      NotebookDocument = None
      Window = None
      General = None
      Experimental = None }

type ClientProfile =
    { LoggingEnabled: bool
      ClientCapabilities: ClientCapabilities }

let defaultClientCapabilities =
    { emptyClientCapabilities with
        TextDocument =
            Some
                { Hover = None
                  Synchronization = None
                  Completion = None
                  SignatureHelp = None
                  Declaration = None
                  Definition = None
                  TypeDefinition = None
                  Implementation = None
                  References = None
                  DocumentHighlight = None
                  DocumentSymbol = None
                  CodeLens = None
                  DocumentLink = None
                  ColorProvider = None
                  Formatting = None
                  RangeFormatting = None
                  OnTypeFormatting = None
                  Rename = None
                  FoldingRange = None
                  SelectionRange = None
                  PublishDiagnostics = None
                  CallHierarchy = None
                  SemanticTokens = None
                  LinkedEditingRange = None
                  Moniker = None
                  TypeHierarchy = None
                  InlineValue = None
                  InlayHint = None
                  Diagnostic = None
                  CodeAction =
                    Some
                        { DynamicRegistration = None
                          IsPreferredSupport = None
                          DisabledSupport = None
                          DataSupport = None
                          ResolveSupport = None
                          HonorsChangeAnnotations = None
                          CodeActionLiteralSupport = Some { CodeActionKind = { ValueSet = Array.empty } } } } }

let defaultClientProfile =
    { LoggingEnabled = false
      ClientCapabilities = defaultClientCapabilities }

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
        match Environment.OSVersion.Platform with
        | PlatformID.Win32NT -> baseServerFileName + ".exe"
        | _ -> baseServerFileName

    if not (File.Exists(serverFileName)) then
        failwithf "makeServerProcessInfo: no '%s' server executable present" serverFileName

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
    { Method: string
      RpcRequestMsg: JObject
      ResultReplyChannel: option<AsyncReplyChannel<Result<JToken, JToken>>> }

type RpcEndpoint =
    | Server
    | Client

type RpcMessageLogEntry =
    { Source: RpcEndpoint
      TimestampMS: int
      Message: JObject }

type ClientState =
    { ClientProfile: ClientProfile
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
      PushDiagnostics: Map<string, int option * Diagnostic[]> }

let defaultClientState =
    { ClientProfile =
        { LoggingEnabled = false
          ClientCapabilities = emptyClientCapabilities }
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
      PushDiagnostics = Map.empty }

type ClientEvent =
    | SetupWithProfile of ClientProfile
    | UpdateState of (ClientState -> ClientState)
    | ServerStartRequest of string * AsyncReplyChannel<ClientState>
    | ServerStopRequest of AsyncReplyChannel<unit>
    | GetState of AsyncReplyChannel<ClientState>
    | ServerStderrLineRead of string option
    | RpcMessageReceived of Result<JObject option, Exception>
    | SendServerRpcRequest of string * JToken * option<AsyncReplyChannel<Result<JToken, JToken>>>
    | ServerRpcCallResultOrError of JObject
    | SendServerRpcNotification of string * JToken
    | ClientRpcCall of JValue * string * JObject
    | SendClientRpcCallResult of JToken * option<JToken>
    | SendRpcMessage of JObject
    | EmitLogMessage of DateTime * string * string
    | GetRpcLog of AsyncReplyChannel<RpcMessageLogEntry list>

let processClientEvent (state: ClientState) (post: ClientEvent -> unit) msg : Async<ClientState> = async {

    let logMessage logger msg =
        post (EmitLogMessage(DateTime.Now, logger, msg))

    match msg with
    | SetupWithProfile clientProfile ->
        return
            { state with
                SetupTimestamp = DateTime.Now
                ClientProfile = clientProfile }

    | UpdateState stateUpdateFn -> return stateUpdateFn (state)

    | ServerStartRequest(projectDir, rc) ->
        let processStartInfo = makeServerProcessInfo projectDir

        let p = new Process()
        p.StartInfo <- processStartInfo
        logMessage "ServerStartRequest" "StartServer: p.Start().."
        let startResult = p.Start()
        logMessage "ServerStartRequest" (String.Format("StartServer: p.Start(): {0}, {1}", startResult, p.Id))

        post (RpcMessageReceived(Ok None))
        post (ServerStderrLineRead None)

        let newState =
            { state with
                ProjectDir = Some projectDir
                ProcessStartInfo = Some processStartInfo
                ServerProcess = Some p }

        rc.Reply(newState)

        return newState

    | ServerStopRequest rc ->
        match state.ServerProcess with
        | None -> ()
        | Some serverProcess ->
            logMessage "StopServer" "p.Kill().."
            serverProcess.Kill()
            logMessage "StopServer" "p.WaitForExit().."
            serverProcess.WaitForExit()
            logMessage "StopServer" "p.WaitForExit(): OK"

            logMessage "StopServer" (sprintf "exit code=%d" serverProcess.ExitCode)

        rc.Reply(())

        return { state with ServerProcess = None }

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
                post (ServerStderrLineRead(Some line))

        let nextStdErrLineReadTask = Task.Run(readNextStdErrLine)

        return
            { state with
                ServerStderrLineReadTask = nextStdErrLineReadTask }

    | RpcMessageReceived result ->
        let tryReadJsonRpcHeader (stdout: Stream) =
            let headerBytes = List<byte>()

            let terminatorReached () =
                headerBytes.Count >= 4
                && headerBytes[headerBytes.Count - 4] = (byte '\r')
                && headerBytes[headerBytes.Count - 3] = (byte '\n')
                && headerBytes[headerBytes.Count - 2] = (byte '\r')
                && headerBytes[headerBytes.Count - 1] = (byte '\n')

            let mutable eof = false

            while (not eof && not (terminatorReached ())) do
                let b: int = stdout.ReadByte()
                if b < 0 then eof <- true else headerBytes.Add(byte b)
                ()

            match eof with
            | true -> None

            | false ->
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

                Some
                    {| ContentLength = contentLength
                       ContentType = None |}

        let readNextJsonRpcMessage () =
            match state.ServerProcess with
            | None ->
                logMessage "RpcMessageReceived" "no state.ServerProcess, will not read next rpc message"
                ()

            | Some serverProcess ->
                try
                    let stdout = serverProcess.StandardOutput.BaseStream

                    if not stdout.CanRead then
                        failwith "stdout.CanRead = false"

                    let headerMaybe = tryReadJsonRpcHeader stdout

                    match headerMaybe with
                    | None ->
                        logMessage "RpcMessageReceived" "readNextJsonRpcMessage: EOF received when reading header"
                        ()

                    | Some header ->
                        let bytesRead: List<byte> = List<byte>()

                        while bytesRead.Count < header.ContentLength do
                            let numBytesToReadInThisChunk =
                                Math.Min(header.ContentLength - bytesRead.Count, 4096)

                            let chunk: byte[] = Array.zeroCreate numBytesToReadInThisChunk
                            let chunkBytesRead: int = stdout.Read(chunk)

                            for i in 0 .. (chunkBytesRead - 1) do
                                bytesRead.Add(chunk[i])

                        if bytesRead.Count <> header.ContentLength then
                            failwith (
                                sprintf
                                    "readNextJsonRpcMessage: could not read the full content of response (bytesRead=%d; header.ContentLength=%d)"
                                    bytesRead.Count
                                    header.ContentLength
                            )

                        let msg = bytesRead |> _.ToArray() |> Encoding.UTF8.GetString |> JObject.Parse

                        post (RpcMessageReceived(Ok(Some msg)))
                with ex ->
                    post (RpcMessageReceived(Error ex))

        match result with
        | Error e ->
            logMessage "RpcMessageReceived" (sprintf "e=%s" (string e))
            return state

        | Ok rpcMsgMaybe ->
            match rpcMsgMaybe with
            | Some rpcMsg ->
                if rpcMsg.ContainsKey("result") || rpcMsg.ContainsKey("error") then
                    post (ServerRpcCallResultOrError rpcMsg)
                else if rpcMsg.ContainsKey("method") then
                    let rpcMsgId: JValue = rpcMsg["id"] :?> JValue | null
                    let rpcMsgMethod: string = string rpcMsg["method"]
                    let rpcMsgParams: JObject = rpcMsg["params"] :?> JObject | null
                    post (ClientRpcCall(rpcMsgId, rpcMsgMethod, rpcMsgParams))
                else
                    failwith (sprintf "RpcMessageReceived: unknown json rpc msg type: %s" (string rpcMsg))
            | None -> ()

            let nextJsonRpcMessageReadTask = Task.Run(readNextJsonRpcMessage)

            let newRpcLog =
                match rpcMsgMaybe with
                | Some rpcMsg ->
                    state.RpcLog
                    @ [ { Source = Server
                          TimestampMS = 0
                          Message = rpcMsg } ]
                | None -> state.RpcLog

            return
                { state with
                    ServerStdoutJsonRpcMessageRead = nextJsonRpcMessageReadTask
                    RpcLog = newRpcLog }

    | ClientRpcCall(id, m, p) ->
        let newState =
            match m with
            | "window/showMessage" ->
                let p = p |> deserialize<ShowMessageParams>
                logMessage "windows/showMessage" (String.Format("[{0}] \"{1}\"", p.Type, p.Message))
                state
            | "window/logMessage" ->
                let p = p |> deserialize<LogMessageParams>
                logMessage "windows/logMessage" (String.Format("[{0}] \"{1}\"", p.Type, p.Message))
                state
            | "$/progress" ->
                let value = p["value"] |> Option.ofObj

                logMessage
                    "$/progress"
                    (String.Format("{0} {1}", value |> indexJToken "kind", value |> indexJToken "message"))

                state
            | "client/registerCapability" ->
                logMessage
                    "ClientRpcCall"
                    (String.Format("client/registerCapability: registrations={0}", p["registrations"]))

                post (SendClientRpcCallResult(id, None))
                state
            | "workspace/configuration" ->
                logMessage "ClientRpcCall" (String.Format("workspace/configuration: params={0}", p))
                post (SendClientRpcCallResult(id, Some(new JArray())))
                state
            | "window/workDoneProgress/create" ->
                logMessage "ClientRpcCall" (String.Format("window/workDoneProgress/create: params={0}", p))
                post (SendClientRpcCallResult(id, None))
                state
            | "textDocument/publishDiagnostics" ->
                logMessage "ClientRpcCall" (sprintf "textDocument/publishDiagnostics: %s" (string p))
                let p = p |> deserialize<PublishDiagnosticsParams>

                let newPushDiagnostics =
                    state.PushDiagnostics |> Map.add p.Uri (p.Version, p.Diagnostics)

                { state with
                    PushDiagnostics = newPushDiagnostics }
            | _ ->
                logMessage "ClientRpcCall" (String.Format("ClientRpcCall: unhandled method call: \"{0}\"", m))
                state

        return newState

    | SendServerRpcRequest(m, p, rc) ->
        let rpcRequestId = state.NextRpcRequestId
        let msg = JObject()
        msg["jsonrpc"] <- JValue "2.0"
        msg["id"] <- JValue rpcRequestId
        msg["method"] <- JValue m
        msg["params"] <- p

        post (SendRpcMessage msg)

        let newOutstandingServerRpcReqs =
            state.OutstandingServerRpcReqs
            |> Map.add
                rpcRequestId
                { Method = m
                  RpcRequestMsg = msg
                  ResultReplyChannel = rc }

        return
            { state with
                OutstandingServerRpcReqs = newOutstandingServerRpcReqs
                NextRpcRequestId = state.NextRpcRequestId + 1 }

    | ServerRpcCallResultOrError rpcMsg ->
        let rpcCallId = int rpcMsg["id"]
        let rpcRequest = state.OutstandingServerRpcReqs |> Map.find rpcCallId

        let newOutstandingServerRpcReqs =
            state.OutstandingServerRpcReqs |> Map.remove rpcCallId

        let resultOrError: Result<JToken, JToken> =
            if rpcMsg.ContainsKey("result") then Ok(rpcMsg["result"])
            elif rpcMsg.ContainsKey("error") then Error(rpcMsg["error"])
            else failwithf "unknown result rpcMsg: %s" (string rpcMsg)

        match rpcRequest.ResultReplyChannel with
        | Some rc -> rc.Reply(resultOrError)
        | None -> ()

        return
            { state with
                OutstandingServerRpcReqs = newOutstandingServerRpcReqs }

    | SendServerRpcNotification(m, p) ->
        let msg = JObject()
        msg["jsonrpc"] <- JValue "2.0"
        msg["method"] <- JValue m
        msg["params"] <- p

        post (SendRpcMessage msg)
        return state

    | SendClientRpcCallResult(id, result) ->
        let msg = JObject()
        msg["jsonrpc"] <- JValue "2.0"
        msg["id"] <- id
        msg["result"] <- result |> Option.defaultValue null
        post (SendRpcMessage msg)

        return state

    | SendRpcMessage rpcMsg ->
        let rpcMsgJson = (string rpcMsg)

        match state.ServerProcess with
        | None ->
            logMessage "SendRpcMessage" (sprintf "dropping rpcMsg as there is no state.ServerProcess: %s " rpcMsgJson)
            return state

        | Some serverProcess ->
            let serverStdin = serverProcess.StandardInput

            let formattedMessage =
                String.Format("Content-Length: {0}\r\n\r\n{1}", rpcMsgJson.Length, rpcMsgJson)

            serverStdin.Write(formattedMessage)
            serverStdin.Flush()

            let newRpcLog =
                state.RpcLog
                @ [ { Source = Client
                      TimestampMS = 0
                      Message = rpcMsg } ]

            return { state with RpcLog = newRpcLog }

    | EmitLogMessage(timestamp, logger, msg) ->
        let offsetMs = int (timestamp - state.SetupTimestamp).TotalMilliseconds

        let timestampFmt =
            if offsetMs >= 0 then
                (sprintf "+%06i" offsetMs)
            else
                (sprintf "%07i" offsetMs)

        if state.LoggingEnabled then
            Console.Error.WriteLine("[{0} {1}] {2}", timestampFmt, logger, msg)

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


let prepareTempTestDirFrom (sourceTestDir: DirectoryInfo) : string =
    if not sourceTestDir.Exists then
        failwith (sprintf "%s does not exist!" (sourceTestDir.ToString()))

    let tempDir =
        Path.Combine(Path.GetTempPath(), sprintf "CSharpLanguageServer.Tests.%s" (Guid.NewGuid() |> string))

    // a hack for macOS
    let tempTestDirName =
        match RuntimeInformation.IsOSPlatform(OSPlatform.OSX), tempDir.StartsWith("/private") with
        | true, false -> "/private" + tempDir
        | _ -> tempDir

    let tempTestDir = new DirectoryInfo(tempTestDirName)
    tempTestDir.Create()

    let fileFilter (file: FileInfo) =
        file.Name = ".editorconfig"
        || file.Name = "global.json"
        || file.Extension = ".cs"
        || file.Extension = ".csproj"
        || file.Extension = ".sln"
        || file.Extension = ".slnx"
        || file.Extension = ".cshtml"
        || file.Extension = ".txt"

    let dirFilter sourceSubdirName =
        sourceSubdirName <> "bin" && sourceSubdirName <> "obj"

    let rec copyDirWithFilter (sourceDir: DirectoryInfo) (targetDir: DirectoryInfo) : DirectoryInfo =
        for file in sourceDir.GetFiles() do
            if fileFilter file then
                let targetFilename = Path.Combine(targetDir.ToString(), file.Name)
                file.CopyTo(targetFilename) |> ignore

        for sourceSubdir in sourceDir.GetDirectories() do
            if dirFilter sourceSubdir.Name then
                let targetSubdir =
                    DirectoryInfo(Path.Combine(targetDir.ToString(), sourceSubdir.Name))

                targetSubdir.Create()
                copyDirWithFilter sourceSubdir targetSubdir |> ignore

        targetDir

    copyDirWithFilter sourceTestDir tempTestDir |> string


let rec deleteDirectory (path: string) =
    if Directory.Exists(path) then
        Directory.GetFileSystemEntries(path)
        |> Array.iter (fun item ->
            if File.Exists(item) then
                File.Delete(item)
            else
                deleteDirectory item)

        Directory.Delete(path)


type FileController(client: MailboxProcessor<ClientEvent>, projectDir: string, filename: string) =
    let mutable fileContents: option<string> = None

    member __.FileName = filename

    member __.Uri =
        match Environment.OSVersion.Platform with
        | PlatformID.Win32NT -> ("file:///" + projectDir + "/" + filename).Replace("\\", "/")
        | _ -> "file://" + projectDir + "/" + filename


    interface IDisposable with
        member this.Dispose() =
            let didCloseParams: DidCloseTextDocumentParams =
                { TextDocument = { Uri = this.Uri } }

            client.Post(SendServerRpcNotification("textDocument/didClose", serialize didCloseParams))
            ()

    member this.Open() =
        let fileText =
            Path.Combine(projectDir, filename)
            |> File.ReadAllBytes
            |> Encoding.UTF8.GetString

        fileContents <- Some fileText

        let textDocument =
            { Uri = this.Uri
              LanguageId = "csharp"
              Version = 1
              Text = fileText }

        let didOpenParams: DidOpenTextDocumentParams = { TextDocument = textDocument }

        client.Post(SendServerRpcNotification("textDocument/didOpen", serialize didOpenParams))

    member this.DidChange(text: string) =
        let didChangeParams: DidChangeTextDocumentParams =
            { TextDocument = { Uri = this.Uri; Version = 2 }
              ContentChanges = [| { Text = text } |> U2.C2 |] }

        client.Post(SendServerRpcNotification("textDocument/didChange", serialize didChangeParams))

    member __.GetFileContents() = fileContents.Value

    member __.GetFileContentsWithTextEditsApplied(tes: TextEdit[]) =
        let indexOfPos (c: string) (pos: Position) : int =
            let lines = c.Split([| '\n' |], StringSplitOptions.None)

            let lineOffset =
                lines |> Seq.take (int pos.Line) |> Seq.sumBy (fun l -> l.Length + 1)

            lineOffset + (int pos.Character)

        let applyTextEdit (c: string) (te: TextEdit) =
            c.Substring(0, indexOfPos c te.Range.Start)
            + te.NewText.Replace("\\n", "\n")
            + c.Substring(indexOfPos c te.Range.End)

        tes |> Array.rev |> Array.fold applyTextEdit fileContents.Value


type ClientController(client: MailboxProcessor<ClientEvent>, testDataDir: DirectoryInfo) =
    let mutable projectDir: string option = None
    let mutable solutionLoaded: bool = false

    let logMessage m msg =
        client.Post(EmitLogMessage(DateTime.Now, (sprintf "ClientActorController.%s" m), msg))

    interface IDisposable with
        member __.Dispose() =
            if solutionLoaded then
                logMessage "Dispose" "sending ServerStopRequest.."
                client.PostAndReply(fun rc -> ServerStopRequest rc)
                logMessage "Dispose" "OK, ServerStopRequest has finished"

            // We don't care about cleanup when running under GitHub Actions C/I,
            // also Windows version of C/I would fail from time to time due to
            // another process locking the files on the temporary dir for some reason.
            let runningInGithubActions =
                System.Environment.GetEnvironmentVariable("GITHUB_ACTIONS")
                |> Option.ofObj
                |> Option.map (fun v -> v = "true")
                |> Option.defaultValue false

            match projectDir, runningInGithubActions with
            | Some projectDir, false ->
                logMessage "Dispose" (sprintf "Removing files on project dir \"%s\".." projectDir)
                deleteDirectory projectDir
            | _ -> ()

    member __.ProjectDir: string = projectDir.Value

    member this.StartAndWaitForSolutionLoad() =
        if solutionLoaded then
            failwith "Solution has already been loaded for this ClientController!"

        solutionLoaded <- true

        let log = logMessage "StartInitializeAndWaitForSolutionLoad"
        projectDir <- Some(prepareTempTestDirFrom testDataDir)

        log "sending ServerStartRequest"
        let state = client.PostAndReply(fun rc -> ServerStartRequest(projectDir.Value, rc))

        let initializeParams: InitializeParams =
            { RootPath = None
              ProcessId = (Process.GetCurrentProcess().Id) |> int |> Some
              RootUri = (sprintf "file://%s" projectDir.Value) |> Some
              Capabilities = state.ClientProfile.ClientCapabilities
              WorkDoneToken = None
              ClientInfo = None
              Locale = None
              InitializationOptions = None
              Trace = None
              WorkspaceFolders = None }

        let initializeResult =
            let initResponse =
                client.PostAndReply<Result<JToken, JToken>>(fun rc ->
                    SendServerRpcRequest("initialize", serialize initializeParams, Some rc))

            match initResponse with
            | Ok result -> deserialize<InitializeResult> result
            | Error error -> failwithf "initialize has failed with %s" (string error)

        client.Post(
            UpdateState(fun s ->
                { s with
                    ServerInfo = initializeResult.ServerInfo
                    ServerCapabilities = Some initializeResult.Capabilities })
        )

        log "OK, 'initialize' request complete"

        let _ =
            client.PostAndReply<Result<JToken, JToken>>(fun rc ->
                SendServerRpcRequest("initialized", JObject(), Some rc))

        log "OK, 'initialized' request complete"

        this.WaitForProgressEnd(fun m -> m = "OK, 1 project file(s) loaded" || m.Contains("Finished loading solution"))

    member __.WaitForProgressEnd(messagePred: (string -> bool)) =
        let timeoutMS = 60 * 1000
        let start = DateTime.Now

        let progressEndMatch m =
            let paramsValue = m.Message["params"] |> Option.ofObj |> indexJToken "value"

            m.Source = Server
            && (m.Message.["method"] |> string) = "$/progress"
            && (paramsValue
                |> indexJToken "kind"
                |> Option.map string
                |> Option.defaultValue "(None)") = "end"
            && messagePred (
                paramsValue
                |> indexJToken "message"
                |> Option.map string
                |> Option.defaultValue "(None)"
            )

        let mutable haveProgressEnd = false

        while (not haveProgressEnd) do
            let rpcLog = client.PostAndReply(fun rc -> GetRpcLog rc)
            haveProgressEnd <- rpcLog |> Seq.exists progressEndMatch

            if (DateTime.Now - start).TotalMilliseconds > timeoutMS then
                failwith (sprintf "WaitForProgressEnd: no $/progress[end] received in %dms" timeoutMS)

            Thread.Sleep(25)

    member __.Shutdown() =
        let _ =
            client.PostAndReply<Result<JToken, JToken>>(fun rc -> SendServerRpcRequest("shutdown", JObject(), Some rc))

        client.Post(SendServerRpcRequest("exit", JObject(), None))

    member __.Stop() =
        client.PostAndReply(fun rc -> ServerStopRequest rc)

    member __.GetState() =
        client.PostAndReply(fun rc -> GetState rc)

    member __.GetRpcLog() =
        client.PostAndReply(fun rc -> GetRpcLog rc)

    member __.GetProgressParams(token: ProgressToken) =
        client.PostAndReply(fun rc -> GetRpcLog rc)
        |> Seq.filter (fun m -> m.Source = Server)
        |> Seq.filter (fun m -> (string m.Message["method"]) = "$/progress")
        |> Seq.map (fun m -> m.Message["params"] |> deserialize<ProgressParams>)
        |> Seq.filter (fun pp -> pp.Token = token)
        |> List.ofSeq

    member __.DumpRpcLog() =
        let rpcLog = client.PostAndReply(fun rc -> GetRpcLog rc)

        for m in rpcLog do
            logMessage "RpcLog" (sprintf "%s; %s" (string m.Source) (string m.Message))

    member __.ServerDidInvoke(rpcMethod: string) =
        let rpcLog = client.PostAndReply(fun rc -> GetRpcLog rc)

        rpcLog
        |> Seq.exists (fun m -> m.Source = Server && (string m.Message["method"]) = rpcMethod)

    member __.ServerDidRespondTo(rpcMethod: string) =
        let rpcLog = client.PostAndReply(fun rc -> GetRpcLog rc)

        let invocation =
            rpcLog
            |> Seq.find (fun m -> m.Source = Client && (string m.Message["method"]) = rpcMethod)

        rpcLog
        |> Seq.exists (fun m ->
            m.Source = Client
            && (m.Message.["id"] |> string) = (invocation.Message.["id"] |> string)
            && (m.Message.["method"] |> string) = rpcMethod)

    member __.ServerMessageLogContains(pred: string -> bool) : bool =
        let rpcLog = client.PostAndReply(fun rc -> GetRpcLog rc)

        let containsPred m =
            let messageParams = m.Message["params"] |> Option.ofObj

            m.Source = Server
            && (m.Message["method"] |> string) = "window/logMessage"
            && (pred (
                messageParams
                |> indexJToken "message"
                |> Option.map string
                |> Option.defaultValue "(None)"
            ))

        rpcLog |> Seq.exists containsPred

    member __.Request<'Request, 'Response>(method: string, request: 'Request) : 'Response =
        let requestJObject = request |> serialize

        let responseJToken =
            client.PostAndReply<Result<JToken, JToken>>(fun rc -> SendServerRpcRequest(method, requestJObject, Some rc))

        match responseJToken with
        | Ok resultJToken -> resultJToken |> deserialize<'Response>
        | Error errorJToken ->
            failwithf "request to method \"%s\" has failed with error: %s" method (string errorJToken)

    member __.Open(filename: string) : FileController =
        let file = new FileController(client, projectDir.Value, filename)
        file.Open()
        file

let setupServerClient (clientProfile: ClientProfile) (testDataDirName: string) =
    let initialState =
        { defaultClientState with
            LoggingEnabled = clientProfile.LoggingEnabled }

    let clientActor = MailboxProcessor.Start(clientEventLoop initialState)
    clientActor.Post(SetupWithProfile clientProfile)

    let testAssemblyLocationDir =
        Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)

    let actualTestDataDir =
        DirectoryInfo(Path.Combine(testAssemblyLocationDir, "..", "..", "..", testDataDirName))

    if not actualTestDataDir.Exists then
        failwithf "setupServerClient: no such test data dir \"%s\"" actualTestDataDir.FullName

    new ClientController(clientActor, actualTestDataDir)


module TextEdit =
    let normalizeNewText (s: TextEdit) =
        { s with
            NewText = s.NewText.ReplaceLineEndings("\n") }
