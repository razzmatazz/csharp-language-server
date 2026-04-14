module CSharpLanguageServer.Tests.Tooling

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Reflection
open System.Runtime.InteropServices
open System.Text
open System.Threading
open System.Threading.Tasks
open System.Xml.Linq

open NUnit.Framework
open Newtonsoft.Json.Linq
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Server

open CSharpLanguageServer.Runtime.JsonRpc

let indexJToken (name: string) (jobj: option<JToken>) : option<JToken> =
    jobj |> Option.bind (fun p -> p[name] |> Option.ofObj)

let emptyClientCapabilities: ClientCapabilities =
    { Workspace = None
      TextDocument = None
      NotebookDocument = None
      Window = None
      General = None
      Experimental = None }

type LspClientProfile =
    { LoggingEnabled: bool
      ClientCapabilities: ClientCapabilities
      SolutionLoadDelay: int option
      AnalyzersEnabled: bool option
      ExtraEnv: Map<string, string>
      ExtraArgs: string list }

let defaultClientCapabilities =
    { emptyClientCapabilities with
        Workspace =
            Some
                { ApplyEdit = None
                  WorkspaceEdit = None
                  DidChangeConfiguration = None
                  DidChangeWatchedFiles = None
                  Symbol = None
                  ExecuteCommand = None
                  WorkspaceFolders = None
                  Configuration = Some true
                  SemanticTokens = None
                  CodeLens = None
                  FileOperations = None
                  InlineValue = None
                  InlayHint = None
                  Diagnostics = None }
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
                  DocumentSymbol =
                    Some
                        { DynamicRegistration = None
                          SymbolKind = None
                          HierarchicalDocumentSymbolSupport = Some true
                          TagSupport = None
                          LabelSupport = None }
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
                          CodeActionLiteralSupport = Some { CodeActionKind = { ValueSet = Array.empty } } } }
        Window =
            Some
                { WorkDoneProgress = Some true
                  ShowMessage = None
                  ShowDocument = None }
        Experimental = {| csharp = {| metadataUris = true |} |} |> serialize |> Some }

let defaultClientProfile =
    { LoggingEnabled = false
      ClientCapabilities = defaultClientCapabilities
      SolutionLoadDelay = None
      AnalyzersEnabled = None  // defaults to false; only set to Some true in analyzer-specific tests
      ExtraEnv = Map.empty
      ExtraArgs = [] }

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
        failwithf "makeServerProcessInfo: no server executable '%s' present" serverFileName

    let processStartInfo = new ProcessStartInfo()
    processStartInfo.FileName <- serverFileName
    processStartInfo.Arguments <- "--features razor-support"
    processStartInfo.RedirectStandardInput <- true
    processStartInfo.RedirectStandardOutput <- true
    processStartInfo.RedirectStandardError <- true
    processStartInfo.UseShellExecute <- false
    processStartInfo.CreateNoWindow <- true
    processStartInfo.WorkingDirectory <- projectTempDir

    processStartInfo

type RpcEndpoint =
    | Server
    | Client

type RpcMessageLogEntry =
    { Source: RpcEndpoint
      TimestampMS: int
      Message: JObject }

type LspClientState =
    { ClientProfile: LspClientProfile
      LoggingEnabled: bool
      ProjectDir: string option
      ServerProcess: Process option
      ServerStderrLineReadTask: Task
      SetupTimestamp: DateTime
      RpcLog: RpcMessageLogEntry list
      ServerInfo: InitializeResultServerInfo option
      ServerCapabilities: ServerCapabilities option
      PushDiagnostics: Map<string, int option * Diagnostic[]> }

let initialClientState =
    { ClientProfile =
        { LoggingEnabled = false
          ClientCapabilities = emptyClientCapabilities
          SolutionLoadDelay = None
          AnalyzersEnabled = None
          ExtraEnv = Map.empty
          ExtraArgs = [] }
      LoggingEnabled = false
      ProjectDir = None
      ServerProcess = None
      ServerStderrLineReadTask = Task.CompletedTask
      SetupTimestamp = DateTime.MinValue
      RpcLog = []
      ServerInfo = None
      ServerCapabilities = None
      PushDiagnostics = Map.empty }

type LspClientEvent =
    | UpdateState of (LspClientState -> LspClientState)
    | ServerStartRequest of LspClientProfile * string * AsyncReplyChannel<LspClientState>
    | ServerStopRequest of AsyncReplyChannel<unit>
    | GetState of AsyncReplyChannel<LspClientState>
    | ServerStderrLineRead of string option
    | EmitLogMessage of DateTime * string * string
    | GetRpcLog of AsyncReplyChannel<RpcMessageLogEntry list>

let buildConfigurationResponse (paramsToken: JToken option) (clientProfile: LspClientProfile) : JToken =
    let getSectionConfiguration (section: string) : Option<JToken> =
        match section with
        | "csharp" ->
            let debugObj =
                match clientProfile.SolutionLoadDelay with
                | Some ms -> Some {| solutionLoadDelay = ms |}
                | None -> None

            let analyzersEnabled = clientProfile.AnalyzersEnabled |> Option.defaultValue false

            {| metadataUris = true
               analyzersEnabled = analyzersEnabled
               debug = debugObj |}
            |> serialize
            |> Some
        | _ -> None

    paramsToken
    |> Option.map (fun p ->
        p
        |> deserialize<ConfigurationParams>
        |> _.Items
        |> Seq.choose _.Section
        |> Seq.map getSectionConfiguration
        |> List.ofSeq
        |> serialize)
    |> Option.defaultValue (serialize ([]: JToken list))

let makeRpcLogCallback (post: LspClientEvent -> unit) (entry: JsonRpcLogEntry) =
    match entry with
    | RpcRead jobj ->
        post (
            UpdateState(fun s ->
                { s with
                    RpcLog =
                        s.RpcLog
                        @ [ { Source = Server
                              TimestampMS = 0
                              Message = jobj } ] })
        )
    | RpcWrite jobj ->
        post (
            UpdateState(fun s ->
                { s with
                    RpcLog =
                        s.RpcLog
                        @ [ { Source = Client
                              TimestampMS = 0
                              Message = jobj } ] })
        )
    | RpcError msg -> post (EmitLogMessage(DateTime.Now, "JsonRpc", sprintf "ERROR: %s" msg))
    | RpcWarn msg -> post (EmitLogMessage(DateTime.Now, "JsonRpc", sprintf "WARN: %s" msg))
    | RpcDebug msg -> post (EmitLogMessage(DateTime.Now, "JsonRpc", sprintf "DEBUG: %s" msg))

let configureRpcTransport
    (clientProfile: LspClientProfile)
    (post: LspClientEvent -> unit)
    (_transport: MailboxProcessor<JsonRpcTransportEvent>)
    : JsonRpcCallHandlerMap * JsonRpcNotificationHandlerMap =
    let callHandlers =
        Map.ofList
            [ "client/registerCapability",
              (fun (ctx: JsonRpcRequestContext) -> async {
                  post (
                      EmitLogMessage(
                          DateTime.Now,
                          "ClientRpcCall",
                          sprintf "client/registerCapability: params=%A" ctx.Params
                      )
                  )

                  return Ok(JValue.CreateNull() :> JToken)
              })
              "workspace/configuration",
              (fun (ctx: JsonRpcRequestContext) -> async {
                  post (
                      EmitLogMessage(
                          DateTime.Now,
                          "ClientRpcCall",
                          sprintf "workspace/configuration: params=%A" ctx.Params
                      )
                  )

                  let result = buildConfigurationResponse ctx.Params clientProfile

                  post (
                      EmitLogMessage(
                          DateTime.Now,
                          "ClientRpcCall",
                          sprintf "workspace/configuration: res=%s" (string result)
                      )
                  )

                  return Ok result
              })
              "window/workDoneProgress/create",
              (fun (ctx: JsonRpcRequestContext) -> async {
                  post (
                      EmitLogMessage(
                          DateTime.Now,
                          "ClientRpcCall",
                          sprintf "window/workDoneProgress/create: params=%A" ctx.Params
                      )
                  )

                  return Ok(JValue.CreateNull() :> JToken)
              }) ]

    let notificationHandlers =
        Map.ofList
            [ "window/showMessage",
              (fun (ctx: JsonRpcRequestContext) -> async {
                  match ctx.Params with
                  | Some p ->
                      let mp = p |> deserialize<ShowMessageParams>

                      post (
                          EmitLogMessage(
                              DateTime.Now,
                              "window/showMessage",
                              String.Format("[{0}] \"{1}\"", mp.Type, mp.Message)
                          )
                      )
                  | None -> ()
              })
              "window/logMessage",
              (fun (ctx: JsonRpcRequestContext) -> async {
                  match ctx.Params with
                  | Some p ->
                      let mp = p |> deserialize<LogMessageParams>

                      post (
                          EmitLogMessage(
                              DateTime.Now,
                              "window/logMessage",
                              String.Format("[{0}] \"{1}\"", mp.Type, mp.Message)
                          )
                      )
                  | None -> ()
              })
              "$/progress",
              (fun (ctx: JsonRpcRequestContext) -> async {
                  match ctx.Params with
                  | Some p ->
                      let value = p["value"] |> Option.ofObj

                      post (
                          EmitLogMessage(
                              DateTime.Now,
                              "$/progress",
                              String.Format("{0} {1}", value |> indexJToken "kind", value |> indexJToken "message")
                          )
                      )
                  | None -> ()
              })
              "$/logTrace",
              (fun (ctx: JsonRpcRequestContext) -> async {
                  match ctx.Params with
                  | Some p ->
                      let mp = p |> deserialize<LogTraceParams>
                      let prefix = mp.Verbose |> Option.map (sprintf "%s: ") |> Option.defaultValue ""
                      post (EmitLogMessage(DateTime.Now, "$/logTrace", sprintf "%s%s" prefix mp.Message))
                  | None -> ()
              })
              "textDocument/publishDiagnostics",
              (fun (ctx: JsonRpcRequestContext) -> async {
                  match ctx.Params with
                  | Some p ->
                      post (
                          EmitLogMessage(
                              DateTime.Now,
                              "ClientRpcCall",
                              sprintf "textDocument/publishDiagnostics: %s" (string p)
                          )
                      )

                      let pd = p |> deserialize<PublishDiagnosticsParams>

                      post (
                          UpdateState(fun s ->
                              { s with
                                  PushDiagnostics = s.PushDiagnostics |> Map.add pd.Uri (pd.Version, pd.Diagnostics) })
                      )
                  | None -> ()
              }) ]

    callHandlers, notificationHandlers

let processClientEvent (state: LspClientState) (post: LspClientEvent -> unit) msg : Async<LspClientState> = async {
    let logMessage logger msg =
        post (EmitLogMessage(DateTime.Now, logger, msg))

    match msg with
    | UpdateState stateUpdateFn -> return stateUpdateFn state

    | ServerStartRequest(clientProfile, projectDir, rc) ->
        let state =
            { state with
                SetupTimestamp = DateTime.Now
                ClientProfile = clientProfile
                LoggingEnabled = clientProfile.LoggingEnabled }

        let processStartInfo = makeServerProcessInfo projectDir

        for KeyValue(k, v) in clientProfile.ExtraEnv do
            processStartInfo.Environment[k] <- v

        if not clientProfile.ExtraArgs.IsEmpty then
            let extraArgStr = clientProfile.ExtraArgs |> String.concat " "
            processStartInfo.Arguments <- processStartInfo.Arguments + " " + extraArgStr

        let p = new Process()
        p.StartInfo <- processStartInfo

        logMessage "ServerStartRequest" "StartServer: p.Start().."
        let startResult = p.Start()
        logMessage "ServerStartRequest" (String.Format("StartServer: p.Start(): {0}, {1}", startResult, p.Id))

        post (ServerStderrLineRead None)

        let state =
            { state with
                ProjectDir = Some projectDir
                ServerProcess = Some p }

        rc.Reply(state)

        return state

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

    | EmitLogMessage(timestamp, logger, msg) ->
        let offsetMs = int (timestamp - state.SetupTimestamp).TotalMilliseconds

        let timestampFmt =
            if offsetMs >= 0 then
                sprintf "+%06i" offsetMs
            else
                sprintf "%07i" offsetMs

        if state.LoggingEnabled then
            Console.Error.WriteLine("[{0} {1}] {2}", timestampFmt, logger, msg)

        return state

    | GetRpcLog rc ->
        rc.Reply(state.RpcLog)
        return state
}

let clientEventLoop (initialState: LspClientState) (inbox: MailboxProcessor<LspClientEvent>) =
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
        match RuntimeInformation.IsOSPlatform OSPlatform.OSX, tempDir.StartsWith "/private" with
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
    if Directory.Exists path then
        Directory.GetFileSystemEntries(path)
        |> Array.iter (fun item ->
            if File.Exists(item) then
                File.Delete(item)
            else
                deleteDirectory item)

        Directory.Delete(path)

let fileUriForProjectDir projectDir filename =
    match Environment.OSVersion.Platform with
    | PlatformID.Win32NT -> ("file:///" + projectDir + "/" + filename).Replace("\\", "/")
    | _ -> "file://" + projectDir + "/" + filename

type LspDocumentHandle(rpcTransport: MailboxProcessor<JsonRpcTransportEvent>, projectDir: string, filename: string) =

    let mutable fileContents: option<string> = None
    let mutable fileVersion: int = 1

    let notify method (p: JToken) =
        sendJsonRpcNotification rpcTransport method p |> Async.RunSynchronously

    member __.FileName = filename

    member __.Uri = filename |> fileUriForProjectDir projectDir

    interface IDisposable with
        member this.Dispose() =
            let didCloseParams: DidCloseTextDocumentParams =
                { TextDocument = { Uri = this.Uri } }

            notify "textDocument/didClose" (serialize didCloseParams)

    member this.OpenWithText(text: string) =
        fileContents <- Some text

        let textDocument =
            let fileLanguageId =
                if Path.GetExtension filename = ".cshtml" then
                    "razor"
                else
                    "csharp"

            { Uri = this.Uri
              LanguageId = fileLanguageId
              Version = fileVersion
              Text = text }

        let didOpenParams: DidOpenTextDocumentParams = { TextDocument = textDocument }

        notify "textDocument/didOpen" (serialize didOpenParams)

    member this.OpenWithTextFromDisk() =
        let fileText =
            Path.Combine(projectDir, filename)
            |> File.ReadAllBytes
            |> Encoding.UTF8.GetString

        this.OpenWithText(fileText)

    member this.Change(text: string) =
        fileVersion <- fileVersion + 1
        fileContents <- Some text

        let didChangeParams: DidChangeTextDocumentParams =
            { TextDocument =
                { Uri = this.Uri
                  Version = fileVersion }
              ContentChanges = [| { Text = text } |> U2.C2 |] }

        notify "textDocument/didChange" (serialize didChangeParams)

    member this.Save() =
        let fullPath = Path.Combine(projectDir, filename)
        File.WriteAllBytes(fullPath, fileContents.Value |> Encoding.UTF8.GetBytes)

        let didSaveParams: DidSaveTextDocumentParams =
            { TextDocument = { Uri = this.Uri }
              Text = fileContents }

        notify "textDocument/didSave" (serialize didSaveParams)

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

type LspTestClient(clientProfile: LspClientProfile) =
    let client = MailboxProcessor.Start(clientEventLoop initialClientState)

    let mutable solutionDir: string option = None
    let mutable solutionLoaded: bool = false
    let mutable rpcTransportOpt: MailboxProcessor<JsonRpcTransportEvent> option = None

    let rpcTransport () =
        rpcTransportOpt
        |> Option.defaultWith (fun () -> failwith "rpcTransport not yet initialised")

    let logMessage m msg =
        client.Post(EmitLogMessage(DateTime.Now, sprintf "ClientActorController.%s" m, msg))

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

            match solutionDir, runningInGithubActions with
            | Some solutionDir, false ->
                logMessage "Dispose" (sprintf "Removing files on solution dir \"%s\".." solutionDir)
                deleteDirectory solutionDir
            | _ -> ()

    member __.SolutionDir: string = solutionDir.Value

    member this.LoadSolution
        (
            fixtureName: string,
            patchSolutionDir: string -> unit,
            initializeParamsUpdate: InitializeParams -> InitializeParams
        ) =
        let log = logMessage "LoadSolution"

        // solution can only be loaded once for LspTestClient instance
        if solutionLoaded then
            failwith "Solution has already been loaded for this LspTestClient!"

        solutionLoaded <- true

        // prepare temp dir and copy in solution contents
        let testAssemblyLocationDir =
            Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName

        let sourceTestDataDir =
            Path.Combine(testAssemblyLocationDir, "..", "..", "..", "Fixtures", fixtureName)
            |> DirectoryInfo

        if not sourceTestDataDir.Exists then
            failwithf "LspTestClient.LoadSolution(): no such test data dir \"%s\"" sourceTestDataDir.FullName

        let tempSolutionDir = prepareTempTestDirFrom sourceTestDataDir
        patchSolutionDir tempSolutionDir
        solutionDir <- Some tempSolutionDir

        // start the server process
        log "sending ServerStartRequest"

        let state =
            client.PostAndReply(fun rc -> ServerStartRequest(clientProfile, solutionDir.Value, rc))

        // wire up the JSON-RPC transport on top of the process stdio
        // server→client direction: we read from server's stdout, write to server's stdin
        let transport =
            startJsonRpcTransport
                state.ServerProcess.Value.StandardOutput.BaseStream
                state.ServerProcess.Value.StandardInput.BaseStream
                (Some(makeRpcLogCallback client.Post))
                (configureRpcTransport clientProfile client.Post)

        rpcTransportOpt <- Some transport

        let initializeParams: InitializeParams =
            let rootUri = solutionDir.Value |> Uri |> string |> DocumentUri

            { RootPath = None
              RootUri = Some rootUri
              ProcessId = Process.GetCurrentProcess() |> _.Id |> int |> Some
              Capabilities = state.ClientProfile.ClientCapabilities
              WorkDoneToken = None
              ClientInfo = None
              Locale = None
              InitializationOptions = None
              Trace = Some TraceValues.Verbose
              WorkspaceFolders = None }
            |> initializeParamsUpdate

        let initializeResult =
            let initResponse =
                sendJsonRpcCall transport "initialize" (serialize initializeParams)
                |> Async.RunSynchronously

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

        sendJsonRpcNotification transport "initialized" (JObject())
        |> Async.RunSynchronously

        log "OK, 'initialized' notification sent"

    member __.WaitForProgressEnd(messagePred: (string -> bool)) =
        let timeoutMS = 20 * 1000
        let start = DateTime.Now

        let progressEndMatch m =
            let paramsValue = m.Message["params"] |> Option.ofObj |> indexJToken "value"

            m.Source = Server
            && (string m.Message.["method"]) = "$/progress"
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
        let transport = rpcTransport ()
        let _ = sendJsonRpcCall transport "shutdown" (JObject()) |> Async.RunSynchronously
        sendJsonRpcNotification transport "exit" (JObject()) |> Async.RunSynchronously
        shutdownJsonRpcTransport transport |> Async.RunSynchronously

    member __.Stop() =
        match rpcTransportOpt with
        | Some transport -> shutdownJsonRpcTransport transport |> Async.RunSynchronously
        | None -> ()

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

    member __.ClientDidSendNotification(rpcMethod: string) =
        let rpcLog = client.PostAndReply(fun rc -> GetRpcLog rc)

        rpcLog
        |> Seq.exists (fun m ->
            m.Source = Client
            && (string m.Message["method"]) = rpcMethod
            && m.Message["id"] |> isNull)

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

    member __.ServerProgressLogContains(pred: string -> bool) : bool =
        let containsPred m =
            let paramsValue = m.Message["params"] |> Option.ofObj |> indexJToken "value"

            m.Source = Server
            && (m.Message["method"] |> string) = "$/progress"
            && (pred (
                paramsValue
                |> indexJToken "message"
                |> Option.map string
                |> Option.defaultValue "(None)"
            ))

        let rpcLog = client.PostAndReply(fun rc -> GetRpcLog rc)
        rpcLog |> Seq.exists containsPred

    member __.Notify<'Params>(method: string, ``params``: 'Params) : unit =
        sendJsonRpcNotification (rpcTransport ()) method (serialize ``params``)
        |> Async.RunSynchronously

    member __.Request<'Request, 'Response>(method: string, request: 'Request) : 'Response =
        let result =
            sendJsonRpcCall (rpcTransport ()) method (serialize request)
            |> Async.RunSynchronously

        match result with
        | Ok token -> token |> deserialize<'Response>
        | Error err -> failwithf "request to method \"%s\" has failed with error: %s" method (string err)

    member __.Open(filename: string) : LspDocumentHandle =
        let file = new LspDocumentHandle(rpcTransport (), solutionDir.Value, filename)
        file.OpenWithTextFromDisk()
        file

    member __.OpenWithText(filename: string, text: string) : LspDocumentHandle =
        let file = new LspDocumentHandle(rpcTransport (), solutionDir.Value, filename)
        file.OpenWithText(text)
        file

/// Serializes all `dotnet build` invocations across the test suite so that concurrent
/// test runs on Windows (where MSBuild file-locking is stricter) do not interfere.
let private dotnetBuildSemaphore = new SemaphoreSlim(1, 1)

/// Run `dotnet build` in <paramref name="dir"/>, serialized globally so that at most
/// one build runs at a time.  Returns (exitCode, stdout, stderr).
let runDotnetBuild (dir: string) =
    dotnetBuildSemaphore.Wait()

    try
        let psi = ProcessStartInfo("dotnet", "build")
        psi.WorkingDirectory <- dir
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        psi.UseShellExecute <- false

        let proc =
            match Process.Start(psi) with
            | null -> failwith "Failed to start dotnet build process"
            | p -> p

        use _ = proc
        // Read stdout/stderr asynchronously to prevent deadlocks when pipe buffers fill up.
        let stdoutTask = proc.StandardOutput.ReadToEndAsync()
        let stderrTask = proc.StandardError.ReadToEndAsync()

        let exited = proc.WaitForExit(120_000)
        let stdout = stdoutTask.Result
        let stderr = stderrTask.Result

        if not exited then
            proc.Kill(entireProcessTree = true)
            failwithf "dotnet build timed out after 120 seconds\nstdout:\n%s\nstderr:\n%s" stdout stderr

        proc.ExitCode, stdout, stderr
    finally
        dotnetBuildSemaphore.Release() |> ignore

let activeClientsSemaphore =
    // Analyzers are disabled by default in tests (see buildConfigurationResponse), so the
    // per-test CPU cost is low enough to run one server per logical core safely.
    let concurrency = Environment.ProcessorCount
    new SemaphoreSlim(concurrency, concurrency)

let activateFixtureExt
    fixtureName
    clientProfile
    (patchFixtureDir: string -> unit)
    (initializeParamsUpdate: InitializeParams -> InitializeParams)
    =
    activeClientsSemaphore.Wait()

    try
        let client = new LspTestClient(clientProfile)
        client.LoadSolution(fixtureName, patchFixtureDir, initializeParamsUpdate)
        client
    finally
        activeClientsSemaphore.Release() |> ignore

let emptyFixturePatch _ = ()

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

let activateFixture fixtureName =
    activateFixtureExt fixtureName defaultClientProfile emptyFixturePatch id

let activateFixtureWithLoggingEnabled fixtureName =
    activateFixtureExt
        fixtureName
        { defaultClientProfile with
            LoggingEnabled = true }
        emptyFixturePatch
        id

module TextEdit =
    let normalizeNewText (s: TextEdit) =
        { s with
            NewText = s.NewText.ReplaceLineEndings("\n") }

let waitUntilOrTimeout (timeout: TimeSpan) (predicate: unit -> bool) (failureMessage: string) =
    let stopwatch = Stopwatch.StartNew()

    while not (predicate ()) && stopwatch.Elapsed < timeout do
        Thread.Sleep(50)

    if stopwatch.Elapsed >= timeout then
        Assert.Fail(failureMessage)

let getWorkspaceDiagnosticsForUri (client: LspTestClient) uri =
    let diagnosticParams: WorkspaceDiagnosticParams =
        { WorkDoneToken = None
          PartialResultToken = None
          Identifier = None
          PreviousResultIds = Array.empty }

    let report: WorkspaceDiagnosticReport option =
        client.Request("workspace/diagnostic", diagnosticParams)

    let matchingDocDiagnosticItemsForUri
        (reportItem: Ionide.LanguageServerProtocol.Types.WorkspaceDocumentDiagnosticReport)
        =
        match reportItem with
        | U2.C2 _ -> failwith "'U2.C1' was expected"
        | U2.C1 fullReport ->
            if fullReport.Uri = uri then
                fullReport.Items |> List.ofArray
            else
                []

    match report with
    | None -> failwith "Some response from workspace/diagnostic was expected"
    | Some report -> report.Items |> Seq.collect matchingDocDiagnosticItemsForUri |> List.ofSeq
