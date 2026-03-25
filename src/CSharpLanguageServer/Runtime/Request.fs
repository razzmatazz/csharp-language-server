namespace CSharpLanguageServer.Runtime

open System
open System.Collections.Generic

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Types
open CSharpLanguageServer.Lsp.Workspace

type ServerRequestMode =
    | ReadOnly
    | ReadWrite
    | ReadOnlyBackground

type ServerRequestState =
    | Pending
    | Running
    | Finished

type ServerRequest =
    { State: ServerRequestState
      Mode: ServerRequestMode
      Name: string
      RpcOrdinal: int64
      Registered: DateTime
      ActivationRC: AsyncReplyChannel<obj> // obj=ServerRequestContext
      RunningSince: option<DateTime>
      BufferedEvents: ServerEvent list }

type ServerRequestContext
    (
        requestMode: ServerRequestMode,
        lspClient: ILspClient,
        settings: ServerSettings,
        workspace: LspWorkspace,
        clientCapabilities: ClientCapabilities,
        shutdownReceived: bool
    ) =
    let bufferedEvents = ResizeArray<ServerEvent>()

    member _.RequestMode = requestMode
    member _.LspClient = lspClient
    member _.Settings = settings
    member _.Workspace = workspace
    member _.ClientCapabilities = clientCapabilities
    member _.ShutdownReceived = shutdownReceived

    member _.Emit(event: ServerEvent) =
        match requestMode with
        | ReadOnlyBackground -> invalidOp "Emit is not allowed on ReadOnlyBackground requests"
        | _ -> bufferedEvents.Add(event)

    member _.GetBufferedEvents() : ServerEvent list = bufferedEvents |> List.ofSeq
