namespace CSharpLanguageServer.Runtime

open System

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Types
open CSharpLanguageServer.Lsp.Workspace

type ServerRequestMode =
    | ReadOnly
    | ReadWrite

type ServerRequestState = | Enqueued

type ServerRequest =
    { State: ServerRequestState
      Name: string
      Id: int
      Registered: DateTime
      Mode: ServerRequestMode
      ActivationListeners: AsyncReplyChannel<obj> list // obj=ServerState
      RunningSince: option<DateTime> }

type ServerRequestContext
    (
        mode: ServerRequestMode,
        lspClient: ILspClient option,
        settings: ServerSettings,
        workspace: LspWorkspace,
        clientCapabilities: ClientCapabilities,
        emit: ServerEvent -> unit
    ) =
    member _.LspClient = lspClient
    member _.Settings = settings
    member _.Workspace = workspace
    member _.ClientCapabilities = clientCapabilities
    member _.Emit = emit
