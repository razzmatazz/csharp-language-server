namespace CSharpLanguageServer.Runtime

open System

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Util
open CSharpLanguageServer.Types
open CSharpLanguageServer.Lsp.Workspace

/// Defines how a server request is retired (i.e. its result committed and state change events emitted).
/// Sequential variants (<c>ReadOnlySequential</c>, <c>ReadWriteSequential</c>) are retired in arrival order,
/// guaranteeing ordered state change events, even though they may run and finish in any order.
/// <c>ReadWriteSequential</c> requests additionally block subsequent requests from starting until fully retired.
/// <c>ReadOnlyBackground</c> requests are retired out of order.
///
/// All modes wait for related solution to be loaded before request is activated except for
/// <c>ReadWriteSequentialImmediate</c>
type ServerRequestMode =
    | ReadOnlyBackground
    | ReadOnlySequential
    | ReadWriteSequential
    | ReadWriteSequentialImmediate

type ServerRequestOutcome =
    | RanToCompletion
    | Cancelled
    | Faulted

type ServerRequestState =
    | Enqueued
    | Pending of AsyncReplyChannel<obj> // obj=ServerState
    | Running
    | Terminated of ServerRequestOutcome * obj list // obj=ServerStateEvent

type ServerRequest =
    { State: ServerRequestState
      Name: string
      Id: int
      Registered: DateTime
      RunningSince: DateTime option
      Mode: ServerRequestMode option
      TargetUri: string option }

type ServerRequestContext
    (
        mode: ServerRequestMode,
        lspClient: ILspClient option,
        settings: ServerSettings,
        workspace: LspWorkspace,
        clientCapabilities: ClientCapabilities,
        emit: obj -> unit
    ) = // obj=ServerEvent
    member _.LspClient = lspClient
    member _.Settings = settings
    member _.Workspace = workspace
    member _.ClientCapabilities = clientCapabilities

    member _.Emit e =
        match mode with
        | ReadOnlySequential
        | ReadWriteSequential
        | ReadWriteSequentialImmediate -> emit e

        | ReadOnlyBackground -> failwith "'ReadOnlyBackground' requests cannot raise events"

type ActivateServerRequest = ServerRequestMode -> option<string> -> Async<ServerRequestContext>
