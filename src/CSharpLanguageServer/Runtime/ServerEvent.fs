namespace CSharpLanguageServer.Runtime

open System

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Types
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Lsp.WorkspaceFolder

type ServerEvent =
    | ServerStarted of ILspClient
    | ClientInitialize
    | ClientShutdown
    | ClientCapabilityChange of ClientCapabilities
    | DocumentClosed of string
    | DocumentOpened of string * int * DateTime
    | DocumentTouched of string * DateTime
    | EnterRequestContext of int64 * string * obj * AsyncReplyChannel<obj> // obj0=ServerRequestMode; obj1=ServerRequestContext
    | LeaveRequestContext of int64 * ServerEvent list
    | PeriodicTimerTick
    | ProcessRequestQueue
    | RequestQueueDrained
    | PushDiagnosticsDocumentBacklogUpdate
    | PushDiagnosticsDocumentDiagnosticsResolution of Result<(string * int option * Diagnostic array), Exception>
    | PushDiagnosticsProcessPendingDocuments
    | ServerReconfigured of CSharpConfiguration
    | TraceLevelChange of TraceValues
    | WorkspaceLoadResult of Result<LspWorkspace, Exception>
    | WorkspaceReconfigured of WorkspaceFolder list
    | WorkspaceFolderChange of LspWorkspaceFolder
    | WorkspaceReloadRequested of TimeSpan
