namespace CSharpLanguageServer.Runtime

open System

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Types
open CSharpLanguageServer.Lsp.WorkspaceFolder

type ServerEvent =
    | ClientInitialize of ILspClient
    | ClientShutdown
    | ClientCapabilityChange of ClientCapabilities
    | DocumentClosed of string
    | DocumentOpened of string * int * DateTime
    | DocumentTouched of string * DateTime
    | RegisterRequest of string * obj * AsyncReplyChannel<int> // obj=ServerRequestMode
    | AwaitRequestActivation of int * AsyncReplyChannel<obj> // obj=ServerState
    | RetireRequest of int
    | PeriodicTimerTick
    | ProcessRequestQueue
    | PushDiagnosticsDocumentBacklogUpdate
    | PushDiagnosticsDocumentDiagnosticsResolution of Result<(string * int option * Diagnostic array), Exception>
    | PushDiagnosticsProcessPendingDocuments
    | SettingsChange of ServerSettings
    | WorkspaceConfigurationChanged of WorkspaceFolder list
    | WorkspaceFolderChange of LspWorkspaceFolder
    | WorkspaceReloadRequested of TimeSpan
