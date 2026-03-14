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
    | RegisterRequest of string * AsyncReplyChannel<int>
    | RequestActivation of int * ServerRequestMode * option<string> * AsyncReplyChannel<obj> // obj=ServerState
    | TerminateRequest of int * ServerRequestOutcome * ServerEvent list
    | CleanupDeadlockedRequestsOnRequestQueue
    | PeriodicTimerTick
    | ProcessRequestQueue
    | RetireRequestsUpToIdCompleted
    | PushDiagnosticsDocumentBacklogUpdate
    | PushDiagnosticsDocumentDiagnosticsResolution of Result<(string * int option * Diagnostic array), Exception>
    | PushDiagnosticsProcessPendingDocuments
    | SettingsChange of ServerSettings
    | WorkspaceFolderChange of LspWorkspaceFolder
    | WorkspaceConfigurationChange of TimeSpan * option<LspWorkspaceFolder list>
    | WorkspaceFolderActivationRequest of string
    | WorkspaceFolderActivationComplete of (string * LspWorkspaceFolderSolution)
