namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Common.Types
open CSharpLanguageServer.Common.LspUtil

[<RequireQualifiedAccess>]
module CallHierarchy =
    let provider: bool option = None

    let prepare (wm: IWorkspaceManager) (p: CallHierarchyPrepareParams) : AsyncLspResult<CallHierarchyItem[] option> =
        notImplemented

    let incomingCalls
        (wm: IWorkspaceManager)
        (p: CallHierarchyIncomingCallsParams)
        : AsyncLspResult<CallHierarchyIncomingCall[] option> =
        notImplemented

    let outgoingCalls
        (wm: IWorkspaceManager)
        (p: CallHierarchyOutgoingCallsParams)
        : AsyncLspResult<CallHierarchyOutgoingCall[] option> =
        notImplemented
