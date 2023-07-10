namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Common.Types
open CSharpLanguageServer.Common.LspUtil

[<RequireQualifiedAccess>]
module TypeHierarchy =
    let provider: bool option = None

    let prepare (wm: IWorkspaceManager) (p: TypeHierarchyPrepareParams) : AsyncLspResult<TypeHierarchyItem[] option> =
        notImplemented

    let supertypes
        (wm: IWorkspaceManager)
        (p: TypeHierarchySupertypesParams)
        : AsyncLspResult<TypeHierarchyItem[] option> =
        notImplemented

    let subtypes (wm: IWorkspaceManager) (p: TypeHierarchySubtypesParams) : AsyncLspResult<TypeHierarchyItem[] option> =
        notImplemented
