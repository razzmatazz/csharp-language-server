namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Common.Types
open CSharpLanguageServer.Common.LspUtil

[<RequireQualifiedAccess>]
module References =
    let provider: bool option = None

    let handle (wm: IWorkspaceManager) (p: ReferenceParams) : AsyncLspResult<Location[] option> = notImplemented
