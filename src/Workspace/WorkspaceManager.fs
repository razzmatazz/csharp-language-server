namespace CSharpLanguageServer.Workspace

open Ionide.LanguageServerProtocol

open CSharpLanguageServer.Common.Types

type WorkspaceManager(lspClient: ILspClient) =
    interface IWorkspaceManager with
        member this.Initialize () = ()
