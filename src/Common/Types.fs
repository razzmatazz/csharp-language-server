module CSharpLanguageServer.Common.Types

open Microsoft.CodeAnalysis
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types

type ServerSettings =
    { SolutionPath: string option
      LogLevel: MessageType }

    static member Default: ServerSettings =
        { SolutionPath = None
          LogLevel = MessageType.Log }

type CSharpMetadataInformation =
    { ProjectName: string
      AssemblyName: string
      SymbolName: string
      Source: string }

type DecompiledMetadataDocument =
    { Metadata: CSharpMetadataInformation
      Document: Document }

type CSharpMetadataParams =
    { TextDocument: TextDocumentIdentifier }

type CSharpMetadataResponse = CSharpMetadataInformation

[<Interface>]
type ICSharpLspServer =
    inherit ILspServer
    abstract CSharpMetadata: CSharpMetadataParams -> AsyncLspResult<CSharpMetadataResponse option>

[<Interface>]
type IWorkspaceManager =
    abstract member Initialize: WorkspaceFolder list -> unit
    abstract member ChangeWorkspaceFolders: WorkspaceFolder [] -> WorkspaceFolder [] -> Async<unit>
    abstract member GetDocument: DocumentUri -> Document option
    abstract member ChangeDocument: DocumentUri -> TextDocumentContentChangeEvent [] -> Async<unit>
