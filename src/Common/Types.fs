module CSharpLanguageServer.Common.Types

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
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
type ICSharpLspClient =
    inherit ILspClient
    // Use a ClientCapabilitiesDTO instead of ClientCapabilities to avoid Option.map & Option.bind?
    // But ClientCapabilities is a complex type, write it again will be a huge work.
    abstract member Capabilities: ClientCapabilities option with get, set

[<Interface>]
type IWorkspaceManager =
    abstract member Initialize: WorkspaceFolder list -> Async<unit>
    abstract member WaitInitialized: unit -> Async<unit>
    abstract member ChangeWorkspaceFolders: WorkspaceFolder [] -> WorkspaceFolder [] -> Async<unit>
    abstract member GetDocument: DocumentUri -> Document option
    abstract member GetDiagnostics: DocumentUri -> Async<Diagnostic array>
    abstract member FindSymbol: DocumentUri -> Position -> Async<ISymbol option>
    abstract member FindSymbol': DocumentUri -> Position -> Async<(ISymbol * Document) option>
    abstract member FindSymbols: string option -> Async<ISymbol seq>
    abstract member FindReferences: ISymbol -> Async<ReferencedSymbol seq>
    abstract member FindImplementations: ISymbol -> Async<ISymbol seq>
    abstract member FindImplementations': INamedTypeSymbol -> bool -> Async<INamedTypeSymbol seq>
    abstract member FindDerivedClasses: INamedTypeSymbol -> Async<INamedTypeSymbol seq>
    abstract member FindDerivedClasses': INamedTypeSymbol -> bool -> Async<INamedTypeSymbol seq>
    abstract member FindDerivedInterfaces: INamedTypeSymbol -> Async<INamedTypeSymbol seq>
    abstract member FindDerivedInterfaces': INamedTypeSymbol -> bool -> Async<INamedTypeSymbol seq>
    abstract member FindCallers: ISymbol -> Async<SymbolCallerInfo seq>
    abstract member ResolveSymbolLocations: ISymbol -> Async<Location list>
    abstract member GetDocumentVersion: DocumentUri -> int option
    abstract member OpenDocument: DocumentUri -> int -> string -> Async<unit>
    abstract member CloseDocument: DocumentUri -> Async<unit>
    abstract member SaveDocument: DocumentUri -> string option -> Async<unit>
    abstract member ChangeDocument: DocumentUri -> int -> TextDocumentContentChangeEvent [] -> Async<unit>
