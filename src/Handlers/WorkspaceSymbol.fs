namespace CSharpLanguageServer.Handlers

open System
open Microsoft.CodeAnalysis
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult

open CSharpLanguageServer.Common
open CSharpLanguageServer.Common.Types

[<RequireQualifiedAccess>]
module WorkspaceSymbol =
    let provider: bool option = Some true

    let handle (wm: IWorkspaceManager) (p: WorkspaceSymbolParams) : AsyncLspResult<SymbolInformation[] option> = async {
        let pattern =
            if String.IsNullOrEmpty(p.Query) then
                None
            else
                Some p.Query
        let! symbols = wm.FindSymbols pattern
        return
            symbols
            |> Seq.map (SymbolInformation.fromSymbol SymbolDisplayFormat.MinimallyQualifiedFormat)
            |> Seq.collect id
            // TODO: make 100 configurable?
            |> Seq.truncate 100
            |> Seq.toArray
            |> Some
            |> success
    }
