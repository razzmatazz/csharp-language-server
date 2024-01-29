namespace CSharpLanguageServer.Handlers

open System

open Ionide.LanguageServerProtocol.Types
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.FindSymbols

open CSharpLanguageServer
open CSharpLanguageServer.State
open CSharpLanguageServer.RoslynHelpers

[<RequireQualifiedAccess>]
module WorkspaceSymbol =
    let provider (clientCapabilities: ClientCapabilities option) : U2<bool, WorkspaceSymbolOptions> option =
        true |> U2.First |> Some

    let handle (scope: ServerRequestScope) (symbolParams: WorkspaceSymbolParams): AsyncLspResult<SymbolInformation [] option> = async {
        let pattern =
            if String.IsNullOrEmpty(symbolParams.Query) then
                None
            else
                Some symbolParams.Query
        let! symbols = findSymbolsInSolution scope.Solution pattern (Some 20)
        return symbols |> Array.ofSeq |> Some |> LspResult.success
    }
