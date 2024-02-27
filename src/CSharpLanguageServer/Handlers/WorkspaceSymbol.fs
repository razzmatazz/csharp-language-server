namespace CSharpLanguageServer.Handlers

open System

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult
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

    let handle (scope: ServerRequestScope)
               (p: WorkspaceSymbolParams)
            : AsyncLspResult<U2<SymbolInformation array,WorkspaceSymbol array> option> = async {
        let pattern =
            if String.IsNullOrEmpty(p.Query) then
                None
            else
                Some p.Query
        let! symbols = findSymbolsInSolution scope.Solution pattern (Some 20)

        return
            symbols
            |> Array.ofSeq
            |> U2.First
            |> Some
            |> success
    }
