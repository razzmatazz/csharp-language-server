namespace CSharpLanguageServer.Handlers

open System

open Ionide.LanguageServerProtocol.Types
open Microsoft.CodeAnalysis.FindSymbols

open CSharpLanguageServer
open CSharpLanguageServer.State
open CSharpLanguageServer.RoslynHelpers

[<RequireQualifiedAccess>]
module References =
    let provider (clientCapabilities: ClientCapabilities option) : bool option =
        Some true

    let handle (scope: ServerRequestScope) (refParams: ReferenceParams): AsyncLspResult<Location [] option> = async {
        let! maybeSymbol = scope.GetSymbolAtPositionOnAnyDocument refParams.TextDocument.Uri refParams.Position
        match maybeSymbol with
        | Some (symbol, _, _) ->
            let! ct = Async.CancellationToken
            let! refs = SymbolFinder.FindReferencesAsync(symbol, scope.Solution, ct) |> Async.AwaitTask
            return refs
                    |> Seq.collect (fun r -> r.Locations)
                    |> Seq.map (fun rl -> lspLocationForRoslynLocation rl.Location)
                    |> Array.ofSeq
                    |> Some
                    |> LspResult.success

        | None -> return None |> LspResult.success
    }
