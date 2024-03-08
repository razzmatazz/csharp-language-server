namespace CSharpLanguageServer.Handlers

open System

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult
open Microsoft.CodeAnalysis.FindSymbols

open CSharpLanguageServer
open CSharpLanguageServer.State
open CSharpLanguageServer.Conversions

[<RequireQualifiedAccess>]
module References =
    let provider (clientCapabilities: ClientCapabilities option) : bool option =
        Some true

    let handle (scope: ServerRequestScope) (refParams: ReferenceParams): AsyncLspResult<Location [] option> = async {
        let! maybeSymbol = scope.GetSymbolAtPositionOnAnyDocument refParams.TextDocument.Uri refParams.Position
        match maybeSymbol with
        | None -> return None |> success
        | Some (symbol, _, _) ->
            let! ct = Async.CancellationToken
            let! refs = SymbolFinder.FindReferencesAsync(symbol, scope.Solution, ct) |> Async.AwaitTask
            // FIXME: refs is wrong. There are lots of false positive even if we add Seq.distinct before Seq.toArray
            return
                refs
                |> Seq.collect (fun r -> r.Locations)
                |> Seq.map (fun rl -> Location.fromRoslynLocation rl.Location)
                |> Seq.toArray
                |> Some
                |> success
    }
