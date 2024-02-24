namespace CSharpLanguageServer.Handlers

open System
open System.Collections.Generic

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Server
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Microsoft.CodeAnalysis.Text

open CSharpLanguageServer
open CSharpLanguageServer.State
open CSharpLanguageServer.RoslynHelpers
open CSharpLanguageServer.Logging

[<RequireQualifiedAccess>]
module CodeAction =
    let private logger = LogProvider.getLoggerByName "CodeAction"

    type CSharpCodeActionResolutionData = {
        TextDocumentUri: string
        Range: Range
    }

    let provider (clientCapabilities: ClientCapabilities option) : U2<bool,CodeActionOptions> option =
        { CodeActionKinds = None
          ResolveProvider = Some true
        }
        |> U2.Second
        |> Some

    let handle (logMessage: Util.AsyncLogFn)
               (scope: ServerRequestScope)
               (actionParams: CodeActionParams)
            : AsyncLspResult<TextDocumentCodeActionResult option> = async {
        let docMaybe = scope.GetUserDocumentForUri actionParams.TextDocument.Uri
        match docMaybe with
        | None ->
            return None |> LspResult.success

        | Some doc ->
            let! ct = Async.CancellationToken
            let! docText = doc.GetTextAsync(ct) |> Async.AwaitTask

            let textSpan =
                actionParams.Range
                |> roslynLinePositionSpanForLspRange docText.Lines
                |> docText.Lines.GetTextSpan

            let! roslynCodeActions =
                getRoslynCodeActions logMessage doc textSpan

            let clientSupportsCodeActionEditResolveWithEditAndData =
                scope.ClientCapabilities
                |> Option.bind (fun x -> x.TextDocument)
                |> Option.bind (fun x -> x.CodeAction)
                |> Option.bind (fun x -> x.ResolveSupport)
                |> Option.map (fun resolveSupport -> resolveSupport.Properties |> Array.contains "edit")
                |> Option.defaultValue false

            let! lspCodeActions =
                match clientSupportsCodeActionEditResolveWithEditAndData with
                | true -> async {
                    let toUnresolvedLspCodeAction (ca: Microsoft.CodeAnalysis.CodeActions.CodeAction) =
                        let resolutionData: CSharpCodeActionResolutionData =
                            { TextDocumentUri = actionParams.TextDocument.Uri
                              Range = actionParams.Range }

                        let lspCa = roslynCodeActionToUnresolvedLspCodeAction ca
                        { lspCa with Data = resolutionData |> serialize |> Some }

                    return roslynCodeActions |> Seq.map toUnresolvedLspCodeAction |> Array.ofSeq
                  }

                | false -> async {
                    let results = List<CodeAction>()

                    for ca in roslynCodeActions do
                        let! maybeLspCa =
                            roslynCodeActionToResolvedLspCodeAction
                                scope.Solution
                                scope.OpenDocVersions.TryFind
                                logMessage
                                doc
                                ca

                        if maybeLspCa.IsSome then
                            results.Add(maybeLspCa.Value)

                    return results |> Array.ofSeq
                  }

            return
               lspCodeActions
               |> Seq.sortByDescending (fun ca -> ca.IsPreferred)
               |> Seq.map U2<Command, CodeAction>.Second
               |> Array.ofSeq
               |> Some
               |> LspResult.success
    }

    let resolve (logMessage: Util.AsyncLogFn)
                (scope: ServerRequestScope)
                (codeAction: CodeAction)
            : AsyncLspResult<CodeAction option> = async {
        let resolutionData =
            codeAction.Data
            |> Option.map deserialize<CSharpCodeActionResolutionData>

        let docMaybe = scope.GetUserDocumentForUri resolutionData.Value.TextDocumentUri

        match docMaybe with
        | None ->
            return None |> LspResult.success

        | Some doc ->
            let! ct = Async.CancellationToken
            let! docText = doc.GetTextAsync(ct) |> Async.AwaitTask

            let textSpan =
                       resolutionData.Value.Range
                       |> roslynLinePositionSpanForLspRange docText.Lines
                       |> docText.Lines.GetTextSpan

            let! roslynCodeActions =
                getRoslynCodeActions logMessage doc textSpan

            let selectedCodeAction = roslynCodeActions |> Seq.tryFind (fun ca -> ca.Title = codeAction.Title)

            let toResolvedLspCodeAction =
                roslynCodeActionToResolvedLspCodeAction
                                                    scope.Solution
                                                    scope.OpenDocVersions.TryFind
                                                    logMessage
                                                    doc

            let! maybeLspCodeAction =
                match selectedCodeAction with
                | Some ca -> async {
                    let! resolvedCA = toResolvedLspCodeAction ca

                    if resolvedCA.IsNone then
                       do! logMessage (sprintf "handleCodeActionResolve: could not resolve %s - null" (string ca))

                    return resolvedCA
                  }
                | None -> async { return None }

            return maybeLspCodeAction |> LspResult.success
    }
