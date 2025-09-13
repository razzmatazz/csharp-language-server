namespace CSharpLanguageServer.Handlers

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Threading

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CodeActions
open Microsoft.CodeAnalysis.CodeRefactorings
open Microsoft.CodeAnalysis.CodeFixes
open Microsoft.CodeAnalysis.Text
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open Microsoft.Extensions.Logging

open CSharpLanguageServer.Logging
open CSharpLanguageServer.Conversions
open CSharpLanguageServer.State
open CSharpLanguageServer.Util


type CSharpCodeActionResolutionData =
    { TextDocumentUri: string
      Range: Range }


[<RequireQualifiedAccess>]
module CodeAction =
    let private logger = Logging.getLoggerByName "CodeAction"

    let private instantiateRoslynProviders<'ProviderType> (isValidProvider: Type -> bool) =
        let assemblies =
            [ "Microsoft.CodeAnalysis.Features"
              "Microsoft.CodeAnalysis.CSharp.Features"
              "Microsoft.CodeAnalysis.Workspaces"
            ]
            |> Seq.map Assembly.Load
            |> Array.ofSeq

        let validType (t: Type) =
            (not (t.GetTypeInfo().IsInterface))
            && (not (t.GetTypeInfo().IsAbstract))
            && (not (t.GetTypeInfo().ContainsGenericParameters))

        let types =
            assemblies
            |> Seq.collect (fun a -> a.GetTypes())
            |> Seq.filter validType
            |> Seq.toArray

        let isProviderType (t: Type) = t.IsAssignableTo(typeof<'ProviderType>)

        let hasParameterlessConstructor (t: Type) = t.GetConstructor(Array.empty) |> isNull |> not

        types
            |> Seq.filter isProviderType
            |> Seq.filter hasParameterlessConstructor
            |> Seq.filter isValidProvider
            |> Seq.map Activator.CreateInstance
            |> Seq.filter (fun i -> i <> null)
            |> Seq.map (fun i -> i :?> 'ProviderType)
            |> Seq.toArray

    let private refactoringProviderInstances =
        instantiateRoslynProviders<CodeRefactoringProvider>
            (fun t -> ((string t) <> "Microsoft.CodeAnalysis.ChangeSignature.ChangeSignatureCodeRefactoringProvider"))

    let private codeFixProviderInstances =
        instantiateRoslynProviders<CodeFixProvider>
            (fun _ -> true)

    // TODO: refactor it. I think a long function in functional language is hard to read :)
    let private getRoslynCodeActions (doc: Document) (textSpan: TextSpan) (ct: CancellationToken)
        : Async<CodeActions.CodeAction list> = async {
        let roslynCodeActions = List<CodeActions.CodeAction>()
        let addCodeAction = Action<CodeActions.CodeAction>(roslynCodeActions.Add)
        let codeActionContext = CodeRefactoringContext(doc, textSpan, addCodeAction, ct)

        for refactoringProvider in refactoringProviderInstances do
            try
                do! refactoringProvider.ComputeRefactoringsAsync(codeActionContext) |> Async.AwaitTask
            with ex ->
                logger.LogError(
                    ex,
                    "cannot compute refactorings for {provider}",
                    refactoringProvider)

        // register code fixes
        let! semanticModel = doc.GetSemanticModelAsync(ct) |> Async.AwaitTask

        let isDiagnosticsOnTextSpan (diag: Microsoft.CodeAnalysis.Diagnostic) =
            diag.Location.SourceSpan.IntersectsWith(textSpan)

        let relatedDiagnostics =
            semanticModel.GetDiagnostics(cancellationToken=ct)
            |> Seq.filter isDiagnosticsOnTextSpan
            |> List.ofSeq

        let diagnosticsBySpan =
            relatedDiagnostics
            |> Seq.groupBy (fun d -> d.Location.SourceSpan)

        for diagnosticSpan, diagnosticsWithSameSpan in diagnosticsBySpan do
            let addCodeFix =
                Action<CodeActions.CodeAction, ImmutableArray<Microsoft.CodeAnalysis.Diagnostic>>(
                    fun ca _ -> roslynCodeActions.Add(ca))

            for codeFixProvider in codeFixProviderInstances do
                let refactoringProviderOK (diag: Microsoft.CodeAnalysis.Diagnostic) =
                    let translatedDiagId diagId =
                        match diagId with
                        | "CS8019" -> "RemoveUnnecessaryImportsFixable"
                        | _ -> ""

                    codeFixProvider.FixableDiagnosticIds.Contains(diag.Id)
                    || codeFixProvider.FixableDiagnosticIds.Contains(translatedDiagId diag.Id)

                let fixableDiagnostics =
                    diagnosticsWithSameSpan
                    |> Seq.filter refactoringProviderOK

                if not (Seq.isEmpty fixableDiagnostics) then
                    let codeFixContext = CodeFixContext(doc, diagnosticSpan, fixableDiagnostics.ToImmutableArray(), addCodeFix, ct)

                    try
                        do! codeFixProvider.RegisterCodeFixesAsync(codeFixContext) |> Async.AwaitTask
                    with ex ->
                        logger.LogError(ex, "error in RegisterCodeFixesAsync()")

        let unwrapRoslynCodeAction (ca: Microsoft.CodeAnalysis.CodeActions.CodeAction) =
            let nestedCAProp =
                ca.GetType().GetProperty("NestedCodeActions", BindingFlags.Instance|||BindingFlags.NonPublic)
                |> Option.ofObj

            match nestedCAProp with
            | Some nestedCAProp ->
                let nestedCAs = nestedCAProp.GetValue(ca, null) :?> ImmutableArray<Microsoft.CodeAnalysis.CodeActions.CodeAction>
                match nestedCAs.Length with
                | 0 -> [ca].ToImmutableArray()
                | _ -> nestedCAs
            | None ->
                [ca].ToImmutableArray()

        return roslynCodeActions
               |> Seq.collect unwrapRoslynCodeAction
               |> List.ofSeq
    }

    let asyncMaybeOnException op = async {
        try
            let! value = op ()
            return Some value
        with ex ->
            logger.LogWarning(ex, "Error in asyncMaybeOnException")
            return None
    }

    let lspCodeActionDetailsFromRoslynCA ca =
        let typeName = ca.GetType() |> string
        if typeName.Contains("CodeAnalysis.AddImport") then
            Some CodeActionKind.QuickFix, Some true
        else
            None, None

    let roslynCodeActionToUnresolvedLspCodeAction (ca: CodeActions.CodeAction): CodeAction =
        let caKind, caIsPreferred = lspCodeActionDetailsFromRoslynCA ca
        { Title = ca.Title
          Kind = caKind
          Diagnostics = None
          Edit = None
          Command = None
          Data = None
          IsPreferred = caIsPreferred
          Disabled = None
        }

    let lspDocChangesFromSolutionDiff
            originalSolution
            (updatedSolution: Solution)
            (tryGetDocVersionByUri: string -> int option)
            (originatingDoc: Document)
            (ct: CancellationToken)
            : Async<TextDocumentEdit list> = async {
        // make a list of changes
        let solutionProjectChanges = updatedSolution.GetChanges(originalSolution).GetProjectChanges()

        let docTextEdits = List<TextDocumentEdit>()

        let addedDocs = solutionProjectChanges |> Seq.collect (fun pc -> pc.GetAddedDocuments())

        for docId in addedDocs do
            let newDoc = updatedSolution.GetDocument(docId) |> nonNull "updatedSolution.GetDocument(docId)"
            let! newDocText = newDoc.GetTextAsync(ct) |> Async.AwaitTask

            let edit: TextEdit =
                { Range = { Start = { Line=0u; Character=0u }; End = { Line=0u; Character=0u } }
                  NewText = newDocText.ToString() }

            let newDocFilePathMaybe =
                if String.IsNullOrWhiteSpace(newDoc.FilePath)
                       || (not <| Path.IsPathRooted(newDoc.FilePath)) then
                    if String.IsNullOrWhiteSpace(originatingDoc.FilePath) then
                        None
                    else
                        let directory = Path.GetDirectoryName(originatingDoc.FilePath)
                        Path.Combine(directory, newDoc.Name) |> Some
                else
                    Some newDoc.FilePath

            match newDocFilePathMaybe with
            | Some newDocFilePath ->
                let textEditDocument = { Uri = newDocFilePath |> Path.toUri
                                         Version = newDocFilePath |> Path.toUri |> tryGetDocVersionByUri }

                docTextEdits.Add({ TextDocument = textEditDocument; Edits = [| U2.C1 edit |] })
            | None -> ()

        let changedDocs = solutionProjectChanges |> Seq.collect (fun pc -> pc.GetChangedDocuments())

        for docId in changedDocs do
            let originalDoc = originalSolution.GetDocument(docId) |> Option.ofObj
            let updatedDoc = updatedSolution.GetDocument(docId) |> Option.ofObj

            match originalDoc, updatedDoc with
            | Some originalDoc, Some updatedDoc ->
                let! originalDocText = originalDoc.GetTextAsync(ct) |> Async.AwaitTask
                let! docChanges = updatedDoc.GetTextChangesAsync(originalDoc, ct) |> Async.AwaitTask

                let diffEdits: U2<TextEdit,AnnotatedTextEdit> array =
                    docChanges
                    |> Seq.sortBy (fun c -> c.Span.Start)
                    |> Seq.map (TextEdit.fromTextChange originalDocText.Lines)
                    |> Seq.map U2.C1
                    |> Array.ofSeq

                let textEditDocument = { Uri = originalDoc.FilePath |> Path.toUri
                                         Version = originalDoc.FilePath |> Path.toUri |> tryGetDocVersionByUri }

                docTextEdits.Add({ TextDocument = textEditDocument; Edits = diffEdits })
            | _, _ -> ()

        return docTextEdits |> List.ofSeq
    }

    let roslynCodeActionToResolvedLspCodeAction
            originalSolution
            tryGetDocVersionByUri
            (originatingDoc: Document)
            (ct: CancellationToken)
            (ca: CodeActions.CodeAction)
        : Async<CodeAction option> = async {

        let! maybeOps = asyncMaybeOnException (fun () -> ca.GetOperationsAsync(ct) |> Async.AwaitTask)

        match maybeOps with
        | None -> return None
        | Some ops ->
            let op = ops |> Seq.map (fun o -> o :?> ApplyChangesOperation)
                         |> Seq.head

            let! docTextEdit =
                lspDocChangesFromSolutionDiff originalSolution
                                              op.ChangedSolution
                                              tryGetDocVersionByUri
                                              originatingDoc
                                              ct
            let edit: WorkspaceEdit = {
                Changes = None
                DocumentChanges = docTextEdit |> Seq.map U4.C1 |> Array.ofSeq |> Some
                ChangeAnnotations = None
            }

            let caKind, caIsPreferred = lspCodeActionDetailsFromRoslynCA ca

            return Some {
                Title = ca.Title
                Kind = caKind
                Diagnostics = None
                Edit = Some edit
                Command = None
                Data = None
                IsPreferred = caIsPreferred
                Disabled = None
            }
    }

    let provider (clientCapabilities: ClientCapabilities) : U2<bool, CodeActionOptions> option =
        let literalSupport =
            clientCapabilities.TextDocument
            |> Option.bind (fun x -> x.CodeAction)
            |> Option.bind (fun x -> x.CodeActionLiteralSupport)

        match literalSupport with
        | Some _ ->
            { CodeActionKinds = None
              ResolveProvider = Some true
              WorkDoneProgress = None }
            |> U2.C2
            |> Some
        | None ->
            true |> U2.C1 |> Some

    let handle (context: ServerRequestContext)
               (p: CodeActionParams)
            : AsyncLspResult<TextDocumentCodeActionResult option> = async {
        match context.GetDocument p.TextDocument.Uri with
        | None -> return None |> LspResult.success
        | Some doc ->
            let! ct = Async.CancellationToken
            let! docText = doc.GetTextAsync(ct) |> Async.AwaitTask
            let textSpan = Range.toTextSpan docText.Lines p.Range

            let! roslynCodeActions = getRoslynCodeActions doc textSpan ct

            let clientSupportsCodeActionEditResolveWithEditAndData =
                context.ClientCapabilities.TextDocument
                |> Option.bind (fun x -> x.CodeAction)
                |> Option.bind (fun x -> x.ResolveSupport)
                |> Option.map (fun resolveSupport -> resolveSupport.Properties |> Array.contains "edit")
                |> Option.defaultValue false

            let! lspCodeActions =
                match clientSupportsCodeActionEditResolveWithEditAndData with
                | true -> async {
                    let toUnresolvedLspCodeAction (ca: Microsoft.CodeAnalysis.CodeActions.CodeAction) =
                        let resolutionData: CSharpCodeActionResolutionData =
                            { TextDocumentUri = p.TextDocument.Uri
                              Range = p.Range }

                        (*
                        logger.trace (
                            Log.setMessage "codeaction data: {data}"
                            >> Log.addContextDestructured "data" resolutionData
                        )
                        *)

                        let lspCa = roslynCodeActionToUnresolvedLspCodeAction ca
                        { lspCa with Data = resolutionData |> serialize |> Some }

                    return roslynCodeActions |> Seq.map toUnresolvedLspCodeAction |> Array.ofSeq
                  }

                | false -> async {
                    let results = List<CodeAction>()

                    for ca in roslynCodeActions do
                        let! maybeLspCa =
                            roslynCodeActionToResolvedLspCodeAction
                                doc.Project.Solution
                                context.GetDocumentVersion
                                doc
                                ct
                                ca

                        if maybeLspCa.IsSome then
                            results.Add(maybeLspCa.Value)

                    return results |> Array.ofSeq
                  }

            return
               lspCodeActions
               |> Seq.sortByDescending (fun ca -> ca.IsPreferred)
               |> Seq.map U2<Command, CodeAction>.C2
               |> Array.ofSeq
               |> Some
               |> LspResult.success
    }

    let resolve (context: ServerRequestContext) (p: CodeAction) : AsyncLspResult<CodeAction> = async {
        let resolutionData =
            p.Data
            |> Option.map deserialize<CSharpCodeActionResolutionData>

        match context.GetDocument resolutionData.Value.TextDocumentUri with
        | None ->
            return raise (Exception(sprintf "no document for uri %s" resolutionData.Value.TextDocumentUri))
        | Some doc ->
            let! ct = Async.CancellationToken
            let! docText = doc.GetTextAsync(ct) |> Async.AwaitTask

            let textSpan = Range.toTextSpan docText.Lines resolutionData.Value.Range

            let! roslynCodeActions =
                getRoslynCodeActions doc textSpan ct

            let selectedCodeAction = roslynCodeActions |> Seq.tryFind (fun ca -> ca.Title = p.Title)

            let toResolvedLspCodeAction =
                roslynCodeActionToResolvedLspCodeAction
                                                    doc.Project.Solution
                                                    context.GetDocumentVersion
                                                    doc
                                                    ct

            let! lspCodeAction =
                match selectedCodeAction with
                | Some ca -> async {
                    let! resolvedCA = toResolvedLspCodeAction ca

                    if resolvedCA.IsNone then
                        logger.LogError("handleCodeActionResolve: could not resolve {action} - null", ca)

                    return resolvedCA.Value
                  }
                | None ->
                    raise (Exception("no CodeAction resolved"))

            return lspCodeAction |> LspResult.success
    }
