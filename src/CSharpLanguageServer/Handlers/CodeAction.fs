namespace CSharpLanguageServer.Handlers

open System
open System.Reflection
open System.Collections.Generic
open System.Collections.Immutable
open System.IO

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult
open Ionide.LanguageServerProtocol.Server
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.FindSymbols
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.CodeRefactorings
open Microsoft.CodeAnalysis.CodeFixes
open Microsoft.CodeAnalysis.CodeActions

open CSharpLanguageServer
open CSharpLanguageServer.State
open CSharpLanguageServer.RoslynHelpers
open CSharpLanguageServer.Logging
open CSharpLanguageServer.Conversions

type CSharpCodeActionResolutionData =
    { TextDocumentUri: string
      Range: Range }

type CodeActionData = { Url: string }

[<RequireQualifiedAccess>]
module CodeAction =
    let private logger = LogProvider.getLoggerByName "CodeAction"

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
    let private getRoslynCodeActions (doc: Document) (textSpan: TextSpan)
        : Async<CodeActions.CodeAction list> = async {
        let! ct = Async.CancellationToken

        let roslynCodeActions = List<CodeActions.CodeAction>()
        let addCodeAction = Action<CodeActions.CodeAction>(roslynCodeActions.Add)
        let codeActionContext = CodeRefactoringContext(doc, textSpan, addCodeAction, ct)

        for refactoringProvider in refactoringProviderInstances do
            try
                do! refactoringProvider.ComputeRefactoringsAsync(codeActionContext) |> Async.AwaitTask
            with ex ->
                logger.error (
                    Log.setMessage "cannot compute refactorings for {provider}"
                    >> Log.addContext "provider" (string refactoringProvider)
                    >> Log.addException ex
                )

        // register code fixes
        let! semanticModel = doc.GetSemanticModelAsync(ct) |> Async.AwaitTask

        let isDiagnosticsOnTextSpan (diag: Microsoft.CodeAnalysis.Diagnostic) =
            diag.Location.SourceSpan.IntersectsWith(textSpan)

        let relatedDiagnostics =
            semanticModel.GetDiagnostics()
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
                        logger.error (
                            Log.setMessage "error in RegisterCodeFixesAsync()"
                            >> Log.addException ex
                        )

        let unwrapRoslynCodeAction (ca: Microsoft.CodeAnalysis.CodeActions.CodeAction) =
            let nestedCAProp = ca.GetType().GetProperty("NestedCodeActions", BindingFlags.Instance|||BindingFlags.NonPublic)
            if not (isNull nestedCAProp) then
                let nestedCAs = nestedCAProp.GetValue(ca, null) :?> ImmutableArray<Microsoft.CodeAnalysis.CodeActions.CodeAction>
                match nestedCAs.Length with
                | 0 -> [ca].ToImmutableArray()
                | _ -> nestedCAs
            else
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
            logger.info (
                Log.setMessage "Error in asyncMaybeOnException"
                >> Log.addException ex
            )
            return None
    }

    let lspCodeActionDetailsFromRoslynCA ca =
        let typeName = ca.GetType() |> string
        if typeName.Contains("CodeAnalysis.AddImport") then
            Some CodeActionKind.QuickFix, Some true
        else
            None, None

    let roslynCodeActionToUnresolvedLspCodeAction (ca: CodeActions.CodeAction): Ionide.LanguageServerProtocol.Types.CodeAction =
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
            : Async<TextDocumentEdit list> = async {

        let! ct = Async.CancellationToken

        // make a list of changes
        let solutionProjectChanges = updatedSolution.GetChanges(originalSolution).GetProjectChanges()

        let docTextEdits = List<TextDocumentEdit>()

        let addedDocs = solutionProjectChanges |> Seq.collect (fun pc -> pc.GetAddedDocuments())

        for docId in addedDocs do
            let newDoc = updatedSolution.GetDocument(docId)
            let! newDocText = newDoc.GetTextAsync(ct) |> Async.AwaitTask

            let edit: TextEdit =
                { Range = { Start = { Line=0; Character=0 }; End = { Line=0; Character=0 } }
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

                docTextEdits.Add({ TextDocument = textEditDocument; Edits = [| edit |] })
            | None -> ()

        let changedDocs = solutionProjectChanges |> Seq.collect (fun pc -> pc.GetChangedDocuments())

        for docId in changedDocs do
            let originalDoc = originalSolution.GetDocument(docId)
            let! originalDocText = originalDoc.GetTextAsync(ct) |> Async.AwaitTask
            let updatedDoc = updatedSolution.GetDocument(docId)
            let! docChanges = updatedDoc.GetTextChangesAsync(originalDoc, ct) |> Async.AwaitTask

            let diffEdits: TextEdit array =
                docChanges
                |> Seq.sortBy (fun c -> c.Span.Start)
                |> Seq.map (TextEdit.fromTextChange originalDocText.Lines)
                |> Array.ofSeq

            let textEditDocument = { Uri = originalDoc.FilePath |> Path.toUri
                                     Version = originalDoc.FilePath |> Path.toUri |> tryGetDocVersionByUri }

            docTextEdits.Add({ TextDocument = textEditDocument; Edits = diffEdits })

        return docTextEdits |> List.ofSeq
    }

    let roslynCodeActionToResolvedLspCodeAction
            originalSolution
            tryGetDocVersionByUri
            (originatingDoc: Document)
            (ca: CodeActions.CodeAction)
        : Async<Ionide.LanguageServerProtocol.Types.CodeAction option> = async {

        let! ct = Async.CancellationToken

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
            let edit: WorkspaceEdit = {
                Changes = None
                DocumentChanges = docTextEdit |> Array.ofList |> Some
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

    let provider (clientCapabilities: ClientCapabilities option) : U2<bool,CodeActionOptions> option =
        { CodeActionKinds = None
          ResolveProvider = Some true
        }
        |> U2.Second
        |> Some

    let handle (logMessage: Util.AsyncLogFn)
               (scope: ServerRequestScope)
               (p: CodeActionParams)
            : AsyncLspResult<TextDocumentCodeActionResult option> = async {
        let clientCapabilities = scope.ClientCapabilities

        let docMaybe = scope.GetUserDocumentForUri p.TextDocument.Uri
        match docMaybe with
        | None -> return None |> success
        | Some doc ->
            let! docText = doc.GetTextAsync() |> Async.AwaitTask
            let textSpan = Range.toTextSpan docText.Lines p.Range

            let! roslynCodeActions = getRoslynCodeActions doc textSpan

            let clientSupportsCodeActionEditResolveWithEditAndData =
                clientCapabilities
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
                            { TextDocumentUri = p.TextDocument.Uri
                              Range = p.Range }

                        logger.debug (
                            Log.setMessage "codeaction data: {data}"
                            >> Log.addContextDestructured "data" resolutionData
                        )
                        let lspCa = roslynCodeActionToUnresolvedLspCodeAction ca
                        { lspCa with Data = resolutionData |> serialize |> Some }

                    return roslynCodeActions |> Seq.map toUnresolvedLspCodeAction |> Array.ofSeq
                  }

                | false -> async {
                    let results = List<Ionide.LanguageServerProtocol.Types.CodeAction>()

                    for ca in roslynCodeActions do
                        let! maybeLspCa =
                            roslynCodeActionToResolvedLspCodeAction
                                scope.Solution
                                scope.OpenDocVersions.TryFind
                                doc
                                ca

                        if maybeLspCa.IsSome then
                            results.Add(maybeLspCa.Value)

                    return results |> Array.ofSeq
                  }

            return
               lspCodeActions
               |> Seq.sortByDescending (fun ca -> ca.IsPreferred)
               |> Seq.map U2<Command, Ionide.LanguageServerProtocol.Types.CodeAction>.Second
               |> Array.ofSeq
               |> Some
               |> success
    }

    let resolve (logMessage: Util.AsyncLogFn)
                (scope: ServerRequestScope)
                (p: Ionide.LanguageServerProtocol.Types.CodeAction)
            : AsyncLspResult<Ionide.LanguageServerProtocol.Types.CodeAction option> = async {
        let resolutionData =
            p.Data
            |> Option.map deserialize<CSharpCodeActionResolutionData>

        let docMaybe = scope.GetUserDocumentForUri resolutionData.Value.TextDocumentUri
        match docMaybe with
        | None -> return None |> success
        | Some doc ->
            let! ct = Async.CancellationToken
            let! docText = doc.GetTextAsync(ct) |> Async.AwaitTask

            let textSpan = Range.toTextSpan docText.Lines resolutionData.Value.Range

            let! roslynCodeActions =
                getRoslynCodeActions doc textSpan

            let selectedCodeAction = roslynCodeActions |> Seq.tryFind (fun ca -> ca.Title = p.Title)

            let toResolvedLspCodeAction =
                roslynCodeActionToResolvedLspCodeAction
                                                    scope.Solution
                                                    scope.OpenDocVersions.TryFind
                                                    doc

            let! maybeLspCodeAction =
                match selectedCodeAction with
                | Some ca -> async {
                    let! resolvedCA = toResolvedLspCodeAction ca

                    if resolvedCA.IsNone then
                        logger.info (
                            Log.setMessage "handleCodeActionResolve: could not resolve {action} - null"
                            >> Log.addContext "action" (string ca)
                        )

                    return resolvedCA
                  }
                | None -> async { return None }

            return maybeLspCodeAction |> success
    }
