module CSharpLanguageServer.RoslynHelpers

open System
open System.Linq
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Threading
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CodeActions
open Microsoft.CodeAnalysis.CodeRefactorings
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.FindSymbols
open Microsoft.CodeAnalysis.Text
open Ionide.LanguageServerProtocol
open Microsoft.CodeAnalysis.MSBuild
open Microsoft.CodeAnalysis.CodeFixes
open Microsoft.CodeAnalysis.CSharp.Syntax
open System.Collections.Immutable

let roslynTagToLspCompletion tag =
    match tag with
    | "Class"         -> Types.CompletionItemKind.Class
    | "Delegate"      -> Types.CompletionItemKind.Class
    | "Enum"          -> Types.CompletionItemKind.Enum
    | "Interface"     -> Types.CompletionItemKind.Interface
    | "Struct"        -> Types.CompletionItemKind.Class
    | "Local"         -> Types.CompletionItemKind.Variable
    | "Parameter"     -> Types.CompletionItemKind.Variable
    | "RangeVariable" -> Types.CompletionItemKind.Variable
    | "Const"         -> Types.CompletionItemKind.Value
    | "EnumMember"    -> Types.CompletionItemKind.Enum
    | "Event"         -> Types.CompletionItemKind.Function
    | "Field"         -> Types.CompletionItemKind.Field
    | "Method"        -> Types.CompletionItemKind.Method
    | "Property"      -> Types.CompletionItemKind.Property
    | "Label"         -> Types.CompletionItemKind.Unit
    | "Keyword"       -> Types.CompletionItemKind.Keyword
    | "Namespace"     -> Types.CompletionItemKind.Module
    | _ -> Types.CompletionItemKind.Property

let lspPositionForRoslynLinePosition (pos: LinePosition): Types.Position =
    { Line = pos.Line ; Character = pos.Character }

let roslynLinePositionForLspPosition (pos: Types.Position) =
    LinePosition(pos.Line, pos.Character)

let roslynLinePositionSpanForLspRange (range: Types.Range) =
    LinePositionSpan(
        roslynLinePositionForLspPosition range.Start,
        roslynLinePositionForLspPosition range.End)

let lspRangeForRoslynLinePosSpan (pos: LinePositionSpan): Types.Range =
    { Start = lspPositionForRoslynLinePosition pos.Start
      End = lspPositionForRoslynLinePosition pos.End }

let lspTextEditForRoslynTextChange (docText: SourceText) (c: TextChange): Types.TextEdit =
    { Range = docText.Lines.GetLinePositionSpan(c.Span) |> lspRangeForRoslynLinePosSpan
      NewText = c.NewText }

let lspLocationForRoslynLocation(loc: Microsoft.CodeAnalysis.Location): Types.Location =
    let getPathUri path = Uri("file://" + path)

    { Uri = loc.SourceTree.FilePath |> getPathUri |> string
      Range = loc.GetLineSpan().Span |> lspRangeForRoslynLinePosSpan }

let lspDocChangesFromSolutionDiff
        originalSolution
        (updatedSolution: Solution)
        (tryGetDocVersionByUri: string -> int option)
        logMessage
        (originatingDoc: Document)
        : Async<Types.TextDocumentEdit list> = async {

    let getPathUri path = Uri("file://" + path)

    // make a list of changes
    let solutionProjectChanges = updatedSolution.GetChanges(originalSolution).GetProjectChanges()

    let docTextEdits = List<Types.TextDocumentEdit>()

    let addedDocs = solutionProjectChanges |> Seq.collect (fun pc -> pc.GetAddedDocuments())

    for docId in addedDocs do
        let newDoc = updatedSolution.GetDocument(docId)
        let! newDocText = newDoc.GetTextAsync() |> Async.AwaitTask

        let edit: Types.TextEdit =
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
            docTextEdits.Add(
                { TextDocument = { Uri = newDocFilePath |> getPathUri |> string
                                   Version = newDocFilePath |> getPathUri |> string |> tryGetDocVersionByUri }
                  Edits = [| edit |] })
        | None -> ()

    let changedDocs = solutionProjectChanges |> Seq.collect (fun pc -> pc.GetChangedDocuments())

    for docId in changedDocs do
        let originalDoc = originalSolution.GetDocument(docId)
        let! originalDocText = originalDoc.GetTextAsync() |> Async.AwaitTask
        let updatedDoc = updatedSolution.GetDocument(docId)
        let! docChanges = updatedDoc.GetTextChangesAsync(originalDoc) |> Async.AwaitTask

        let diffEdits: Types.TextEdit array =
            docChanges
            |> Seq.sortBy (fun c -> c.Span.Start)
            |> Seq.map (lspTextEditForRoslynTextChange originalDocText)
            |> Array.ofSeq

        docTextEdits.Add(
            { TextDocument =  { Uri = originalDoc.FilePath |> getPathUri |> string
                                Version = originalDoc.FilePath |> getPathUri |> string |> tryGetDocVersionByUri }
              Edits = diffEdits })

    return docTextEdits |> List.ofSeq
}

type CodeActionData = { Url: string }

let asyncMaybeOnException op = async {
    try
        let! value = op ()
        return Some value
    with _ex ->
        return None
}

let roslynCodeActionToUnresolvedLspCodeAction (ca: CodeActions.CodeAction): Types.CodeAction =
    { Title = ca.Title
      Kind = None
      Diagnostics = None
      Edit = None
      Command = None
      Data = None
      IsPreferred = None
      Disabled = None
    }

let roslynCodeActionToResolvedLspCodeAction
        originalSolution
        tryGetDocVersionByUri
        logMessage
        (originatingDoc: Document)
        (ca: CodeActions.CodeAction)
    : Async<Types.CodeAction option> = async {

    let! maybeOps = asyncMaybeOnException (fun () -> ca.GetOperationsAsync(CancellationToken.None) |> Async.AwaitTask)

    match maybeOps with
    | None -> return None
    | Some ops ->
        let op = ops |> Seq.map (fun o -> o :?> ApplyChangesOperation)
                     |> Seq.head

        let! docTextEdit = lspDocChangesFromSolutionDiff originalSolution
                                                         op.ChangedSolution
                                                         tryGetDocVersionByUri
                                                         logMessage
                                                         originatingDoc
        let edit: Types.WorkspaceEdit = {
            Changes = None
            DocumentChanges = docTextEdit |> Array.ofList |> Some
        }

        return Some {
            Title = ca.Title
            Kind = None
            Diagnostics = None
            Edit = Some edit
            Command = None
            Data = None
            IsPreferred = None
            Disabled = None
        }
}

type DocumentSymbolCollector(documentUri) =
    inherit CSharpSyntaxWalker(SyntaxWalkerDepth.Token)

    let mutable collectedSymbols: Types.SymbolInformation list = []

    let collect (identifier: SyntaxToken) kind =
        let location: Types.Location =
            { Uri = documentUri
              Range = identifier.GetLocation().GetLineSpan().Span
                      |> lspRangeForRoslynLinePosSpan
            }

        let symbol: Types.SymbolInformation =
            { Name = identifier.ToString()
              Kind = kind
              Location = location
              ContainerName = None
            }

        collectedSymbols <- symbol :: collectedSymbols

    member __.GetSymbols() = collectedSymbols |> List.rev |> Array.ofList

    override __.VisitClassDeclaration(node) =
        collect node.Identifier Types.SymbolKind.Class

        base.VisitClassDeclaration(node)

    override __.VisitMethodDeclaration(node) =
        collect node.Identifier Types.SymbolKind.Method

        base.VisitMethodDeclaration(node)


type DocumentSymbolCollectorForMatchingSymbolName (documentUri, symbolName: string) =
    inherit CSharpSyntaxWalker(SyntaxWalkerDepth.Token)

    let mutable collectedLocations: Types.Location list = []

    let collectIdentifier (identifier: SyntaxToken) =
        if identifier.ValueText = symbolName then
            let location: Types.Location =
                { Uri = documentUri
                  Range = identifier.GetLocation().GetLineSpan().Span
                          |> lspRangeForRoslynLinePosSpan }

            collectedLocations <- location :: collectedLocations

    member __.GetLocations() = collectedLocations |> Seq.rev |> List.ofSeq

    override __.Visit(node) =
        // TODO: collect other type of syntax nodes too
        if node :? MethodDeclarationSyntax then
            let methodDecl = node :?> MethodDeclarationSyntax
            collectIdentifier methodDecl.Identifier

        else if node :? TypeDeclarationSyntax then
             let typeDecl = node :?> TypeDeclarationSyntax
             collectIdentifier typeDecl.Identifier

        else if node :? PropertyDeclarationSyntax then
             let propertyDecl = node :?> PropertyDeclarationSyntax
             collectIdentifier propertyDecl.Identifier

        else if node :? EventDeclarationSyntax then
             let eventDecl = node :?> EventDeclarationSyntax
             collectIdentifier eventDecl.Identifier

        base.Visit(node)


let symbolToLspSymbolInformation (symbol: ISymbol): Types.SymbolInformation =
    let symbolLocation = symbol.Locations |> Seq.head

    { Name = symbol.Name
      Kind = Types.SymbolKind.File
      Location = symbolLocation |> lspLocationForRoslynLocation
      ContainerName = None }

let roslynToLspDiagnosticSeverity s: Types.DiagnosticSeverity option =
    match s with
    | Microsoft.CodeAnalysis.DiagnosticSeverity.Info -> Some Types.DiagnosticSeverity.Information
    | Microsoft.CodeAnalysis.DiagnosticSeverity.Warning -> Some Types.DiagnosticSeverity.Warning
    | Microsoft.CodeAnalysis.DiagnosticSeverity.Error -> Some Types.DiagnosticSeverity.Error
    | Microsoft.CodeAnalysis.DiagnosticSeverity.Hidden -> None
    | _ -> None

let roslynToLspDiagnostic (d: Microsoft.CodeAnalysis.Diagnostic) : Types.Diagnostic =
    { Range = d.Location.GetLineSpan().Span |> lspRangeForRoslynLinePosSpan
      Severity = d.Severity |> roslynToLspDiagnosticSeverity
      Code = None
      CodeDescription = None
      Source = "lsp"
      Message = d.GetMessage()
      RelatedInformation = None
      Tags = None
      Data = None }

let findSymbols (solution: Solution) pattern (_limit: int option): Async<Types.SymbolInformation list> = async {
    let mutable symbolsFound = []

    for project in solution.Projects do
        let! symbols = SymbolFinder.FindSourceDeclarationsWithPatternAsync(
                           project, pattern, SymbolFilter.TypeAndMember)
                       |> Async.AwaitTask
        symbolsFound <- (List.ofSeq symbols) @ symbolsFound

    return Seq.map symbolToLspSymbolInformation symbolsFound |> List.ofSeq
}

let instantiateRoslynProviders<'ProviderType> (isValidProvider: Type -> bool) =
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

let refactoringProviderInstances =
    instantiateRoslynProviders<CodeRefactoringProvider>
        (fun t -> ((string t) <> "Microsoft.CodeAnalysis.ChangeSignature.ChangeSignatureCodeRefactoringProvider"))

let codeFixProviderInstances =
    instantiateRoslynProviders<CodeFixProvider>
        (fun _ -> true)

let tryLoadSolutionOnPath logMessage solutionPath = async {
    try
        logMessage ("loading solution: " + solutionPath)

        let msbuildWorkspace = MSBuildWorkspace.Create()
        msbuildWorkspace.LoadMetadataForReferencedProjects <- true

        let! _ = msbuildWorkspace.OpenSolutionAsync(solutionPath) |> Async.AwaitTask

        for diag in msbuildWorkspace.Diagnostics do
            logMessage ("msbuildWorkspace.Diagnostics: " + diag.ToString())

        //workspace <- Some(msbuildWorkspace :> Workspace)
        return Some msbuildWorkspace.CurrentSolution
    with
    | ex ->
        logMessage ("solution loading has failed with error: " + ex.ToString())
        return None
}

let tryLoadSolutionFromProjectFiles logMessage (projFiles: string list) = async {
    let msbuildWorkspace = MSBuildWorkspace.Create()
    msbuildWorkspace.LoadMetadataForReferencedProjects <- true

    for file in projFiles do
        logMessage ("loading proj file " + file + "..")
        try
            do! msbuildWorkspace.OpenProjectAsync(file) |> Async.AwaitTask |> Async.Ignore
        with ex ->
            logMessage (sprintf "could not OpenProjectAsync('%s'): %s" file (ex |> string))
        ()

    logMessage (sprintf "OK, %d project files loaded" projFiles.Length)

    for diag in msbuildWorkspace.Diagnostics do
        logMessage ("msbuildWorkspace.Diagnostics: " + diag.ToString())

    //workspace <- Some(msbuildWorkspace :> Workspace)
    return Some msbuildWorkspace.CurrentSolution
}

let findAndLoadSolutionOnDir logMessage dir = async {
    let fileNotOnNodeModules (filename: string) =
        filename.Split(Path.DirectorySeparatorChar)
        |> Seq.contains "node_modules"
        |> not

    let solutionFiles =
        Directory.GetFiles(dir, "*.sln", SearchOption.AllDirectories)
        |> Seq.filter fileNotOnNodeModules
        |> Seq.toList

    logMessage (sprintf "%d solution(s) found: [%s]" solutionFiles.Length (String.Join(", ", solutionFiles)) )

    let singleSolutionFound =
        match solutionFiles with
        | [x] -> Some x
        | _ -> None

    match singleSolutionFound with
    | None ->
        logMessage ("no or multiple .sln files found on " + dir)
        logMessage ("looking for .csproj/fsproj files on " + dir + "..")

        let projFiles =
            let csprojFiles = Directory.GetFiles(dir, "*.csproj", SearchOption.AllDirectories)
            let fsprojFiles = Directory.GetFiles(dir, "*.fsproj", SearchOption.AllDirectories)

            [ csprojFiles; fsprojFiles ] |> Seq.concat
                                            |> Seq.filter fileNotOnNodeModules
                                            |> Seq.toList

        if projFiles.Length = 0 then
            let message = "no or .csproj/.fsproj or sln files found on " + dir
            logMessage message
            Exception message |> raise

        let! solution = tryLoadSolutionFromProjectFiles logMessage projFiles
        return solution

    | Some solutionPath ->
        let! solution = tryLoadSolutionOnPath logMessage solutionPath
        return solution
}

let getRoslynCodeActions (doc: Document) (textSpan: TextSpan): Async<CodeAction list> = async {
    let roslynCodeActions = List<CodeActions.CodeAction>()
    let addCodeAction = Action<CodeActions.CodeAction>(roslynCodeActions.Add)
    let codeActionContext = CodeRefactoringContext(doc, textSpan, addCodeAction, CancellationToken.None)

    for refactoringProvider in refactoringProviderInstances do
        do! refactoringProvider.ComputeRefactoringsAsync(codeActionContext) |> Async.AwaitTask

    // register code fixes
    let! semanticModel = doc.GetSemanticModelAsync() |> Async.AwaitTask

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
                let codeFixContext = CodeFixContext(doc, diagnosticSpan, fixableDiagnostics.ToImmutableArray(), addCodeFix, CancellationToken.None)

                try
                    do! codeFixProvider.RegisterCodeFixesAsync(codeFixContext) |> Async.AwaitTask
                with _ex ->
                    //sprintf "error in RegisterCodeFixesAsync(): %s" (ex.ToString()) |> logMessage
                    ()

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


let getContainingTypeOrThis (symbol: ISymbol): INamedTypeSymbol =
    if (symbol :? INamedTypeSymbol) then
        symbol :?> INamedTypeSymbol
    else
        symbol.ContainingType

let getFullReflectionName (containingType: INamedTypeSymbol) =
    let stack = Stack<string>();
    stack.Push(containingType.MetadataName);
    let mutable ns = containingType.ContainingNamespace;

    let mutable doContinue = true
    while doContinue do
        stack.Push(ns.Name);
        ns <- ns.ContainingNamespace

        doContinue <- ns <> null && not ns.IsGlobalNamespace

    String.Join(".", stack)

let tryAddDocument logMessage
                   (docFilePath: string)
                   (text: string)
                   (solution: Solution)
                   : Document option =

    let docDir = Path.GetDirectoryName(docFilePath)
    //logMessage (sprintf "TextDocumentDidOpen: docFilename=%s docDir=%s" docFilename docDir)

    let matchesPath (p: Project) =
        let projectDir = Path.GetDirectoryName(p.FilePath)
        (docDir |> string).StartsWith(projectDir |> string)

    let projectOnPath = solution.Projects |> Seq.filter matchesPath |> Seq.tryHead

    match projectOnPath with
    | Some proj ->
        let projectBaseDir = Path.GetDirectoryName(proj.FilePath)
        let docName = docFilePath.Substring(projectBaseDir.Length+1)

        //logMessage (sprintf "Adding file %s (\"%s\") to project %s" docName docFilePath proj.FilePath)

        let newDoc = proj.AddDocument(name=docName, text=SourceText.From(text), folders=null, filePath=docFilePath)
        Some newDoc

    | None -> None
