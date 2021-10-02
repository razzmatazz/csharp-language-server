module CSharpLanguageServer.RoslynHelpers

open System
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
open LanguageServerProtocol
open Microsoft.CodeAnalysis.MSBuild
open Microsoft.CodeAnalysis.CodeFixes
open Microsoft.CodeAnalysis.CSharp.Syntax

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
        : Async<Types.TextDocumentEdit list> = async {

    let getPathUri path = Uri("file://" + path)

    // make a list of changes
    let changedDocs = updatedSolution
                            .GetChanges(originalSolution)
                            .GetProjectChanges()
                            |> Seq.collect (fun pc -> pc.GetChangedDocuments())

    let docTextEdits = List<Types.TextDocumentEdit>()

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

let roslynCodeActionToLspCodeAction
        originalSolution
        tryGetDocVersionByUri
        _logMessage
        (ca: CodeActions.CodeAction): Async<Types.CodeAction option> = async {

    let asyncMaybeOnException op = async {
        try
            let! value = op ()
            return Some value
        with _ex ->
            (*
            logMessage (sprintf "roslynCodeActionToLspCodeAction: failed on %s; ex=%s; inner ex=%s"
                                (string ca)
                                (string ex)
                                (if ex.InnerException <> null then ex.InnerException.ToString() else "(none)" ))
            *)
            return None
    }

    let! maybeOps = asyncMaybeOnException (fun () -> ca.GetOperationsAsync(CancellationToken.None) |> Async.AwaitTask)

    match maybeOps with
    | None -> return None
    | Some ops ->

        let op = ops |> Seq.map (fun o -> o :?> ApplyChangesOperation)
                    |> Seq.head

        let! docTextEdit = lspDocChangesFromSolutionDiff originalSolution
                                                         op.ChangedSolution
                                                         tryGetDocVersionByUri

        let edit: Types.WorkspaceEdit = {
            Changes = None
            DocumentChanges = docTextEdit |> Array.ofList |> Some
        }

        return Some {
            Title = ca.Title
            Kind = None
            Diagnostics = None
            Edit = edit
            Command = None
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
    let documentUri = "file:///xxx"

    let symbolLocation = symbol.Locations |> Seq.head

    let location: Types.Location =
        { Uri = documentUri
          Range = symbolLocation.GetLineSpan().Span |> lspRangeForRoslynLinePosSpan
        }

    { Name = symbol.Name
      Kind = Types.SymbolKind.File
      Location = location
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
        logMessage ("loadSolution: loading solution: " + solutionPath)

        let msbuildWorkspace = MSBuildWorkspace.Create()
        msbuildWorkspace.LoadMetadataForReferencedProjects <- true

        let! _ = msbuildWorkspace.OpenSolutionAsync(solutionPath) |> Async.AwaitTask

        for diag in msbuildWorkspace.Diagnostics do
            logMessage ("msbuildWorkspace.Diagnostics: " + diag.ToString())

        //workspace <- Some(msbuildWorkspace :> Workspace)
        return Some msbuildWorkspace.CurrentSolution
    with
    | ex ->
        logMessage ("loadSolution: failed with " + ex.ToString())
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
