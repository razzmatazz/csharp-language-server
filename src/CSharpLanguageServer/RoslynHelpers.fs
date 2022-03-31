module CSharpLanguageServer.RoslynHelpers

open System
open System.Linq
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Threading
open Ionide.LanguageServerProtocol.Types
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CodeActions
open Microsoft.CodeAnalysis.CodeRefactorings
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.FindSymbols
open Microsoft.CodeAnalysis.Formatting
open Microsoft.CodeAnalysis.Text
open Ionide.LanguageServerProtocol
open Microsoft.CodeAnalysis.MSBuild
open Microsoft.CodeAnalysis.CodeFixes
open Microsoft.CodeAnalysis.CSharp.Syntax
open System.Collections.Immutable

let roslynTagToLspCompletion tag =
    match tag with
    | "Class"         -> Types.CompletionItemKind.Class
    | "Delegate"      -> Types.CompletionItemKind.Function
    | "Enum"          -> Types.CompletionItemKind.Enum
    | "EnumMember"    -> Types.CompletionItemKind.EnumMember
    | "Interface"     -> Types.CompletionItemKind.Interface
    | "Struct"        -> Types.CompletionItemKind.Struct
    | "Local"         -> Types.CompletionItemKind.Variable
    | "Parameter"     -> Types.CompletionItemKind.Variable
    | "RangeVariable" -> Types.CompletionItemKind.Variable
    | "Const"         -> Types.CompletionItemKind.Constant
    | "Event"         -> Types.CompletionItemKind.Event
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
    if loc.IsInSource then
        let getPathUri path = Uri("file://" + path)

        { Uri = loc.SourceTree.FilePath |> getPathUri |> string
          Range = loc.GetLineSpan().Span |> lspRangeForRoslynLinePosSpan }
    else
        { Uri = "";
          Range = { Start = { Line=0; Character=0; }; End = { Line=0; Character=0; } } }

let lspContentChangeEventToRoslynTextChange (sourceText: SourceText) (change: Types.TextDocumentContentChangeEvent) =
    let changeTextSpan =
        change.Range.Value
        |> roslynLinePositionSpanForLspRange
        |> sourceText.Lines.GetTextSpan

    TextChange(changeTextSpan, change.Text)

let applyLspContentChangesOnRoslynSourceText (changes: Types.TextDocumentContentChangeEvent[]) (sourceText: SourceText) =
    changes
    |> Seq.map (lspContentChangeEventToRoslynTextChange sourceText)
    |> sourceText.WithChanges

let lspDocChangesFromSolutionDiff
        originalSolution
        (updatedSolution: Solution)
        (tryGetDocVersionByUri: string -> int option)
        logMessage
        (originatingDoc: Document)
        : Async<Types.TextDocumentEdit list> = async {

    let! ct = Async.CancellationToken

    let getPathUri path = Uri("file://" + path)

    // make a list of changes
    let solutionProjectChanges = updatedSolution.GetChanges(originalSolution).GetProjectChanges()

    let docTextEdits = List<Types.TextDocumentEdit>()

    let addedDocs = solutionProjectChanges |> Seq.collect (fun pc -> pc.GetAddedDocuments())

    for docId in addedDocs do
        let newDoc = updatedSolution.GetDocument(docId)
        let! newDocText = newDoc.GetTextAsync(ct) |> Async.AwaitTask

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
        let! originalDocText = originalDoc.GetTextAsync(ct) |> Async.AwaitTask
        let updatedDoc = updatedSolution.GetDocument(docId)
        let! docChanges = updatedDoc.GetTextChangesAsync(originalDoc, ct) |> Async.AwaitTask

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

let lspCodeActionDetailsFromRoslynCA ca =
    let typeName = ca.GetType() |> string
    if typeName.Contains("CodeAnalysis.AddImport") then
        Some Types.CodeActionKind.QuickFix, Some true
    else
        None, None

let roslynCodeActionToUnresolvedLspCodeAction (ca: CodeActions.CodeAction): Types.CodeAction =
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

let roslynCodeActionToResolvedLspCodeAction
        originalSolution
        tryGetDocVersionByUri
        logMessage
        (originatingDoc: Document)
        (ca: CodeActions.CodeAction)
    : Async<Types.CodeAction option> = async {

    let! ct = Async.CancellationToken

    let! maybeOps = asyncMaybeOnException (fun () -> ca.GetOperationsAsync(ct) |> Async.AwaitTask)

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

let formatSymbol (sym: ISymbol)
                 showAttributes
                 (semanticModelMaybe: SemanticModel option)
                 (posMaybe: int option) =
    match showAttributes, semanticModelMaybe, posMaybe with
    | true, Some semanticModel, Some pos -> sym.ToMinimalDisplayString(semanticModel, pos)
    | true, _, _ -> sym.ToDisplayString()
    | false, _, _ -> sym.Name

let symbolToLspSymbolInformation
        showAttributes
        (symbol: ISymbol)
        (semanticModel: SemanticModel option)
        (pos: int option)
        : Types.SymbolInformation =
    let (symbolName, symbolKind) =
        match symbol with
        | :? ILocalSymbol as ls ->
            (formatSymbol ls showAttributes semanticModel pos,
             Types.SymbolKind.Variable)

        | :? IFieldSymbol as fs ->
            (formatSymbol fs showAttributes semanticModel pos,
             Types.SymbolKind.Field)

        | :? IPropertySymbol as ps ->
            (formatSymbol ps showAttributes semanticModel pos,
             Types.SymbolKind.Property)

        | :? IMethodSymbol as ms ->
            (formatSymbol ms showAttributes semanticModel pos,
             match ms.MethodKind with
                | MethodKind.Constructor -> Types.SymbolKind.Constructor
                | _ -> Types.SymbolKind.Method)

        | _ ->
            (symbol.ToString(), Types.SymbolKind.File)

    { Name = symbolName
      Kind = symbolKind
      Location = symbol.Locations |> Seq.head |> lspLocationForRoslynLocation
      ContainerName = None }


type DocumentSymbolCollector (documentUri, semanticModel: SemanticModel, showAttributes) =
    inherit CSharpSyntaxWalker(SyntaxWalkerDepth.Token)

    let mutable collectedSymbols: Types.SymbolInformation list = []

    let collect (symbol: ISymbol) (identifier: SyntaxToken) =
        let identifierLocation: Types.Location =
            { Uri = documentUri
              Range = identifier.GetLocation().GetLineSpan().Span |> lspRangeForRoslynLinePosSpan
            }

        let rawSymbol = symbolToLspSymbolInformation showAttributes symbol None None
        let symbol = { rawSymbol with Location = identifierLocation }

        collectedSymbols <- symbol :: collectedSymbols

    member __.GetSymbols() = collectedSymbols |> List.rev |> Array.ofList

    override __.VisitClassDeclaration(node) =
        let symbol = semanticModel.GetDeclaredSymbol(node)
        collect symbol node.Identifier
        base.VisitClassDeclaration(node)

    override __.VisitConstructorDeclaration(node) =
        let symbol = semanticModel.GetDeclaredSymbol(node)
        collect symbol node.Identifier
        base.VisitConstructorDeclaration(node)

    override __.VisitMethodDeclaration(node) =
        let symbol = semanticModel.GetDeclaredSymbol(node)
        collect symbol node.Identifier
        base.VisitMethodDeclaration(node)

    override __.VisitPropertyDeclaration(node) =
        let symbol = semanticModel.GetDeclaredSymbol(node)
        collect symbol node.Identifier
        base.VisitPropertyDeclaration(node)

    override __.VisitEventDeclaration(node) =
        let symbol = semanticModel.GetDeclaredSymbol(node)
        collect symbol node.Identifier
        base.VisitEventDeclaration(node)


type DocumentSymbolCollectorForCodeLens (semanticModel: SemanticModel) =
    inherit CSharpSyntaxWalker(SyntaxWalkerDepth.Token)

    let mutable collectedSymbols: (ISymbol * Location) list = []

    let collect (symbol: ISymbol) (identifier: SyntaxToken) =
        collectedSymbols <- (symbol, identifier.GetLocation()) :: collectedSymbols

    member __.GetSymbols() = collectedSymbols |> List.rev |> Array.ofList

    override __.VisitClassDeclaration(node) =
        let symbol = semanticModel.GetDeclaredSymbol(node)
        collect symbol node.Identifier
        base.VisitClassDeclaration(node)

    override __.VisitConstructorDeclaration(node) =
        let symbol = semanticModel.GetDeclaredSymbol(node)
        collect symbol node.Identifier
        base.VisitConstructorDeclaration(node)

    override __.VisitMethodDeclaration(node) =
        let symbol = semanticModel.GetDeclaredSymbol(node)
        collect symbol node.Identifier
        base.VisitMethodDeclaration(node)

    override __.VisitPropertyDeclaration(node) =
        let symbol = semanticModel.GetDeclaredSymbol(node)
        collect symbol node.Identifier
        base.VisitPropertyDeclaration(node)

    override __.VisitEventDeclaration(node) =
        let symbol = semanticModel.GetDeclaredSymbol(node)
        collect symbol node.Identifier
        base.VisitEventDeclaration(node)


type DocumentSymbolCollectorForMatchingSymbolName
        (documentUri, sym: ISymbol, logMessage: string -> unit) =
    inherit CSharpSyntaxWalker(SyntaxWalkerDepth.Token)

    let mutable collectedLocations: Types.Location list = []
    let mutable suggestedLocations: Types.Location list = []

    let collectIdentifier (identifier: SyntaxToken) exactMatch =
        let location: Types.Location =
            { Uri = documentUri
              Range = identifier.GetLocation().GetLineSpan().Span
                      |> lspRangeForRoslynLinePosSpan }

        if exactMatch then
            collectedLocations <- location :: collectedLocations
        else
            suggestedLocations <- location :: suggestedLocations

    member __.GetLocations() =
        if not (Seq.isEmpty collectedLocations) then
            collectedLocations |> Seq.rev |> List.ofSeq
        else
            suggestedLocations |> Seq.rev |> List.ofSeq

    override __.Visit(node) =
        if sym.Kind = SymbolKind.Method then
            if node :? MethodDeclarationSyntax then
                let nodeMethodDecl = node :?> MethodDeclarationSyntax

                if nodeMethodDecl.Identifier.ValueText = sym.Name then
                    let methodArityMatches =
                        let symMethod = sym :?> IMethodSymbol
                        symMethod.Parameters.Length = nodeMethodDecl.ParameterList.Parameters.Count

                    collectIdentifier nodeMethodDecl.Identifier methodArityMatches
        else
            if node :? TypeDeclarationSyntax then
                let typeDecl = node :?> TypeDeclarationSyntax
                if typeDecl.Identifier.ValueText = sym.Name then
                    collectIdentifier typeDecl.Identifier false

            else if node :? PropertyDeclarationSyntax then
                let propertyDecl = node :?> PropertyDeclarationSyntax
                if propertyDecl.Identifier.ValueText = sym.Name then
                    collectIdentifier propertyDecl.Identifier false

            else if node :? EventDeclarationSyntax then
                let eventDecl = node :?> EventDeclarationSyntax
                if eventDecl.Identifier.ValueText = sym.Name then
                    collectIdentifier eventDecl.Identifier false

            // TODO: collect other type of syntax nodes too

        base.Visit(node)

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

let findSymbolsInSolution (solution: Solution)
                          pattern
                          (_limit: int option)
        : Async<Types.SymbolInformation list> = async {
    let mutable symbolsFound = []

    for project in solution.Projects do
        let! symbols = SymbolFinder.FindSourceDeclarationsWithPatternAsync(
                           project, pattern, SymbolFilter.TypeAndMember)
                       |> Async.AwaitTask

        symbolsFound <- (List.ofSeq symbols) @ symbolsFound

    return Seq.map (fun s -> symbolToLspSymbolInformation true s None None) symbolsFound
           |> List.ofSeq
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
    let! ct = Async.CancellationToken

    let roslynCodeActions = List<CodeActions.CodeAction>()
    let addCodeAction = Action<CodeActions.CodeAction>(roslynCodeActions.Add)
    let codeActionContext = CodeRefactoringContext(doc, textSpan, addCodeAction, ct)

    for refactoringProvider in refactoringProviderInstances do
        do! refactoringProvider.ComputeRefactoringsAsync(codeActionContext) |> Async.AwaitTask

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

let processChange (oldText: SourceText) (change: TextChange) : TextEdit =
    let mapToTextEdit(linePosition: LinePositionSpan, newText: string) : TextEdit =
           { NewText = newText
             Range = {
                 Start = { Line = linePosition.Start.Line
                           Character = linePosition.Start.Character }
                 End = { Line = linePosition.End.Line
                         Character = linePosition.End.Character } } }

    let defaultTextEdit(oldText: SourceText, change: TextChange) : TextEdit =
        let linePosition = oldText.Lines.GetLinePositionSpan change.Span
        mapToTextEdit(linePosition, change.NewText)

    let padLeft(span: TextSpan) : TextSpan =
        TextSpan.FromBounds(span.Start - 1, span.End)
    let padRight(span: TextSpan): TextSpan =
        TextSpan.FromBounds(span.Start, span.End + 1)

    let rec checkSpanLineEndings(newText: string, oldText: SourceText, span: TextSpan, prefix: string) : TextEdit =
        if span.Start > 0 && newText[0].Equals('\n') && oldText[span.Start - 1].Equals('\r') then
           checkSpanLineEndings(newText, oldText, padLeft(span), "\r") |> ignore
        if span.End < oldText.Length - 1 && newText[newText.Length - 1].Equals('\r') && oldText[span.End].Equals('\n') then
           let linePosition = oldText.Lines.GetLinePositionSpan(padRight(span))
           mapToTextEdit(linePosition, (prefix + newText.ToString() + "\n"))
        else
            let linePosition = oldText.Lines.GetLinePositionSpan span
            mapToTextEdit(linePosition, newText.ToString())

    let newText = change.NewText

    if newText.Length > 0 then
        checkSpanLineEndings(newText, oldText, change.Span, String.Empty)
    else
        defaultTextEdit(oldText, change)

let convert (oldText: SourceText) (changes: TextChange[]) : TextEdit[] =
    //why doesnt it pick up that TextSpan implements IComparable<T>?
    //one of life's many mysteries
    let comparer (lhs: TextChange) (rhs: TextChange) : int =
        lhs.Span.CompareTo(rhs.Span)
    changes
    |> Seq.sortWith comparer
    |> Seq.map(fun x -> processChange oldText x)
    |> Seq.toArray

let getChanges (doc: Document) (oldDoc: Document) : Async<TextEdit[]> =
    async {
        let! changes = doc.GetTextChangesAsync oldDoc |> Async.AwaitTask
        let! oldText = oldDoc.GetTextAsync() |> Async.AwaitTask
        return convert oldText (changes |> Seq.toArray)    
    }

let handleTextDocumentFormatAsync (doc: Document) : Async<TextEdit[]> =
    async {
        let options = doc.Project.Solution.Options
        let! newDoc = Formatter.FormatAsync(doc, options) |> Async.AwaitTask
        return! getChanges newDoc doc
    }

let rec getSyntaxNode (token: SyntaxToken) : SyntaxNode option =
    if token.IsKind(SyntaxKind.EndOfFileToken) then
        getSyntaxNode(token.GetPreviousToken())
    else
        match token.Kind() with
        | SyntaxKind.SemicolonToken -> token.Parent |> Some
        | SyntaxKind.CloseBraceToken ->
            let parent = token.Parent
            match parent.Kind() with
            | SyntaxKind.Block -> parent.Parent |> Some
            | _ -> parent |> Some
        | SyntaxKind.CloseParenToken ->
            if token.GetPreviousToken().IsKind(SyntaxKind.SemicolonToken) && token.Parent.IsKind(SyntaxKind.ForStatement) then
                token.Parent |> Some
            else
                None
        | _ -> None

let findFormatTarget (root: SyntaxNode) (position: int) : SyntaxNode option =
    let token = root.FindToken position
    getSyntaxNode token

let handleTextDocumentRangeFormatAsync (doc: Document) (range: Range) : Async<TextEdit[]> =
    async {
        let options = doc.Project.Solution.Options
        let! text = doc.GetTextAsync() |> Async.AwaitTask
        let startPos = text.Lines.GetPosition(new LinePosition(range.Start.Line, range.Start.Character))
        let endPos = text.Lines.GetPosition(new LinePosition(range.End.Line, range.End.Character))
        let! syntaxTree = doc.GetSyntaxRootAsync() |> Async.AwaitTask
        let tokenStart = syntaxTree.FindToken(startPos).FullSpan.Start
        let! newDoc = Formatter.FormatAsync(doc, TextSpan.FromBounds(tokenStart, endPos), options) |> Async.AwaitTask
        return! getChanges newDoc doc
    }

let handleTextOnTypeFormatAsync (doc: Document) (ch: char) (position: Position) : Async<TextEdit[]> =
    async {
        let options = doc.Project.Solution.Options
        let! text = doc.GetTextAsync() |> Async.AwaitTask
        let pos = text.Lines.GetPosition(new LinePosition(position.Line, position.Character))
        match ch with
        | ';' | '}' | ')' ->
            let! root = doc.GetSyntaxRootAsync() |> Async.AwaitTask
            let maybeNode = findFormatTarget root pos
            match maybeNode with
            | Some node ->
                let! newDoc = Formatter.FormatAsync(doc, TextSpan.FromBounds(node.FullSpan.Start, node.FullSpan.End), options) |> Async.AwaitTask
                return! getChanges newDoc doc
            | None -> return Array.empty<TextEdit>
        | _ -> return Array.empty<TextEdit>
    }
