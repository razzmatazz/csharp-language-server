module CSharpLanguageServer.RoslynHelpers

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Threading.Tasks
open System.Collections.Immutable

open Castle.DynamicProxy
open ICSharpCode.Decompiler
open ICSharpCode.Decompiler.CSharp
open ICSharpCode.Decompiler.CSharp.Transforms
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.Host
open Microsoft.CodeAnalysis.Host.Mef
open Microsoft.CodeAnalysis.MSBuild
open Microsoft.CodeAnalysis.Text

open CSharpLanguageServer.Util
open CSharpLanguageServer.Conversions

let applyLspContentChangesOnRoslynSourceText
        (changes: Types.TextDocumentContentChangeEvent[])
        (initialSourceText: SourceText) =

    let applyLspContentChangeOnRoslynSourceText (sourceText: SourceText) (change: Types.TextDocumentContentChangeEvent) =
        match change.Range with
        | Some changeRange ->
            let changeTextSpan =
                changeRange |> Range.toLinePositionSpan sourceText.Lines
                            |> sourceText.Lines.GetTextSpan

            TextChange(changeTextSpan, change.Text) |> sourceText.WithChanges

        | None -> SourceText.From(change.Text)

    changes |> Seq.fold applyLspContentChangeOnRoslynSourceText initialSourceText

let formatSymbol (sym: ISymbol)
                 showAttributes
                 (semanticModelMaybe: SemanticModel option)
                 (posMaybe: int option) =
    match showAttributes, semanticModelMaybe, posMaybe with
    | true, Some semanticModel, Some pos -> sym.ToMinimalDisplayString(semanticModel, pos)
    | true, _, _ -> sym.ToDisplayString()
    | false, _, _ -> sym.Name

let getSymbolNameAndKind
        (semanticModel: SemanticModel option)
        (pos: int option)
        (symbol: ISymbol) =
    let showAttributes = true

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
            | MethodKind.StaticConstructor -> Types.SymbolKind.Constructor
            | MethodKind.BuiltinOperator -> Types.SymbolKind.Operator
            | MethodKind.UserDefinedOperator -> Types.SymbolKind.Operator
            | MethodKind.Conversion -> Types.SymbolKind.Operator
            | _ -> Types.SymbolKind.Method)

    | :? ITypeSymbol as ts ->
        (formatSymbol ts showAttributes semanticModel pos,
            match ts.TypeKind with
            | TypeKind.Class -> Types.SymbolKind.Class
            | TypeKind.Enum -> Types.SymbolKind.Enum
            | TypeKind.Struct -> Types.SymbolKind.Struct
            | TypeKind.Interface -> Types.SymbolKind.Interface
            | TypeKind.Delegate -> Types.SymbolKind.Class
            | TypeKind.Array -> Types.SymbolKind.Array
            | TypeKind.TypeParameter -> Types.SymbolKind.TypeParameter
            | _ -> Types.SymbolKind.Class)

    | :? IEventSymbol as es ->
        (formatSymbol es showAttributes semanticModel pos,
            Types.SymbolKind.Event)

    | :? INamespaceSymbol as ns ->

        (formatSymbol ns showAttributes semanticModel pos,
            Types.SymbolKind.Namespace)

    | _ ->
        (symbol.ToString(), Types.SymbolKind.File)

type DocumentSymbolCollectorForMatchingSymbolName
        (documentUri, sym: ISymbol) =
    inherit CSharpSyntaxWalker(SyntaxWalkerDepth.Token)

    let mutable collectedLocations = []
    let mutable suggestedLocations = []

    let collectIdentifier (identifier: SyntaxToken) exactMatch =
        let location: Types.Location =
            { Uri = documentUri
              Range = identifier.GetLocation().GetLineSpan().Span
                      |> Range.fromLinePositionSpan }

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

type CleanCodeGenerationOptionsProviderInterceptor (_logMessage) =
    interface IInterceptor with
        member __.Intercept(invocation: IInvocation) =
            match invocation.Method.Name with
            "GetCleanCodeGenerationOptionsAsync" ->
                let workspacesAssembly = Assembly.Load("Microsoft.CodeAnalysis.Workspaces")
                let cleanCodeGenOptionsType = workspacesAssembly.GetType("Microsoft.CodeAnalysis.CodeGeneration.CleanCodeGenerationOptions")

                let methodGetDefault = cleanCodeGenOptionsType.GetMethod("GetDefault")

                let argLanguageServices = invocation.Arguments[0]
                let defaultCleanCodeGenOptions = methodGetDefault.Invoke(null, [| argLanguageServices |])

                let valueTaskType = typedefof<ValueTask<_>>
                let valueTaskTypeForCleanCodeGenOptions = valueTaskType.MakeGenericType([| cleanCodeGenOptionsType |])

                invocation.ReturnValue <-
                    Activator.CreateInstance(valueTaskTypeForCleanCodeGenOptions, defaultCleanCodeGenOptions)

            | _ ->
                NotImplementedException(string invocation.Method) |> raise

type LegacyWorkspaceOptionServiceInterceptor (logMessage) =
    interface IInterceptor with
        member __.Intercept(invocation: IInvocation) =
            //logMessage (sprintf "LegacyWorkspaceOptionServiceInterceptor: %s" (string invocation.Method))

            match invocation.Method.Name with
            | "RegisterWorkspace" ->
                ()
            | "GetGenerateEqualsAndGetHashCodeFromMembersGenerateOperators" ->
                invocation.ReturnValue <- box true
            | "GetGenerateEqualsAndGetHashCodeFromMembersImplementIEquatable" ->
                invocation.ReturnValue <- box true
            | "GetGenerateConstructorFromMembersOptionsAddNullChecks" ->
                invocation.ReturnValue <- box true
            | "get_GenerateOverrides" ->
                invocation.ReturnValue <- box true
            | "get_CleanCodeGenerationOptionsProvider" ->
                let workspacesAssembly = Assembly.Load("Microsoft.CodeAnalysis.Workspaces")
                let cleanCodeGenOptionsProvType = workspacesAssembly.GetType("Microsoft.CodeAnalysis.CodeGeneration.AbstractCleanCodeGenerationOptionsProvider")

                let generator = ProxyGenerator()
                let interceptor = CleanCodeGenerationOptionsProviderInterceptor(logMessage)
                let proxy = generator.CreateClassProxy(cleanCodeGenOptionsProvType, interceptor)
                invocation.ReturnValue <- proxy

            | _ ->
                NotImplementedException(string invocation.Method) |> raise

type PickMembersServiceInterceptor (_logMessage) =
    interface IInterceptor with
         member __.Intercept(invocation: IInvocation) =

            match invocation.Method.Name with
            | "PickMembers" ->
                let argMembers = invocation.Arguments[1]
                let argOptions = invocation.Arguments[2]

                let pickMembersResultType = invocation.Method.ReturnType

                invocation.ReturnValue <-
                    Activator.CreateInstance(pickMembersResultType, argMembers, argOptions, box true)

            | _ ->
                NotImplementedException(string invocation.Method) |> raise

type ExtractClassOptionsServiceInterceptor (_logMessage) =
    interface IInterceptor with
        member __.Intercept(invocation: IInvocation) =

            match invocation.Method.Name with
            | "GetExtractClassOptionsAsync" ->
                let _argDocument = invocation.Arguments[0] :?> Document
                let argOriginalType = invocation.Arguments[1] :?> INamedTypeSymbol
                let _argSelectedMembers = invocation.Arguments[2] :?> ImmutableArray<ISymbol>

                let featuresAssembly = Assembly.Load("Microsoft.CodeAnalysis.Features")
                let extractClassOptionsType = featuresAssembly.GetType("Microsoft.CodeAnalysis.ExtractClass.ExtractClassOptions")

                let typeName = "Base" + argOriginalType.Name
                let fileName = typeName + ".cs"
                let sameFile = box true

                let immArrayType = typeof<ImmutableArray>
                let extractClassMemberAnalysisResultType = featuresAssembly.GetType("Microsoft.CodeAnalysis.ExtractClass.ExtractClassMemberAnalysisResult")

                let resultListType = typedefof<List<_>>.MakeGenericType(extractClassMemberAnalysisResultType)
                let resultList = Activator.CreateInstance(resultListType)

                let memberFilter (m: ISymbol) =
                    match m with
                    | :? IMethodSymbol as ms -> ms.MethodKind = MethodKind.Ordinary
                    | :? IFieldSymbol as fs -> not fs.IsImplicitlyDeclared
                    | _ -> m.Kind = SymbolKind.Property || m.Kind = SymbolKind.Event

                let selectedMembersToAdd =
                    argOriginalType.GetMembers()
                    |> Seq.filter memberFilter

                for memberToAdd in selectedMembersToAdd do
                    let memberAnalysisResult =
                        Activator.CreateInstance(extractClassMemberAnalysisResultType, memberToAdd, false)

                    resultListType.GetMethod("Add").Invoke(resultList, [| memberAnalysisResult |])
                    |> ignore

                let resultListAsArray =
                    resultListType.GetMethod("ToArray").Invoke(resultList, null)

                let immArrayCreateFromArray =
                    immArrayType.GetMethods()
                    |> Seq.filter (fun m -> m.GetParameters().Length = 1 && (m.GetParameters()[0]).ParameterType.IsArray)
                    |> Seq.head

                let emptyMemberAnalysisResults =
                    immArrayCreateFromArray.MakeGenericMethod([| extractClassMemberAnalysisResultType |]).Invoke(null, [| resultListAsArray |])

                let extractClassOptionsValue =
                    Activator.CreateInstance(
                        extractClassOptionsType, fileName, typeName, sameFile, emptyMemberAnalysisResults)

                let fromResultMethod = typeof<Task>.GetMethod("FromResult")
                let typedFromResultMethod = fromResultMethod.MakeGenericMethod([| extractClassOptionsType |])

                invocation.ReturnValue <-
                    typedFromResultMethod.Invoke(null, [| extractClassOptionsValue |])

            | _ ->
                NotImplementedException(string invocation.Method) |> raise

type MoveStaticMembersOptionsServiceInterceptor (_logMessage) =
    interface IInterceptor with
       member __.Intercept(invocation: IInvocation) =

            match invocation.Method.Name with
            | "GetMoveMembersToTypeOptions" ->
                let _argDocument = invocation.Arguments[0] :?> Document
                let _argOriginalType = invocation.Arguments[1] :?> INamedTypeSymbol
                let argSelectedMembers = invocation.Arguments[2] :?> ImmutableArray<ISymbol>

                let featuresAssembly = Assembly.Load("Microsoft.CodeAnalysis.Features")
                let msmOptionsType = featuresAssembly.GetType("Microsoft.CodeAnalysis.MoveStaticMembers.MoveStaticMembersOptions")

                let newStaticClassName = "NewStaticClass"

                let msmOptions =
                    Activator.CreateInstance(
                        msmOptionsType,
                        newStaticClassName + ".cs",
                        newStaticClassName,
                        argSelectedMembers,
                        false |> box)

                invocation.ReturnValue <- msmOptions

            | _ ->
                NotImplementedException(string invocation.Method) |> raise

type WorkspaceServicesInterceptor (logMessage) =
    interface IInterceptor with
        member __.Intercept(invocation: IInvocation) =
            invocation.Proceed()

            if invocation.Method.Name = "GetService" && invocation.ReturnValue = null then
                let updatedReturnValue =
                    let serviceType = invocation.GenericArguments[0]
                    let generator = ProxyGenerator()

                    match serviceType.FullName with
                    | "Microsoft.CodeAnalysis.Options.ILegacyGlobalOptionsWorkspaceService" ->
                        let interceptor = LegacyWorkspaceOptionServiceInterceptor(logMessage)
                        generator.CreateInterfaceProxyWithoutTarget(serviceType, interceptor)

                    | "Microsoft.CodeAnalysis.PickMembers.IPickMembersService" ->
                        let interceptor = PickMembersServiceInterceptor(logMessage)
                        generator.CreateInterfaceProxyWithoutTarget(serviceType, interceptor)

                    | "Microsoft.CodeAnalysis.ExtractClass.IExtractClassOptionsService" ->
                        let interceptor = ExtractClassOptionsServiceInterceptor(logMessage)
                        generator.CreateInterfaceProxyWithoutTarget(serviceType, interceptor)

                    | "Microsoft.CodeAnalysis.MoveStaticMembers.IMoveStaticMembersOptionsService" ->
                        let interceptor = MoveStaticMembersOptionsServiceInterceptor(logMessage)
                        generator.CreateInterfaceProxyWithoutTarget(serviceType, interceptor)

                    | _ ->
                        //logMessage (sprintf "WorkspaceServicesInterceptor: GetService(%s) resulted in null!" serviceType.FullName)
                        null

                invocation.ReturnValue <- updatedReturnValue

type CSharpLspHostServices (logMessage) =
    inherit HostServices()

    member private this.hostServices = MSBuildMefHostServices.DefaultServices

    override this.CreateWorkspaceServices (workspace: Workspace) =
        // Ugly but we can't:
        // 1. use Castle since there is no default constructor of MefHostServices.
        // 2. call this.hostServices.CreateWorkspaceServices directly since it's internal.
        let methodInfo = this.hostServices.GetType().GetMethod("CreateWorkspaceServices", BindingFlags.Instance|||BindingFlags.NonPublic)
        let services =
            methodInfo.Invoke(this.hostServices, [| workspace |])
            |> Unchecked.unbox<HostWorkspaceServices>
        let generator = ProxyGenerator()
        let interceptor = WorkspaceServicesInterceptor(logMessage)
        generator.CreateClassProxyWithTarget(services, interceptor)

let tryLoadSolutionOnPath (logMessage: AsyncLogFn) solutionPath = async {
    try
        do! logMessage (sprintf "loading solution \"%s\".." solutionPath)

        let msbuildWorkspace = MSBuildWorkspace.Create(CSharpLspHostServices(logMessage))
        msbuildWorkspace.LoadMetadataForReferencedProjects <- true

        let! _ = msbuildWorkspace.OpenSolutionAsync(solutionPath) |> Async.AwaitTask

        for diag in msbuildWorkspace.Diagnostics do
            do! logMessage ("msbuildWorkspace.Diagnostics: " + diag.ToString())

        do! logMessage (sprintf "finished loading solution \"%s\"" solutionPath)

        return Some msbuildWorkspace.CurrentSolution
    with
    | ex ->
        do! logMessage ("solution loading has failed with error: " + ex.ToString())
        return None
}

let tryLoadSolutionFromProjectFiles (logMessage: AsyncLogFn) (projFiles: string list) = async {
    let msbuildWorkspace = MSBuildWorkspace.Create(CSharpLspHostServices(logMessage))
    msbuildWorkspace.LoadMetadataForReferencedProjects <- true

    for file in projFiles do
        do! logMessage (sprintf "loading project \"%s\".." file)
        try
            do! msbuildWorkspace.OpenProjectAsync(file) |> Async.AwaitTask |> Async.Ignore
        with ex ->
            do! logMessage (sprintf "could not OpenProjectAsync('%s'): %s" file (ex |> string))
        ()

    do! logMessage (sprintf "OK, %d project files loaded" projFiles.Length)

    for diag in msbuildWorkspace.Diagnostics do
        do! logMessage ("msbuildWorkspace.Diagnostics: " + diag.ToString())

    //workspace <- Some(msbuildWorkspace :> Workspace)
    return Some msbuildWorkspace.CurrentSolution
}

let findAndLoadSolutionOnDir (logMessage: AsyncLogFn) dir = async {
    let fileNotOnNodeModules (filename: string) =
        filename.Split(Path.DirectorySeparatorChar)
        |> Seq.contains "node_modules"
        |> not

    let solutionFiles =
        Directory.GetFiles(dir, "*.sln", SearchOption.AllDirectories)
        |> Seq.filter fileNotOnNodeModules
        |> Seq.toList

    do! logMessage (sprintf "%d solution(s) found: [%s]" solutionFiles.Length (String.Join(", ", solutionFiles)) )

    let singleSolutionFound =
        match solutionFiles with
        | [x] -> Some x
        | _ -> None

    match singleSolutionFound with
    | None ->
        do! logMessage ("no or multiple .sln files found on " + dir)
        do! logMessage ("looking for .csproj/fsproj files on " + dir + "..")

        let projFiles =
            let csprojFiles = Directory.GetFiles(dir, "*.csproj", SearchOption.AllDirectories)
            let fsprojFiles = Directory.GetFiles(dir, "*.fsproj", SearchOption.AllDirectories)

            [ csprojFiles; fsprojFiles ] |> Seq.concat
                                            |> Seq.filter fileNotOnNodeModules
                                            |> Seq.toList

        if projFiles.Length = 0 then
            let message = "no or .csproj/.fsproj or sln files found on " + dir
            do! logMessage message
            Exception message |> raise

        let! solution = tryLoadSolutionFromProjectFiles logMessage projFiles
        return solution

    | Some solutionPath ->
        let! solution = tryLoadSolutionOnPath logMessage solutionPath
        return solution
}

let loadSolutionOnSolutionPathOrDir (logMessage: AsyncLogFn) solutionPathMaybe rootPath =
    match solutionPathMaybe with
    | Some solutionPath -> async {
        return! tryLoadSolutionOnPath logMessage solutionPath
      }

    | None -> async {
        do! logMessage (sprintf "attempting to find and load solution based on root path (\"%s\").." rootPath)
        return! findAndLoadSolutionOnDir logMessage rootPath
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

let tryAddDocument (logMessage: AsyncLogFn)
                   (docFilePath: string)
                   (text: string)
                   (solution: Solution)
                   : Async<Document option> =
  async {
    let docDir = Path.GetDirectoryName(docFilePath)
    //logMessage (sprintf "TextDocumentDidOpen: docFilename=%s docDir=%s" docFilename docDir)

    let fileOnProjectDir (p: Project) =
        let projectDir = Path.GetDirectoryName(p.FilePath)
        let projectDirWithDirSepChar = projectDir + (string Path.DirectorySeparatorChar)

        (docDir = projectDir) || docDir.StartsWith(projectDirWithDirSepChar)

    let projectOnPath =
        solution.Projects
        |> Seq.filter fileOnProjectDir
        |> Seq.tryHead

    let! newDocumentMaybe =
        match projectOnPath with
        | Some proj ->
            let projectBaseDir = Path.GetDirectoryName(proj.FilePath)
            let docName = docFilePath.Substring(projectBaseDir.Length+1)

            //logMessage (sprintf "Adding \"%s\" (\"%s\") to project %s" docName docFilePath proj.FilePath)

            let newDoc = proj.AddDocument(name=docName, text=SourceText.From(text), folders=null, filePath=docFilePath)
            Some newDoc |> async.Return

        | None -> async {
            do! logMessage (sprintf "No parent project could be resolved to add file \"%s\" to workspace" docFilePath)
            return None
          }

    return newDocumentMaybe
  }

let makeDocumentFromMetadata
        (compilation: Microsoft.CodeAnalysis.Compilation)
        (project: Microsoft.CodeAnalysis.Project)
        (l: Microsoft.CodeAnalysis.Location)
        (fullName: string) =
    let mdLocation = l
    let reference = compilation.GetMetadataReference(mdLocation.MetadataModule.ContainingAssembly)
    let peReference = reference :?> PortableExecutableReference |> Option.ofObj
    let assemblyLocation = peReference |> Option.map (fun r -> r.FilePath) |> Option.defaultValue "???"

    let decompilerSettings = DecompilerSettings()
    decompilerSettings.ThrowOnAssemblyResolveErrors <- false // this shouldn't be a showstopper for us

    let decompiler = CSharpDecompiler(assemblyLocation, decompilerSettings)

    // Escape invalid identifiers to prevent Roslyn from failing to parse the generated code.
    // (This happens for example, when there is compiler-generated code that is not yet recognized/transformed by the decompiler.)
    decompiler.AstTransforms.Add(EscapeInvalidIdentifiers())

    let fullTypeName = ICSharpCode.Decompiler.TypeSystem.FullTypeName(fullName)

    let text = decompiler.DecompileTypeAsString(fullTypeName)

    let mdDocumentFilename = $"$metadata$/projects/{project.Name}/assemblies/{mdLocation.MetadataModule.ContainingAssembly.Name}/symbols/{fullName}.cs"
    let mdDocumentEmpty = project.AddDocument(mdDocumentFilename, String.Empty)

    let mdDocument = SourceText.From(text) |> mdDocumentEmpty.WithText
    (mdDocument, text)

