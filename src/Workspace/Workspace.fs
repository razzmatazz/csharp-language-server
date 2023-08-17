namespace CSharpLanguageServer.Workspace

open Castle.DynamicProxy
open System
open System.Collections.Immutable
open System.Reflection
open System.Threading.Tasks
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Host
open Microsoft.CodeAnalysis.Host.Mef
open Microsoft.CodeAnalysis.MSBuild

open CSharpLanguageServer.Logging

[<RequireQualifiedAccess>]
module Workspace =
    let private logger = LogProvider.getLoggerByName "Workspace"

    let private generator = ProxyGenerator()

    type private CleanCodeGenerationOptionsProviderInterceptor() =
        interface IInterceptor with
            member __.Intercept(invocation: IInvocation) =
                match invocation.Method.Name with
                | "GetCleanCodeGenerationOptionsAsync" ->
                    let workspacesAssembly = Assembly.Load("Microsoft.CodeAnalysis.Workspaces")
                    let cleanCodeGenOptionsType = workspacesAssembly.GetType("Microsoft.CodeAnalysis.CodeGeneration.CleanCodeGenerationOptions")

                    let methodGetDefault = cleanCodeGenOptionsType.GetMethod("GetDefault")

                    let argLanguageServices = invocation.Arguments[0]
                    let defaultCleanCodeGenOptions = methodGetDefault.Invoke(null, [| argLanguageServices |])

                    let valueTaskType = typedefof<ValueTask<_>>
                    let valueTaskTypeForCleanCodeGenOptions = valueTaskType.MakeGenericType([| cleanCodeGenOptionsType |])

                    invocation.ReturnValue <-
                        Activator.CreateInstance(valueTaskTypeForCleanCodeGenOptions, defaultCleanCodeGenOptions)

                | _ -> NotImplementedException(string invocation.Method) |> raise

    type private LegacyWorkspaceOptionServiceInterceptor() =
        interface IInterceptor with
            member __.Intercept(invocation: IInvocation) =
                match invocation.Method.Name with
                | "RegisterWorkspace" -> ()
                | "GetGenerateEqualsAndGetHashCodeFromMembersGenerateOperators" -> invocation.ReturnValue <- box true
                | "GetGenerateEqualsAndGetHashCodeFromMembersImplementIEquatable" -> invocation.ReturnValue <- box true
                | "GetGenerateConstructorFromMembersOptionsAddNullChecks" -> invocation.ReturnValue <- box true
                | "get_GenerateOverrides" -> invocation.ReturnValue <- box true
                | "get_CleanCodeGenerationOptionsProvider" ->
                    let workspacesAssembly = Assembly.Load("Microsoft.CodeAnalysis.Workspaces")
                    let cleanCodeGenOptionsProvType = workspacesAssembly.GetType("Microsoft.CodeAnalysis.CodeGeneration.AbstractCleanCodeGenerationOptionsProvider")
                    let interceptor = CleanCodeGenerationOptionsProviderInterceptor()
                    let proxy = generator.CreateClassProxy(cleanCodeGenOptionsProvType, interceptor)
                    invocation.ReturnValue <- proxy

                | _ -> NotImplementedException(string invocation.Method) |> raise

    type private PickMembersServiceInterceptor() =
        interface IInterceptor with
            member __.Intercept(invocation: IInvocation) =
                match invocation.Method.Name with
                | "PickMembers" ->
                    let argMembers = invocation.Arguments[1]
                    let argOptions = invocation.Arguments[2]

                    let pickMembersResultType = invocation.Method.ReturnType

                    invocation.ReturnValue <-
                        Activator.CreateInstance(pickMembersResultType, argMembers, argOptions, box true)

                | _ -> NotImplementedException(string invocation.Method) |> raise

    type private ExtractClassOptionsServiceInterceptor() =
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

                    let selectedMembersToAdd = argOriginalType.GetMembers() |> Seq.filter memberFilter

                    for memberToAdd in selectedMembersToAdd do
                        let memberAnalysisResult =
                            Activator.CreateInstance(extractClassMemberAnalysisResultType, memberToAdd, false)

                        resultListType.GetMethod("Add").Invoke(resultList, [| memberAnalysisResult |])
                        |> ignore

                    let resultListAsArray = resultListType.GetMethod("ToArray").Invoke(resultList, null)

                    let immArrayCreateFromArray =
                        immArrayType.GetMethods()
                        |> Seq.filter (fun m -> m.GetParameters().Length = 1 && (m.GetParameters()[0]).ParameterType.IsArray)
                        |> Seq.head

                    let emptyMemberAnalysisResults =
                        immArrayCreateFromArray.MakeGenericMethod([| extractClassMemberAnalysisResultType |]).Invoke(null, [| resultListAsArray |])

                    let extractClassOptionsValue =
                        Activator.CreateInstance(
                            extractClassOptionsType,
                            fileName,
                            typeName,
                            sameFile,
                            emptyMemberAnalysisResults
                        )

                    let fromResultMethod = typeof<Task>.GetMethod("FromResult")
                    let typedFromResultMethod = fromResultMethod.MakeGenericMethod([| extractClassOptionsType |])

                    invocation.ReturnValue <- typedFromResultMethod.Invoke(null, [| extractClassOptionsValue |])

                | _ -> NotImplementedException(string invocation.Method) |> raise

    type private MoveStaticMembersOptionsServiceInterceptor() =
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
                            false |> box
                        )

                    invocation.ReturnValue <- msmOptions

                | _ -> NotImplementedException(string invocation.Method) |> raise

    type private WorkspaceServicesInterceptor() =
        interface IInterceptor with
            member __.Intercept(invocation: IInvocation) =
                invocation.Proceed()

                if invocation.Method.Name = "GetService" && invocation.ReturnValue = null then
                    let updatedReturnValue =
                        let serviceType = invocation.GenericArguments[0]
                        match serviceType.FullName with
                        | "Microsoft.CodeAnalysis.Options.ILegacyGlobalOptionsWorkspaceService" ->
                            let interceptor = LegacyWorkspaceOptionServiceInterceptor()
                            generator.CreateInterfaceProxyWithoutTarget(serviceType, interceptor)

                        | "Microsoft.CodeAnalysis.PickMembers.IPickMembersService" ->
                            let interceptor = PickMembersServiceInterceptor()
                            generator.CreateInterfaceProxyWithoutTarget(serviceType, interceptor)

                        | "Microsoft.CodeAnalysis.ExtractClass.IExtractClassOptionsService" ->
                            let interceptor = ExtractClassOptionsServiceInterceptor()
                            generator.CreateInterfaceProxyWithoutTarget(serviceType, interceptor)

                        | "Microsoft.CodeAnalysis.MoveStaticMembers.IMoveStaticMembersOptionsService" ->
                            let interceptor = MoveStaticMembersOptionsServiceInterceptor()
                            generator.CreateInterfaceProxyWithoutTarget(serviceType, interceptor)

                        | _ -> null

                    invocation.ReturnValue <- updatedReturnValue

    type private CSharpLspHostServices() =
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
            let interceptor = WorkspaceServicesInterceptor()
            generator.CreateClassProxyWithTarget(services, interceptor)

    let create () : MSBuildWorkspace =
        MSBuildWorkspace.Create(CSharpLspHostServices())
