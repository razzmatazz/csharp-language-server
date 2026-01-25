namespace CSharpLanguageServer.Roslyn.WorkspaceServices

open System
open System.Collections.Generic
open System.Reflection
open System.Threading.Tasks
open System.Collections.Immutable

open Castle.DynamicProxy
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Host
open Microsoft.CodeAnalysis.Host.Mef
open Microsoft.Extensions.Logging

open CSharpLanguageServer.Logging
open CSharpLanguageServer.Util


type CleanCodeGenerationOptionsProviderInterceptor(_logMessage) =
    interface IInterceptor with
        member __.Intercept(invocation: IInvocation) =
            match invocation.Method.Name with
            | "GetCleanCodeGenerationOptionsAsync" ->
                let workspacesAssembly = Assembly.Load "Microsoft.CodeAnalysis.Workspaces"

                let cleanCodeGenOptionsType =
                    workspacesAssembly.GetType "Microsoft.CodeAnalysis.CodeGeneration.CleanCodeGenerationOptions"
                    |> nonNull
                        "workspacesAssembly.GetType('Microsoft.CodeAnalysis.CodeGeneration.CleanCodeGenerationOptions')"

                let getDefaultMI =
                    cleanCodeGenOptionsType.GetMethod "GetDefault"
                    |> nonNull "cleanCodeGenOptionsType.GetMethod('GetDefault')"

                let argLanguageServices = invocation.Arguments[0]

                let defaultCleanCodeGenOptions =
                    getDefaultMI.Invoke(null, [| argLanguageServices |])

                let valueTaskType = typedefof<ValueTask<_>>

                let valueTaskTypeForCleanCodeGenOptions =
                    valueTaskType.MakeGenericType [| cleanCodeGenOptionsType |]

                invocation.ReturnValue <-
                    Activator.CreateInstance(valueTaskTypeForCleanCodeGenOptions, defaultCleanCodeGenOptions)

            | _ -> NotImplementedException(string invocation.Method) |> raise


type CleanCodeGenOptionsProxy(logMessage) =
    static let workspacesAssembly = Assembly.Load "Microsoft.CodeAnalysis.Workspaces"
    static let generator = ProxyGenerator()

    static let cleanCodeGenOptionsProvTypeMaybe =
        workspacesAssembly.GetType "Microsoft.CodeAnalysis.CodeGeneration.AbstractCleanCodeGenerationOptionsProvider"
        |> Option.ofObj


    member __.Create() =
        let interceptor = CleanCodeGenerationOptionsProviderInterceptor logMessage

        let proxyMaybe =
            cleanCodeGenOptionsProvTypeMaybe
            |> Option.map (fun cleanCodeGenOptionsProvType ->
                generator.CreateClassProxy(cleanCodeGenOptionsProvType, interceptor))

        match proxyMaybe with
        | Some proxy -> proxy
        | None -> failwith "Could not create CleanCodeGenerationOptionsProvider proxy"


type LegacyWorkspaceOptionServiceInterceptor(logMessage) =
    interface IInterceptor with
        member __.Intercept(invocation: IInvocation) =
            //logMessage (sprintf "LegacyWorkspaceOptionServiceInterceptor: %s" (string invocation.Method))

            match invocation.Method.Name with
            | "RegisterWorkspace" -> ()
            | "GetGenerateEqualsAndGetHashCodeFromMembersGenerateOperators" -> invocation.ReturnValue <- box true
            | "GetGenerateEqualsAndGetHashCodeFromMembersImplementIEquatable" -> invocation.ReturnValue <- box true
            | "GetGenerateConstructorFromMembersOptionsAddNullChecks" -> invocation.ReturnValue <- box true
            | "get_GenerateOverrides" -> invocation.ReturnValue <- box true
            | "get_CleanCodeGenerationOptionsProvider" ->
                invocation.ReturnValue <- CleanCodeGenOptionsProxy(logMessage).Create()
            | _ -> NotImplementedException(string invocation.Method) |> raise


type PickMembersServiceInterceptor(_logMessage) =
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


type ExtractClassOptionsServiceInterceptor(_logMessage) =

    let getExtractClassOptionsImpl (argOriginalType: INamedTypeSymbol) : Object =
        let featuresAssembly = Assembly.Load "Microsoft.CodeAnalysis.Features"

        let typeName = "Base" + argOriginalType.Name
        let fileName = typeName + ".cs"
        let sameFile = box true

        let immArrayType = typeof<ImmutableArray>

        let extractClassMemberAnalysisResultType =
            featuresAssembly.GetType "Microsoft.CodeAnalysis.ExtractClass.ExtractClassMemberAnalysisResult"
            |> nonNull
                "featuresAssembly.GetType('Microsoft.CodeAnalysis.ExtractClass.ExtractClassMemberAnalysisResult')"

        let resultListType =
            typedefof<List<_>>.MakeGenericType extractClassMemberAnalysisResultType

        let resultList = Activator.CreateInstance resultListType

        let memberFilter (m: ISymbol) =
            match m with
            | :? IMethodSymbol as ms -> ms.MethodKind = MethodKind.Ordinary
            | :? IFieldSymbol as fs -> not fs.IsImplicitlyDeclared
            | _ -> m.Kind = SymbolKind.Property || m.Kind = SymbolKind.Event

        let selectedMembersToAdd = argOriginalType.GetMembers() |> Seq.filter memberFilter

        for memberToAdd in selectedMembersToAdd do
            let memberAnalysisResult =
                Activator.CreateInstance(extractClassMemberAnalysisResultType, memberToAdd, false)

            let resultListAddMI =
                resultListType.GetMethod "Add" |> nonNull "resultListType.GetMethod('Add')"

            resultListAddMI.Invoke(resultList, [| memberAnalysisResult |]) |> ignore

        let resultListToArrayMI =
            resultListType.GetMethod "ToArray"
            |> nonNull "resultListType.GetMethod('ToArray')"

        let resultListAsArray = resultListToArrayMI.Invoke(resultList, null)

        let immArrayCreateFromArrayMI =
            immArrayType.GetMethods()
            |> Seq.filter (fun m -> m.GetParameters().Length = 1 && (m.GetParameters()[0]).ParameterType.IsArray)
            |> Seq.head

        let emptyMemberAnalysisResults =
            immArrayCreateFromArrayMI
                .MakeGenericMethod([| extractClassMemberAnalysisResultType |])
                .Invoke(null, [| resultListAsArray |])
            |> nonNull "MakeGenericMethod()"

        let extractClassOptionsType =
            featuresAssembly.GetType "Microsoft.CodeAnalysis.ExtractClass.ExtractClassOptions"
            |> nonNull "featuresAssembly.GetType('Microsoft.CodeAnalysis.ExtractClass.ExtractClassOptions')"

        Activator.CreateInstance(extractClassOptionsType, fileName, typeName, sameFile, emptyMemberAnalysisResults)
        |> nonNull (sprintf "could not Activator.CreateInstance(%s,..)" (string extractClassOptionsType))

    interface IInterceptor with
        member __.Intercept(invocation: IInvocation) =

            match invocation.Method.Name with
            | "GetExtractClassOptionsAsync" ->
                let argOriginalType = invocation.Arguments[1] :?> INamedTypeSymbol
                let extractClassOptionsValue = getExtractClassOptionsImpl argOriginalType
                invocation.ReturnValue <- Task.fromResult (extractClassOptionsValue.GetType(), extractClassOptionsValue)

            | "GetExtractClassOptions" ->
                let argOriginalType = invocation.Arguments[1] :?> INamedTypeSymbol
                invocation.ReturnValue <- getExtractClassOptionsImpl argOriginalType

            | _ -> NotImplementedException(string invocation.Method) |> raise


type ExtractInterfaceOptionsServiceInterceptor(logMessage) =
    interface IInterceptor with

        member __.Intercept(invocation: IInvocation) =
            let argExtractableMembers, argDefaultInterfaceName =
                match
                    invocation.Method.Name, invocation.Arguments[1], invocation.Arguments[2], invocation.Arguments[3]
                with
                | "GetExtractInterfaceOptions",
                  (:? ImmutableArray<ISymbol> as extractableMembers),
                  (:? string as interfaceName),
                  _ -> extractableMembers, interfaceName
                | "GetExtractInterfaceOptions",
                  _,
                  (:? ImmutableArray<ISymbol> as extractableMembers),
                  (:? string as interfaceName) -> extractableMembers, interfaceName
                | "GetExtractInterfaceOptionsAsync",
                  _,
                  (:? List<ISymbol> as extractableMembers),
                  (:? string as interfaceName) -> extractableMembers.ToImmutableArray(), interfaceName
                | _ -> NotImplementedException(string invocation.Method.Name) |> raise

            let fileName = sprintf "%s.cs" argDefaultInterfaceName

            let featuresAssembly = Assembly.Load "Microsoft.CodeAnalysis.Features"

            let extractInterfaceOptionsResultType =
                featuresAssembly.GetType "Microsoft.CodeAnalysis.ExtractInterface.ExtractInterfaceOptionsResult"
                |> nonNull
                    "featuresAssembly.GetType('Microsoft.CodeAnalysis.ExtractInterface.ExtractInterfaceOptionsResult')"

            let locationEnumType =
                extractInterfaceOptionsResultType.GetNestedType "ExtractLocation"
                |> nonNull "extractInterfaceOptionsResultType.GetNestedType('ExtractLocation')"

            let location =
                Enum.Parse(locationEnumType, "NewFile")
                |> fun v -> Convert.ChangeType(v, locationEnumType)

            invocation.ReturnValue <-
                match invocation.Method.Name with
                | "GetExtractInterfaceOptionsAsync" ->
                    Task.fromResult (
                        extractInterfaceOptionsResultType,
                        Activator.CreateInstance(
                            extractInterfaceOptionsResultType,
                            false, // isCancelled
                            argExtractableMembers,
                            argDefaultInterfaceName,
                            fileName,
                            location,
                            CleanCodeGenOptionsProxy(logMessage).Create()
                        )
                    )

                | _ ->
                    Activator.CreateInstance(
                        extractInterfaceOptionsResultType,
                        false, // isCancelled
                        argExtractableMembers,
                        argDefaultInterfaceName,
                        fileName,
                        location
                    )


type MoveStaticMembersOptionsServiceInterceptor(_logMessage) =
    interface IInterceptor with
        member __.Intercept(invocation: IInvocation) =

            match invocation.Method.Name with
            | "GetMoveMembersToTypeOptions" ->
                let _argDocument = invocation.Arguments[0] :?> Document
                let _argOriginalType = invocation.Arguments[1] :?> INamedTypeSymbol
                let argSelectedMembers = invocation.Arguments[2] :?> ImmutableArray<ISymbol>

                let featuresAssembly = Assembly.Load "Microsoft.CodeAnalysis.Features"

                let msmOptionsType =
                    featuresAssembly.GetType "Microsoft.CodeAnalysis.MoveStaticMembers.MoveStaticMembersOptions"
                    |> nonNull "typeof<Microsoft.CodeAnalysis.MoveStaticMembers.MoveStaticMembersOptions>"

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


type RemoteHostClientProviderInterceptor(_logMessage) =
    interface IInterceptor with
        member __.Intercept(invocation: IInvocation) =

            match invocation.Method.Name with
            | "TryGetRemoteHostClientAsync" ->
                let workspacesAssembly = Assembly.Load "Microsoft.CodeAnalysis.Workspaces"

                let remoteHostClientType =
                    workspacesAssembly.GetType "Microsoft.CodeAnalysis.Remote.RemoteHostClient"
                    |> nonNull "GetType(Microsoft.CodeAnalysis.Remote.RemoteHostClient)"

                invocation.ReturnValue <- Task.fromResult (remoteHostClientType, null)

            | _ -> NotImplementedException(string invocation.Method) |> raise


type WorkspaceServicesInterceptor() =
    let logger = Logging.getLoggerByName "WorkspaceServicesInterceptor"

    interface IInterceptor with
        member __.Intercept(invocation: IInvocation) =
            invocation.Proceed()

            if invocation.Method.Name = "GetService" && invocation.ReturnValue = null then
                let updatedReturnValue =
                    let serviceType = invocation.GenericArguments[0]
                    let generator = ProxyGenerator()

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

                    | "Microsoft.CodeAnalysis.ExtractInterface.IExtractInterfaceOptionsService" ->
                        let interceptor = ExtractInterfaceOptionsServiceInterceptor()
                        generator.CreateInterfaceProxyWithoutTarget(serviceType, interceptor)

                    | "Microsoft.CodeAnalysis.MoveStaticMembers.IMoveStaticMembersOptionsService" ->
                        let interceptor = MoveStaticMembersOptionsServiceInterceptor()
                        generator.CreateInterfaceProxyWithoutTarget(serviceType, interceptor)

                    | "Microsoft.CodeAnalysis.Remote.IRemoteHostClientProvider" ->
                        let interceptor = RemoteHostClientProviderInterceptor()
                        generator.CreateInterfaceProxyWithoutTarget(serviceType, interceptor)

                    | "Microsoft.CodeAnalysis.SourceGeneratorTelemetry.ISourceGeneratorTelemetryCollectorWorkspaceService"
                    | "Microsoft.CodeAnalysis.CodeRefactorings.PullMemberUp.Dialog.IPullMemberUpOptionsService"
                    | "Microsoft.CodeAnalysis.Packaging.IPackageInstallerService"
                    | "Microsoft.CodeAnalysis.SourceGeneratorTelemetry.ISourceGeneratorTelemetryReporterWorkspaceService" ->
                        // supress "GetService failed" messages for these services
                        null

                    | _ ->
                        logger.LogDebug("GetService failed for {serviceType}", serviceType.FullName)
                        null

                invocation.ReturnValue <- updatedReturnValue


type CSharpLspHostServices() =
    inherit HostServices()

    member private _.hostServices = MSBuildMefHostServices.DefaultServices

    override this.CreateWorkspaceServices(workspace: Workspace) =
        // Ugly but we can't:
        // 1. use Castle since there is no default constructor of MefHostServices.
        // 2. call this.hostServices.CreateWorkspaceServices directly since it's internal.
        let createWorkspaceServicesMI =
            this.hostServices
                .GetType()
                .GetMethod("CreateWorkspaceServices", BindingFlags.Instance ||| BindingFlags.NonPublic)
            |> nonNull (sprintf "no %s.CreateWorkspaceServices()" (this.hostServices.GetType() |> string))

        let services =
            createWorkspaceServicesMI.Invoke(this.hostServices, [| workspace |])
            |> Unchecked.unbox<HostWorkspaceServices>

        let generator = ProxyGenerator()
        let interceptor = WorkspaceServicesInterceptor()
        generator.CreateClassProxyWithTarget(services, interceptor)
