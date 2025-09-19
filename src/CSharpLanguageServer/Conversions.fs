namespace CSharpLanguageServer.Conversions

open System
open System.IO

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Completion
open Microsoft.CodeAnalysis.Text
open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Util

module Uri =
    // Unescape some necessary char before passing string to Uri.
    // Can't use Uri.UnescapeDataString here. For example, if uri is "file:///z%3a/src/c%23/ProjDir" ("%3a" is
    // ":" and "%23" is "#"), Uri.UnescapeDataString will unescape both "%3a" and "%23". Then Uri will think
    /// "#/ProjDir" is Fragment instead of part of LocalPath.
    let unescape (uri: string) = uri.Replace("%3a", ":", true, null)

    let toPath (uri: string) =
        Uri.UnescapeDataString(Uri(unescape uri).LocalPath)

    let fromPath (path: string) =
        let metadataPrefix = "$metadata$/"

        if path.StartsWith metadataPrefix then
            "csharp:/metadata/" + path.Substring metadataPrefix.Length
        else
            Uri(path).ToString()

    let toWorkspaceFolder (uri: string) : WorkspaceFolder =
        { Uri = uri
          Name = Uri.UnescapeDataString(Uri(unescape uri).Segments |> Array.last) }


module Path =
    let toUri = Uri.fromPath

    let fromUri = Uri.toPath

    let toWorkspaceFolder = toUri >> Uri.toWorkspaceFolder


module Position =
    let fromLinePosition (pos: LinePosition) : Position =
        { Line = uint32 pos.Line
          Character = uint32 pos.Character }

    let toLinePosition (lines: TextLineCollection) (pos: Position) : LinePosition =
        if int pos.Line >= lines.Count then
            LinePosition(lines.Count - 1, lines[lines.Count - 1].EndIncludingLineBreak - lines[lines.Count - 1].Start)
        else
            LinePosition(int pos.Line, int pos.Character)

    let toRoslynPosition (lines: TextLineCollection) =
        toLinePosition lines >> lines.GetPosition


module Range =
    let toLinePositionSpan (lines: TextLineCollection) (range: Range) : LinePositionSpan =
        LinePositionSpan(Position.toLinePosition lines range.Start, Position.toLinePosition lines range.End)

    let fromLinePositionSpan (pos: LinePositionSpan) : Range =
        { Start = Position.fromLinePosition pos.Start
          End = Position.fromLinePosition pos.End }

    let toTextSpan (lines: TextLineCollection) =
        toLinePositionSpan lines >> lines.GetTextSpan

    let fromTextSpan (lines: TextLineCollection) =
        lines.GetLinePositionSpan >> fromLinePositionSpan


module Location =
    let fromRoslynLocation (loc: Microsoft.CodeAnalysis.Location) : option<Location> =
        let toLspLocation (path: string) span : Location =
            { Uri = path |> Path.toUri
              Range = span |> Range.fromLinePositionSpan }

        match loc.Kind with
        | LocationKind.SourceFile ->
            let mappedSourceLocation =
                loc.GetMappedLineSpan()
                |> Some
                |> Option.bind (fun mappedLoc ->
                    if mappedLoc.IsValid && File.Exists mappedLoc.Path then
                        Some mappedLoc
                    else
                        None)
                |> Option.map (fun mappedLoc -> toLspLocation mappedLoc.Path mappedLoc.Span)

            let sourceLocation =
                loc.SourceTree
                |> Option.ofObj
                |> Option.map _.FilePath
                |> Option.bind (fun filePath -> if File.Exists filePath then Some filePath else None)
                |> Option.map (fun filePath -> toLspLocation filePath (loc.GetLineSpan().Span))

            mappedSourceLocation |> Option.orElse sourceLocation

        | _ -> None


module TextEdit =
    let fromTextChange (lines: TextLineCollection) (changes: TextChange) : TextEdit =
        { Range = changes.Span |> Range.fromTextSpan lines
          NewText = changes.NewText }


module SymbolKind =
    let fromSymbol (symbol: ISymbol) : SymbolKind =
        match symbol with
        | :? ILocalSymbol -> SymbolKind.Variable
        | :? IFieldSymbol as fs ->
            if
                not (isNull fs.ContainingType)
                && fs.ContainingType.TypeKind = TypeKind.Enum
                && fs.HasConstantValue
            then
                SymbolKind.EnumMember
            else
                SymbolKind.Field
        | :? IPropertySymbol -> SymbolKind.Property
        | :? IMethodSymbol as ms ->
            match ms.MethodKind with
            | MethodKind.Constructor -> SymbolKind.Constructor
            | MethodKind.StaticConstructor -> SymbolKind.Constructor
            | MethodKind.BuiltinOperator -> SymbolKind.Operator
            | MethodKind.UserDefinedOperator -> SymbolKind.Operator
            | MethodKind.Conversion -> SymbolKind.Operator
            | _ -> SymbolKind.Method
        | :? ITypeSymbol as ts ->
            match ts.TypeKind with
            | TypeKind.Class -> SymbolKind.Class
            | TypeKind.Enum -> SymbolKind.Enum
            | TypeKind.Struct -> SymbolKind.Struct
            | TypeKind.Interface -> SymbolKind.Interface
            | TypeKind.Delegate -> SymbolKind.Class
            | TypeKind.Array -> SymbolKind.Array
            | TypeKind.TypeParameter -> SymbolKind.TypeParameter
            | _ -> SymbolKind.Class
        | :? IEventSymbol -> SymbolKind.Event
        | :? INamespaceSymbol -> SymbolKind.Namespace
        | _ -> SymbolKind.File


module SymbolName =
    let fromSymbol (format: SymbolDisplayFormat) (symbol: ISymbol) : string = symbol.ToDisplayString format


module CallHierarchyItem =
    let private displayStyle =
        SymbolDisplayFormat(
            typeQualificationStyle = SymbolDisplayTypeQualificationStyle.NameOnly,
            genericsOptions = SymbolDisplayGenericsOptions.IncludeTypeParameters,
            memberOptions =
                (SymbolDisplayMemberOptions.IncludeParameters
                 ||| SymbolDisplayMemberOptions.IncludeExplicitInterface),
            parameterOptions =
                (SymbolDisplayParameterOptions.IncludeParamsRefOut
                 ||| SymbolDisplayParameterOptions.IncludeExtensionThis
                 ||| SymbolDisplayParameterOptions.IncludeType),
            miscellaneousOptions = SymbolDisplayMiscellaneousOptions.UseSpecialTypes
        )

    let fromSymbolAndLocation (symbol: ISymbol) (location: Location) : CallHierarchyItem =
        let kind = SymbolKind.fromSymbol symbol
        let containingType = (symbol.ContainingType :> ISymbol) |> Option.ofObj
        let containingNamespace = (symbol.ContainingNamespace :> ISymbol) |> Option.ofObj

        { Name = symbol.ToDisplayString displayStyle
          Kind = kind
          Tags = None
          Detail =
            containingType
            |> Option.orElse containingNamespace
            |> Option.map (fun sym -> sym.ToDisplayString())
          Uri = location.Uri
          Range = location.Range
          SelectionRange = location.Range
          Data = None }

    let fromSymbol
        (wmResolveSymbolLocations: ISymbol -> Project option -> Async<list<Location>>)
        (symbol: ISymbol)
        : Async<CallHierarchyItem list> =
        wmResolveSymbolLocations symbol None
        |> Async.map (List.map (fromSymbolAndLocation symbol))

module TypeHierarchyItem =
    let private displayStyle =
        SymbolDisplayFormat(
            typeQualificationStyle = SymbolDisplayTypeQualificationStyle.NameOnly,
            genericsOptions = SymbolDisplayGenericsOptions.IncludeTypeParameters,
            memberOptions =
                (SymbolDisplayMemberOptions.IncludeParameters
                 ||| SymbolDisplayMemberOptions.IncludeExplicitInterface),
            parameterOptions =
                (SymbolDisplayParameterOptions.IncludeParamsRefOut
                 ||| SymbolDisplayParameterOptions.IncludeExtensionThis
                 ||| SymbolDisplayParameterOptions.IncludeType),
            miscellaneousOptions = SymbolDisplayMiscellaneousOptions.UseSpecialTypes
        )

    let fromSymbolAndLocation (symbol: ISymbol) (location: Location) : TypeHierarchyItem =
        let kind = SymbolKind.fromSymbol symbol
        let containingType = (symbol.ContainingType :> ISymbol) |> Option.ofObj
        let containingNamespace = (symbol.ContainingNamespace :> ISymbol) |> Option.ofObj

        { Name = symbol.ToDisplayString displayStyle
          Kind = kind
          Tags = None
          Detail =
            containingType
            |> Option.orElse containingNamespace
            |> Option.map (fun sym -> sym.ToDisplayString())
          Uri = location.Uri
          Range = location.Range
          SelectionRange = location.Range
          Data = None }

    let fromSymbol
        (wmResolveSymbolLocations: ISymbol -> Project option -> Async<list<Location>>)
        (symbol: ISymbol)
        : Async<TypeHierarchyItem list> =
        wmResolveSymbolLocations symbol None
        |> Async.map (List.map (fromSymbolAndLocation symbol))

module SymbolInformation =
    let fromSymbol (format: SymbolDisplayFormat) (symbol: ISymbol) : SymbolInformation list =
        let toSymbolInformation loc =
            { Name = SymbolName.fromSymbol format symbol
              Kind = SymbolKind.fromSymbol symbol
              Location = loc
              ContainerName = None
              Deprecated = None
              Tags = None }

        symbol.Locations
        |> Seq.map Location.fromRoslynLocation
        |> Seq.filter _.IsSome
        |> Seq.map _.Value
        |> Seq.map toSymbolInformation
        |> Seq.toList


module DiagnosticSeverity =
    let fromRoslynDiagnosticSeverity (sev: Microsoft.CodeAnalysis.DiagnosticSeverity) : DiagnosticSeverity =
        match sev with
        | Microsoft.CodeAnalysis.DiagnosticSeverity.Info -> DiagnosticSeverity.Information
        | Microsoft.CodeAnalysis.DiagnosticSeverity.Warning -> DiagnosticSeverity.Warning
        | Microsoft.CodeAnalysis.DiagnosticSeverity.Error -> DiagnosticSeverity.Error
        | _ -> DiagnosticSeverity.Warning


module Diagnostic =
    let fromRoslynDiagnostic (diagnostic: Microsoft.CodeAnalysis.Diagnostic) : Diagnostic =
        let diagnosticCodeUrl = diagnostic.Descriptor.HelpLinkUri |> Option.ofObj

        { Range = diagnostic.Location.GetLineSpan().Span |> Range.fromLinePositionSpan
          Severity = Some(diagnostic.Severity |> DiagnosticSeverity.fromRoslynDiagnosticSeverity)
          Code = Some(U2.C2 diagnostic.Id)
          CodeDescription = diagnosticCodeUrl |> Option.map (fun x -> { Href = x |> URI })
          Source = Some "lsp"
          Message = diagnostic.GetMessage()
          RelatedInformation = None
          // TODO: Convert diagnostic.Descriptor.CustomTags to Tags
          Tags = None
          Data = None }


module CompletionContext =
    let toCompletionTrigger (context: CompletionContext option) : CompletionTrigger =
        context
        |> Option.bind (fun ctx ->
            match ctx.TriggerKind with
            | CompletionTriggerKind.Invoked
            | CompletionTriggerKind.TriggerForIncompleteCompletions -> Some CompletionTrigger.Invoke
            | CompletionTriggerKind.TriggerCharacter ->
                ctx.TriggerCharacter
                |> Option.map Seq.head
                |> Option.map CompletionTrigger.CreateInsertionTrigger
            | _ -> None)
        |> Option.defaultValue CompletionTrigger.Invoke
