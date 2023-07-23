namespace CSharpLanguageServer.Common

open System
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text
open Ionide.LanguageServerProtocol.Types
open FSharpPlus

open CSharpLanguageServer.Common.Types


module Uri =
    // Unescape some necessary char before passing string to Uri.
    // Can't use Uri.UnescapeDataString here. For example, if uri is "file:///z%3a/src/c%23/ProjDir" ("%3a" is
    // ":" and "%23" is "#"), Uri.UnescapeDataString will unescape both "%3a" and "%23". Then Uri will think
    /// "#/ProjDir" is Fragment instead of part of LocalPath.
    let unescape (uri: string) = uri.Replace("%3a", ":", true, null)

    let toPath (uri: string) = Uri.UnescapeDataString(Uri(unescape(uri)).LocalPath)

    let fromPath (path: string) = Uri(path).ToString()

    let toWorkspaceFolder(uri: string): WorkspaceFolder =
        { Uri = uri
          Name = Uri.UnescapeDataString(Uri(unescape(uri)).Segments |> Array.last) }


module Path =
    let toUri = Uri.fromPath

    let fromUri = Uri.toPath

    let toWorkspaceFolder = toUri >> Uri.toWorkspaceFolder


module Position =
    let fromLinePosition (pos: LinePosition): Position =
        { Line = pos.Line ; Character = pos.Character }

    let toLinePosition (lines: TextLineCollection) (pos: Position): LinePosition =
        if pos.Line < 0 then
            LinePosition(0, 0)
        else if pos.Line >= lines.Count then
            LinePosition(lines.Count - 1, lines[lines.Count - 1].EndIncludingLineBreak - lines[lines.Count - 1].Start)
        else
            LinePosition(pos.Line, pos.Character)

    let toRoslynPosition (lines: TextLineCollection) = toLinePosition lines >> lines.GetPosition


module Range =
    let toLinePositionSpan (lines: TextLineCollection) (range: Range): LinePositionSpan =
        LinePositionSpan(
            Position.toLinePosition lines range.Start,
            Position.toLinePosition lines range.End)

    let fromLinePositionSpan (pos: LinePositionSpan): Range =
        { Start = Position.fromLinePosition pos.Start
          End = Position.fromLinePosition pos.End }

    let toTextSpan (lines: TextLineCollection) = toLinePositionSpan lines >> lines.GetTextSpan

    let fromTextSpan (lines: TextLineCollection) = lines.GetLinePositionSpan >> fromLinePositionSpan


module Location =
    let fromRoslynLocation (loc: Microsoft.CodeAnalysis.Location): Location =
        if loc.IsInSource then
            { Uri = loc.SourceTree.FilePath |> Path.toUri
              Range = loc.GetLineSpan().Span |> Range.fromLinePositionSpan }
        else
            { Uri = ""
              Range = { Start = { Line = 0; Character = 0 }; End = { Line = 0; Character = 0 } } }


module TextEdit =
    let fromTextChange (lines: TextLineCollection) (changes: TextChange): TextEdit =
        { Range = changes.Span |> Range.fromTextSpan lines
          NewText = changes.NewText }


module SymbolKind =
    let fromSymbol (symbol: ISymbol): SymbolKind =
        match symbol with
        | :? ILocalSymbol -> SymbolKind.Variable
        | :? IFieldSymbol as fs ->
            if not(isNull fs.ContainingType) && fs.ContainingType.TypeKind = TypeKind.Enum && fs.HasConstantValue then
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
    let fromSymbol (format: SymbolDisplayFormat) (symbol: ISymbol): string = symbol.ToDisplayString(format)


module HierarchyItem =
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

    let fromSymbolAndLocation (symbol: ISymbol) (location: Location): HierarchyItem =
        let kind = SymbolKind.fromSymbol symbol
        let containingType = (symbol.ContainingType :> ISymbol) |> Option.ofObj
        let containingNamespace = (symbol.ContainingNamespace :> ISymbol) |> Option.ofObj

        { Name = symbol.ToDisplayString(displayStyle)
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

    let fromSymbol (wm: IWorkspaceManager) (symbol: ISymbol): Async<HierarchyItem list> =
        wm.ResolveSymbolLocations symbol
        |> map (List.map (fromSymbolAndLocation symbol))


module SymbolInformation =
    let fromSymbol (format: SymbolDisplayFormat) (symbol: ISymbol): SymbolInformation list =
        let name = SymbolName.fromSymbol format symbol
        let kind = SymbolKind.fromSymbol symbol
        symbol.Locations
        |> Seq.map (fun loc ->
            { Name = name
              Kind = kind
              Location = Location.fromRoslynLocation loc
              ContainerName = None
              Deprecated = None
              Tags = None })
        |> Seq.toList


module DiagnosticSeverity =
    let fromRoslynDiagnosticSeverity (sev: Microsoft.CodeAnalysis.DiagnosticSeverity): DiagnosticSeverity =
        match sev with
        | Microsoft.CodeAnalysis.DiagnosticSeverity.Info -> DiagnosticSeverity.Information
        | Microsoft.CodeAnalysis.DiagnosticSeverity.Warning -> DiagnosticSeverity.Warning
        | Microsoft.CodeAnalysis.DiagnosticSeverity.Error -> DiagnosticSeverity.Error
        | _ -> DiagnosticSeverity.Warning


module Diagnostic =
    let fromRoslynDiagnostic (diagnostic: Microsoft.CodeAnalysis.Diagnostic): Diagnostic =
        let diagnosticCodeUrl =
            diagnostic.Id.ToLowerInvariant()
            |> sprintf "https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/compiler-messages/%s"
            |> Uri
        { Range = diagnostic.Location.GetLineSpan().Span |> Range.fromLinePositionSpan
          Severity = Some (diagnostic.Severity |> DiagnosticSeverity.fromRoslynDiagnosticSeverity)
          Code = Some diagnostic.Id
          CodeDescription = Some { Href = Some diagnosticCodeUrl }
          Source = Some "lsp"
          Message = diagnostic.GetMessage()
          RelatedInformation = None
          // TODO: Convert diagnostic.Descriptor.CustomTags to Tags
          Tags = None
          Data = None }
