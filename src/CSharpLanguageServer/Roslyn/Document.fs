module CSharpLanguageServer.Roslyn.Document

open System

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp.Formatting
open Microsoft.CodeAnalysis.Options
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.Formatting
open Ionide.LanguageServerProtocol.Types
open ICSharpCode.Decompiler
open ICSharpCode.Decompiler.CSharp
open ICSharpCode.Decompiler.CSharp.Transforms

open CSharpLanguageServer.Types
open CSharpLanguageServer.Util


let private processChange (oldText: SourceText) (change: TextChange) : TextEdit =
    let mapToTextEdit (linePosition: LinePositionSpan, newText: string) : TextEdit =
        { NewText = newText
          Range =
            { Start =
                { Line = uint32 linePosition.Start.Line
                  Character = uint32 linePosition.Start.Character }
              End =
                { Line = uint32 linePosition.End.Line
                  Character = uint32 linePosition.End.Character } } }

    let defaultTextEdit (oldText: SourceText, change: TextChange) : TextEdit =
        let linePosition = oldText.Lines.GetLinePositionSpan change.Span
        mapToTextEdit (linePosition, change.NewText)

    let padLeft (span: TextSpan) : TextSpan =
        TextSpan.FromBounds(span.Start - 1, span.End)

    let padRight (span: TextSpan) : TextSpan =
        TextSpan.FromBounds(span.Start, span.End + 1)

    let rec checkSpanLineEndings (newText: string, oldText: SourceText, span: TextSpan, prefix: string) : TextEdit =
        if span.Start > 0 && newText[0].Equals '\n' && oldText[span.Start - 1].Equals '\r' then
            checkSpanLineEndings (newText, oldText, padLeft span, "\r") |> ignore

        if
            span.End < oldText.Length - 1
            && newText[newText.Length - 1].Equals '\r'
            && oldText[span.End].Equals '\n'
        then
            let linePosition = oldText.Lines.GetLinePositionSpan(padRight span)
            mapToTextEdit (linePosition, prefix + newText.ToString() + "\n")
        else
            let linePosition = oldText.Lines.GetLinePositionSpan span
            mapToTextEdit (linePosition, newText.ToString())

    let newText = change.NewText

    if newText.Length > 0 then
        checkSpanLineEndings (newText, oldText, change.Span, "")
    else
        defaultTextEdit (oldText, change)


let private convert (oldText: SourceText) (changes: TextChange[]) : TextEdit[] =
    //why doesnt it pick up that TextSpan implements IComparable<T>?
    //one of life's many mysteries
    let comparer (lhs: TextChange) (rhs: TextChange) : int = lhs.Span.CompareTo rhs.Span

    changes
    |> Seq.sortWith comparer
    |> Seq.map (fun x -> processChange oldText x)
    |> Seq.toArray


let getDocumentDiffAsLspTextEdits (doc: Document) (oldDoc: Document) : Async<TextEdit[]> = async {
    let! ct = Async.CancellationToken
    let! changes = doc.GetTextChangesAsync(oldDoc, ct) |> Async.AwaitTask
    let! oldText = oldDoc.GetTextAsync ct |> Async.AwaitTask
    return convert oldText (changes |> Seq.toArray)
}


let getDocumentFormattingOptionSet (doc: Document) (lspFormattingOptions: FormattingOptions option) : Async<OptionSet> = async {
    let! docOptions = doc.GetOptionsAsync() |> Async.AwaitTask

    return
        match lspFormattingOptions with
        | None -> docOptions
        | Some lspFormattingOptions ->
            docOptions
            |> _.WithChangedOption(
                FormattingOptions.IndentationSize,
                LanguageNames.CSharp,
                int lspFormattingOptions.TabSize
            )
            |> _.WithChangedOption(
                FormattingOptions.UseTabs,
                LanguageNames.CSharp,
                not lspFormattingOptions.InsertSpaces
            )
            |> match lspFormattingOptions.InsertFinalNewline with
               | Some insertFinalNewline ->
                   _.WithChangedOption(CSharpFormattingOptions.NewLineForFinally, insertFinalNewline)
               | None -> id
            |> match lspFormattingOptions.TrimFinalNewlines with
               | Some trimFinalNewlines ->
                   _.WithChangedOption(CSharpFormattingOptions.NewLineForFinally, not trimFinalNewlines)
               | None -> id
}


let documentFromMetadata
    (compilation: Microsoft.CodeAnalysis.Compilation)
    (project: Microsoft.CodeAnalysis.Project)
    (l: Microsoft.CodeAnalysis.Location)
    (fullName: string)
    =
    let mdLocation = l

    let containingAssembly =
        mdLocation.MetadataModule
        |> nonNull "mdLocation.MetadataModule"
        |> _.ContainingAssembly

    let reference =
        compilation.GetMetadataReference containingAssembly
        |> nonNull "compilation.GetMetadataReference(containingAssembly)"

    let peReference = reference :?> PortableExecutableReference |> Option.ofObj

    let assemblyLocation =
        peReference |> Option.map (fun r -> r.FilePath) |> Option.defaultValue "???"

    let decompilerSettings = DecompilerSettings()
    decompilerSettings.ThrowOnAssemblyResolveErrors <- false // this shouldn't be a showstopper for us

    let decompiler = CSharpDecompiler(assemblyLocation, decompilerSettings)

    // Escape invalid identifiers to prevent Roslyn from failing to parse the generated code.
    // (This happens for example, when there is compiler-generated code that is not yet recognized/transformed by the decompiler.)
    decompiler.AstTransforms.Add(EscapeInvalidIdentifiers())

    let fullTypeName = TypeSystem.FullTypeName fullName

    let text = decompiler.DecompileTypeAsString fullTypeName

    let mdDocumentFilename =
        $"$metadata$/projects/{project.Name}/assemblies/{containingAssembly.Name}/symbols/{fullName}.cs"

    let mdDocumentEmpty = project.AddDocument(mdDocumentFilename, String.Empty)

    let mdDocument = SourceText.From text |> mdDocumentEmpty.WithText
    mdDocument, text
