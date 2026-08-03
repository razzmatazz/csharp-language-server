module CSharpLanguageServer.Roslyn.Document

open System.IO
open System.Text

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Options
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.Formatting
open Ionide.LanguageServerProtocol.Types

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
}

let normalizeDocumentEndOfFile (doc: Document) (options: OptionSet) (lspFormattingOptions: FormattingOptions option) = async {
    let insertFinalNewline =
        (lspFormattingOptions |> Option.bind _.InsertFinalNewline) = Some true

    let trimFinalNewlines =
        (lspFormattingOptions |> Option.bind _.TrimFinalNewlines) = Some true

    if not insertFinalNewline && not trimFinalNewlines then
        return doc
    else
        let! ct = Async.CancellationToken
        let! text = doc.GetTextAsync(ct) |> Async.AwaitTask
        let mutable finalNewlinesStart = text.Length
        let isNewline char = char = '\r' || char = '\n'

        while finalNewlinesStart > 0 && isNewline text[finalNewlinesStart - 1] do
            finalNewlinesStart <- finalNewlinesStart - 1

        let normalizedText =
            if trimFinalNewlines && finalNewlinesStart < text.Length then
                let newlineLength =
                    if
                        text[finalNewlinesStart] = '\r'
                        && finalNewlinesStart < text.Length - 1
                        && text[finalNewlinesStart + 1] = '\n'
                    then
                        2
                    else
                        1

                let newline = text.ToString(TextSpan(finalNewlinesStart, newlineLength))
                let finalNewlines = TextSpan.FromBounds(finalNewlinesStart, text.Length)
                text.WithChanges(TextChange(finalNewlines, newline))
            elif insertFinalNewline && finalNewlinesStart = text.Length then
                let newline =
                    text.Lines
                    |> Seq.rev
                    |> Seq.tryPick (fun line ->
                        let span = TextSpan.FromBounds(line.End, line.EndIncludingLineBreak)
                        if span.IsEmpty then None else Some(text.ToString(span)))
                    |> Option.defaultWith (fun () -> options.GetOption(FormattingOptions.NewLine, LanguageNames.CSharp))

                text.WithChanges(TextChange(TextSpan(text.Length, 0), newline))
            else
                text

        return doc.WithText(normalizedText)
}

let sourceTextFromFile (filename: string) : SourceText =
    filename |> File.ReadAllBytes |> Encoding.UTF8.GetString |> SourceText.From
