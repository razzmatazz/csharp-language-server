namespace CSharpLanguageServer.Handlers

open System
open System.Collections.Generic

open Ionide.LanguageServerProtocol.Types
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.Classification

open CSharpLanguageServer.State
open CSharpLanguageServer.Util
open CSharpLanguageServer.Conversions

[<RequireQualifiedAccess>]
module SemanticTokens =
    let toSemanticToken (lines: TextLineCollection) (textSpan: TextSpan, spans: IEnumerable<ClassifiedSpan>) =
        let (typeId, modifiers) =
            spans
            |> Seq.fold (fun (t, m) s ->
                if ClassificationTypeNames.AdditiveTypeNames.Contains(s.ClassificationType) then
                    (t, m ||| (GetSemanticTokenModifierFlagFromClassification s.ClassificationType))
                else
                    (GetSemanticTokenIdFromClassification s.ClassificationType, m)
            ) (None, 0u)
        let pos = lines.GetLinePositionSpan(textSpan)
        (uint32 pos.Start.Line, uint32 pos.Start.Character, uint32 (pos.End.Character - pos.Start.Character), typeId, modifiers)

    let computePosition (((pLine, pChar, _, _, _), (cLine, cChar, cLen, cToken, cModifiers)): ((uint32 * uint32 * uint32 * uint32 * uint32) * (uint32 * uint32 * uint32 * uint32 * uint32))) =
        let deltaLine = cLine - pLine
        let deltaChar =
            if deltaLine = 0u then
                cChar - pChar
            else
                cChar
        (deltaLine, deltaChar, cLen, cToken, cModifiers)

    let getSemanticTokensRange (scope: ServerRequestScope) (uri: string) (range: Range option): AsyncLspResult<SemanticTokens option> = async {
        let docMaybe = scope.GetUserDocumentForUri uri
        match docMaybe with
        | None -> return None |> LspResult.success
        | Some doc ->
            let! sourceText = doc.GetTextAsync() |> Async.AwaitTask
            let textSpan =
                match range with
                | Some r ->
                    r
                    |> Range.toLinePositionSpan sourceText.Lines
                    |> sourceText.Lines.GetTextSpan
                | None ->
                    TextSpan(0, sourceText.Length)
            let! spans = Classifier.GetClassifiedSpansAsync(doc, textSpan) |> Async.AwaitTask
            let tokens =
                spans
                |> Seq.groupBy (fun span -> span.TextSpan)
                |> Seq.map (toSemanticToken sourceText.Lines)
                |> Seq.filter (fun (_, _, _, oi, _) -> Option.isSome oi)
                |> Seq.map (fun (line, startChar, len, tokenId, modifiers) -> (line, startChar, len, Option.get tokenId, modifiers))

            let response =
                { Data =
                    Seq.zip (seq {yield (0u,0u,0u,0u,0u); yield! tokens}) tokens
                    |> Seq.map computePosition
                    |> Seq.map (fun (a,b,c,d,e) -> [a;b;c;d;e])
                    |> Seq.concat
                    |> Seq.toArray
                  ResultId = None } // TODO: add a result id after we support delta semantic tokens
            return Some response |> LspResult.success
    }

    let provider (clientCapabilities: ClientCapabilities option) : SemanticTokensOptions option =
        Some { Legend = { TokenTypes = SemanticTokenTypes |> Seq.toArray
                          TokenModifiers = SemanticTokenModifiers |> Seq.toArray }
               Range = Some true
               Full = true |> First |> Some
             }

    let handleFull (scope: ServerRequestScope) (semParams: SemanticTokensParams): AsyncLspResult<SemanticTokens option> =
        getSemanticTokensRange scope semParams.TextDocument.Uri None

    let handleRange (scope: ServerRequestScope) (semParams: SemanticTokensRangeParams): AsyncLspResult<SemanticTokens option> =
        getSemanticTokensRange scope semParams.TextDocument.Uri (Some semParams.Range)
