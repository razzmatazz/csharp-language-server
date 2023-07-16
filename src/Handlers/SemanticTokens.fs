namespace CSharpLanguageServer.Handlers

open System.Collections.Generic
open Microsoft.CodeAnalysis.Classification
open Microsoft.CodeAnalysis.Text
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult
open FSharpPlus

open CSharpLanguageServer.Common
open CSharpLanguageServer.Common.Types
open CSharpLanguageServer.Common.LspUtil

[<RequireQualifiedAccess>]
module SemanticTokens =
    let private classificationTypeMap = Map [
        (ClassificationTypeNames.ClassName,             "class");
        (ClassificationTypeNames.Comment,               "comment");
        (ClassificationTypeNames.ConstantName,          "property");
        (ClassificationTypeNames.ControlKeyword,        "keyword");
        (ClassificationTypeNames.DelegateName,          "class");
        (ClassificationTypeNames.EnumMemberName,        "enumMember");
        (ClassificationTypeNames.EnumName,              "enum");
        (ClassificationTypeNames.EventName,             "event");
        (ClassificationTypeNames.ExtensionMethodName,   "method");
        (ClassificationTypeNames.FieldName,             "property");
        (ClassificationTypeNames.Identifier,            "variable");
        (ClassificationTypeNames.InterfaceName,         "interface");
        (ClassificationTypeNames.LabelName,             "variable");
        (ClassificationTypeNames.LocalName,             "variable");
        (ClassificationTypeNames.Keyword,               "keyword");
        (ClassificationTypeNames.MethodName,            "method");
        (ClassificationTypeNames.NamespaceName,         "namespace");
        (ClassificationTypeNames.NumericLiteral,        "number");
        (ClassificationTypeNames.Operator,              "operator");
        (ClassificationTypeNames.OperatorOverloaded,    "operator");
        (ClassificationTypeNames.ParameterName,         "parameter");
        (ClassificationTypeNames.PropertyName,          "property");
        (ClassificationTypeNames.RecordClassName,       "class");
        (ClassificationTypeNames.RecordStructName,      "struct");
        (ClassificationTypeNames.RegexText,             "regex");
        (ClassificationTypeNames.StringLiteral,         "string");
        (ClassificationTypeNames.StructName,            "struct");
        (ClassificationTypeNames.TypeParameterName,     "typeParameter");
        (ClassificationTypeNames.VerbatimStringLiteral, "string")
    ]

    let private classificationModifierMap = Map [
        (ClassificationTypeNames.StaticSymbol, "static")
    ]

    let private semanticTokenTypeMap =
        classificationTypeMap
        |> Map.values
        |> Seq.distinct
        |> flip Seq.zip (Seq.initInfinite uint32)
        |> Map.ofSeq

    let private semanticTokenModifierMap =
        classificationModifierMap
        |> Map.values
        |> Seq.distinct
        |> flip Seq.zip (Seq.initInfinite uint32)
        |> Map.ofSeq

    let private semanticTokenTypes =
        semanticTokenTypeMap
        |> Seq.sortBy (fun kvp -> kvp.Value)
        |> Seq.map (fun kvp -> kvp.Key)

    let private semanticTokenModifiers =
        semanticTokenModifierMap
        |> Seq.sortBy (fun kvp -> kvp.Value)
        |> Seq.map (fun kvp -> kvp.Key)

    let private getSemanticTokenIdFromClassification (classification: string) =
        classificationTypeMap
        |> Map.tryFind classification
        |> Option.bind (flip Map.tryFind semanticTokenTypeMap)

    let private getSemanticTokenModifierFlagFromClassification (classification: string) =
        classificationModifierMap
        |> Map.tryFind classification
        |> Option.bind (flip Map.tryFind semanticTokenModifierMap)
        |> Option.defaultValue 0u
        |> int32
        |> (<<<) 1u

    let private toSemanticToken (lines: TextLineCollection) (textSpan: TextSpan, spans: IEnumerable<ClassifiedSpan>) =
        let (typeId, modifiers) =
            spans
            |> Seq.fold (fun (t, m) s ->
                if ClassificationTypeNames.AdditiveTypeNames.Contains(s.ClassificationType) then
                    (t, m ||| (getSemanticTokenModifierFlagFromClassification s.ClassificationType))
                else
                    (getSemanticTokenIdFromClassification s.ClassificationType, m)
            ) (None, 0u)
        let pos = lines.GetLinePositionSpan(textSpan)
        (uint32 pos.Start.Line, uint32 pos.Start.Character, uint32 (pos.End.Character - pos.Start.Character), typeId, modifiers)

    let private computePosition (((pLine, pChar, _, _, _), (cLine, cChar, cLen, cToken, cModifiers)): ((uint32 * uint32 * uint32 * uint32 * uint32) * (uint32 * uint32 * uint32 * uint32 * uint32))) =
        let deltaLine = cLine - pLine
        let deltaChar =
            if deltaLine = 0u then
                cChar - pChar
            else
                cChar
        (deltaLine, deltaChar, cLen, cToken, cModifiers)

    let private getSemanticTokensRange (wm: IWorkspaceManager) (uri: string) (range: Range option): AsyncLspResult<SemanticTokens option> = async {
        match wm.GetDocument uri with
        | None -> return None |> success
        | Some doc ->
            let! sourceText = doc.GetTextAsync() |> Async.AwaitTask
            let textSpan =
                range
                |> Option.map (Range.toTextSpan sourceText.Lines)
                |> Option.defaultValue (TextSpan(0, sourceText.Length))
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
            return Some response |> success
    }

    let provider: SemanticTokensOptions option =
        Some { Legend = { TokenTypes = semanticTokenTypes |> Seq.toArray
                          TokenModifiers = semanticTokenModifiers |> Seq.toArray }
               Range = Some true
               Full = true |> First |> Some }

    let handleFull (wm: IWorkspaceManager) (p: SemanticTokensParams) : AsyncLspResult<SemanticTokens option> =
        getSemanticTokensRange wm p.TextDocument.Uri None

    let handleFullDelta
        (wm: IWorkspaceManager)
        (p: SemanticTokensDeltaParams)
        : AsyncLspResult<U2<SemanticTokens, SemanticTokensDelta> option> =
        notImplemented

    let handleRange (wm: IWorkspaceManager) (p: SemanticTokensRangeParams) : AsyncLspResult<SemanticTokens option> =
        getSemanticTokensRange wm p.TextDocument.Uri (Some p.Range)
