namespace CSharpLanguageServer.Handlers

open System
open System.Collections.Generic

open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Types.LspResult
open Microsoft.CodeAnalysis.Classification
open Microsoft.CodeAnalysis.Text

open CSharpLanguageServer.State
open CSharpLanguageServer.Util
open CSharpLanguageServer.Conversions
open CSharpLanguageServer.Types

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

    let private getSemanticTokensRange (context: ServerRequestContext) (uri: string) (range: Range option): AsyncLspResult<SemanticTokens option> = async {
        let docMaybe = context.GetUserDocument uri
        match docMaybe with
        | None -> return None |> success
        | Some doc ->
            let! ct = Async.CancellationToken
            let! sourceText = doc.GetTextAsync(ct) |> Async.AwaitTask
            let textSpan =
                range
                |> Option.map (Range.toTextSpan sourceText.Lines)
                |> Option.defaultValue (TextSpan(0, sourceText.Length))
            let! spans = Classifier.GetClassifiedSpansAsync(doc, textSpan, ct) |> Async.AwaitTask
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

    let private dynamicRegistration (clientCapabilities: ClientCapabilities option) =
        clientCapabilities
        |> Option.bind (fun x -> x.TextDocument)
        |> Option.bind (fun x -> x.SemanticTokens)
        |> Option.bind (fun x -> x.DynamicRegistration)
        |> Option.defaultValue false

    let provider (clientCapabilities: ClientCapabilities option) : SemanticTokensOptions option =
        match dynamicRegistration clientCapabilities with
        | true -> None
        | false ->
            Some { Legend = { TokenTypes = semanticTokenTypes |> Seq.toArray
                              TokenModifiers = semanticTokenModifiers |> Seq.toArray }
                   Range = Some true
                   Full = true |> First |> Some }

    let registration (clientCapabilities: ClientCapabilities option) : Registration option =
        match dynamicRegistration clientCapabilities with
        | false -> None
        | true ->
            Some
                { Id = Guid.NewGuid().ToString()
                  Method = "textDocument/semanticTokens"
                  RegisterOptions =
                      { Legend = { TokenTypes = semanticTokenTypes |> Seq.toArray
                                   TokenModifiers = semanticTokenModifiers |> Seq.toArray }
                        Range = Some true
                        Full = true |> First |> Some
                        DocumentSelector = Some defaultDocumentSelector } |> serialize |> Some }

    // TODO: Everytime the server will re-compute semantic tokens, is it possible to cache the result?
    let handleFull (context: ServerRequestContext) (p: SemanticTokensParams): AsyncLspResult<SemanticTokens option> =
        getSemanticTokensRange context p.TextDocument.Uri None

    let handleFullDelta
        (context: ServerRequestContext)
        (p: SemanticTokensDeltaParams)
        : AsyncLspResult<U2<SemanticTokens, SemanticTokensDelta> option> =
        LspResult.notImplemented<U2<SemanticTokens, SemanticTokensDelta> option>
        |> async.Return

    let handleRange (context: ServerRequestContext) (p: SemanticTokensRangeParams): AsyncLspResult<SemanticTokens option> =
        getSemanticTokensRange context p.TextDocument.Uri (Some p.Range)
