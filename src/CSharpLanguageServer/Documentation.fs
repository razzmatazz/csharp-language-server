module CSharpLanguageServer.Documentation

open System
open System.Xml.Linq
open System.Collections.Generic
open Microsoft.CodeAnalysis
open RoslynHelpers
open Ionide.LanguageServerProtocol.Types

type TripleSlashComment = {
     Summary: XElement list;
     Params: (string * XElement) list;
     Exceptions: (string * XElement) list;
     Returns: XElement list;
     Types: (string * XElement) list;
     Remarks: XElement list;
     OtherLines: XElement list; }

let emptyTripleSlashComment = {
    Summary = []
    Params = []
    Exceptions = []
    Returns = []
    Types = []
    Remarks = []
    OtherLines = [] }

let listOfOption o = match o with | Some v -> [v] | _ -> []
let listOfOne i = [i]

let parseCref (cref: string) =
    let parts = cref.Split(':')
    match parts.Length with
    | 1 -> cref
    | _ -> String.Join(":", parts |> Seq.skip 1)


let normalizeWhitespace (s: string) =
    let mutable modified = s
    let mutable prevModified = ""
    while modified <> prevModified do
        prevModified <- modified
        modified <- modified.Replace("  ", " ").Replace("\r\n", " ").Replace("\n", " ")

    modified


let formatTextElement (n: XElement) =

    let formatSeeElement (e: XElement) =
        let crefMaybe =
            e.Attribute(XName.Get("cref"))
            |> Option.ofObj
            |> Option.map (fun x -> x.Value)
            |> Option.map parseCref
            |> Option.map (fun s -> sprintf "`%s`" s)
            |> listOfOption

        let langWordMaybe =
            e.Attribute(XName.Get("langword"))
            |> Option.ofObj
            |> Option.map (fun x -> sprintf "`%s`" x.Value)
            |> listOfOption

        crefMaybe |> Seq.append langWordMaybe |> List.ofSeq

    let formatTextNode (subnode: XNode) =
        match subnode with
        | :? XElement as e ->
            match e.Name.LocalName with
            | "c" -> [sprintf "`%s`" e.Value]
            | "see" -> formatSeeElement e
            | "paramref" -> e.Attribute(XName.Get("name"))
                            |> Option.ofObj
                            |> Option.map (fun x -> sprintf "`%s`" x.Value)
                            |> listOfOption
            | _ -> [e.Value]
        | :? XText as t -> t.Value |> normalizeWhitespace |> listOfOne
        | _ -> []

    n.Nodes()
    |> Seq.collect formatTextNode
    |> (fun ss -> String.Join("", ss))


let extendCommentWithElement comment (n: XElement) =
    match n.Name.LocalName with
    | "summary" ->
        let newSummary = comment.Summary |> List.append [n]
        { comment with Summary = newSummary }

    | "remarks" ->
        let newRemarks = comment.Remarks |> List.append [n]
        { comment with Remarks = newRemarks }

    | "param" ->
        let name = n.Attribute(XName.Get("name")).Value
        { comment with Params = comment.Params |> List.append [(name, n)] }

    | "returns" ->
        { comment with Returns = comment.Returns |> List.append [n] }

    | "exception" ->
        let name = n.Attribute(XName.Get("cref")).Value |> parseCref
        { comment with Exceptions = comment.Exceptions |> List.append [(name, n)] }

    | "typeparam" ->
        let name = n.Attribute(XName.Get("name")).Value
        { comment with Types = comment.Types |> List.append [(name, n)] }

    | _ ->
        { comment with OtherLines = comment.OtherLines |> List.append [n] }


let parseComment xmlDocumentation: TripleSlashComment =
    let doc = XDocument.Parse("<docroot>" + xmlDocumentation + "</docroot>")

    let unwrapDocRoot (root: XElement) =
        let elementNames (el: XElement) =
            el.Elements() |> Seq.map (fun e -> e.Name.LocalName) |> List.ofSeq

        match elementNames root with
        | ["member"] -> root.Element(XName.Get("member"))
        | _ -> root

    doc.Root
        |> unwrapDocRoot
        |> fun r -> r.Elements()
        |> Seq.fold extendCommentWithElement emptyTripleSlashComment


let formatComment model : string list =

    let appendNamed name (kvs: (string * XElement) seq) markdownLines =
        match Seq.isEmpty kvs with
        | true -> markdownLines
        | false ->
            let formatItem (key, value) =
                sprintf "- `%s`: %s" key (formatTextElement value)

            markdownLines
            |> List.append [name + ":"; ""]
            |> List.append (kvs |> Seq.map formatItem |> List.ofSeq)

    let appendFormatted name elms markdownLines =
        let formattedLines =
            elms
            |> List.map formatTextElement
            |> List.filter (not << String.IsNullOrWhiteSpace)

        match Seq.isEmpty formattedLines with
        | true -> markdownLines
        | false ->
            markdownLines
            |> List.append [""]
            |> List.append (formattedLines |> List.map (fun s -> name + ": " + s))

    []
    |> List.append (model.Summary |> List.map formatTextElement)
    |> appendNamed "Parameters" model.Params
    |> appendFormatted "Returns" model.Returns
    |> appendNamed "Exceptions" model.Exceptions
    |> appendNamed "Types" model.Types
    |> appendFormatted "Remarks" model.Remarks
    |> List.append (model.OtherLines |> List.map string)
    |> List.rev
    |> List.map (fun s -> s.Trim())


let formatDocXml xmlDocumentation =
    String.Join("\n", xmlDocumentation |> parseComment |> formatComment)

let markdownDocForSymbol (sym: ISymbol) =
    let comment = parseComment (sym.GetDocumentationCommentXml())
    let formattedDocLines = formatComment comment

    formattedDocLines
        |> (fun ss -> String.Join("\n", ss))

let markdownDocForSymbolWithSignature (sym: ISymbol) (semanticModel: SemanticModel) pos =
    let symbolInfo = symbolToLspSymbolInformation true sym (Some semanticModel) (Some pos)

    let symAssemblyName =
        sym.ContainingAssembly
        |> Option.ofObj
        |> Option.map (fun a -> a.Name)
        |> Option.defaultValue ""

    let symbolInfoLines =
        match symbolInfo.Name, symAssemblyName with
        | "", "" -> []
        | typeName, "" -> [sprintf "`%s`" typeName]
        | _, _ ->
            let docAssembly = semanticModel.Compilation.Assembly
            if symAssemblyName = docAssembly.Name then
                [sprintf "`%s`" symbolInfo.Name]
            else
                [sprintf "`%s` from assembly `%s`" symbolInfo.Name symAssemblyName]

    let comment = parseComment (sym.GetDocumentationCommentXml())
    let formattedDocLines = formatComment comment

    formattedDocLines
        |> Seq.append (if symbolInfoLines.Length > 0 && formattedDocLines.Length > 0 then [""] else [])
        |> Seq.append symbolInfoLines
        |> (fun ss -> String.Join("\n", ss))
