module CSharpLanguageServer.Documentation

open System
open System.Xml.Linq
open System.Collections.Generic

type TripleSlashComment = {
     Summary: XElement list;
     Params: Map<string, XElement>;
     Exceptions: Map<string, XElement>;
     Returns: XElement list;
     Remarks: XElement list;
     OtherLines: XElement list; }

let emptyTripleSlashComment = {
    Summary = []
    Params = Map.empty
    Exceptions = Map.empty
    Returns = []
    Remarks = []
    OtherLines = [] }

let listOfOption o = match o with | Some v -> [v] | _ -> []
let listOfOne i = [i]

let formatCref (cref: string) =
    let parts = cref.Split(':')
    let typeName = match parts.Length with
                    | 1 -> cref
                    | _ -> String.Join(":", parts |> Seq.skip 1)
    "`" + typeName + "`"

let normalizeWhitespace (s: string) =
    s.Replace("\r\n", " ").Replace("\n", " ")

let formatTextElement (n: XElement) =

    let formatSeeElement (e: XElement) =
        let crefMaybe =
            e.Attribute(XName.Get("cref"))
            |> Option.ofObj
            |> Option.map (fun x -> x.Value)
            |> Option.map formatCref
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
        { comment with Params = comment.Params |> Map.add name n }

    | "returns" ->
        { comment with Returns = comment.Returns |> List.append [n] }

    | "exception" ->
        let name = n.Attribute(XName.Get("cref")).Value |> formatCref
        { comment with Exceptions = comment.Exceptions |> Map.add name n }

    | _ ->
        { comment with OtherLines = comment.OtherLines |> List.append [n] }


let parseComment xmlDocumentation: TripleSlashComment =
    //printf "xmlDocumentation=%s" xmlDocumentation
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
    let formatParamItem (item: KeyValuePair<string, XElement>) =
        sprintf "- Param `%s`: %s" item.Key (formatTextElement item.Value)

    let formatExceptionItem (item: KeyValuePair<string, XElement>) =
        sprintf "- Exception %s: %s" item.Key (formatTextElement item.Value)

    let formatReturn n =
        sprintf "- Returns: %s" (formatTextElement n)

    []
    |> List.append (model.Summary |> List.map formatTextElement)
    |> List.append (model.Params |> Seq.map formatParamItem |> List.ofSeq)
    |> List.append (model.Returns |> List.map formatReturn)
    |> List.append (model.Exceptions |> Seq.map formatExceptionItem |> List.ofSeq)
    |> List.append (model.Remarks |> List.map formatTextElement)
    |> List.append (model.OtherLines |> List.map string)
    |> List.rev
    |> List.map (fun s -> s.Trim())


let formatDocXml xmlDocumentation =
    let comment = parseComment xmlDocumentation
    String.Join("\r\n", comment |> formatComment)


let formatDocXmlWithTypeInfo xmlDocumentation typeName typeAssemblyName =
    let comment = parseComment xmlDocumentation
    let formattedDocLines = formatComment comment

    let symbolInfoLines =
        match typeName, typeAssemblyName with
        | "", "" -> []
        | typeName, "" -> [sprintf "`%s`" typeName]
        | _, _ -> [sprintf "`%s` from assembly `%s`" typeName typeAssemblyName]

    let formattedDoc =
        formattedDocLines
        |> Seq.append (if symbolInfoLines.Length > 0 && formattedDocLines.Length > 0 then [""] else [])
        |> Seq.append symbolInfoLines
        |> (fun ss -> String.Join("\r\n", ss))

    formattedDoc // + (xmlDocumentation)
