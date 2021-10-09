module CSharpLanguageServer.DocumentationUtil

open System
open System.Xml.Linq

let listOfOption o = match o with | Some v -> [v] | _ -> []
let listOfOne i = [i]

let formatDocXml xmlDocumentation typeName typeAssemblyName =
    //printf "xmlDocumentation=%s" xmlDocumentation
    let doc = XDocument.Parse("<docroot>" + xmlDocumentation + "</docroot>")

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

        let formatTextSubnode (subnode: XNode) =
            match subnode with
            | :? XElement as e ->
                match e.Name.LocalName with
                | "see" -> formatSeeElement e
                | "paramref" -> e.Attribute(XName.Get("name"))
                                |> Option.ofObj
                                |> Option.map (fun x -> sprintf "`%s`" x.Value)
                                |> listOfOption
                | _ -> []
            | :? XText as t -> t.Value |> normalizeWhitespace |> listOfOne
            | _ -> []

        n.Nodes()
        |> Seq.collect formatTextSubnode
        |> (fun ss -> String.Join("", ss))

    let elementToStrings (n: XElement) =
        match n.Name.LocalName with
        | "summary" ->
            n |> formatTextElement |> listOfOne
        | "param" ->
            sprintf "- Param `%s`: %s"
                    (n.Attribute(XName.Get("name")).Value)
                    (formatTextElement n)
            |> listOfOne
        | "returns" ->
            sprintf "- Returns: %s" (formatTextElement n)
            |> listOfOne
        | "exception" ->
            sprintf "- Exception %s: %s"
                    (n.Attribute(XName.Get("cref")).Value |> formatCref)
                    (formatTextElement n)
            |> listOfOne
        | _ -> []

    let formattedDocLines =
        doc.Root.Elements()
        |> Seq.collect elementToStrings
        |> Seq.map (fun s -> s.Trim())
        |> List.ofSeq

    let symbolInfoLines =
        match typeName, typeAssemblyName with
        | "", "" -> []
        | _ -> ["Full name `" + typeName + "`; Assembly `" + typeAssemblyName + "`"]

    let formattedDoc =
        symbolInfoLines
        |> Seq.append (if symbolInfoLines.Length > 0 && formattedDocLines.Length > 0 then [""] else [])
        |> Seq.append formattedDocLines
        |> (fun ss -> String.Join("\r\n", ss))

    formattedDoc // + (xmlDocumentation)
