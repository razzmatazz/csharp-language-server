module CSharpLanguageServer.DocumentationUtil

open System
open System.Xml.Linq

let listOfOption o = match o with | Some v -> [v] | _ -> []

let formatDocXml xmlDocumentation typeName typeAssemblyName =
    printf "xmlDocumentation=%s" xmlDocumentation
    let doc = XDocument.Parse("<docroot>" + xmlDocumentation + "</docroot>")

    let formatCref (cref: string) =
        let parts = cref.Split(':')
        let typeName = match parts.Length with
                       | 1 -> cref
                       | _ -> String.Join(":", parts |> Seq.skip 1)
        "`" + typeName + "`"

    let formatTextNode (n: XElement) =
        let formatNode (subnode: XNode) =
            match subnode with
            | :? XText as t -> [t.Value]
            | :? XElement as e ->
               match e.Name.LocalName with
               | "see" -> e.Attribute(XName.Get("cref"))
                           |> Option.ofObj
                           |> listOfOption
                           |> Seq.map (fun x -> x.Value)
                           |> Seq.map formatCref
                           |> List.ofSeq
               | _ -> []
            | _ -> []

        let removeHeadingEmptyLines ss = ss |> Seq.skipWhile String.IsNullOrWhiteSpace
        let removeTrailingEmptyLines ss = ss |> Seq.rev |> removeHeadingEmptyLines |> Seq.rev

        let removeBaseWhitespace (ss: string seq) =
            let baseWhitespaceCount =
                ss |> Seq.map (fun s -> s |> Seq.takeWhile Char.IsWhiteSpace |> Seq.length)
                   |> Seq.filter (fun l -> l > 0)
                   |> (fun ss -> if Seq.isEmpty ss then 0 else Seq.min ss)

            ss |> Seq.map (fun s -> if s.Length > baseWhitespaceCount then s.Substring(baseWhitespaceCount) else s)

        let lines =
            n.Nodes()
            |> Seq.collect formatNode
            |> Seq.collect (fun s -> s.Split("\n"))
            |> Seq.map (fun s -> s.TrimEnd('\r', ' ', '\t'))
            |> removeHeadingEmptyLines
            |> removeTrailingEmptyLines
            |> removeBaseWhitespace

        String.Join(Environment.NewLine, lines)

    let elementToStringSeq (n: XElement) =
        match n.Name.LocalName with
        | "summary" -> [n |> formatTextNode]
        | "param" -> [sprintf "- Param `%s`: %s" (n.Attribute(XName.Get("name")).Value) (formatTextNode n)]
        | "returns" -> [sprintf "- Returns: %s" (formatTextNode n)]
        | "exception" -> [sprintf "- Exception %s: %s" (n.Attribute(XName.Get("cref")).Value |> formatCref)
                                                     (formatTextNode n)]
        | _ -> []

    let rec nodeToStringSeq (n: XNode): string list =
        match n with
        | :? XElement as e ->
          let thisItem = e |> elementToStringSeq
          let subItems = e.Nodes() |> Seq.collect nodeToStringSeq |> List.ofSeq
          thisItem |> Seq.append subItems |> List.ofSeq
        | _ -> []

    let formattedDocLines = doc.Root.Nodes() |> Seq.collect nodeToStringSeq

    let symbolInfoLines =
        match typeName, typeAssemblyName with
        | "", "" -> []
        | _ -> ["Full name `" + typeName + "`; Assembly `" + typeAssemblyName + "`"]

    let formattedDoc =
        symbolInfoLines
        |> Seq.append formattedDocLines
        |> (fun ss -> String.Join("\r\n", ss))

    formattedDoc // + (xmlDocumentation)
