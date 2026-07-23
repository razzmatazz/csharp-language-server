namespace CSharpLanguageServer

open System
open System.Reflection
open System.Threading
open System.Xml.Linq

open Microsoft.CodeAnalysis

open CSharpLanguageServer.Roslyn.Conversions

module DocumentationUtil =
    type TripleSlashComment =
        { Summary: XElement list
          Params: (string * XElement) list
          Exceptions: (string * XElement) list
          Returns: XElement list
          Value: XElement list
          Types: (string * XElement) list
          Remarks: XElement list
          Examples: XElement list
          SeeAlso: XElement list
          OtherLines: XElement list }

        static member Default =
            { Summary = []
              Params = []
              Exceptions = []
              Returns = []
              Value = []
              Types = []
              Remarks = []
              Examples = []
              SeeAlso = []
              OtherLines = [] }

    let parseCref (cref: string) =
        let parts = cref.Split ':'

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

    let private getDocumentationCommentMethod =
        let symbolExtensionsType =
            typeof<Workspace>.Assembly.GetType("Microsoft.CodeAnalysis.Shared.Extensions.ISymbolExtensions")
            |> Option.ofObj
            |> Option.defaultWith (fun () ->
                failwith "Microsoft.CodeAnalysis.Shared.Extensions.ISymbolExtensions was not found")

        symbolExtensionsType.GetMethods(BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic)
        |> Seq.tryFind (fun methodInfo ->
            let parameters = methodInfo.GetParameters()

            methodInfo.Name = "GetDocumentationComment"
            && parameters.Length = 6
            && parameters[0].ParameterType = typeof<ISymbol>
            && parameters[1].ParameterType = typeof<Compilation>)
        |> Option.defaultWith (fun () ->
            failwith "Microsoft.CodeAnalysis.Shared.Extensions.ISymbolExtensions.GetDocumentationComment was not found")

    let private fullXmlFragmentProperty =
        getDocumentationCommentMethod.ReturnType.GetProperty(
            "FullXmlFragment",
            BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic
        )
        |> Option.ofObj
        |> Option.defaultWith (fun () -> failwith "DocumentationComment.FullXmlFragment was not found")

    let private documentationXmlForSymbol (compilation: Compilation) (sym: ISymbol) =
        let comment =
            getDocumentationCommentMethod.Invoke(
                null,
                [| box sym
                   box compilation
                   null
                   box true // expand includes
                   box true // expand inheritdoc
                   box CancellationToken.None |]
            )

        match Option.ofObj comment with
        | None -> ""
        | Some comment ->
            fullXmlFragmentProperty.GetValue(comment)
            |> Option.ofObj
            |> Option.map string
            |> Option.defaultValue ""

    let private formatReferenceElement (e: XElement) =
        let attribute name =
            e.Attribute(XName.Get name) |> Option.ofObj |> Option.map _.Value

        let label = e.Value |> normalizeWhitespace |> _.Trim()

        match attribute "cref", attribute "langword", attribute "href" with
        | Some _, _, _ when not (String.IsNullOrWhiteSpace label) -> label
        | Some cref, _, _ -> sprintf "``%s``" (parseCref cref)
        | _, Some langword, _ -> sprintf "``%s``" langword
        | _, _, Some href when String.IsNullOrWhiteSpace label -> sprintf "<%s>" href
        | _, _, Some href -> sprintf "[%s](%s)" label href
        | _ -> label

    let private formatCodeElement (e: XElement) =
        let lines =
            e.Value.Replace("\r\n", "\n").Split('\n')
            |> Array.map _.TrimEnd()
            |> Array.skipWhile String.IsNullOrWhiteSpace
            |> Array.rev
            |> Array.skipWhile String.IsNullOrWhiteSpace
            |> Array.rev

        if Array.isEmpty lines then
            ""
        else
            let indentation =
                lines
                |> Array.filter (not << String.IsNullOrWhiteSpace)
                |> Array.map (fun line -> line.Length - line.TrimStart().Length)
                |> Array.min

            let code =
                lines
                |> Array.map (fun line ->
                    if String.IsNullOrWhiteSpace line then
                        ""
                    else
                        line.Substring(min indentation line.Length))
                |> String.concat "\n"

            sprintf "```csharp\n%s\n```" code

    let formatTextElement (n: XElement) =

        let rec formatTextNode (subnode: XNode) =
            match subnode with
            | :? XElement as e ->
                match e.Name.LocalName with
                | "c" -> [ sprintf "``%s``" e.Value ]
                | "see"
                | "seealso"
                | "a" -> [ formatReferenceElement e ]
                | "paramref"
                | "typeparamref" ->
                    e.Attribute(XName.Get "name")
                    |> Option.ofObj
                    |> Option.map (fun x -> sprintf "``%s``" x.Value)
                    |> Option.toList
                | "b" -> [ sprintf "**%s**" (formatTextNodes (e.Nodes())) ]
                | "i" -> [ sprintf "*%s*" (formatTextNodes (e.Nodes())) ]
                | "u" -> [ formatTextNodes (e.Nodes()) ]
                | "p"
                | "para" -> [ sprintf "\n\n%s\n" (formatTextNodes (e.Nodes())) ]
                | "br" -> [ "  \n" ]
                | "code" -> [ formatCodeElement e ]
                | "list" -> [ formatListElement e ]
                | _ -> [ formatTextNodes (e.Nodes()) ]
            | :? XText as t -> [ t.Value |> normalizeWhitespace ]
            | _ -> []

        and formatTextNodes (nodes: seq<XNode>) =
            nodes |> Seq.collect formatTextNode |> (fun parts -> String.Join("", parts))

        and formatListElement (e: XElement) =
            let listType =
                e.Attribute(XName.Get "type")
                |> Option.ofObj
                |> Option.map _.Value
                |> Option.defaultValue "bullet"

            let formatElementContents (element: XElement) =
                element.Nodes() |> formatTextNodes |> (fun text -> text.Trim())

            let childContents name (element: XElement) =
                element.Element(XName.Get name)
                |> Option.ofObj
                |> Option.map formatElementContents
                |> Option.defaultValue ""

            let items = e.Elements(XName.Get "item") |> List.ofSeq

            match listType, items with
            | _, [] -> ""
            | "table", _ ->
                let listHeader = e.Element(XName.Get "listheader") |> Option.ofObj

                let header name fallback =
                    listHeader
                    |> Option.map (childContents name)
                    |> Option.filter (not << String.IsNullOrWhiteSpace)
                    |> Option.defaultValue fallback

                let escapeCell (text: string) =
                    text.Replace("|", "\\|").Replace("\r\n", "<br>").Replace("\n", "<br>")

                let row term description =
                    sprintf "| %s | %s |" (escapeCell term) (escapeCell description)

                let rows =
                    items
                    |> List.map (fun item -> row (childContents "term" item) (childContents "description" item))

                "\n\n"
                + row (header "term" "Term") (header "description" "Description")
                + "\n| --- | --- |\n"
                + String.Join("\n", rows)
                + "\n\n"
            | _, _ ->
                let formatItem index (item: XElement) =
                    let term = childContents "term" item
                    let description = childContents "description" item

                    let content =
                        match String.IsNullOrWhiteSpace term, String.IsNullOrWhiteSpace description with
                        | false, false -> sprintf "%s: %s" term description
                        | false, true -> term
                        | true, false -> description
                        | true, true -> formatElementContents item

                    let marker =
                        match listType with
                        | "number" -> sprintf "%d." (index + 1)
                        | _ -> "-"

                    sprintf "%s %s" marker content

                "\n\n" + (items |> List.mapi formatItem |> String.concat "\n") + "\n\n"

        n.Nodes() |> formatTextNodes

    let extendCommentWithElement comment (n: XElement) =
        match n.Name.LocalName with
        | "summary" ->
            let newSummary = comment.Summary |> List.append [ n ]
            { comment with Summary = newSummary }

        | "remarks" ->
            let newRemarks = comment.Remarks |> List.append [ n ]
            { comment with Remarks = newRemarks }

        | "param" ->
            let name =
                n.Attribute(XName.Get "name")
                |> Option.ofObj
                |> Option.map (fun a -> a.Value)
                |> Option.defaultValue "(unspecified)"

            { comment with
                Params = comment.Params |> List.append [ (name, n) ] }

        | "returns" ->
            { comment with
                Returns = comment.Returns |> List.append [ n ] }

        | "value" ->
            { comment with
                Value = comment.Value |> List.append [ n ] }

        | "example" ->
            { comment with
                Examples = comment.Examples |> List.append [ n ] }

        | "exception" ->
            let name =
                n.Attribute(XName.Get "cref")
                |> Option.ofObj
                |> Option.map (fun a -> parseCref a.Value)
                |> Option.defaultValue "(unspecified)"

            { comment with
                Exceptions = comment.Exceptions |> List.append [ (name, n) ] }

        | "typeparam" ->
            let name =
                n.Attribute(XName.Get "name")
                |> Option.ofObj
                |> Option.map (fun a -> a.Value)
                |> Option.defaultValue "(unspecified)"

            { comment with
                Types = comment.Types |> List.append [ (name, n) ] }

        | "seealso" ->
            { comment with
                SeeAlso = comment.SeeAlso |> List.append [ n ] }

        | _ ->
            { comment with
                OtherLines = comment.OtherLines |> List.append [ n ] }


    let parseComment xmlDocumentation : TripleSlashComment =
        let doc = XDocument.Parse("<docroot>" + xmlDocumentation + "</docroot>")

        let unwrapDocRoot (root: XElement) =
            let elementNames (el: XElement) =
                el.Elements() |> Seq.map (fun e -> e.Name.LocalName) |> List.ofSeq

            match elementNames root with
            | [ "member" ] -> root.Element(XName.Get "member")
            | [ "doc" ] -> root.Element(XName.Get "doc")
            | _ -> root

        doc.Root
        |> unwrapDocRoot
        |> fun r -> r.Elements()
        |> Seq.fold extendCommentWithElement TripleSlashComment.Default


    let formatComment model : string list =

        let indentContinuation (text: string) =
            text.Split('\n')
            |> Seq.mapi (fun index line ->
                if index = 0 || String.IsNullOrWhiteSpace line then
                    line
                else
                    "  " + line)
            |> fun lines -> String.Join("\n", lines)

        let appendNamed name (kvs: (string * XElement) seq) markdownLines =
            match Seq.isEmpty kvs with
            | true -> markdownLines
            | false ->
                let formatItem (key, value) =
                    sprintf "- ``%s``: %s" key (formatTextElement value |> indentContinuation)

                markdownLines
                |> List.append [ name + ":"; "" ]
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
                |> List.append [ "" ]
                |> List.append (formattedLines |> List.map (fun s -> name + ": " + s))

        let appendSection name elements markdownLines =
            let formatted =
                elements
                |> List.map formatTextElement
                |> List.filter (not << String.IsNullOrWhiteSpace)

            match formatted with
            | [] -> markdownLines
            | _ -> markdownLines |> List.append [ name + ":"; "" ] |> List.append formatted

        let appendSeeAlso elements markdownLines =
            let references =
                elements
                |> List.map formatReferenceElement
                |> List.filter (not << String.IsNullOrWhiteSpace)

            match references with
            | [] -> markdownLines
            | _ ->
                markdownLines
                |> List.append [ "See also:"; "" ]
                |> List.append (references |> List.map (sprintf "- %s"))

        []
        |> List.append (model.Summary |> List.map formatTextElement)
        |> appendNamed "Parameters" model.Params
        |> appendFormatted "Returns" model.Returns
        |> appendFormatted "Value" model.Value
        |> appendNamed "Exceptions" model.Exceptions
        |> appendNamed "Types" model.Types
        |> appendFormatted "Remarks" model.Remarks
        |> appendSection "Example" model.Examples
        |> appendSeeAlso model.SeeAlso
        |> List.append (model.OtherLines |> List.map string)
        |> List.rev
        |> List.map (fun s -> s.Trim())


    let formatDocXml xmlDocumentation =
        String.Join("\n", xmlDocumentation |> parseComment |> formatComment)

    let markdownDocForSymbol (compilation: Compilation) (sym: ISymbol) =
        let comment = parseComment (documentationXmlForSymbol compilation sym)
        let formattedDocLines = formatComment comment

        formattedDocLines |> fun ss -> String.Join("\n", ss)

    let markdownDocForSymbolWithSignature (compilation: Compilation) (sym: ISymbol) =
        let symbolName =
            SymbolName.fromSymbol SymbolDisplayFormat.MinimallyQualifiedFormat sym

        let symbolInfoLines =
            match symbolName with
            | "" -> []
            | typeName -> [ sprintf "```csharp\n%s\n```" typeName ]

        let comment = parseComment (documentationXmlForSymbol compilation sym)
        let formattedDocLines = formatComment comment

        formattedDocLines
        |> Seq.append (
            if symbolInfoLines.Length > 0 && formattedDocLines.Length > 0 then
                [ "" ]
            else
                []
        )
        |> Seq.append symbolInfoLines
        |> fun ss -> String.Join("\n", ss)
