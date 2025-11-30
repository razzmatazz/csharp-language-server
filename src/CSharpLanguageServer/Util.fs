module CSharpLanguageServer.Util

open System
open System.Threading.Tasks

let nonNull name (value: 'T when 'T: null) : 'T =
    if Object.ReferenceEquals(value, null) then
        raise (new Exception(sprintf "A non-null value was expected: %s" name))
    else
        value

module Uri =
    // Unescape some necessary char before passing string to Uri.
    // Can't use Uri.UnescapeDataString here. For example, if uri is "file:///z%3a/src/c%23/ProjDir" ("%3a" is
    // ":" and "%23" is "#"), Uri.UnescapeDataString will unescape both "%3a" and "%23". Then Uri will think
    /// "#/ProjDir" is Fragment instead of part of LocalPath.
    let unescape (uri: string) = uri.Replace("%3a", ":", true, null)

    let fromPath (path: string) =
        let metadataPrefix = "$metadata$/"

        if path.StartsWith metadataPrefix then
            "csharp:/metadata/" + path.Substring metadataPrefix.Length
        else
            Uri(path).ToString()

    let parseFileUri s : string = Uri(s).LocalPath

    let tryParseFileUri s : string option =
        try
            let uri = Uri s
            Some uri.LocalPath
        with _ex ->
            None

let unwindProtect cleanupFn op = async {
    try
        return! op
    finally
        cleanupFn ()
}

// TPL Task's wrap exceptions in AggregateException, -- this fn unpacks them
let rec unpackException (exn: Exception) =
    match exn with
    | :? AggregateException as agg ->
        match Seq.tryExactlyOne agg.InnerExceptions with
        | Some x -> unpackException x
        | None -> exn
    | _ -> exn

let formatInColumns (data: list<list<string>>) : string =
    if List.isEmpty data then
        ""
    else
        let numCols = data |> List.map List.length |> List.max

        let columnWidths =
            [ 0 .. numCols - 1 ]
            |> List.map (fun colIdx ->
                data
                |> List.map (fun row -> if colIdx < row.Length then row.[colIdx].Length else 0)
                |> List.max)

        data
        |> List.map (fun row ->
            [ 0 .. numCols - 1 ]
            |> List.map (fun colIdx ->
                let value = if colIdx < row.Length then row.[colIdx] else ""
                let width = columnWidths.[colIdx]
                value.PadRight width)
            |> String.concat "  ")
        |> String.concat Environment.NewLine

[<AutoOpen>]
module TaskExtensions =
    type Task with
        static member private fromResultMI =
            typeof<Task>.GetMethod("FromResult")
            |> nonNull (sprintf "%s.FromResult()" (string typeof<Task>))

        static member fromResult(taskType: Type, resultValue: obj | null) =
            Task.fromResultMI.MakeGenericMethod([| taskType |]).Invoke(null, [| resultValue |])

module Seq =
    let inline tryMaxBy (projection: 'T -> 'U) (source: 'T seq) : 'T option =
        if isNull source || Seq.isEmpty source then
            None
        else
            Seq.maxBy projection source |> Some

module Option =
    let inline ofString (value: string) =
        match String.IsNullOrWhiteSpace value with
        | true -> None
        | false -> Some value

module Async =
    let map f computation =
        async.Bind(computation, f >> async.Return)

    let bindOption f computation =
        async.Bind(
            computation,
            fun v ->
                match v with
                | Some v -> f v
                | None -> async.Return None
        )

module Map =
    let union map1 map2 =
        Map.fold (fun acc key value -> Map.add key value acc) map1 map2
