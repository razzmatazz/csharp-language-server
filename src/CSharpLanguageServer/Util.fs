module CSharpLanguageServer.Util

open System
open System.Runtime.InteropServices
open System.IO

let nonNull name (value: 'T when 'T: null) : 'T =
    if Object.ReferenceEquals(value, null) then
        raise (new Exception(sprintf "A non-null value was expected: %s" name))
    else
        value

let parseFileUri s : string = Uri(s).LocalPath

let tryParseFileUri s : string option =
    try
        let uri = Uri s
        Some uri.LocalPath
    with _ex ->
        None

let makeFileUri (path: string) : string =
    let fullPath = Path.GetFullPath path

    match RuntimeInformation.IsOSPlatform OSPlatform.Windows with
    | true -> "file:///" + fullPath
    | false -> "file://" + fullPath

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

// flip f takes its (first) two arguments in the reverse order of f, just like
// the function with the same name in Haskell.
let flip f x y = f y x

let curry f x y = f (x, y)
let uncurry f (x, y) = f x y


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

module Map =
    let union map1 map2 =
        Map.fold (fun acc key value -> Map.add key value acc) map1 map2
