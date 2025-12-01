module CSharpLanguageServer.Util

open System
open System.Threading.Tasks

let nonNull name (value: 'T when 'T: null) : 'T =
    if Object.ReferenceEquals(value, null) then
        raise (new Exception(sprintf "A non-null value was expected: %s" name))
    else
        value

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
