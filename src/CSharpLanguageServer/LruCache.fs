namespace CSharpLanguageServer

// It's more OO than functional, but it makes user happier than a functional & immutable version.
// In LSP, there will not be lot of cocurrent requests, so a simple cache based on LRU should be enough, and the size is
// usually small.
type LruCache<'entry>(size: int) =

    let lockObj : obj = new obj()

    let mutable key = 0UL

    let mutable list : (uint64 * 'entry) list = list.Empty

    member this.add(entry: 'entry) : uint64 =
        lock lockObj (fun () ->
            let k = key
            key <- key + 1UL
            list <- List.append list [(k, entry)]
            if List.length list > size then
                list <- List.tail list
            k)

    member this.get(key: uint64) : 'entry option =
        lock lockObj (fun () -> List.tryFind (fun (k, _) -> k = key) list |> Option.map snd)
