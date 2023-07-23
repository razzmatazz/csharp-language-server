[<AutoOpen>]
module CSharpLanguageServer.Common.Extensions

open System.Collections.Concurrent


type ConcurrentDictionary<'key, 'value> with
    member this.TryFind key =
        match this.TryGetValue key with
        | true, value -> Some value
        | _ -> None
