module CSharpLanguageServer.ReflectedTypes

open System
open System.Threading.Tasks

open CSharpLanguageServer.Util

type TaskOfType (taskType: Type) =
    static let fromResultMethod =
        typeof<Task>.GetMethod("FromResult")
        |> nonNull (sprintf "%s.FromResult()" (string typeof<Task>))
    let typedFromResultMethod = fromResultMethod.MakeGenericMethod([| taskType |])

    member __.FromResult(resultValue: obj | null) =
        typedFromResultMethod.Invoke(null, [| resultValue |])
