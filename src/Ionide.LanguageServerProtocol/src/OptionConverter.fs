namespace Ionide.LanguageServerProtocol.JsonUtils

open Newtonsoft.Json
open Microsoft.FSharp.Reflection
open System
open System.Collections.Concurrent
open System.Reflection


module internal Converters =
  open System.Collections.Concurrent

  let inline memorise (f: 'a -> 'b) : 'a -> 'b =
    let d = ConcurrentDictionary<'a, 'b>()
    fun key -> d.GetOrAdd(key, f)

  let inline memoriseByHash (f: 'a -> 'b) : 'a -> 'b =
    let d = ConcurrentDictionary<int, 'b>()

    fun key ->
      let hash = key.GetHashCode()

      match d.TryGetValue(hash) with
      | (true, value) -> value
      | _ ->
        let value = f key

        d.TryAdd(hash, value)
        |> ignore

        value

open Converters


type private CaseInfo = {
  Info: UnionCaseInfo
  Fields: PropertyInfo[]
  GetFieldValues: obj -> obj[]
  Create: obj[] -> obj
}

type private UnionInfo = {
  Cases: CaseInfo[]
  GetTag: obj -> int
} with

  member u.GetCaseOf(value: obj) =
    let tag = u.GetTag value

    u.Cases
    |> Array.find (fun case -> case.Info.Tag = tag)

module private UnionInfo =

  let private create (ty: Type) =
    assert
      (ty
       |> FSharpType.IsUnion)

    let cases =
      FSharpType.GetUnionCases ty
      |> Array.map (fun case -> {
        Info = case
        Fields = case.GetFields()
        GetFieldValues = FSharpValue.PreComputeUnionReader case
        Create = FSharpValue.PreComputeUnionConstructor case
      })

    { Cases = cases; GetTag = FSharpValue.PreComputeUnionTagReader ty }

  let get: Type -> _ = memoriseByHash (create)

module Type =
  let numerics = [|
    typeof<int>
    typeof<float>
    typeof<byte>
    typeof<uint>
  //ENHANCEMENT: other number types
  |]

  let numericHashes =
    numerics
    |> Array.map (fun t -> t.GetHashCode())

  let stringHash = typeof<string>.GetHashCode()
  let boolHash = typeof<bool>.GetHashCode()

  let inline isOption (t: Type) =
    t.IsGenericType
    && t.GetGenericTypeDefinition() = typedefof<_ option>

  let inline isString (t: Type) = t.GetHashCode() = stringHash
  let inline isBool (t: Type) = t.GetHashCode() = boolHash

  let inline isNumeric (t: Type) =
    let hash = t.GetHashCode()

    numericHashes
    |> Array.contains hash

[<Sealed>]
type OptionConverter() =
  inherit JsonConverter()

  let getInnerType =
    memoriseByHash (fun (t: Type) ->
      let innerType = t.GetGenericArguments()[0]

      if innerType.IsValueType then
        typedefof<Nullable<_>>.MakeGenericType([| innerType |])
      else
        innerType
    )

  let canConvert = memoriseByHash (Type.isOption)

  override __.CanConvert(t) = canConvert t

  override __.WriteJson(writer, value, serializer) =
    let value =
      if isNull value then
        null
      else
        let union = UnionInfo.get (value.GetType())
        let case = union.GetCaseOf value

        case.GetFieldValues value
        |> Array.head

    serializer.Serialize(writer, value)

  override __.ReadJson(reader, t, _existingValue, serializer) =
    match reader.TokenType with
    | JsonToken.Null -> null // = None
    | _ ->
      let innerType = getInnerType t

      let value = serializer.Deserialize(reader, innerType)

      if isNull value then
        null
      else
        let union = UnionInfo.get t
        union.Cases[1].Create [| value |]