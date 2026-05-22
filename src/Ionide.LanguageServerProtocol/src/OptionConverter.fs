namespace Ionide.LanguageServerProtocol.JsonUtils

open Microsoft.FSharp.Reflection
open System
open System.Collections.Concurrent
open System.Reflection


module internal Converters =
  open System.Collections.Concurrent

  let inline memorise (f: 'a -> 'b) : 'a -> 'b =
    let d = ConcurrentDictionary<'a, 'b>()
    fun key -> d.GetOrAdd(key, f)

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

  let get: Type -> _ = memorise (create)

module Type =
  let numerics = [|
    typeof<int>
    typeof<float>
    typeof<byte>
    typeof<uint>
  //ENHANCEMENT: other number types
  |]

  let inline isOption (t: Type) =
    t.IsGenericType
    && t.GetGenericTypeDefinition() = typedefof<_ option>

  let inline isString (t: Type) = t = typeof<string>
  let inline isBool (t: Type) = t = typeof<bool>

  let inline isNumeric (t: Type) =
    numerics
    |> Array.contains t


// Backing converter for UnitConverterFactory.
// Must be defined at module level — F# does not allow nested type definitions.
// We inherit JsonConverter<obj> rather than JsonConverter<unit> to avoid FS0017:
// when 'T = unit, F# unifies the abstract Read return type with the void-unit,
// which the compiler rejects as a signature mismatch.
// STJ invokes CreateConverter (below) and receives this converter; the factory's
// CanConvert guards ensure it is only ever applied to the `unit` type.
[<Sealed>]
type private UnitInnerConverter() =
  inherit System.Text.Json.Serialization.JsonConverter<obj>()

  override _.Read(reader, _t, _opts) =
    reader.Skip()
    null // unit has no runtime representation beyond null/()

  override _.Write(writer, _value, _opts) =
    writer.WriteNullValue()

  override _.CanConvert(t) = t = typeof<unit>

/// STJ JsonConverterFactory for F# unit type.
/// Reads (and discards) any JSON value — null, object, or primitive — and produces unit.
/// Writes unit as JSON null.
/// This is needed because STJ cannot construct Unit (it has no public parameterless constructor),
/// but LSP notification handlers that take `unit` params still go through the generic
/// `deserialize<unit>` path in wrapHandler.
[<Sealed>]
type UnitConverterFactory() =
  inherit System.Text.Json.Serialization.JsonConverterFactory()

  override _.CanConvert(t) = t = typeof<unit>

  override _.CreateConverter(_t, _opts) =
    UnitInnerConverter() :> System.Text.Json.Serialization.JsonConverter

/// STJ JsonConverter for F# option types.
/// None serializes as JSON null; Some v serializes as v.
type FSharpOptionConverter<'T>(options: System.Text.Json.JsonSerializerOptions) =
  inherit System.Text.Json.Serialization.JsonConverter<'T option>()

  override _.Read(reader, _, opts) =
    if reader.TokenType = System.Text.Json.JsonTokenType.Null then
      None
    else
      Some(System.Text.Json.JsonSerializer.Deserialize<'T>(&reader, opts))

  override _.Write(writer, value, opts) =
    match value with
    | None -> writer.WriteNullValue()
    | Some v -> System.Text.Json.JsonSerializer.Serialize(writer, v, opts)

/// STJ JsonConverterFactory that creates FSharpOptionConverter<'T> for any 'T option type.
[<Sealed>]
type FSharpOptionConverterFactory() =
  inherit System.Text.Json.Serialization.JsonConverterFactory()

  override _.CanConvert(t) = Type.isOption t

  override _.CreateConverter(t, opts) =
    let inner = t.GetGenericArguments().[0]
    let converterType = typedefof<FSharpOptionConverter<_>>.MakeGenericType(inner)
    Activator.CreateInstance(converterType, opts) :?> System.Text.Json.Serialization.JsonConverter

/// Newtonsoft JsonConverter for F# option types.
/// Kept for use by MetaModelGenerator (which uses Newtonsoft to parse metaModel.json).
[<Sealed>]
type OptionConverter() =
  inherit Newtonsoft.Json.JsonConverter()

  let getInnerType =
    memorise (fun (t: Type) ->
      let innerType = t.GetGenericArguments()[0]

      if innerType.IsValueType then
        typedefof<Nullable<_>>.MakeGenericType([| innerType |])
      else
        innerType
    )

  let canConvert = memorise (Type.isOption)

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
    | Newtonsoft.Json.JsonToken.Null -> null // = None
    | _ ->
      let innerType = getInnerType t

      let value = serializer.Deserialize(reader, innerType)

      if isNull value then
        null
      else
        let union = UnionInfo.get t
        union.Cases[1].Create [| value |]
