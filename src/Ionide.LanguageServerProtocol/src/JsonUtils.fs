namespace Ionide.LanguageServerProtocol.JsonUtils

open Microsoft.FSharp.Reflection
open Newtonsoft.Json
open System
open System.Collections.Concurrent
open Ionide.LanguageServerProtocol.Types
open Newtonsoft.Json.Linq
open Newtonsoft.Json.Serialization
open System.Reflection
open Converters


/// Handles fields of type `Option`:
/// * Allows missing json properties when `Option` -> Optional
/// * Fails when missing json property when not `Option` -> Required
/// * Additional properties in json are always ignored
///
/// Example:
/// ```fsharp
/// type Data = { Name: string; Value: int option }
/// ```
/// ```json
/// { "name": "foo", "value": 42 }    // ok
/// { "name": "foo" }                 // ok
/// { "value": 42 }                   // error
/// {}                                // error
/// { "name": "foo", "data": "bar" }  // ok
/// ```
[<Sealed>]
type OptionAndCamelCasePropertyNamesContractResolver() as this =
  inherit CamelCasePropertyNamesContractResolver()

  do this.NamingStrategy.ProcessDictionaryKeys <- false

  let isOptionType (ty: Type) =
    ty.IsGenericType
    && ty.GetGenericTypeDefinition() = typedefof<Option<_>>

  override _.CreateProperty(memberInfo, memberSerialization) =
    // mutable properties in records have their corresponding field deserialized too
    // field has postfix `@`
    // -> exclude everything ending in `@` (-> ~ private fields)
    if memberInfo.Name.EndsWith "@" then
      null
    else
      let prop = ``base``.CreateProperty(memberInfo, memberSerialization)

      let shouldUpdateRequired =
        // change nothing when specified:
        // * `JsonProperty.Required`
        //    * Don't know if specified -> compare with `Default`
        match memberInfo.GetCustomAttribute<JsonPropertyAttribute>() with
        | null -> true
        | jp -> jp.Required = Required.Default

      if shouldUpdateRequired then
        if Type.isOption prop.PropertyType then
          prop.Required <- Required.Default
        else
          prop.Required <- Required.Always

      prop


/// Newtonsoft.Json parses parses a number inside quotations as number too:
/// `"42"` -> can be parsed to `42: int`
/// This converter prevents that. `"42"` cannot be parsed to `int` (or `float`) any more
[<Sealed>]
type StrictNumberConverter() =
  inherit JsonConverter()

  static let defaultSerializer = JsonSerializer()

  override _.CanConvert(t) =
    t
    |> Type.isNumeric

  override __.ReadJson(reader, t, _, serializer) =
    match reader.TokenType with
    | JsonToken.Integer
    | JsonToken.Float ->
      // cannot use `serializer`: Endless recursion into StrictNumberConverter for same value
      defaultSerializer.Deserialize(reader, t)
    | _ -> failwith $"Expected a number, but was {reader.TokenType}"

  override _.CanWrite = false
  override _.WriteJson(_, _, _) = raise (NotImplementedException())

/// Like `StrictNumberConverter`, but prevents numbers to be parsed as string:
/// `42` -> no quotation marks -> not a string
[<Sealed>]
type StrictStringConverter() =
  inherit JsonConverter()

  override _.CanConvert(t) =
    t
    |> Type.isString

  override __.ReadJson(reader, t, _, serializer) =
    match reader.TokenType with
    | JsonToken.String -> reader.Value
    | JsonToken.Null -> null
    | _ -> failwith $"Expected a string, but was {reader.TokenType}"

  override _.CanWrite = false
  override _.WriteJson(_, _, _) = raise (NotImplementedException())

/// Like `StrictNumberConverter`, but prevents boolean to be parsed as string:
/// `true` -> no quotation marks -> not a string
[<Sealed>]
type StrictBoolConverter() =
  inherit JsonConverter()

  override _.CanConvert(t) =
    t
    |> Type.isBool

  override __.ReadJson(reader, t, _, serializer) =
    match reader.TokenType with
    | JsonToken.Boolean -> reader.Value
    | _ -> failwith $"Expected a bool, but was {reader.TokenType}"

  override _.CanWrite = false
  override _.WriteJson(_, _, _) = raise (NotImplementedException())

[<Sealed>]
type ErasedUnionConverter() =
  inherit JsonConverter()

  let canConvert =
    memoriseByHash (fun t ->
      FSharpType.IsUnion t
      && (
      // Union
      t.GetCustomAttributes(typedefof<ErasedUnionAttribute>, false).Length > 0
      ||
      // Case
      t.BaseType.GetCustomAttributes(typedefof<ErasedUnionAttribute>, false).Length > 0)
    )

  override __.CanConvert(t) = canConvert t

  override __.WriteJson(writer, value, serializer) =
    let union = UnionInfo.get (value.GetType())
    let case = union.GetCaseOf value
    // Must be exactly 1 field
    // Deliberately fail here to signal incorrect usage
    // (vs. `CanConvert` = `false` -> silent and fallback to serialization with `case` & `fields`)
    match case.GetFieldValues value with
    | [| value |] -> serializer.Serialize(writer, value)
    | values -> failwith $"Expected exactly one field for case `{value.GetType().Name}`, but were {values.Length}"

  override __.ReadJson(reader: JsonReader, t, _existingValue, serializer) =
    let tryReadPrimitive (json: JToken) (targetType: Type) =
      if Type.isString targetType then
        if json.Type = JTokenType.String then
          reader.Value
          |> Some
        else
          None
      elif Type.isBool targetType then
        if json.Type = JTokenType.Boolean then
          reader.Value
          |> Some
        else
          None
      elif Type.isNumeric targetType then
        match json.Type with
        | JTokenType.Integer
        | JTokenType.Float ->
          json.ToObject(targetType, serializer)
          |> Some
        | _ -> None
      else
        None

    let tryReadUnionKind (json: JToken) (targetType: Type) =
      try
        let fields = json.Children<JProperty>()
        let props = targetType.GetProperties()

        match
          fields
          |> Seq.tryFind (fun f -> f.Name.ToLowerInvariant() = "kind"),
          props
          |> Seq.tryFind (fun p -> p.Name.ToLowerInvariant() = "kind")
        with
        | Some f, Some p ->
          match
            p.GetCustomAttribute(typeof<UnionKindAttribute>)
            |> Option.ofObj
          with
          | Some(:? UnionKindAttribute as k) when k.Value = string f.Value ->
            json.ToObject(targetType, serializer)
            |> Some
          | _ -> None
        | _ -> None
      with _ ->
        None

    let tryReadAllMatchingFields (json: JToken) (targetType: Type) =
      try
        let fields =
          json.Children<JProperty>()
          |> Seq.map (fun f -> f.Name.ToLowerInvariant())

        let props =
          targetType.GetProperties()
          |> Seq.map (fun p -> p.Name.ToLowerInvariant())

        if
          fields
          |> Seq.forall (fun f ->
            props
            |> Seq.contains f
          )
        then
          json.ToObject(targetType, serializer)
          |> Some
        else
          None
      with _ ->
        None

    let union = UnionInfo.get t
    let json = JToken.ReadFrom reader

    let tryMakeUnionCase tryReadValue (json: JToken) (case: CaseInfo) =
      match case.Fields with
      | [| field |] ->
        let ty = field.PropertyType

        match tryReadValue json ty with
        | None -> None
        | Some value ->
          case.Create [| value |]
          |> Some
      | fields ->
        failwith
          $"Expected union {case.Info.DeclaringType.Name} to have exactly one field in each case, but case {case.Info.Name} has {fields.Length} fields"

    let c =
      union.Cases
      |> Array.tryPick (tryMakeUnionCase tryReadPrimitive json)
      |> Option.orElseWith (fun () ->
        union.Cases
        |> Array.tryPick (tryMakeUnionCase tryReadUnionKind json)
      )
      |> Option.orElseWith (fun () ->
        union.Cases
        |> Array.tryPick (tryMakeUnionCase tryReadAllMatchingFields json)
      )

    match c with
    | None -> failwith $"Could not create an instance of the type '%s{t.Name}'"
    | Some c -> c

/// converter that can convert enum-style DUs
[<Sealed>]
type SingleCaseUnionConverter() =
  inherit JsonConverter()

  let canConvert =
    let allCases (t: System.Type) = FSharpType.GetUnionCases t

    memoriseByHash (fun t ->
      FSharpType.IsUnion t
      && allCases t
         |> Array.forall (fun c -> c.GetFields().Length = 0)
    )

  override _.CanConvert t = canConvert t

  override _.WriteJson(writer: Newtonsoft.Json.JsonWriter, value: obj, serializer: Newtonsoft.Json.JsonSerializer) =
    serializer.Serialize(writer, string value)

  override _.ReadJson(reader: Newtonsoft.Json.JsonReader, t, _existingValue, serializer) =
    let caseName = string reader.Value

    let union = UnionInfo.get t

    let case =
      union.Cases
      |> Array.tryFind (fun c -> c.Info.Name.Equals(caseName, StringComparison.OrdinalIgnoreCase))

    match case with
    | Some case -> case.Create [||]
    | None -> failwith $"Could not create an instance of the type '%s{t.Name}' with the name '%s{caseName}'"