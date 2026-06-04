namespace Ionide.LanguageServerProtocol.JsonUtils

open Microsoft.FSharp.Reflection
open System
open System.Text.Json
open System.Text.Json.Serialization
open System.Reflection
open Ionide.LanguageServerProtocol.Types
open Converters


/// Newtonsoft ErasedUnionConverter kept for the legacy Client module in LanguageServerProtocol.fs.
/// Do not use in new code — prefer ErasedUnionConverterFactory (STJ).
[<Sealed>]
type ErasedUnionConverter() =
  inherit Newtonsoft.Json.JsonConverter()

  let canConvert =
    memorise (fun t ->
      FSharpType.IsUnion t
      && (
        t.GetCustomAttributes(typedefof<ErasedUnionAttribute>, false).Length > 0
        ||
        t.BaseType <> null
        && t.BaseType.GetCustomAttributes(typedefof<ErasedUnionAttribute>, false).Length > 0
      )
    )

  override __.CanConvert(t) = canConvert t

  override __.WriteJson(writer, value, serializer) =
    let union = UnionInfo.get (value.GetType())
    let case = union.GetCaseOf value
    match case.GetFieldValues value with
    | [| v |] -> serializer.Serialize(writer, v)
    | values ->
      failwith $"Expected exactly one field for case `{value.GetType().Name}`, but were {values.Length}"

  override __.ReadJson(reader, t, _existingValue, serializer) =
    let json = Newtonsoft.Json.Linq.JToken.ReadFrom reader

    let tryReadPrimitive (json: Newtonsoft.Json.Linq.JToken) (targetType: Type) =
      if Type.isString targetType then
        if json.Type = Newtonsoft.Json.Linq.JTokenType.String then reader.Value |> Some
        else None
      elif Type.isBool targetType then
        if json.Type = Newtonsoft.Json.Linq.JTokenType.Boolean then reader.Value |> Some
        else None
      elif Type.isNumeric targetType then
        match json.Type with
        | Newtonsoft.Json.Linq.JTokenType.Integer
        | Newtonsoft.Json.Linq.JTokenType.Float ->
          json.ToObject(targetType, serializer) |> Some
        | _ -> None
      else
        None

    let tryReadAllMatchingFields (json: Newtonsoft.Json.Linq.JToken) (targetType: Type) =
      try
        let fields =
          json.Children<Newtonsoft.Json.Linq.JProperty>()
          |> Seq.map (fun f -> f.Name.ToLowerInvariant())
        let props = targetType.GetProperties() |> Seq.map (fun p -> p.Name.ToLowerInvariant())
        if fields |> Seq.forall (fun f -> props |> Seq.contains f) then
          json.ToObject(targetType, serializer) |> Some
        else
          None
      with _ ->
        None

    let union = UnionInfo.get t

    let tryMakeUnionCase tryReadValue (json: Newtonsoft.Json.Linq.JToken) (case: CaseInfo) =
      match case.Fields with
      | [| field |] ->
        match tryReadValue json field.PropertyType with
        | None -> None
        | Some value -> case.Create [| value |] |> Some
      | fields ->
        failwith
          $"Expected union {case.Info.DeclaringType.Name} to have exactly one field in each case, but case {case.Info.Name} has {fields.Length} fields"

    let c =
      union.Cases
      |> Array.tryPick (tryMakeUnionCase tryReadPrimitive json)
      |> Option.orElseWith (fun () ->
        union.Cases
        |> Array.tryPick (tryMakeUnionCase tryReadAllMatchingFields json)
      )

    match c with
    | None -> failwith $"Could not create an instance of the type '%s{t.Name}'"
    | Some c -> c


/// STJ JsonConverter for .NET enum types whose cases carry [<EnumMember(Value = "...")>] attributes.
/// These LSP enums are serialized as strings (e.g. ResourceOperationKind: "create"/"rename"/"delete"),
/// not as integers. Reads case-insensitively against the EnumMember Value; writes the Value string.
type EnumMemberConverter<'T when 'T : struct and 'T :> Enum and 'T : equality>(options: JsonSerializerOptions) =
  inherit JsonConverter<'T>()

  // Build a bidirectional map between the string Value and the enum case.
  let enumType = typeof<'T>

  let valueToEnum: System.Collections.Generic.Dictionary<string, 'T> =
    let d = System.Collections.Generic.Dictionary<string, 'T>(StringComparer.OrdinalIgnoreCase)
    for case in Enum.GetValues(enumType) :?> 'T[] do
      let field = enumType.GetField(string case)
      let attr =
        field.GetCustomAttribute(typeof<System.Runtime.Serialization.EnumMemberAttribute>)
        :?> System.Runtime.Serialization.EnumMemberAttribute
      let key = if attr <> null && attr.Value <> null then attr.Value else string case
      d.[key] <- case
    d

  let enumToValue: System.Collections.Generic.Dictionary<'T, string> =
    let d = System.Collections.Generic.Dictionary<'T, string>()
    for case in Enum.GetValues(enumType) :?> 'T[] do
      let field = enumType.GetField(string case)
      let attr =
        field.GetCustomAttribute(typeof<System.Runtime.Serialization.EnumMemberAttribute>)
        :?> System.Runtime.Serialization.EnumMemberAttribute
      let value = if attr <> null && attr.Value <> null then attr.Value else string case
      d.[case] <- value
    d

  override _.Read(reader, _t, _opts) =
    let s = reader.GetString()
    match valueToEnum.TryGetValue(s) with
    | true, v -> v
    | _ ->
      failwith $"Could not convert string '{s}' to enum type '{enumType.Name}'"

  override _.Write(writer, value, _opts) =
    match enumToValue.TryGetValue(value) with
    | true, s -> writer.WriteStringValue(s)
    | _ -> writer.WriteStringValue(string value)

/// STJ JsonConverterFactory that creates EnumMemberConverter<'T> for any .NET enum type
/// where at least one case carries a [<EnumMember>] attribute (i.e. string-valued LSP enums).
[<Sealed>]
type EnumMemberConverterFactory() =
  inherit JsonConverterFactory()

  let hasEnumMemberAttr =
    memorise (fun (t: Type) ->
      t.IsEnum
      && t.GetFields(BindingFlags.Public ||| BindingFlags.Static)
         |> Array.exists (fun f ->
           f.GetCustomAttribute(typeof<System.Runtime.Serialization.EnumMemberAttribute>) <> null
         )
    )

  override _.CanConvert(t) = hasEnumMemberAttr t

  override _.CreateConverter(t, opts) =
    let converterType = typedefof<EnumMemberConverter<System.DayOfWeek>>.GetGenericTypeDefinition().MakeGenericType(t)
    Activator.CreateInstance(converterType, opts) :?> System.Text.Json.Serialization.JsonConverter


/// STJ JsonConverter for F# single-case (enum-style) discriminated unions.
/// Writes the union case name as a string; reads by case-insensitive name match.
type SingleCaseUnionConverter<'T>(options: JsonSerializerOptions) =
  inherit JsonConverter<'T>()

  override _.Read(reader, t, _opts) =
    let caseName = reader.GetString()
    let union = UnionInfo.get t

    let case =
      union.Cases
      |> Array.tryFind (fun c -> c.Info.Name.Equals(caseName, StringComparison.OrdinalIgnoreCase))

    match case with
    | Some case -> case.Create [||] :?> 'T
    | None ->
      failwith $"Could not create an instance of the type '%s{t.Name}' with the name '%s{caseName}'"

  override _.Write(writer, value, _opts) =
    writer.WriteStringValue(string (box value))

/// STJ JsonConverterFactory that creates SingleCaseUnionConverter<'T> for zero-field F# union types.
[<Sealed>]
type SingleCaseUnionConverterFactory() =
  inherit JsonConverterFactory()

  let canConvert =
    memorise (fun t ->
      FSharpType.IsUnion t
      && FSharpType.GetUnionCases t
         |> Array.forall (fun c -> c.GetFields().Length = 0)
    )

  override _.CanConvert(t) = canConvert t

  override _.CreateConverter(t, opts) =
    let converterType = typedefof<SingleCaseUnionConverter<_>>.MakeGenericType(t)
    Activator.CreateInstance(converterType, opts) :?> System.Text.Json.Serialization.JsonConverter


/// STJ JsonConverter for erased-union F# types (U2, U3, U4, and types marked [<ErasedUnion>]).
/// Reads by trying each union case in order: primitives first, then UnionKind, then field-subset.
/// Writes by unwrapping the single field value and serializing it.
type ErasedUnionConverter<'T>(options: JsonSerializerOptions) =
  inherit JsonConverter<'T>()

  override _.Read(reader, t, opts) =
    // Capture the token upfront — Utf8JsonReader is forward-only and can't be rewound.
    use doc = JsonDocument.ParseValue(&reader)
    let json = doc.RootElement.Clone()

    let union = UnionInfo.get t

    let tryReadPrimitive (json: JsonElement) (targetType: Type) =
      if Type.isString targetType then
        if json.ValueKind = JsonValueKind.String then
          json.GetString() :> obj |> Some
        else
          None
      elif Type.isBool targetType then
        if json.ValueKind = JsonValueKind.True then
          (true :> obj) |> Some
        elif json.ValueKind = JsonValueKind.False then
          (false :> obj) |> Some
        else
          None
      elif Type.isNumeric targetType then
        match json.ValueKind with
        | JsonValueKind.Number ->
          try
            JsonSerializer.Deserialize(json, targetType, opts) |> Some
          with :? JsonException ->
            None
        | _ -> None
      else
        None

    let tryReadUnionKind (json: JsonElement) (targetType: Type) =
      try
        if json.ValueKind <> JsonValueKind.Object then
          None
        else
          let props = targetType.GetProperties()

          let kindField =
            match json.TryGetProperty("kind") with
            | true, v -> Some v
            | _ ->
              // case-insensitive search
              json.EnumerateObject()
              |> Seq.tryFind (fun p ->
                p.Name.Equals("kind", StringComparison.OrdinalIgnoreCase)
              )
              |> Option.map (fun p -> p.Value)

          let kindProp =
            props
            |> Seq.tryFind (fun p ->
              p.Name.Equals("kind", StringComparison.OrdinalIgnoreCase)
            )

          match kindField, kindProp with
          | Some f, Some p ->
            match
              p.GetCustomAttribute(typeof<UnionKindAttribute>)
              |> Option.ofObj
            with
            | Some (:? UnionKindAttribute as k) when
              f.ValueKind = JsonValueKind.String && k.Value = f.GetString()
              ->
              JsonSerializer.Deserialize(json, targetType, opts) |> Some
            | _ -> None
          | _ -> None
      with :? JsonException ->
        None

    let tryReadArray (json: JsonElement) (targetType: Type) =
      try
        if json.ValueKind <> JsonValueKind.Array then
          None
        elif targetType.IsArray then
          // For plain array types, guard against false positives by checking that every
          // field in the first JSON element is a known property of the element type.
          // Without this, e.g. DocumentSymbol[] elements would silently deserialize as
          // SymbolInformation[] because STJ ignores unknown properties by default, and
          // SymbolInformation appears first in the union case ordering.
          let elemType = targetType.GetElementType()
          let firstElemOk =
            match json.EnumerateArray() |> Seq.tryHead with
            | None -> true // empty array — always valid
            | Some firstElem ->
              if firstElem.ValueKind <> JsonValueKind.Object then
                true // primitive/bool/number elements — no field check needed
              else
                let propNames =
                  elemType.GetProperties()
                  |> Seq.map (fun p -> p.Name.ToLowerInvariant())
                  |> Set.ofSeq
                firstElem.EnumerateObject()
                |> Seq.forall (fun p -> propNames |> Set.contains (p.Name.ToLowerInvariant()))
          if firstElemOk then
            JsonSerializer.Deserialize(json, targetType, opts) |> Some
          else
            None
        else
          // For erased-union types that wrap an array (e.g. Definition = U2<Location, Location[]>),
          // delegate to the recursive ErasedUnionConverter — it will handle nested dispatch.
          JsonSerializer.Deserialize(json, targetType, opts) |> Some
      with :? JsonException ->
        None

    // Exact match: every JSON field maps to a type property AND every type property
    // appears in the JSON.  This wins over the subset check below so that, e.g.,
    // U2<{range;rangeLength;text}, {text}> correctly picks the second case for
    // JSON {"text":""} instead of the first.
    let tryReadExactMatchingFields (json: JsonElement) (targetType: Type) =
      try
        if json.ValueKind <> JsonValueKind.Object then
          None
        else
          let fieldNames =
            json.EnumerateObject()
            |> Seq.map (fun p -> p.Name.ToLowerInvariant())
            |> Set.ofSeq

          let propNames =
            targetType.GetProperties()
            |> Seq.map (fun p -> p.Name.ToLowerInvariant())
            |> Set.ofSeq

          if fieldNames = propNames then
            JsonSerializer.Deserialize(json, targetType, opts) |> Some
          else
            None
      with :? JsonException ->
        None

    // Subset match: every JSON field maps to a type property (type may have more).
    let tryReadAllMatchingFields (json: JsonElement) (targetType: Type) =
      try
        if json.ValueKind <> JsonValueKind.Object then
          None
        else
          let fieldNames =
            json.EnumerateObject()
            |> Seq.map (fun p -> p.Name.ToLowerInvariant())
            |> Set.ofSeq

          let propNames =
            targetType.GetProperties()
            |> Seq.map (fun p -> p.Name.ToLowerInvariant())
            |> Set.ofSeq

          if fieldNames |> Set.forall (fun f -> propNames |> Set.contains f) then
            JsonSerializer.Deserialize(json, targetType, opts) |> Some
          else
            None
      with :? JsonException ->
        None

    let tryMakeUnionCase tryReadValue (json: JsonElement) (case: CaseInfo) =
      match case.Fields with
      | [| field |] ->
        let ty = field.PropertyType

        match tryReadValue json ty with
        | None -> None
        | Some value ->
          case.Create [| value |] |> Some
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
        |> Array.tryPick (tryMakeUnionCase tryReadArray json)
      )
      |> Option.orElseWith (fun () ->
        union.Cases
        |> Array.tryPick (tryMakeUnionCase tryReadExactMatchingFields json)
      )
      |> Option.orElseWith (fun () ->
        union.Cases
        |> Array.tryPick (tryMakeUnionCase tryReadAllMatchingFields json)
      )

    match c with
    | None -> failwith $"Could not create an instance of the type '%s{t.Name}'"
    | Some c -> c :?> 'T

  override _.Write(writer, value, opts) =
    let union = UnionInfo.get (value.GetType())
    let case = union.GetCaseOf(value :> obj)

    match case.GetFieldValues(value :> obj) with
    | [| v |] -> JsonSerializer.Serialize(writer, v, opts)
    | values ->
      failwith
        $"Expected exactly one field for case `{(value :> obj).GetType().Name}`, but were {values.Length}"

/// STJ JsonConverterFactory that creates ErasedUnionConverter<'T> for erased union types.
[<Sealed>]
type ErasedUnionConverterFactory() =
  inherit JsonConverterFactory()

  let canConvert =
    memorise (fun t ->
      FSharpType.IsUnion t
      && (
        t.GetCustomAttributes(typedefof<ErasedUnionAttribute>, false).Length > 0
        ||
        t.BaseType <> null
        && t.BaseType.GetCustomAttributes(typedefof<ErasedUnionAttribute>, false).Length > 0
      )
    )

  override _.CanConvert(t) = canConvert t

  override _.CreateConverter(t, opts) =
    let converterType = typedefof<ErasedUnionConverter<_>>.MakeGenericType(t)
    Activator.CreateInstance(converterType, opts) :?> System.Text.Json.Serialization.JsonConverter
