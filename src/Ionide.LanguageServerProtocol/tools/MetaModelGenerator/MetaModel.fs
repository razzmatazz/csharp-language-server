namespace MetaModelGenerator


type StructuredDocs = string list

module StructuredDocs =
  let parse (s: string) =
    s.Trim('\n').Split([| '\n' |])
    |> Array.toList

module Proposed =
  let skipProposed = true

  let inline checkProposed x =
    if skipProposed then
      (^a: (member Proposed: bool option) x)
      <> Some true
    else
      true

module rec MetaModel =
  open System
  open System.Text.Json
  open System.Text.Json.Serialization

  type MetaData = { Version: string }

  /// Indicates in which direction a message is sent in the protocol.
  [<JsonConverter(typeof<JsonStringEnumConverter>)>]
  type MessageDirection =
    | ClientToServer = 0
    | ServerToClient = 1
    | Both = 2

  /// Represents a LSP request
  type Request = {
    /// Whether the request is deprecated or not. If deprecated the property contains the deprecation message."
    Deprecated: string option
    /// An optional documentation;
    Documentation: string option
    /// An optional error data type.
    ErrorData: Type option
    /// The direction in which this request is sent in the protocol.
    MessageDirection: MessageDirection
    /// The request's method name.
    Method: string
    /// The parameter type(s) if any.
    Params: Type array option
    /// Optional partial result type if the request supports partial result reporting.
    PartialResult: Type option
    /// Whether this is a proposed feature. If omitted the feature is final.",
    Proposed: bool option
    /// Optional a dynamic registration method if it different from the request's method."
    RegistrationMethod: string option
    /// Optional registration options if the request supports dynamic registration."
    RegistrationOptions: Type option
    /// The result type.
    Result: Type
    /// Since when (release number) this request is available. Is undefined if not known.",
    Since: string option
  } with

    member x.ParamsSafe =
      x.Params
      |> Option.Array.toArray

    member x.StructuredDocs =
      x.Documentation
      |> Option.map StructuredDocs.parse

  /// Represents a LSP notification
  type Notification = {

    /// Whether the notification is deprecated or not. If deprecated the property contains the deprecation message."
    Deprecated: string option
    /// An optional documentation;
    Documentation: string option
    /// The direction in which this notification is sent in the protocol.
    MessageDirection: MessageDirection
    /// The request's method name.
    Method: string
    /// The parameter type(s) if any.
    Params: Type array option
    /// Whether this is a proposed feature. If omitted the notification is final.",
    Proposed: bool option
    /// Optional a dynamic registration method if it different from the request's method."
    RegistrationMethod: string option
    /// Optional registration options if the notification supports dynamic registration."
    RegistrationOptions: Type option
    /// Since when (release number) this notification is available. Is undefined if not known.",
    Since: string option
  } with

    member x.ParamsSafe =
      x.Params
      |> Option.Array.toArray

    member x.StructuredDocs =
      x.Documentation
      |> Option.map StructuredDocs.parse

  [<RequireQualifiedAccess>]
  type BaseTypes =
    | Uri
    | DocumentUri
    | Integer
    | Uinteger
    | Decimal
    | RegExp
    | String
    | Boolean
    | Null

    static member Parse(s: string) =
      match s with
      | "URI" -> Uri
      | "DocumentUri" -> DocumentUri
      | "integer" -> Integer
      | "uinteger" -> Uinteger
      | "decimal" -> Decimal
      | "RegExp" -> RegExp
      | "string" -> String
      | "boolean" -> Boolean
      | "null" -> Null
      | _ -> failwithf "Unknown base type: %s" s

    member x.ToDotNetType() =
      match x with
      | Uri -> "URI"
      | DocumentUri -> "DocumentUri"
      | Integer -> "int32"
      | Uinteger -> "uint32"
      | Decimal -> "decimal"
      | RegExp -> "RegExp"
      | String -> "string"
      | Boolean -> "bool"
      | Null -> "null"

  [<Literal>]
  let BaseTypeConst = "base"

  type BaseType = { Kind: string; Name: BaseTypes }

  [<Literal>]
  let ReferenceTypeConst = "reference"

  type ReferenceType = { Kind: string; Name: string }

  [<Literal>]
  let ArrayTypeConst = "array"

  type ArrayType = { Kind: string; Element: Type }

  [<Literal>]
  let MapTypeConst = "map"

  type MapType = { Kind: string; Key: MapKeyType; Value: Type }

  [<RequireQualifiedAccess>]
  type MapKeyNameEnum =
    | Uri
    | DocumentUri
    | String
    | Integer

    static member Parse(s: string) =
      match s with
      | "URI" -> Uri
      | "DocumentUri" -> DocumentUri
      | "string" -> String
      | "integer" -> Integer
      | _ -> failwithf "Unknown map key name: %s" s

    member x.ToDotNetType() =
      match x with
      | Uri -> "URI"
      | DocumentUri -> "DocumentUri"
      | String -> "string"
      | Integer -> "int32"

  [<RequireQualifiedAccess>]
  type MapKeyType =
    | ReferenceType of ReferenceType
    | Base of {| Kind: string; Name: MapKeyNameEnum |}

  [<Literal>]
  let AndTypeConst = "and"

  type AndType = { Kind: string; Items: Type array }

  [<Literal>]
  let OrTypeConst = "or"

  type OrType = { Kind: string; Items: Type array }

  [<Literal>]
  let TupleTypeConst = "tuple"

  type TupleType = { Kind: string; Items: Type array }

  type Property = {
    Deprecated: string option
    Documentation: string option
    Name: string
    Optional: bool option
    Proposed: bool option
    Required: bool option
    Since: string option
    Type: Type
  } with

    member x.IsOptional =
      x.Optional
      |> Option.defaultValue false

    member x.NameAsPascalCase = String.toPascalCase x.Name

    member x.StructuredDocs =
      x.Documentation
      |> Option.map StructuredDocs.parse


  [<Literal>]
  let StructureTypeLiteral = "literal"

  type StructureLiteral = {
    Deprecated: string option
    Documentation: string option
    Properties: Property array
    Proposed: bool option
    Since: string option
  } with

    member x.StructuredDocs =
      x.Documentation
      |> Option.map StructuredDocs.parse

    member x.PropertiesSafe =
      x.Properties
      |> Array.filter Proposed.checkProposed

  type StructureLiteralType = { Kind: string; Value: StructureLiteral }

  [<Literal>]
  let StringLiteralTypeConst = "stringLiteral"

  type StringLiteralType = { Kind: string; Value: string }

  [<Literal>]
  let IntegerLiteralTypeConst = "integerLiteral"

  type IntegerLiteralType = { Kind: string; Value: decimal }

  [<Literal>]
  let BooleanLiteralTypeConst = "booleanLiteral"

  type BooleanLiteralType = { Kind: string; Value: bool }

  [<RequireQualifiedAccess>]
  type Type =
    | BaseType of BaseType
    | ReferenceType of ReferenceType
    | ArrayType of ArrayType
    | MapType of MapType
    | AndType of AndType
    | OrType of OrType
    | TupleType of TupleType
    | StructureLiteralType of StructureLiteralType
    | StringLiteralType of StringLiteralType
    | IntegerLiteralType of IntegerLiteralType
    | BooleanLiteralType of BooleanLiteralType

    member x.isStructureLiteralType =
      match x with
      | StructureLiteralType _ -> true
      | _ -> false


  type Structure = {
    Deprecated: string option
    Documentation: string option
    Extends: Type array option
    Mixins: Type array option
    Name: string
    Properties: Property array option
    Proposed: bool option
    Since: string option
  } with

    member x.ExtendsSafe = Option.Array.toArray x.Extends
    member x.MixinsSafe = Option.Array.toArray x.Mixins

    member x.PropertiesSafe =
      Option.Array.toArray x.Properties
      |> Seq.filter Proposed.checkProposed

    member x.StructuredDocs =
      x.Documentation
      |> Option.map StructuredDocs.parse

  type TypeAlias = {
    Deprecated: string option
    Documentation: string option
    Name: string
    Proposed: bool option
    Since: string option
    Type: Type
  } with

    member x.StructuredDocs =
      x.Documentation
      |> Option.map StructuredDocs.parse

  [<JsonConverter(typeof<JsonStringEnumConverter>)>]
  type EnumerationTypeNameValues =
    | String = 0
    | Integer = 1
    | Uinteger = 2

  type EnumerationType = { Kind: string; Name: EnumerationTypeNameValues }

  type EnumerationEntry = {
    Deprecated: string option
    Documentation: string option

    Name: string
    Proposed: bool option
    Since: string option
    [<JsonConverter(typeof<Converters.StringOrNumberAsStringConverter>)>]
    Value: string
  } with

    member x.StructuredDocs =
      x.Documentation
      |> Option.map StructuredDocs.parse

  type Enumeration = {
    Deprecated: string option
    Documentation: string option
    Name: string
    Proposed: bool option
    Since: string option
    SupportsCustomValues: bool option
    Type: EnumerationType
    Values: EnumerationEntry array
  } with

    member x.StructuredDocs =
      x.Documentation
      |> Option.map StructuredDocs.parse

    member x.ValuesSafe =
      x.Values
      |> Array.filter Proposed.checkProposed

  type MetaModel = {
    MetaData: MetaData
    Requests: Request array
    Notifications: Notification array
    Structures: Structure array
    TypeAliases: TypeAlias array
    Enumerations: Enumeration array
  } with

    member x.StructuresSafe =
      x.Structures
      |> Array.filter Proposed.checkProposed

    member x.TypeAliasesSafe =
      x.TypeAliases
      |> Array.filter Proposed.checkProposed

    member x.EnumerationsSafe =
      x.Enumerations
      |> Array.filter Proposed.checkProposed

  module Converters =

    /// Reads a JSON value that is either a JSON string or a JSON number, returning it as a string.
    /// This is needed because integer/uinteger enumeration values are serialised as numbers in metaModel.json.
    type StringOrNumberAsStringConverter() =
      inherit JsonConverter<string>()

      override _.Read(reader: byref<Utf8JsonReader>, _typeToConvert: System.Type, _options: JsonSerializerOptions) =
        match reader.TokenType with
        | JsonTokenType.String -> reader.GetString()
        | JsonTokenType.Number ->
          // Preserve the raw number token as a string (e.g. "-32700")
          reader.GetInt64()
          |> string
        | other -> failwithf "StringOrNumberAsStringConverter: unexpected token %A" other

      override _.Write(writer: Utf8JsonWriter, value: string, _options: JsonSerializerOptions) =
        writer.WriteStringValue(value)

    type MapKeyTypeConverter() =
      inherit JsonConverter<MapKeyType>()

      override _.Read(reader: byref<Utf8JsonReader>, _typeToConvert: System.Type, options: JsonSerializerOptions) =
        use doc = JsonDocument.ParseValue(&reader)
        let root = doc.RootElement
        let kind = root.GetProperty("kind").GetString()

        match kind with
        | ReferenceTypeConst ->
          let name = root.GetProperty("name").GetString()
          MapKeyType.ReferenceType { Kind = kind; Name = name }
        | "base" ->
          let name = root.GetProperty("name").GetString()
          MapKeyType.Base {| Kind = kind; Name = MapKeyNameEnum.Parse name |}
        | _ -> failwithf "Unknown map key type: %s" kind

      override _.Write(_writer, _value, _options) =
        failwith "Should never be writing this structure, it comes from Microsoft LSP Spec"

    /// Reads a value that may be either a single item or an array of items,
    /// deserializing it as 'T array option (None when the JSON token is null).
    type SingleOrArrayConverter<'T>() =
      inherit JsonConverter<'T array option>()

      override _.Read(reader: byref<Utf8JsonReader>, _typeToConvert: System.Type, options: JsonSerializerOptions) =
        match reader.TokenType with
        | JsonTokenType.Null -> None
        | JsonTokenType.StartArray ->
          let arr = JsonSerializer.Deserialize<'T array>(&reader, options)
          Some arr
        | _ ->
          let item = JsonSerializer.Deserialize<'T>(&reader, options)
          Some [| item |]

      override _.Write(_writer, _value, _options) =
        failwith "Should never be writing this structure, it comes from Microsoft LSP Spec"

    type TypeConverter() =
      inherit JsonConverter<Type>()

      override _.Read(reader: byref<Utf8JsonReader>, _typeToConvert: System.Type, options: JsonSerializerOptions) =
        use doc = JsonDocument.ParseValue(&reader)
        let root = doc.RootElement
        let kind = root.GetProperty("kind").GetString()
        // Helper: re-serialise a sub-element and deserialize as 'T
        let deser (t: System.Type) (el: JsonElement) = JsonSerializer.Deserialize(el.GetRawText(), t, options)

        match kind with
        | BaseTypeConst ->
          let name = root.GetProperty("name").GetString()
          Type.BaseType { Kind = kind; Name = BaseTypes.Parse name }
        | ReferenceTypeConst ->
          let name = root.GetProperty("name").GetString()
          Type.ReferenceType { Kind = kind; Name = name }
        | ArrayTypeConst ->
          let element = deser typeof<Type> (root.GetProperty("element")) :?> Type
          Type.ArrayType { Kind = kind; Element = element }
        | MapTypeConst ->
          let key = deser typeof<MapKeyType> (root.GetProperty("key")) :?> MapKeyType
          let value = deser typeof<Type> (root.GetProperty("value")) :?> Type
          Type.MapType { Kind = kind; Key = key; Value = value }
        | AndTypeConst ->
          let items = deser typeof<Type[]> (root.GetProperty("items")) :?> Type[]
          Type.AndType { Kind = kind; Items = items }
        | OrTypeConst ->
          let items = deser typeof<Type[]> (root.GetProperty("items")) :?> Type[]
          Type.OrType { Kind = kind; Items = items }
        | TupleTypeConst ->
          let items = deser typeof<Type[]> (root.GetProperty("items")) :?> Type[]
          Type.TupleType { Kind = kind; Items = items }
        | StructureTypeLiteral ->
          let value = deser typeof<StructureLiteral> (root.GetProperty("value")) :?> StructureLiteral
          Type.StructureLiteralType { Kind = kind; Value = value }
        | StringLiteralTypeConst ->
          let value = root.GetProperty("value").GetString()
          Type.StringLiteralType { Kind = kind; Value = value }
        | IntegerLiteralTypeConst ->
          let value = root.GetProperty("value").GetDecimal()
          Type.IntegerLiteralType { Kind = kind; Value = value }
        | BooleanLiteralTypeConst ->
          let value = root.GetProperty("value").GetBoolean()
          Type.BooleanLiteralType { Kind = kind; Value = value }
        | _ -> failwithf "Unknown type kind: %s" kind

      override _.Write(_writer, _value, _options) =
        failwith "Should never be writing this structure, it comes from Microsoft LSP Spec"

    /// STJ converter for F# option types.
    type OptionConverter<'T>() =
      inherit JsonConverter<'T option>()

      override _.Read(reader: byref<Utf8JsonReader>, _typeToConvert: System.Type, options: JsonSerializerOptions) =
        match reader.TokenType with
        | JsonTokenType.Null -> None
        | _ -> Some(JsonSerializer.Deserialize<'T>(&reader, options))

      override _.Write(writer: Utf8JsonWriter, value: 'T option, options: JsonSerializerOptions) =
        match value with
        | None -> writer.WriteNullValue()
        | Some v -> JsonSerializer.Serialize(writer, v, options)

    type OptionConverterFactory() =
      inherit JsonConverterFactory()

      override _.CanConvert(t: System.Type) =
        t.IsGenericType
        && t.GetGenericTypeDefinition() = typedefof<option<_>>

      override _.CreateConverter(t: System.Type, _options: JsonSerializerOptions) =
        let innerType = t.GetGenericArguments()[0]
        let converterType = typedefof<OptionConverter<_>>.MakeGenericType(innerType)
        System.Activator.CreateInstance(converterType) :?> JsonConverter

    /// STJ converter for 'T array option fields that may be a single item or an array in JSON.
    type SingleOrArrayConverterFactory() =
      inherit JsonConverterFactory()

      override _.CanConvert(t: System.Type) =
        t.IsGenericType
        && t.GetGenericTypeDefinition() = typedefof<option<_>>
        && t.GetGenericArguments().[0].IsArray

      override _.CreateConverter(t: System.Type, _options: JsonSerializerOptions) =
        let elementType = t.GetGenericArguments().[0].GetElementType()
        let converterType = typedefof<SingleOrArrayConverter<_>>.MakeGenericType([| elementType |])
        System.Activator.CreateInstance(converterType) :?> JsonConverter

  let metaModelSerializerOptions =
    let options = JsonSerializerOptions(PropertyNamingPolicy = JsonNamingPolicy.CamelCase)
    options.Converters.Add(Converters.TypeConverter())
    options.Converters.Add(Converters.MapKeyTypeConverter())
    // SingleOrArrayConverterFactory must come before OptionConverterFactory
    // so that 'Type array option' fields hit the right converter
    options.Converters.Add(Converters.SingleOrArrayConverterFactory())
    options.Converters.Add(Converters.OptionConverterFactory())
    options

  let isNullableType (t: MetaModel.Type) =
    match t with
    | MetaModel.Type.BaseType { Name = MetaModel.BaseTypes.Null } -> true
    | _ -> false