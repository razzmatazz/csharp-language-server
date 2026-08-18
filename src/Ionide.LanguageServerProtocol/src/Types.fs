namespace Ionide.LanguageServerProtocol.Types

open Ionide.LanguageServerProtocol

/// Types in typescript can have hardcoded values for their fields, this attribute is used to mark
/// the default value for a field in a type and is used when deserializing the type to json
/// but these types might not actually be used as a discriminated union or only partially used
/// so we don't generate a dedicated union type because of that
///
/// see https://microsoft.github.io/language-server-protocol/specifications/lsp/3.18/specification/#resourceChanges for a dedicated example
type UnionKindAttribute(value: string) =
  inherit System.Attribute()
  member x.Value = value

/// Represents a Union type where the individual cases are erased when serialized or deserialized
/// For instance a union could be defined as: "string | int | bool" and when serialized it would be
/// serialized as a only a value based on the actual case
type ErasedUnionAttribute() =
  inherit System.Attribute()

/// Represents a Union type where the individual cases are erased when serialized or deserialized
/// For instance a union could be defined as: "string | int | bool" and when serialized it would be
/// serialized as a only a value based on the actual case
[<ErasedUnion>]
type U2<'T1, 'T2> =
  /// Represents a single case of a Union type where the individual cases are erased when serialized or deserialized
  /// For instance a union could be defined as: "string | int | bool" and when serialized it would be
  /// serialized as a only a value based on the actual case
  | C1 of 'T1
  /// Represents a single case of a Union type where the individual cases are erased when serialized or deserialized
  /// For instance a union could be defined as: "string | int | bool" and when serialized it would be
  /// serialized as a only a value based on the actual case
  | C2 of 'T2

  override x.ToString() =
    match x with
    | C1 c -> string c
    | C2 c -> string c

/// Represents a Union type where the individual cases are erased when serialized or deserialized
/// For instance a union could be defined as: "string | int | bool" and when serialized it would be
/// serialized as a only a value based on the actual case
[<ErasedUnion>]
type U3<'T1, 'T2, 'T3> =
  /// Represents a Union type where the individual cases are erased when serialized or deserialized
  /// For instance a union could be defined as: "string | int | bool" and when serialized it would be
  /// serialized as a only a value based on the actual case
  | C1 of 'T1
  /// Represents a Union type where the individual cases are erased when serialized or deserialized
  /// For instance a union could be defined as: "string | int | bool" and when serialized it would be
  /// serialized as a only a value based on the actual case
  | C2 of 'T2
  /// Represents a Union type where the individual cases are erased when serialized or deserialized
  /// For instance a union could be defined as: "string | int | bool" and when serialized it would be
  /// serialized as a only a value based on the actual case
  | C3 of 'T3

  override x.ToString() =
    match x with
    | C1 c -> string c
    | C2 c -> string c
    | C3 c -> string c

/// Represents a Union type where the individual cases are erased when serialized or deserialized
/// For instance a union could be defined as: "string | int | bool" and when serialized it would be
/// serialized as a only a value based on the actual case
[<ErasedUnion>]
type U4<'T1, 'T2, 'T3, 'T4> =
  /// Represents a Union type where the individual cases are erased when serialized or deserialized
  /// For instance a union could be defined as: "string | int | bool" and when serialized it would be
  /// serialized as a only a value based on the actual case
  | C1 of 'T1
  /// Represents a Union type where the individual cases are erased when serialized or deserialized
  /// For instance a union could be defined as: "string | int | bool" and when serialized it would be
  /// serialized as a only a value based on the actual case
  | C2 of 'T2
  /// Represents a Union type where the individual cases are erased when serialized or deserialized
  /// For instance a union could be defined as: "string | int | bool" and when serialized it would be
  /// serialized as a only a value based on the actual case
  | C3 of 'T3
  /// Represents a Union type where the individual cases are erased when serialized or deserialized
  /// For instance a union could be defined as: "string | int | bool" and when serialized it would be
  /// serialized as a only a value based on the actual case
  | C4 of 'T4

  override x.ToString() =
    match x with
    | C1 c -> string c
    | C2 c -> string c
    | C3 c -> string 3
    | C4 c -> string 3

/// The LSP any type.
///
/// Wraps a <see cref="System.Text.Json.JsonElement"/> and provides structural equality and
/// hashing (via the raw JSON text) so that values can safely be used in sets, maps, and
/// comparisons — <c>JsonElement</c> provides neither on its own.
[<Sealed>]
type LSPAny(element: System.Text.Json.JsonElement) =

  /// The underlying JSON value.
  member _.JsonElement: System.Text.Json.JsonElement = element

  override _.ToString() = element.GetRawText()

  override _.GetHashCode() = element.GetRawText().GetHashCode()

  override x.Equals(obj) =
    match obj with
    | :? LSPAny as other -> element.GetRawText() = other.JsonElement.GetRawText()
    | _ -> false

  interface System.IEquatable<LSPAny> with
    member x.Equals(other) = element.GetRawText() = other.JsonElement.GetRawText()

  /// Wraps a <see cref="System.Text.Json.JsonElement"/> in an <see cref="LSPAny"/>.
  static member inline fromJsonElement(element: System.Text.Json.JsonElement) = LSPAny(element)

#if NEWTONSOFT_LEGACY_UNUSED
// Newtonsoft.Json converter for LSPAny, kept for the legacy StreamJsonRpc wire path
// (Server.defaultJsonRpcFormatter / Server.start* / the Client module), which is disabled
// (see LanguageServerProtocol.fs) so that csharp-ls no longer ships Newtonsoft.Json. Re-enable
// by defining NEWTONSOFT_LEGACY_UNUSED, restoring a JToken-backed LSPAny.fromJToken factory,
// and re-adding `[<JsonConverter(typeof<LSPAnyConverter>)>]` above the LSPAny type.
open Newtonsoft.Json
open Newtonsoft.Json.Linq

type LSPAnyConverter() =
  inherit JsonConverter()

  override _.CanConvert(t) = t = typeof<LSPAny>

  override _.ReadJson(reader, _t, _existing, _serializer) =
    let token = JToken.ReadFrom(reader)
    LSPAny.fromJsonElement (System.Text.Json.JsonSerializer.Deserialize<System.Text.Json.JsonElement>(token.ToString(Formatting.None))) :> obj

  override _.WriteJson(writer, value, _serializer) =
    JToken.Parse((value :?> LSPAny).JsonElement.GetRawText()).WriteTo(writer)
#endif

/// System.Text.Json converter for <see cref="LSPAny"/>.
/// Reads any JSON value into a <see cref="System.Text.Json.JsonElement"/> and wraps it via
/// <see cref="LSPAny.fromJsonElement"/>; writes by delegating to the underlying JsonElement.
/// This is what <c>Ionide.LanguageServerProtocol.Server.lspSerializerOptions</c> registers so
/// that <c>LSPAny</c>-typed fields round-trip through the STJ-based serializer.
type LSPAnyJsonConverter() =
  inherit System.Text.Json.Serialization.JsonConverter<LSPAny>()

  override _.Read(reader, _typeToConvert, _options) =
    use doc = System.Text.Json.JsonDocument.ParseValue(&reader)
    LSPAny.fromJsonElement (doc.RootElement.Clone())

  override _.Write(writer, value, _options) = value.JsonElement.WriteTo(writer)