namespace Ionide.LanguageServerProtocol.Types

open Ionide.LanguageServerProtocol
open Newtonsoft.Json
open Newtonsoft.Json.Linq

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
/// Wraps a <see cref="JToken"/> and provides structural equality and hashing so that values
/// can safely be used in sets, maps, and comparisons.
///
/// Note: structural equality and hashing are implemented here explicitly because while
/// <see cref="Newtonsoft.Json.Linq.JValue"/> does provide structural equality for primitive values,
/// <see cref="Newtonsoft.Json.Linq.JObject"/> and <see cref="Newtonsoft.Json.Linq.JArray"/>
/// have a known-broken <c>GetHashCode</c> that can return different values for structurally equal
/// instances. <c>System.Text.Json.JsonElement</c> — the intended future backing type — provides
/// neither structural equality nor hashing at all. This wrapper is therefore necessary regardless
/// of which JSON library is used underneath.
///
/// The internal representation is intentionally kept behind a single <c>JToken</c> property so
/// that the backing type can be swapped to <c>System.Text.Json.JsonElement</c> in the future
/// with minimal impact on call sites. Prefer the <c>fromJToken</c> / <c>fromJsonElement</c>
/// factories over the constructor directly.
[<JsonConverter(typeof<LSPAnyConverter>)>]
type LSPAny(token: JToken) =

  /// The underlying JSON token.
  member _.JToken: JToken = token

  /// The value as a <see cref="System.Text.Json.JsonElement"/>, bridged via raw JSON text.
  /// Once the backing type is migrated to <see cref="System.Text.Json.JsonElement"/> this will be a direct accessor.
  member _.JsonElement: System.Text.Json.JsonElement =
    System.Text.Json.JsonSerializer.Deserialize<System.Text.Json.JsonElement>(token.ToString(Formatting.None))

  override _.ToString() = token.ToString(Formatting.None)

  override _.GetHashCode() =
    // JToken does not override GetHashCode, so we compute one from the raw JSON text.
    // This is consistent with the Equals implementation below (same raw text ↔ same hash).
    token.ToString(Formatting.None).GetHashCode()

  override x.Equals(obj) =
    match obj with
    | :? LSPAny as other -> JToken.DeepEquals(token, other.JToken)
    | _ -> false

  interface System.IEquatable<LSPAny> with
    member x.Equals(other) = JToken.DeepEquals(token, other.JToken)

  /// Wraps a <see cref="JToken"/> in an <see cref="LSPAny"/>.
  static member inline fromJToken(token: JToken) = LSPAny(token)

  /// Wraps a <see cref="System.Text.Json.JsonElement"/> in an <see cref="LSPAny"/>, bridged via raw JSON text.
  /// Once the backing type is migrated to <see cref="System.Text.Json.JsonElement"/> this will be a direct wrap.
  static member inline fromJsonElement(element: System.Text.Json.JsonElement) =
    LSPAny(JToken.Parse(element.GetRawText()))

/// Newtonsoft.Json converter for <see cref="LSPAny"/>.
/// Reads any JSON value into a <see cref="JToken"/> and wraps it; writes by delegating to the token.
and LSPAnyConverter() =
  inherit JsonConverter()

  override _.CanConvert(t) = t = typeof<LSPAny>

  override _.ReadJson(reader, _t, _existing, _serializer) = LSPAny(JToken.ReadFrom(reader)) :> obj

  override _.WriteJson(writer, value, _serializer) = (value :?> LSPAny).JToken.WriteTo(writer)