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