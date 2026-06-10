namespace MetaModelGenerator

module Option =
  module Array =
    /// Returns true if the given array is empty or None
    let isEmpty (x: 'a array option) =
      match x with
      | None -> true
      | Some x -> Array.isEmpty x

    /// Returns empty array if None, otherwise the array
    let toArray (x: 'a array option) = Option.defaultValue [||] x

  let mapOrDefault def f o =
    match o with
    | Some o -> f o
    | None -> def

module String =
  open System

  let toPascalCase (s: string) =
    s.[0]
    |> Char.ToUpper
    |> fun c ->
        c.ToString()
        + s.Substring(1)

module Array =
  /// <summary>Places separator between each element of items</summary>
  let intersperse (separator: 'a) (items: 'a array) : 'a array = [|
    let mutable notFirst = false

    for element in items do
      if notFirst then
        yield separator

      yield element
      notFirst <- true
  |]