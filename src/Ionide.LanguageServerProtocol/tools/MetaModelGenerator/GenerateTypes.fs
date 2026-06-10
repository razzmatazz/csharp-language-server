namespace MetaModelGenerator


module GenerateTypes =

  open System.Runtime.CompilerServices


  open System
  open Fantomas.Core
  open Fantomas.Core.SyntaxOak

  open Fabulous.AST

  open type Fabulous.AST.Ast

  open Fantomas.FCS.Syntax


  let getIdent (x: IdentifierOrDot list) =
    x
    |> List.map (
      function
      | IdentifierOrDot.Ident i -> i.Text
      | _ -> ""
    )
    |> String.concat ""


  let JToken = LongIdent "JToken"

  let createOption (t: WidgetBuilder<Type>) = Ast.OptionPostfix t

  let createDictionary (types: WidgetBuilder<Type> list) = AppPrefix(LongIdent(nameof Map), types)

  let createErasedUnion (types: WidgetBuilder<Type> array) =
    if types.Length > 1 then
      let duType = LongIdent $"U%d{types.Length}"
      AppPrefix(duType, (Array.toList types))
    else
      types.[0]

  module DebuggerDisplay =
    let range_debuggerDisplay (r: WidgetBuilder<TypeDefnRecordNode>) =
      r.attribute(Attribute("DebuggerDisplay(\"{DebuggerDisplay}\")")).members () {
        Member("x.DebuggerDisplay", "$\"{x.Start.DebuggerDisplay}-{x.End.DebuggerDisplay}\"")
          .attributes (
            [
              Attribute("DebuggerBrowsable(DebuggerBrowsableState.Never)")
              Attribute("JsonIgnore")
            ]
          )
      }

    let position_debuggerDisplay (r: WidgetBuilder<TypeDefnRecordNode>) =
      r.attribute(Attribute("DebuggerDisplay(\"{DebuggerDisplay}\")")).members () {
        Member("x.DebuggerDisplay", "$\"({x.Line},{x.Character})\"")
          .attributes (
            [
              Attribute("DebuggerBrowsable(DebuggerBrowsableState.Never)")
              Attribute("JsonIgnore")
            ]
          )
      }

    let diagnostic_debuggerDisplay (r: WidgetBuilder<TypeDefnRecordNode>) =
      r.attribute(Attribute("DebuggerDisplay(\"{DebuggerDisplay}\")")).members () {
        Member(
          "x.DebuggerDisplay",
          "$\"[{defaultArg x.Severity DiagnosticSeverity.Error}] ({x.Range.DebuggerDisplay}) {x.Message} ({Option.map string x.Code |> Option.defaultValue String.Empty})\""
        )
          .attributes (
            [
              Attribute("DebuggerBrowsable(DebuggerBrowsableState.Never)")
              Attribute("JsonIgnore")
            ]
          )
      }

    /// A set of records we want to configure custom DebuggerDisplay attributes for
    let debuggerDisplays =
      Map [
        "Range", range_debuggerDisplay
        "Position", position_debuggerDisplay
        "Diagnostic", diagnostic_debuggerDisplay
      ]

  type FieldInformation = {
    Name: string
    TypeInfo: WidgetBuilder<Type>
    StructuredDocs: StructuredDocs option
    Attribute: WidgetBuilder<AttributeNode> option
    /// Typescript uses Anonymous Records, while F# has them they're not that flexible so we generate named records for them
    NamedAnonymousRecords: WidgetBuilder<TypeDefnRecordNode> list
  }

  /// Handles structures that share all the same properties/fields but the only difference is one is optional
  ///
  /// For instance, there may be a union that defines `{ First: string; Last? : string} | {First?: string; Last : string}`
  /// so that one of the fields are always populated. This is however hard to deserialize in F# so we want to create a
  /// singular type with all properties as optional, such like:  `{First?: string; Last?: string}`
  let handleSameShapeStructuredUnions path createField (ts: MetaModel.Type array) =
    if
      ts
      |> Array.forall (fun t -> t.isStructureLiteralType)
    then
      let ts =
        ts
        |> Array.map (fun t ->
          match t with
          | MetaModel.Type.StructureLiteralType s -> s.Value
          | _ -> failwithf "Expected StructureLiteralType %A" t
        )

      let allProperties =
        ts
        |> Array.collect (fun s -> s.PropertiesSafe)
        |> Array.groupBy (fun p -> p.Name)

      if
        allProperties
        |> Array.forall (fun (_, props) ->
          let (_, first) = allProperties.[0]
          props.Length = first.Length
        )
      then
        let fields: (string * WidgetBuilder<_> * _ * _ list) list =
          allProperties
          |> Array.map (fun (name, props) ->
            let prop =
              props
              |> Array.tryFind (fun x -> x.IsOptional) // Prefer optional properties
              |> Option.defaultValue (props.[0])

            let fi =
              createField
                (path
                 @ [ prop.NameAsPascalCase ])
                prop.Type
                prop

            prop.NameAsPascalCase, fi.TypeInfo, prop.StructuredDocs, fi.NamedAnonymousRecords
          )
          |> Array.toList

        let namedAnonRecs =
          fields
          |> Seq.collect (fun (a, b, _, d) -> d)
          |> Seq.toList

        let fields =
          fields
          |> List.map (fun (n, f, docs, _) -> n, f, docs)

        let (fieldTy, record) =
          let name = String.concat "" path

          LongIdent name,
          Record(name) {
            for (n, f, docs) in fields do
              let f = Field(n, f)

              docs
              |> Option.mapOrDefault f f.xmlDocs
          }

        Some(
          fieldTy,
          record
          :: namedAnonRecs
        )
      else
        None
    else
      None


  let rec createField path (currentType: MetaModel.Type) (currentProperty: MetaModel.Property) : FieldInformation =
    try
      let rec getType
        path
        (currentType: MetaModel.Type)
        : WidgetBuilder<Type> * WidgetBuilder<AttributeNode> option * _ list =
        match currentType with
        | MetaModel.Type.ReferenceType r ->
          let name = r.Name
          LongIdent name, None, []

        | MetaModel.Type.BaseType b ->
          let name = b.Name.ToDotNetType()
          LongIdent name, None, []

        | MetaModel.Type.OrType o ->

          match handleSameShapeStructuredUnions path (createField) o.Items with
          | Some(x: WidgetBuilder<Type>, namedAnonRecs) -> x, None, namedAnonRecs
          | None ->

            // TS types can have optional properties (myKey?: string)
            // and unions with null (string | null)
            // we need to handle both cases
            let isOptional, items =
              if Array.exists MetaModel.isNullableType o.Items then
                true,
                o.Items
                |> Array.filter (fun x -> not (MetaModel.isNullableType x))
              else
                false, o.Items

            let ts =
              items
              |> Array.mapi (fun i ->
                getType (
                  path
                  @ [ $"C{i + 1}" ]
                )
              )

            let namedAnonRecs =
              ts
              |> Seq.collect (fun (a, b, c) -> c)
              |> Seq.toList

            let ts =
              ts
              |> Array.map (fun (a, b, c) -> a)
            // if this is already marked as Optional in the schema, ignore the union case
            // as we'll wrap it in an option type near the end
            if
              isOptional
              && not currentProperty.IsOptional
            then
              createOption (createErasedUnion ts),
              Some(Attribute "JsonProperty(NullValueHandling = NullValueHandling.Include)"),
              namedAnonRecs
            else
              createErasedUnion ts, None, namedAnonRecs

        | MetaModel.Type.ArrayType a ->
          let (t, _, namedAnonRecs) = getType path a.Element
          Array(t, 1), None, namedAnonRecs
        | MetaModel.Type.StructureLiteralType l ->
          if
            l.Value.PropertiesSafe
            |> Array.isEmpty
          then
            JToken, None, []
          else
            let ts =
              l.Value.PropertiesSafe
              |> Array.map (fun p ->
                let fi =
                  createField
                    (path
                     @ [ p.NameAsPascalCase ])
                    p.Type
                    p

                {
                  Name = fi.Name
                  TypeInfo = fi.TypeInfo
                  Attribute = None
                  StructuredDocs = p.StructuredDocs
                  NamedAnonymousRecords = fi.NamedAnonymousRecords
                }
              )
              |> Array.toList

            let fieldTy, record =
              let fields =
                ts
                |> List.map (fun fi ->
                  let f = Field(fi.Name, fi.TypeInfo)

                  fi.StructuredDocs
                  |> Option.mapOrDefault f f.xmlDocs
                )

              let name = String.concat "" path

              let r =
                Record(name) {
                  for f in fields do
                    f
                }

              let r =
                l.Value.StructuredDocs
                |> Option.mapOrDefault r r.xmlDocs

              LongIdent name, r

            let namedAnonRecs =
              ts
              |> List.collect (fun fi -> fi.NamedAnonymousRecords)

            fieldTy,
            None,
            record
            :: namedAnonRecs

        | MetaModel.Type.MapType m ->
          let key =
            match m.Key with
            | MetaModel.MapKeyType.Base b ->
              b.Name.ToDotNetType()
              |> LongIdent
            | MetaModel.MapKeyType.ReferenceType r -> LongIdent(r.Name)

          let (value, _, namedAnonRecs) = getType path m.Value

          createDictionary [
            key
            value
          ],
          None,
          namedAnonRecs

        | MetaModel.Type.StringLiteralType t ->
          LongIdent("string"), Some(Attribute($"UnionKindAttribute(\"{t.Value}\")")), []
        | MetaModel.Type.TupleType t ->

          let ts =
            t.Items
            |> Array.mapi (fun i ->
              getType (
                path
                @ [ $"T{i + 1}" ]
              )
            )
            |> Array.toList

          let tuple =
            ts
            |> List.map (fun (a, b, c) -> a)
            |> Tuple

          let namedAnonRecs =
            ts
            |> List.collect (fun (a, b, c) -> c)

          tuple, None, namedAnonRecs

        | _ -> failwithf $"todo Property %A{currentType}"

      let (t, attribute, namedAnonRecs) = getType path currentType
      let t = if currentProperty.IsOptional then createOption t else t
      let name = currentProperty.NameAsPascalCase

      {
        Name = name
        TypeInfo = t
        Attribute = attribute
        StructuredDocs = currentProperty.StructuredDocs
        NamedAnonymousRecords = namedAnonRecs
      }
    with e ->
      raise
      <| Exception(sprintf "createField on %A  " currentProperty, e)


  let isUnitStructure (structure: MetaModel.Structure) =

    let isEmptyExtends =
      structure.Extends
      |> Option.Array.isEmpty

    let isEmptyMixins =
      structure.Mixins
      |> Option.Array.isEmpty

    let isEmptyProperties =
      structure.Properties
      |> Option.Array.isEmpty

    isEmptyExtends
    && isEmptyMixins
    && isEmptyProperties

  //HACK: need to add these since it's a mixin but really should be an interface
  let extensionsButNotReally = [
    "WorkDoneProgressParams"
    "WorkDoneProgressOptions"
    "PartialResultParams"
  ]

  /// Scans all structures for use as an interface and generates the interfaces
  let createInterfaceStructures (structure: MetaModel.Structure array) (model: MetaModel.MetaModel) =
    // Scan and find all interfaces
    let interfaceStructures =
      structure
      |> Array.collect (fun s ->
        s.ExtendsSafe
        |> Array.map (fun e ->
          match e with
          | MetaModel.Type.ReferenceType r ->
            match
              model.StructuresSafe
              |> Array.tryFind (fun s -> s.Name = r.Name)
            with
            | Some s -> s
            | None -> failwithf "Could not find structure %s" r.Name
          | _ -> failwithf "todo Extends %A" e
        )
      )
      |> Array.distinctBy (fun x -> x.Name)

      //HACK: need to add additional types since it's a mixin but really should be an interface
      |> Array.append [|
        yield!
          model.StructuresSafe
          |> Array.filter (fun s ->
            extensionsButNotReally
            |> List.contains s.Name
          )
      |]

    interfaceStructures
    |> Array.map (fun s ->
      let widget =
        TypeDefn($"I{s.Name}") {
          let properties = s.PropertiesSafe

          for p in properties do
            let fi =
              createField
                [
                  s.Name
                  p.NameAsPascalCase
                ]
                p.Type
                p

            let ap = AbstractMember(fi.Name, fi.TypeInfo)

            yield
              fi.StructuredDocs
              |> Option.mapOrDefault ap ap.xmlDocs

          // MetaModel is incorrect we need to use Mixin instead of extends
          for e in s.MixinsSafe do
            match e with
            | MetaModel.Type.ReferenceType r -> yield Inherit($"I{r.Name}")
            | _ -> ()
        }

      let widget =
        s.StructuredDocs
        |> Option.mapOrDefault widget widget.xmlDocs

      s, widget
    )


  /// Creates Record types based on a given Structure
  let createStructure
    (structure: MetaModel.Structure)
    (interfaceStructures: MetaModel.Structure array)
    (model: MetaModel.MetaModel)
    =

    // deduplicate any by name choose the heaviest weighted one
    let maxByWeight groupBy (xs: ('T * int) list) =
      xs
      |> List.groupBy (fun (i, _) -> groupBy i)
      |> List.map (
        snd
        >> List.maxBy snd
        >> fst
      )

    let rec expandFields (structure: MetaModel.Structure) : list<FieldInformation> =
      [

        for e in structure.ExtendsSafe do
          match e with
          | MetaModel.Type.ReferenceType r ->
            match
              model.StructuresSafe
              |> Array.tryFind (fun s -> s.Name = r.Name)
            with
            | Some s ->
              for info in expandFields s do
                {
                  Name = info.Name
                  TypeInfo = info.TypeInfo
                  StructuredDocs = info.StructuredDocs
                  Attribute = info.Attribute
                  NamedAnonymousRecords = info.NamedAnonymousRecords
                },
                10
            | None -> failwithf "Could not find structure %s" r.Name
          | _ -> failwithf "todo Extends %A" e

        // Mixins are inlined fields
        for m in structure.MixinsSafe do
          match m with
          | MetaModel.Type.ReferenceType r ->
            match
              model.StructuresSafe
              |> Array.tryFind (fun s -> s.Name = r.Name)
            with
            | Some s ->
              for p in s.PropertiesSafe do
                let fi =
                  createField
                    [
                      s.Name
                      p.NameAsPascalCase
                    ]
                    p.Type
                    p

                {
                  Name = fi.Name
                  TypeInfo = fi.TypeInfo
                  StructuredDocs = fi.StructuredDocs
                  Attribute = fi.Attribute
                  NamedAnonymousRecords = fi.NamedAnonymousRecords
                },
                1

            | None -> failwithf "Could not find structure %s" r.Name
          | _ -> failwithf "todo Mixins %A" m

        for p in structure.PropertiesSafe do
          let fi =
            createField
              [
                structure.Name
                p.NameAsPascalCase
              ]
              p.Type
              p

          {
            Name = fi.Name
            TypeInfo = fi.TypeInfo
            StructuredDocs = fi.StructuredDocs
            Attribute = fi.Attribute
            NamedAnonymousRecords = fi.NamedAnonymousRecords
          },
          100

      ]
      |> maxByWeight (fun fi -> fi.Name)

    let rec implementInterface (structure: MetaModel.Structure) = [|


      // Implement interface
      yield!
        interfaceStructures
        |> Array.tryFind (fun s -> s.Name = structure.Name)
        |> Option.map (fun s ->
          let interfaceName = Ast.LongIdent($"I{s.Name}")

          InterfaceWith(interfaceName) {
            for p in s.PropertiesSafe do
              let name = Constant($"x.{p.NameAsPascalCase}")
              let outp = Member(ConstantPat(name), ConstantExpr(name))

              p.StructuredDocs
              |> Option.mapOrDefault outp outp.xmlDocs
          }

        )
        |> Option.toArray

      for e in structure.ExtendsSafe do
        match e with
        | MetaModel.Type.ReferenceType r ->
          yield!
            interfaceStructures
            |> Array.tryFind (fun s -> s.Name = r.Name)
            |> Option.map implementInterface
            |> Option.Array.toArray
        | _ -> ()

      // hack mixin with `extensionsButNotReally`
      for m in structure.MixinsSafe do
        match m with
        | MetaModel.Type.ReferenceType r ->
          yield!
            interfaceStructures
            |> Array.tryFind (fun s -> s.Name = r.Name)
            |> Option.map implementInterface
            |> Option.Array.toArray
        | _ -> ()
    |]


    try
      let recordFields = expandFields structure

      let namedAnonymousRecords =
        recordFields
        |> List.collect (fun fi -> fi.NamedAnonymousRecords)

      [
        yield! namedAnonymousRecords
        Record(structure.Name) {
          yield!
            recordFields
            |> List.map (fun fi ->


              let f = Field(fi.Name, fi.TypeInfo)

              let f =
                fi.StructuredDocs
                |> Option.mapOrDefault f f.xmlDocs

              let f =
                fi.Attribute
                |> Option.mapOrDefault f f.attribute

              f

            )
        }
        |> fun r ->

          let r =
            structure.StructuredDocs
            |> Option.mapOrDefault r r.xmlDocs

          let r =
            DebuggerDisplay.debuggerDisplays
            |> Map.tryFind structure.Name
            |> Option.mapOrDefault r (fun f -> f r)

          match implementInterface structure with
          | [||] -> r
          | interfaces ->
            r.members () {
              for i in interfaces do
                i
            }
      ]

    with e ->
      raise
      <| Exception(sprintf "createStructure on %A" structure, e)

  /// Creates F# Type Aliases or Records based on a TypeAlias
  let createTypeAlias (alias: MetaModel.TypeAlias) =
    let rec getType path (t: MetaModel.Type) =
      if alias.Name = "LSPAny" then
        JToken, []
      else
        match t with
        | MetaModel.Type.ReferenceType r -> LongIdent r.Name, []
        | MetaModel.Type.BaseType b -> LongIdent(b.Name.ToDotNetType()), []
        | MetaModel.Type.OrType o ->
          match handleSameShapeStructuredUnions path (createField) o.Items with
          | Some(x, namedAnonRecs) -> x, namedAnonRecs
          | None ->

            let types =
              o.Items
              |> Array.mapi (fun i item ->
                getType
                  (path
                   @ [ $"C{i + 1}" ])
                  item
              )

            let types2 =
              types
              |> Array.map fst

            let namedAnonRecs =
              types
              |> Seq.collect snd

            let x =
              types2
              |> createErasedUnion

            x, Seq.toList namedAnonRecs
        | MetaModel.Type.ArrayType a ->
          let (types, namedAnonRecs) = getType path a.Element
          Array(types, 1), namedAnonRecs
        | MetaModel.Type.StructureLiteralType l when Proposed.checkProposed l.Value ->
          if
            l.Value.PropertiesSafe
            |> Array.isEmpty
          then
            JToken, []
          else
            let ts =
              l.Value.PropertiesSafe
              |> Array.map (fun p ->
                let fi = createField [ alias.Name ] p.Type p
                fi.Name, fi.TypeInfo, p.StructuredDocs
              )
              |> Array.toList

            let name =
              path
              |> String.concat ""

            LongIdent(name),
            [
              let r =
                Record(name) {
                  for (n, t, docs) in ts do
                    let f = Field(n, t)

                    docs
                    |> Option.mapOrDefault f f.xmlDocs
                }

              l.Value.StructuredDocs
              |> Option.mapOrDefault r r.xmlDocs
            ]

        | MetaModel.Type.MapType m ->
          let key =
            match m.Key with
            | MetaModel.MapKeyType.Base b ->
              b.Name.ToDotNetType()
              |> LongIdent
            | MetaModel.MapKeyType.ReferenceType r ->
              r.Name
              |> LongIdent

          let (value, namedAnonRecs) = getType path m.Value

          createDictionary [
            key
            value
          ],
          namedAnonRecs

        | MetaModel.Type.StringLiteralType t -> String(), []
        | MetaModel.Type.TupleType t ->
          let types =

            t.Items
            |> Array.mapi (fun i item ->
              getType
                (path
                 @ [ "$T{i+1}" ])
                item
            )

          let namedAnonRecs =
            types
            |> Seq.collect snd
            |> Seq.toList

          let tuple =
            types
            |> Array.map fst
            |> Array.toList
            |> Tuple

          tuple, namedAnonRecs

        | _ -> failwithf "todo Property %A" t

    let (types: WidgetBuilder<Type>, namedAnonRecs) = getType [ alias.Name ] alias.Type

    let (|AliasIsSameAsRecordName|_|) (alias: WidgetBuilder<Type>, namedAnonRecs) =
      let typeAlias = Gen.mkOak alias

      let nestedRecord =
        namedAnonRecs
        |> Seq.tryExactlyOne

      match typeAlias, Option.map Gen.mkOak nestedRecord with
      | Type.LongIdent i, Some r when (getIdent i.Content) = getIdent ((r :> ITypeDefn).TypeName.Identifier.Content) ->
        nestedRecord
      | _ -> None

    let abbrev =
      match types, namedAnonRecs with
      | AliasIsSameAsRecordName r ->
        // If the record being emitted is the same as the type alias, ignore the type alias and just emit the record
        AnonymousModule() {
          alias.StructuredDocs
          |> Option.mapOrDefault r r.xmlDocs
        }
      | _ ->
        AnonymousModule() {
          let abbrev = Abbrev(alias.Name, types)

          let abbrev =
            alias.StructuredDocs
            |> Option.mapOrDefault abbrev abbrev.xmlDocs

          abbrev

          for o in namedAnonRecs do
            o
        }

    // LSPAny is defined as a proper wrapper class (with structural equality) in Types.fs.
    // Do not emit a generated alias for it here.
    if alias.Name = "LSPAny" then
      AnonymousModule() { () }
    else
      AnonymousModule() { abbrev }


  /// Creates Open or Closed Enums based on an Enumeration
  let createEnumeration (enumeration: MetaModel.Enumeration) =
    AnonymousModule() {
      match enumeration.Type.Name with
      | MetaModel.EnumerationTypeNameValues.String when enumeration.SupportsCustomValues = Some true ->
        // This creates an "Open" enum. Essentially these are strings well known string values but allows for custom values

        let ab = Abbrev(enumeration.Name, "string")

        enumeration.StructuredDocs
        |> Option.mapOrDefault ab ab.xmlDocs

        Module(enumeration.Name) {
          for v in enumeration.ValuesSafe do
            let name = PrettyNaming.NormalizeIdentifierBackticks v.Name
            let l = Value(ConstantPat(Constant(name)), ConstantExpr(String(v.Value))).attribute (Attribute "Literal")
            let l = l.returnType (LongIdent enumeration.Name)

            v.StructuredDocs
            |> Option.mapOrDefault l l.xmlDocs

        }


      | MetaModel.EnumerationTypeNameValues.String ->
        // Otherwise generate a normal F# closed enum with "string" values
        let enum =
          Enum enumeration.Name {
            for i, v in
              enumeration.ValuesSafe
              |> Array.mapi (fun i x -> i, x) do
              let case = EnumCase(v.Name, string i)

              let case = case.attribute (Attribute($"EnumMember(Value = \"{v.Value}\")"))

              v.StructuredDocs
              |> Option.mapOrDefault case case.xmlDocs
          }

        let enum =
          enumeration.StructuredDocs
          |> Option.mapOrDefault enum enum.xmlDocs

        enum.attribute (Attribute("JsonConverter(typeof<Converters.StringEnumConverter>)"))

      | MetaModel.EnumerationTypeNameValues.Integer
      | MetaModel.EnumerationTypeNameValues.Uinteger -> // Create enums with number values
        let enum =
          Enum enumeration.Name {
            for v in enumeration.ValuesSafe do
              let case = EnumCase(String.toPascalCase v.Name, v.Value)

              v.StructuredDocs
              |> Option.mapOrDefault case case.xmlDocs
          }

        enumeration.StructuredDocs
        |> Option.mapOrDefault enum enum.xmlDocs
      | _ -> failwithf "todo Enumeration %A" enumeration
    }


  let private formatConfig = Formatting.formatConfig

  /// The main entry point to generating types from a metaModel.json file
  let generateType (parsedMetaModel: MetaModel.MetaModel) outputPath =
    async {
      let documentUriDocs =
        """
URI's are transferred as strings. The URI's format is defined in https://tools.ietf.org/html/rfc3986

See: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#uri
"""

      let regexpDocs =
        """
Regular expressions are transferred as strings.

See https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#regExp
"""

      printfn "Generating Types"

      let oak =
        Ast.Oak() {
          Namespace("Ionide.LanguageServerProtocol.Types") {
            // open namespaces we know we'll need
            Open("System")
            Open("System.Runtime.Serialization")
            Open("System.Diagnostics")
            Open("Newtonsoft.Json")
            Open("Newtonsoft.Json.Linq")

            // Simple aliases for types that are not in dotnet
            Abbrev(Widgets.UriString, "string")
              .xmlDocs (
                documentUriDocs
                |> StructuredDocs.parse
              )

            Abbrev(Widgets.DocumentUriString, "string")
              .xmlDocs (
                documentUriDocs
                |> StructuredDocs.parse
              )

            Abbrev(Widgets.RegExpString, "string")
              .xmlDocs (
                regexpDocs
                |> StructuredDocs.parse
              )

            // Generate Interfaces
            let structures = parsedMetaModel.StructuresSafe

            let (knownInterfaces, interfaceWidgets) =
              createInterfaceStructures structures parsedMetaModel
              |> Array.unzip


            for w in interfaceWidgets do
              w

            let records = ResizeArray<_>()

            for s in structures do
              if isUnitStructure s then
                Abbrev(s.Name, "obj")
              else
                createStructure s knownInterfaces parsedMetaModel
                |> List.map (fun r ->

                  let name =
                    let x = Gen.mkOak r :> ITypeDefn

                    x.TypeName.Identifier.Content
                    |> getIdent


                  name, r
                )
                |> records.AddRange

            for r in
              records
              |> Seq.distinctBy fst
              |> Seq.map snd do
              r
            // Generate Type Aliases
            for t in parsedMetaModel.TypeAliasesSafe do
              createTypeAlias t

            // Generate Enumerations
            for e in parsedMetaModel.EnumerationsSafe do
              createEnumeration e
          }
          |> fun x -> x.toRecursive ()
        }


      let! formattedText =
        oak
        |> Gen.mkOak
        |> fun oak -> CodeFormatter.FormatOakAsync(oak, formatConfig)

      do! FileWriters.writeIfChanged outputPath formattedText
    }