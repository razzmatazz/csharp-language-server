namespace MetaModelGenerator

module GenerateClientServer =
  open System
  open Fantomas.Core.SyntaxOak
  open Fabulous.AST
  open type Fabulous.AST.Ast
  open Fantomas.Core


  let private formatConfig = Formatting.formatConfig

  let generateClientServer (parsedMetaModel: MetaModel.MetaModel) outputPath =
    async {
      printfn "Generating generateClientServer"


      let requests =
        parsedMetaModel.Requests
        |> Array.filter Proposed.checkProposed
        |> Array.groupBy (fun x -> x.MessageDirection)
        |> Map

      let notifications =
        parsedMetaModel.Notifications
        |> Array.filter Proposed.checkProposed
        |> Array.groupBy (fun x -> x.MessageDirection)
        |> Map


      let serverRequests = [
        yield!
          requests
          |> Map.tryFind MetaModel.MessageDirection.ClientToServer
          |> Option.defaultValue [||]
        yield!
          requests
          |> Map.tryFind MetaModel.MessageDirection.Both
          |> Option.defaultValue [||]
      ]

      let serverNotifications = [
        yield!
          notifications
          |> Map.tryFind MetaModel.MessageDirection.ClientToServer
          |> Option.defaultValue [||]
        yield!
          notifications
          |> Map.tryFind MetaModel.MessageDirection.Both
          |> Option.defaultValue [||]
      ]


      let clientRequests = [
        yield!
          requests
          |> Map.tryFind MetaModel.MessageDirection.ServerToClient
          |> Option.defaultValue [||]
        yield!
          requests
          |> Map.tryFind MetaModel.MessageDirection.Both
          |> Option.defaultValue [||]
      ]

      let clientNotifications = [
        yield!
          notifications
          |> Map.tryFind MetaModel.MessageDirection.ServerToClient
          |> Option.defaultValue [||]
        yield!
          notifications
          |> Map.tryFind MetaModel.MessageDirection.Both
          |> Option.defaultValue [||]
      ]

      let normalizeMethod (s: string) =
        let parts =
          s.Split(
            "/",
            StringSplitOptions.RemoveEmptyEntries
            ||| StringSplitOptions.TrimEntries
          )

        parts
        |> Array.filter (fun x -> x <> "$")
        |> Array.map (fun x ->
          (string x.[0]).ToUpper()
          + x.[1..]
        )
        |> String.concat ""

      let oak =
        Ast.Oak() {
          Namespace "Ionide.LanguageServerProtocol" {
            Open "Ionide.LanguageServerProtocol.Types"
            Open "Ionide.LanguageServerProtocol.JsonRpc"

            let generateInterface
              name
              (notifications: list<MetaModel.Notification>)
              (requests: list<MetaModel.Request>)
              =


              TypeDefn name {

                Inherit "System.IDisposable"

                let notificationComment = SingleLine "Notifications"

                let mutable writtenNotificationComment = false

                for n in notifications do
                  let methodName = normalizeMethod n.Method

                  let parameters = [
                    match n.Params with
                    | None -> yield Unit()
                    | Some ps ->
                      for p in ps do
                        match p with
                        | MetaModel.Type.ReferenceType r -> yield LongIdent r.Name
                        | _ -> ()
                  ]

                  let returnType = AsyncPrefix(Unit())


                  let wb = AbstractMember(methodName, parameters, returnType)

                  let wb =
                    n.StructuredDocs
                    |> Option.mapOrDefault wb wb.xmlDocs

                  let wb =
                    if not writtenNotificationComment then
                      writtenNotificationComment <- true
                      wb.triviaBefore notificationComment
                    else
                      wb

                  wb

                let requestComment = SingleLine "Requests"

                let mutable writtenRequestComment = false


                for r in requests do
                  let methodName = normalizeMethod r.Method

                  let parameters = [
                    match r.Params with
                    | None -> yield Unit()
                    | Some ps ->
                      for p in ps do
                        match p with
                        | MetaModel.Type.ReferenceType r -> yield (LongIdent r.Name)
                        | _ -> ()
                  ]

                  let returnType =
                    let rec returnType (ty: MetaModel.Type) =
                      match ty with
                      | MetaModel.Type.ReferenceType r -> LongIdent r.Name
                      | MetaModel.Type.BaseType b ->
                        match b.Name with
                        | MetaModel.BaseTypes.Null -> Unit()
                        | MetaModel.BaseTypes.Boolean -> Boolean()
                        | MetaModel.BaseTypes.Integer -> Int()
                        | MetaModel.BaseTypes.Uinteger -> UInt32()
                        | MetaModel.BaseTypes.Decimal -> Float()
                        | MetaModel.BaseTypes.String -> String()
                        | MetaModel.BaseTypes.DocumentUri -> DocumentUri()
                        | MetaModel.BaseTypes.Uri -> LspUri()
                        | MetaModel.BaseTypes.RegExp -> LspRegExp()
                      | MetaModel.Type.OrType o ->
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

                        let types =
                          items
                          |> Array.map returnType
                          |> Array.toList

                        let retType =
                          if types.Length > 1 then
                            let duType = $"U{types.Length}"
                            AppPrefix(duType, types)
                          else
                            types.[0]

                        if isOptional then OptionPrefix retType else retType
                      | MetaModel.Type.ArrayType a -> ArrayPrefix(returnType a.Element)
                      | _ -> LongIdent "Unsupported Type"

                    AsyncLspResultPrefix(returnType r.Result)

                  let wb = AbstractMember(methodName, parameters, returnType)

                  let wb =
                    r.StructuredDocs
                    |> Option.mapOrDefault wb wb.xmlDocs

                  let wb =
                    if not writtenRequestComment then
                      writtenRequestComment <- true
                      wb.triviaBefore (requestComment)
                    else
                      wb

                  wb
              }

            generateInterface "ILspServer" serverNotifications serverRequests
            generateInterface "ILspClient" clientNotifications clientRequests

            let generateServerRequestHandlingRecord =
              let serverTypeArg = "'server"

              Record "ServerRequestHandling" { Field("Run", Funs([ serverTypeArg ], "System.Delegate")) }
              |> _.typeParams(PostfixList(TyparDecl(serverTypeArg), SubtypeOf(serverTypeArg, LongIdent("ILspServer"))))

            let generateRoutes =

              let body =
                let generateRoute requestParams (method: string) configureValue =
                  let callWith =
                    if Array.isEmpty requestParams then
                      ParenExpr ""
                    else
                      ParenExpr "request"

                  TupleExpr [

                    ConstantExpr(String method)

                    AppWithLambdaExpr(
                      ConstantExpr "serverRequestHandling",
                      [
                        ConstantPat "server"
                        ConstantPat "request"
                      ],
                      AppLongIdentAndSingleParenArgExpr(
                        [
                          "server"
                          normalizeMethod method
                        ],
                        callWith
                      )
                      |> configureValue
                    )
                  ]

                let generateRouteHandler =
                  LetOrUseExpr(
                    Function(
                      "serverRequestHandling",
                      NamedPat "run",
                      RecordExpr [
                        RecordFieldExpr(
                          "Run",
                          LambdaExpr(
                            [ ParameterPat "server" ],
                            AppLongIdentAndSingleParenArgExpr([ "run" ], ConstantExpr "server")
                            |> pipe (ConstantExpr "JsonRpc.Requests.requestHandling")
                          )
                        )
                      ]
                    )
                  )

                CompExprBodyExpr [
                  generateRouteHandler
                  OtherExpr(
                    ListExpr [
                      for serverRequest in serverRequests do
                        generateRoute serverRequest.ParamsSafe serverRequest.Method id

                      for serverNotification in serverNotifications do
                        generateRoute
                          serverNotification.ParamsSafe
                          serverNotification.Method
                          (pipe (ConstantExpr "Requests.notificationSuccess"))


                    ]
                  )
                ]


              Function("routeMappings", [ UnitPat() ], body)

            Module "Mappings" {


              generateServerRequestHandlingRecord
              generateRoutes

            }

          }
        }


      let! formattedText =
        oak
        |> Gen.mkOak
        |> fun oak -> CodeFormatter.FormatOakAsync(oak, formatConfig)

      do! FileWriters.writeIfChanged outputPath formattedText

    }