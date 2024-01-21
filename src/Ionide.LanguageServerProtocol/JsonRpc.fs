module Ionide.LanguageServerProtocol.JsonRpc

open Newtonsoft.Json
open Newtonsoft.Json.Linq

type MessageTypeTest =
  { [<JsonProperty("jsonrpc")>]
    Version: string
    Id: int option
    Method: string option }

[<RequireQualifiedAccess>]
type MessageType =
  | Notification
  | Request
  | Response
  | Error

let getMessageType messageTest =
  match messageTest with
  | { Version = "2.0"; Id = Some _; Method = Some _ } -> MessageType.Request
  | { Version = "2.0"; Id = Some _; Method = None } -> MessageType.Response
  | { Version = "2.0"; Id = None; Method = Some _ } -> MessageType.Notification
  | _ -> MessageType.Error

type Request =
  { [<JsonProperty("jsonrpc")>]
    Version: string
    Id: int
    Method: string
    Params: JToken option }

  static member Create(id: int, method': string, rpcParams: JToken option) =
    { Version = "2.0"; Id = id; Method = method'; Params = rpcParams }

type Notification =
  { [<JsonProperty("jsonrpc")>]
    Version: string
    Method: string
    Params: JToken option }

  static member Create(method': string, rpcParams: JToken option) =
    { Version = "2.0"; Method = method'; Params = rpcParams }

module ErrorCodes =
  open System
  let parseError = -32700
  let invalidRequest = -32600
  let methodNotFound = -32601
  let invalidParams = -32602
  let internalError = -32603


  ///<summary>This is the start range of JSON-RPC reserved error codes.
  ///It doesn't denote a real error code. No LSP error codes should
  ///be defined between the start and end range. For backwards
  ///compatibility the <see cref="ServerNotInitialized"/> and the <see cref="UnknownErrorCode" />
  ///are left in the range.</summary>
  let jsonrpcReservedErrorRangeStart = -32099

  [<Obsolete("Use jsonRpcReservedErrorRangeStart instead")>]
  let serverErrorStart = jsonrpcReservedErrorRangeStart

  /// Error code indicating that a server received a notification or
  /// request before the server has received the `initialize` request.
  let serverNotInitialized = -32002

  ///This is the end range of JSON-RPC reserved error codes. It doesn't denote a real error code.
  let jsonrpcReservedErrorRangeEnd = -32000

  [<Obsolete("Use jsonRpcReservedErrorRangeEnd instead")>]
  let serverErrorEnd = jsonrpcReservedErrorRangeEnd


  /// This is the start range of LSP reserved error codes. It doesn't denote a real error code.
  let lspReservedErrorRangeStart = -32899

  /// A request failed but it was syntactically correct, e.g the
  /// method name was known and the parameters were valid. The error
  /// message should contain human readable information about why
  /// the request failed.
  /// @since 3.17.0
  let RequestFailed = -32803


  /// The server cancelled the request. This error code should
  /// only be used for requests that explicitly support being
  /// server cancellable.
  ///
  /// @since 3.17.0
  let serverCancelled = -32802

  /// The server detected that the content of a document got
  /// modified outside normal conditions. A server should
  /// NOT send this error code if it detects a content change
  /// in it unprocessed messages. The result even computed
  /// on an older state might still be useful for the client.
  ///
  /// If a client decides that a result is not of any use anymore
  /// the client should cancel the request.
  let contentModified = -32801

  /// The client has canceled a request and a server has detected the cancel.
  let requestCancelled = -32800
  /// This is the end range of LSP reserved error codes. It doesn't denote a real error code.
  let lspReservedErrorRangeEnd = -32899

type Error =
  { Code: int
    Message: string
    Data: JToken option }

  static member Create(code: int, message: string) = { Code = code; Message = message; Data = None }
  static member ParseError = Error.Create(ErrorCodes.parseError, "Parse error")
  static member InvalidRequest = Error.Create(ErrorCodes.invalidRequest, "Invalid Request")
  static member MethodNotFound = Error.Create(ErrorCodes.methodNotFound, "Method not found")
  static member InvalidParams = Error.Create(ErrorCodes.invalidParams, "Invalid params")
  static member InternalError = Error.Create(ErrorCodes.internalError, "Internal error")
  static member InternalErrorMessage message = Error.Create(ErrorCodes.internalError, message)
  static member RequestCancelled = Error.Create(ErrorCodes.requestCancelled, "Request cancelled")
  static member RequestCancelledMessage message = Error.Create(ErrorCodes.requestCancelled, message)

type Response =
  { [<JsonProperty("jsonrpc")>]
    Version: string
    Id: int option
    Error: Error option
    [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
    Result: JToken option }

  /// Json.NET conditional property serialization, controlled by naming convention
  member x.ShouldSerializeResult() = x.Error.IsNone

  static member Success(id: int, result: JToken option) =
    { Version = "2.0"; Id = Some id; Result = result; Error = None }

  static member Failure(id: int, error: Error) = { Version = "2.0"; Id = Some id; Result = None; Error = Some error }