namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open Microsoft.Extensions.Logging

open CSharpLanguageServer.Runtime.RequestScheduling
open CSharpLanguageServer.Logging

[<RequireQualifiedAccess>]
module Trace =
    let private logger = Logging.getLoggerByName "Trace"

    let handleSetTrace (context: RequestContext) (p: SetTraceParams) : Async<LspResult<unit>> = async {
        logger.LogDebug("handleSetTrace: trace level changed to {traceLevel}", p.Value)
        context.UpdateEffects(_.WithTraceLevelChange(p.Value))
        return Ok()
    }
