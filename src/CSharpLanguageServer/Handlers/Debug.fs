namespace CSharpLanguageServer.Handlers

open Newtonsoft.Json.Linq

open Ionide.LanguageServerProtocol.JsonRpc

open CSharpLanguageServer.Runtime.DebugInfo
open CSharpLanguageServer.Runtime.RequestScheduling
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Types

/// The `$/csharp/debugInfo` endpoint runs as an `OutOfBand` request so it is
/// never
/// gated by any scheduling rule.
[<RequireQualifiedAccess>]
module Debug =
    let handle
        (getDebugInfo: unit -> Async<DebugInfo option>)
        (_ctx: RequestContext)
        (_params: JObject)
        : Async<LspResult<DebugInfo option> * LspWorkspaceUpdate> =
        async {
            let! info = getDebugInfo ()
            return Ok info, LspWorkspaceUpdate.Empty
        }
