namespace CSharpLanguageServer.Handlers

open CSharpLanguageServer.Runtime.DebugInfo

/// The `$/csharp/debugInfo` endpoint bypasses the normal request-scheduling
/// pipeline entirely. `Server.fs` posts a `GetDebugInfo` event directly to the
/// state actor, which assembles and replies with `DebugInfo option` (None when
/// debugMode is off). `Debug.handle` passes the result through.
[<RequireQualifiedAccess>]
module Debug =
    let handle (info: DebugInfo option) : DebugInfo option = info
