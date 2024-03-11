namespace CSharpLanguageServer.Common

open Ionide.LanguageServerProtocol.Types

module LspUtil =
    /// Return the JSON-RPC "not implemented" error
    let notImplemented<'t> = async.Return LspResult.notImplemented<'t>

    /// Do nothing and ignore the notification
    let ignoreNotification: Async<unit> = async.Return(())
