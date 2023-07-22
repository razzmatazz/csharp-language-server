namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Common.Types
open CSharpLanguageServer.Common.LspUtil

[<RequireQualifiedAccess>]
module LinkEditingRange =
    let provider (clientCapabilities: ClientCapabilities option) = None

    let registration (clientCapabilities: ClientCapabilities option) : Registration option = None
