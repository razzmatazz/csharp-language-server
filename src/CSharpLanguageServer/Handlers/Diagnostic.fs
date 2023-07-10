namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Types
open CSharpLanguageServer.Common.LspUtil

[<RequireQualifiedAccess>]
module Diagnostic =
    let provider = None
