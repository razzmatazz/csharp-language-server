namespace CSharpLanguageServer.Lsp

open System
open CSharpLanguageServer.Server

module Server =
    let start options =
        try
            let result = startCore options
            int result
        with
        | _ex ->
            // logger.error (Log.setMessage "Start - LSP mode crashed" >> Log.addExn ex)
            3
