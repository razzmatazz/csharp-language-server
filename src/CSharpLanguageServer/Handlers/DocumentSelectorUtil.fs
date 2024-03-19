namespace CSharpLanguageServer.Handlers

open System.Runtime.CompilerServices

open Ionide.LanguageServerProtocol.Types

[<AutoOpen>]
module DocumentSelectorUtil =
    type DocumentFilter with
        static member Default: DocumentFilter =
            { Language = Some "cs"
              Scheme = Some "file"
              Pattern = Some "**/*.cs" }

    // Type abbreviations cannot have augmentations, extensions
    let defaultDocumentSelector: DocumentSelector = [| DocumentFilter.Default |]
