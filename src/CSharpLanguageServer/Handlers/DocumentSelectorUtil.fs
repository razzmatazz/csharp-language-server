namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

[<AutoOpen>]
module DocumentSelectorUtil =
    type DocumentFilter with
        static member Create(language: string) : DocumentFilter =
            { Language = Some language
              Scheme = Some "file"
              Pattern = Some "**/*.cs" }

    // Type abbreviations cannot have augmentations, extensions
    let defaultDocumentSelector: DocumentSelector =
        [| DocumentFilter.Create "csharp"; DocumentFilter.Create "cs" |]
