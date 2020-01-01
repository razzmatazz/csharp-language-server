namespace LSP 

open LSP.Log
open System
open System.IO
open System.Collections.Generic
open System.Text
open Types 

type private Version = {
    text: StringBuilder 
    mutable version: int
}

module DocumentStoreUtils = 
    let findRange(text: StringBuilder, range: Range): int * int = 
        let mutable line = 0
        let mutable char = 0
        let mutable startOffset = 0
        let mutable endOffset = 0
        for offset = 0 to text.Length do 
            if line = range.start.line && char = range.start.character then 
                startOffset <- offset 
            if line = range.``end``.line && char = range.``end``.character then 
                endOffset <- offset 
            if offset < text.Length then 
                let c = text.[offset]
                if c = '\n' then 
                    line <- line + 1
                    char <- 0
                else 
                    char <- char + 1
        (startOffset, endOffset)

open DocumentStoreUtils

type DocumentStore() = 
    /// All open documents, organized by absolute path
    let activeDocuments = new Dictionary<string, Version>()
    /// Replace a section of an open file
    let patch(doc: VersionedTextDocumentIdentifier, range: Range, text: string): unit = 
        let file = FileInfo(doc.uri.LocalPath)
        let existing = activeDocuments.[file.FullName]
        let startOffset, endOffset = findRange(existing.text, range)
        existing.text.Remove(startOffset, endOffset - startOffset) |> ignore
        existing.text.Insert(startOffset, text) |> ignore
        existing.version <- doc.version
    /// Replace the entire contents of an open file
    let replace(doc: VersionedTextDocumentIdentifier, text: string): unit = 
        let file = FileInfo(doc.uri.LocalPath)
        let existing = activeDocuments.[file.FullName]
        existing.text.Clear() |> ignore
        existing.text.Append(text) |> ignore
        existing.version <- doc.version
    
    member this.Open(doc: DidOpenTextDocumentParams): unit = 
        let file = FileInfo(doc.textDocument.uri.LocalPath)
        let text = StringBuilder(doc.textDocument.text)
        let version = {text = text; version = doc.textDocument.version}
        activeDocuments.[file.FullName] <- version
    
    member this.Change(doc: DidChangeTextDocumentParams): unit = 
        let file = FileInfo(doc.textDocument.uri.LocalPath)
        let existing = activeDocuments.[file.FullName]
        if doc.textDocument.version <= existing.version then 
            let oldVersion = existing.version
            let newVersion = doc.textDocument.version 
            dprintfn "Change %d to doc %s is earlier than existing version %d" newVersion file.Name oldVersion
        else 
            for change in doc.contentChanges do 
                match change.range with 
                | Some range -> patch(doc.textDocument, range, change.text) 
                | None -> replace(doc.textDocument, change.text) 

    member this.GetText(file: FileInfo): string option = 
        let found, value = activeDocuments.TryGetValue(file.FullName)
        if found then Some(value.text.ToString()) else None 

    member this.GetVersion(file: FileInfo): int option = 
        let found, value = activeDocuments.TryGetValue(file.FullName)
        if found then Some(value.version) else None 

    member this.Get(file: FileInfo): option<string * int> = 
        let found, value = activeDocuments.TryGetValue(file.FullName)
        if found then Some(value.text.ToString(), value.version) else None 

    member this.Close(doc: DidCloseTextDocumentParams): unit = 
        let file = FileInfo(doc.textDocument.uri.LocalPath)
        activeDocuments.Remove(file.FullName) |> ignore

    member this.OpenFiles(): FileInfo list = 
        [for file in activeDocuments.Keys do yield FileInfo(file)]