namespace CSharpLanguageServer.Handlers

open System
open System.Collections.Immutable

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.State

[<RequireQualifiedAccess>]
module CSharpMetadata =
    type CSharpMetadataParams = {
        TextDocument: TextDocumentIdentifier
    }

    type CSharpMetadataResponse = CSharpMetadata

    let handle (scope: ServerRequestScope) (metadataParams: CSharpMetadataParams): AsyncLspResult<CSharpMetadataResponse option> = async {
        let uri = metadataParams.TextDocument.Uri

        let metadataMaybe =
            scope.DecompiledMetadata
            |> Map.tryFind uri
            |> Option.map (fun x -> x.Metadata)

        return metadataMaybe |> LspResult.success
    }
