namespace CSharpLanguageServer

open System
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types

type ProgressReporter(client: ILspClient) =
    let mutable canReport = false

    let mutable endSent = false

    member val Token = ProgressToken.C2 (Guid.NewGuid().ToString())

    member this.Begin(title, ?cancellable, ?message, ?percentage) = async {
        match! client.WorkDoneProgressCreate this.Token with
        | Error _ ->
            canReport <- false
        | Ok() ->
            canReport <- true
            let param = WorkDoneProgressBegin.Create(
                title = title,
                ?cancellable = cancellable,
                ?message = message,
                ?percentage = percentage
            )
            do! client.Progress(this.Token, param)
    }

    member this.Report(?cancellable, ?message, ?percentage) = async {
        if canReport && not endSent then
            let param = WorkDoneProgressReport.Create(
                ?cancellable = cancellable,
                ?message = message,
                ?percentage = percentage
            )
            do! client.Progress(this.Token, param)
    }

    member this.End(?message) = async {
        if canReport && not endSent then
            endSent <- true
            let param = WorkDoneProgressEnd.Create(
                ?message = message
            )
            do! client.Progress(this.Token, param)
    }
