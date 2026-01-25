namespace CSharpLanguageServer.Lsp

open System
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types

type ProgressReporter(client: ILspClient, clientCapabilities: ClientCapabilities) =
    let mutable canReport = false
    let mutable endSent = false

    let workDoneProgressSupported =
        clientCapabilities.Window
        |> Option.bind _.WorkDoneProgress
        |> Option.defaultValue false

    member val Token = ProgressToken.C2(Guid.NewGuid().ToString())

    member this.Begin(title, ?cancellable, ?message, ?percentage) = async {
        if not workDoneProgressSupported then
            canReport <- false
        else
            let! progressCreateResult = client.WindowWorkDoneProgressCreate { Token = this.Token }

            match progressCreateResult with
            | Error _ -> canReport <- false
            | Ok() ->
                canReport <- true

                let param =
                    WorkDoneProgressBegin.Create(
                        title = title,
                        ?cancellable = cancellable,
                        ?message = message,
                        ?percentage = percentage
                    )

                do!
                    client.Progress
                        { Token = this.Token
                          Value = serialize param }
    }

    member this.Report(?cancellable, ?message, ?percentage) = async {
        if canReport && not endSent then
            let param =
                WorkDoneProgressReport.Create(?cancellable = cancellable, ?message = message, ?percentage = percentage)

            do!
                client.Progress
                    { Token = this.Token
                      Value = serialize param }
    }

    member this.End(?message) = async {
        if canReport && not endSent then
            endSent <- true
            let param = WorkDoneProgressEnd.Create(?message = message)

            do!
                client.Progress
                    { Token = this.Token
                      Value = serialize param }
    }
