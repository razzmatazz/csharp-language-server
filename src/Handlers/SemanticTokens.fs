namespace CSharpLanguageServer.Handlers

open Ionide.LanguageServerProtocol.Types

open CSharpLanguageServer.Common.Types
open CSharpLanguageServer.Common.LspUtil

[<RequireQualifiedAccess>]
module SemanticTokens =
    let provider: SemanticTokensOptions option = None

    let handleFull (wm: IWorkspaceManager) (p: SemanticTokensParams) : AsyncLspResult<SemanticTokens option> =
        notImplemented

    let handleFullDelta
        (wm: IWorkspaceManager)
        (p: SemanticTokensDeltaParams)
        : AsyncLspResult<U2<SemanticTokens, SemanticTokensDelta> option> =
        notImplemented

    let handleRange (wm: IWorkspaceManager) (p: SemanticTokensRangeParams) : AsyncLspResult<SemanticTokens option> =
        notImplemented
