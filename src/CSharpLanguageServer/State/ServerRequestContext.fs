namespace CSharpLanguageServer.State

open CSharpLanguageServer.State.ServerState

type ServerRequestContext(state: ServerState, emit: ServerStateEvent -> unit) =
    member _.State = state
    member _.Workspace = state.Workspace
    member _.ClientCapabilities = state.ClientCapabilities
    member _.Emit = emit
