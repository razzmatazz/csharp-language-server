module CSharpLanguageServer.Runtime.DebugInfo

open Microsoft.Extensions.Logging

open CSharpLanguageServer.Logging
open CSharpLanguageServer.Util
open CSharpLanguageServer.Types
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Lsp.WorkspaceFolder
open CSharpLanguageServer.Runtime.RequestScheduling

let private logger = Logging.getLoggerByName "Runtime.DebugInfo"

// Types

type DebugWorkspaceFolderInfo =
    { uri: string
      name: string
      solutionState: string }

type DebugWorkspaceInfo =
    { phase: string
      folders: DebugWorkspaceFolderInfo list }

type DebugRequestInfo =
    { ordinal: int64
      name: string
      mode: string
      phase: string }

type DebugRequestStatsInfo =
    { name: string
      count: int
      avgDurationMs: float
      maxDurationMs: float }

type DebugRequestQueueInfo =
    { mode: string
      requests: DebugRequestInfo list
      stats: DebugRequestStatsInfo list }

type DebugInfo =
    { workspace: DebugWorkspaceInfo
      requestQueue: DebugRequestQueueInfo }

// Assembly

let private toDebugWorkspaceFolderInfo (wf: LspWorkspaceFolder) : DebugWorkspaceFolderInfo =
    let solutionState =
        match wf.Solution with
        | Uninitialized -> "Uninitialized"
        | Loading _ -> "Loading"
        | Ready _ -> "Ready"
        | Defunct _ -> "Defunct"

    { uri = wf.Uri
      name = wf.Name
      solutionState = solutionState }

let private toDebugRequestInfo (ordinal: int64) (r: RequestInfo) : DebugRequestInfo =
    let mode =
        match r.Mode with
        | ReadOnly -> "ReadOnly"
        | ReadWrite -> "ReadWrite"
        | ReadOnlyBackground -> "ReadOnlyBackground"

    let phase =
        match r.Phase with
        | Pending -> "Pending"
        | Running -> "Running"
        | Finished -> "Finished"

    { ordinal = ordinal
      name = r.Name
      mode = mode
      phase = phase }

let assembleDebugInfo
    (config: CSharpConfiguration)
    (workspace: LspWorkspace)
    (requestQueue: RequestQueue)
    : DebugInfo option =

    let debugMode = config.debug |> Option.bind _.debugMode |> Option.defaultValue false

    if not debugMode then
        None
    else
        let workspacePhase =
            match workspace.ReloadPending with
            | Some _ -> "ReloadPending"
            | None ->
                match requestQueue.Mode with
                | Dispatching -> "Dispatching"
                | DrainingUpTo ord -> $"DrainingUpTo({ord})"

        let queueMode =
            match requestQueue.Mode with
            | Dispatching -> "Dispatching"
            | DrainingUpTo ord -> $"DrainingUpTo({ord})"

        Some
            { workspace =
                { phase = workspacePhase
                  folders = workspace.Folders |> List.map toDebugWorkspaceFolderInfo }
              requestQueue =
                { mode = queueMode
                  requests =
                    requestQueue.Requests
                    |> Map.toList
                    |> List.map (fun (ord, r) -> toDebugRequestInfo ord r)
                  stats =
                    requestQueue.Stats
                    |> Map.toList
                    |> List.map (fun (name, m) ->
                        { name = name
                          count = m.Count
                          avgDurationMs =
                            if m.Count > 0 then
                                m.TotalDuration.TotalMilliseconds / float m.Count
                            else
                                0.0
                          maxDurationMs = m.MaxDuration.TotalMilliseconds }) } }

// Formatting

let private formatDebugRequests (requests: DebugRequestInfo list) : string =
    let headerRow = [ "Ordinal"; "Name"; "Mode"; "Phase" ]

    let dataRows =
        requests
        |> List.map (fun r -> [ string r.ordinal; $"\"{r.name}\""; r.mode; r.phase ])

    formatInColumns (headerRow :: dataRows)

let private formatDebugRequestStats (stats: DebugRequestStatsInfo list) : string =
    let headerRow = [ "Name"; "Count"; "AvgDuration (ms)"; "MaxDuration (ms)" ]

    let dataRows =
        stats
        |> List.sortByDescending _.avgDurationMs
        |> List.map (fun s ->
            [ $"\"{s.name}\""
              string s.count
              s.avgDurationMs.ToString("F2")
              s.maxDurationMs.ToString("F2") ])

    formatInColumns (headerRow :: dataRows)

// Logging

let dumpDebugInfo (info: DebugInfo) : unit =
    logger.LogDebug("------ Workspace ({phase}) ------", info.workspace.phase)

    if not (List.isEmpty info.workspace.folders) then
        for f in info.workspace.folders do
            logger.LogDebug("  {uri} [{name}]: {state}", f.uri, f.name, f.solutionState)

    logger.LogDebug("---------------------------------")

    if not (List.isEmpty info.requestQueue.requests) then
        logger.LogDebug("------ Current Requests ({mode}) ------", info.requestQueue.mode)
        logger.LogDebug("{requests}", (info.requestQueue.requests |> formatDebugRequests))
        logger.LogDebug("---------------------------------------")

    if not (List.isEmpty info.requestQueue.stats) then
        logger.LogDebug("--------- Request Stats ---------")
        logger.LogDebug("{stats}", (info.requestQueue.stats |> formatDebugRequestStats))
        logger.LogDebug("---------------------------------")
    else
        logger.LogDebug("------- No request stats  -------")
