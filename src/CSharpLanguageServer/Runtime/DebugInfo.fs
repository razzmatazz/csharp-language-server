module CSharpLanguageServer.Runtime.DebugInfo

open Microsoft.Extensions.Logging

open CSharpLanguageServer.Logging
open CSharpLanguageServer.Util
open CSharpLanguageServer.Types
open CSharpLanguageServer.Lsp.Workspace
open CSharpLanguageServer.Lsp.WorkspaceFolder
open CSharpLanguageServer.Runtime.RequestScheduling
open CSharpLanguageServer.Runtime.JsonRpc

let private logger = Logging.getLoggerByName "Runtime.DebugInfo"

// Types

type DebugWorkspaceFolderInfo =
    { uri: string
      name: string
      solutionState: string
      solutionPathOverride: string option
      generation: string
      pushDiagnosticsBacklogUpdatePending: bool }

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

type DebugRpcStats =
    { phase: string
      writeQueueLength: int
      pendingOutboundCallCount: int
      runningInboundRequestCount: int
      timerArmed: bool
      recentlyTimedOutCallCount: int }

type DebugInfo =
    { workspace: DebugWorkspaceInfo
      requestQueue: DebugRequestQueueInfo
      jsonRpc: DebugRpcStats option }

// Assembly

let private toDebugWorkspaceFolderInfo (wf: LspWorkspaceFolder) : DebugWorkspaceFolderInfo =
    { uri = wf.Uri
      name = wf.Name
      solutionState = sprintf "%A" wf.Solution
      solutionPathOverride = wf.SolutionPathOverride
      generation = string wf.Generation
      pushDiagnosticsBacklogUpdatePending = wf.PushDiagnosticsBacklogUpdatePending }

let private toDebugRequestInfo (ordinal: int64) (r: RequestInfo) : DebugRequestInfo =
    { ordinal = ordinal
      name = r.Name
      mode = string r.Mode
      phase = string r.Phase }

let assembleDebugInfo
    (config: CSharpConfiguration)
    (workspace: LspWorkspace)
    (requestQueue: RequestQueue)
    (jsonRpcInfo: JsonRpcStats option)
    : DebugInfo option =

    let debugMode = config.debug |> Option.bind _.debugMode |> Option.defaultValue false

    if not debugMode then
        None
    else
        let queueMode =
            match requestQueue.Mode with
            | Dispatching -> "Dispatching"
            | DrainingUpTo ord -> $"DrainingUpTo({ord})"

        let debugJsonRpcInfo: DebugRpcStats option =
            jsonRpcInfo
            |> Option.map (fun r ->
                { phase = r.Phase
                  writeQueueLength = r.WriteQueueLength
                  pendingOutboundCallCount = r.PendingOutboundCallCount
                  runningInboundRequestCount = r.RunningInboundRequestCount
                  timerArmed = r.TimerArmed
                  recentlyTimedOutCallCount = r.RecentlyTimedOutCallCount })

        Some
            { workspace =
                { phase = string workspace.Phase
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
                          maxDurationMs = m.MaxDuration.TotalMilliseconds }) }
              jsonRpc = debugJsonRpcInfo }

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
            let pathOverride = f.solutionPathOverride |> Option.defaultValue "(none)"

            logger.LogDebug(
                "  {uri} [{name}]: {state} override={pathOverride} gen={generation} pdPending={pdPending}",
                f.uri,
                f.name,
                f.solutionState,
                pathOverride,
                f.generation,
                f.pushDiagnosticsBacklogUpdatePending
            )

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

    match info.jsonRpc with
    | None -> ()
    | Some rpc ->
        logger.LogDebug("-------- JSON-RPC Transport ({phase}) --------", rpc.phase)
        logger.LogDebug("  WriteQueue length     : {n}", rpc.writeQueueLength)
        logger.LogDebug("  Outbound calls pending: {n}", rpc.pendingOutboundCallCount)
        logger.LogDebug("  Inbound reqs running  : {n}", rpc.runningInboundRequestCount)
        logger.LogDebug("  Timer armed           : {v}", rpc.timerArmed)
        logger.LogDebug("  Recently timed out    : {n}", rpc.recentlyTimedOutCallCount)
        logger.LogDebug("----------------------------------------------")
