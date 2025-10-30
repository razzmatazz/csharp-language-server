namespace CSharpLanguageServer.State

open System
open System.IO
open System.Threading
open System.Collections.Generic
open System.Text.RegularExpressions

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open Microsoft.Build.Construction
open Microsoft.Build.Exceptions
open Microsoft.Build.Locator
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.MSBuild
open Microsoft.CodeAnalysis.Text
open Microsoft.Extensions.Logging

open CSharpLanguageServer
open CSharpLanguageServer.Lsp
open CSharpLanguageServer.Logging
open CSharpLanguageServer.Util
open CSharpLanguageServer.Roslyn.Conversions
open CSharpLanguageServer.Roslyn.WorkspaceServices
open CSharpLanguageServer.Roslyn.Solution
open CSharpLanguageServer.Types


type DecompiledMetadataDocument =
    { Metadata: CSharpMetadataInformation
      Document: Document }


/// Codebase wraps roslyn Solution and encapsulates extra metadata
/// that goes along
type Codebase =
    { Solution: Solution
      DecompiledMetadata: Map<string, DecompiledMetadataDocument> }

    static member LoadWithPathOrOnCwd(lspClient: ILspClient, solutionPathMaybe: string option, cwd: string) = async {
        let! solution = async {
            match solutionPathMaybe with
            | Some solutionPath ->
                let rootedSolutionPath =
                    match Path.IsPathRooted solutionPath with
                    | true -> solutionPath
                    | false -> Path.Combine(cwd, solutionPath)

                return! solutionTryLoadOnPath lspClient rootedSolutionPath

            | None ->
                let logMessage: LogMessageParams =
                    { Type = MessageType.Info
                      Message = sprintf "csharp-ls: attempting to find and load solution based on cwd (\"%s\").." cwd }

                do! lspClient.WindowLogMessage logMessage
                return! solutionFindAndLoadOnDir lspClient cwd
        }

        return
            { Solution = solution
              DecompiledMetadata = Map.empty }
    }
