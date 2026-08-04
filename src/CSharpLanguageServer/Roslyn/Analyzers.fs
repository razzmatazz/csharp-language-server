module CSharpLanguageServer.Roslyn.Analyzers

open System
open System.Collections.Concurrent
open System.Collections.Immutable
open System.Runtime.CompilerServices
open System.Threading
open System.Threading.Tasks

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Diagnostics

type private SolutionAnalysisCache = ConcurrentDictionary<ProjectId, Lazy<Task<ImmutableArray<Diagnostic>>>>

let private solutionAnalysisCaches =
    ConditionalWeakTable<Solution, SolutionAnalysisCache>()

let private projectAnalyzers (project: Project) =
    project.AnalyzerReferences
    |> Seq.collect _.GetAnalyzers(LanguageNames.CSharp)
    |> ImmutableArray.CreateRange

let private getSharedProjectAnalysis
    (project: Project)
    (compilation: Compilation)
    (analyzers: ImmutableArray<DiagnosticAnalyzer>)
    : Async<ImmutableArray<Diagnostic>> =
    async {
        let solutionCache =
            solutionAnalysisCaches.GetValue(project.Solution, fun _ -> SolutionAnalysisCache())

        let analysisTask =
            solutionCache
                .GetOrAdd(
                    project.Id,
                    fun _ ->
                        lazy
                            // Cancellation applies to each waiter, not to the shared analysis.
                            let cwa = compilation.WithAnalyzers(analyzers, project.AnalyzerOptions)
                            cwa.GetAllDiagnosticsAsync(CancellationToken.None)
                )
                .Value

        let! ct = Async.CancellationToken
        return! analysisTask.WaitAsync(ct) |> Async.AwaitTask
    }

/// Returns compiler diagnostics + all analyzer diagnostics for an entire compilation.
/// Falls back to compiler-only if the project has no analyzer references.
let getCompilationDiagnosticsWithAnalyzers (project: Project) (compilation: Compilation) : Async<Diagnostic list> = async {
    let! ct = Async.CancellationToken

    let analyzers = projectAnalyzers project

    if analyzers.IsEmpty then
        return compilation.GetDiagnostics(ct) |> List.ofSeq
    else
        let! allDiags = getSharedProjectAnalysis project compilation analyzers
        return allDiags |> List.ofSeq
}

/// Returns compiler diagnostics + analyzer diagnostics for a single document's semantic model.
/// Falls back to compiler-only if the project has no analyzer references.
///
/// Uses GetAllDiagnosticsAsync on the full compilation and filters to the document's file
/// path rather than GetAnalyzerSemanticDiagnosticsAsync, because some IDE analyzers (e.g.
/// IDE0040 "accessibility modifiers") emit diagnostics whose reported location causes them
/// to be missed by the span-based semantic filter.
let getDocumentDiagnosticsWithAnalyzers (project: Project) (semanticModel: SemanticModel) : Async<Diagnostic list> = async {
    let! ct = Async.CancellationToken

    let analyzers = projectAnalyzers project

    if analyzers.IsEmpty then
        return semanticModel.GetDiagnostics(cancellationToken = ct) |> List.ofSeq
    else
        let! allDiags = getSharedProjectAnalysis project semanticModel.Compilation analyzers

        // Filter to only diagnostics whose source location is within this document.
        // Diagnostics with no source location (e.g. compilation-level errors) are excluded
        // so they aren't duplicated across every open document.
        let mappedPathMatchesDocFilePath (d: Diagnostic) =
            let path = d.Location.GetMappedLineSpan().Path
            path = semanticModel.SyntaxTree.FilePath

        return allDiags |> Seq.filter mappedPathMatchesDocFilePath |> List.ofSeq
}
