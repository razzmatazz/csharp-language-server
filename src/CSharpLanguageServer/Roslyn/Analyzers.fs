module CSharpLanguageServer.Roslyn.Analyzers

open System.Collections.Immutable
open System.Threading

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Diagnostics

/// Returns compiler diagnostics + all analyzer diagnostics for an entire compilation.
/// Falls back to compiler-only if the project has no analyzer references.
let getCompilationDiagnosticsWithAnalyzers (project: Project) (compilation: Compilation) : Async<Diagnostic list> = async {
    let! ct = Async.CancellationToken

    let analyzers =
        project.AnalyzerReferences
        |> Seq.collect _.GetAnalyzers(LanguageNames.CSharp)
        |> ImmutableArray.CreateRange

    if analyzers.IsEmpty then
        return compilation.GetDiagnostics(ct) |> List.ofSeq
    else
        // project.AnalyzerOptions is provided by MSBuildWorkspace and already contains
        // the AnalyzerConfigOptionsProvider that reads .editorconfig severity rules.
        //
        // concurrentAnalysis is explicitly disabled so a single project's analyzer pass
        // cannot itself fan out across every core via Roslyn's AnalyzerDriver, matching
        // the precedent set by Roslyn's own IDE layer (DiagnosticIncrementalAnalyzer.
        // CompilationManager) and OmniSharp, both of which always run analyzers
        // non-concurrently to avoid starving other interactive work on the same process.
        let analysisOptions =
            CompilationWithAnalyzersOptions(
                options = project.AnalyzerOptions,
                onAnalyzerException = null,
                concurrentAnalysis = false,
                logAnalyzerExecutionTime = false
            )

        let cwa = compilation.WithAnalyzers(analyzers, analysisOptions)
        let! allDiags = cwa.GetAllDiagnosticsAsync(ct) |> Async.AwaitTask
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

    let analyzers =
        project.AnalyzerReferences
        |> Seq.collect _.GetAnalyzers(LanguageNames.CSharp)
        |> ImmutableArray.CreateRange

    if analyzers.IsEmpty then
        return semanticModel.GetDiagnostics(cancellationToken = ct) |> List.ofSeq
    else
        // project.AnalyzerOptions is provided by MSBuildWorkspace and already contains
        // the AnalyzerConfigOptionsProvider that reads .editorconfig severity rules.
        //
        // concurrentAnalysis is explicitly disabled — see the comment in
        // getCompilationDiagnosticsWithAnalyzers above for the rationale.
        let analysisOptions =
            CompilationWithAnalyzersOptions(
                options = project.AnalyzerOptions,
                onAnalyzerException = null,
                concurrentAnalysis = false,
                logAnalyzerExecutionTime = false
            )

        let cwa = semanticModel.Compilation.WithAnalyzers(analyzers, analysisOptions)

        let! allDiags = cwa.GetAllDiagnosticsAsync(ct) |> Async.AwaitTask

        // Filter to only diagnostics whose source location is within this document.
        // Diagnostics with no source location (e.g. compilation-level errors) are excluded
        // so they aren't duplicated across every open document.
        let mappedPathMatchesDocFilePath (d: Diagnostic) =
            let path = d.Location.GetMappedLineSpan().Path
            path = semanticModel.SyntaxTree.FilePath

        return allDiags |> Seq.filter mappedPathMatchesDocFilePath |> List.ofSeq
}
