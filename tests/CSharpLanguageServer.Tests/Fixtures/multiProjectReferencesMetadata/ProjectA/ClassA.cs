// ProjectA is listed FIRST in Solution.sln and deliberately does NOT reference the
// Newtonsoft.Json package that ProjectB below depends on. `textDocument/references`
// with IncludeDeclaration=true picks `solution.Projects |> Seq.head` as "any project"
// to resolve decompiled/metadata symbol locations (see References.fs), which lands on
// this project. Its compilation has no metadata reference for the Newtonsoft.Json
// assembly, so `compilation.GetMetadataReference(containingAssembly)` returns null.
class ClassA { }
