// ProjectB is intentionally clean (no diagnostics).  The bug being tested caused
// ProjectB's diagnostic pass to emit an empty full report for ProjectA/ClassA.cs
// (because that URI was in previousResultIds but has no diagnostics in ProjectB),
// overwriting its resultId with ProjectB's resultId.  On the next poll ProjectA then
// saw a mismatched resultId and re-emitted the real diagnostics, cycling endlessly.
class ClassB { }
