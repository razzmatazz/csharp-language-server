// ProjectA has a deliberate compiler error so that the first workspace/diagnostic
// poll emits a non-empty full report for this file.  The bug being tested is that a
// subsequent poll (with the correct previousResultIds) does NOT re-emit a full report
// for this file as a side-effect of processing ProjectB.
XXX
