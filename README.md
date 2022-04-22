# Truth maintenance system libraries for Scala and Haskell

This repository is a translation from Common Lisp of the truth
maintenance systems and other tools from Forbus and de Kleer's
*Building Problem Solvers* into both Scala and Haskell.

The current version includes working versions of the justification-
and assumption-based truth maintenance systems (JTMS and ATMS) in both
languages.  The Haskell translation omits the two top-level functions
`interpretations` and `explainNode` of the original system, but
otherwise both translations include all of the original functionality
of these two TMSes.  This branch includes an in-progress
implementation of LTMS.  There is a partial translation of the rule
engine wrapper for these TMSes in Scala.

To contribute, submit pull requests to one of the three branches
`atms` (Scala), `jtms` (Scala), `ltms` (Scala) or `haskell` (both JTMS
and ATMS), or start a new branch for one of the other BPS tools, at
[its repository](https://github.com/jphmrst/bps-scala).
