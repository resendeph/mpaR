# CRAN submission comments

## Summary

This is a minor update of mpaR (0.4.0 to 0.5.0), which is already on CRAN. It is
a performance and correctness release and does not change the output of any
existing function.

Changes in this version:

* The internal dynamic-programming routines behind `traversal_weights()`,
  `node_weights()`, and all three `main_path()` strategies (global, local,
  key-route) were rewritten to be vectorised and layered. Results are identical
  to 0.4.0; on large citation networks (tens of thousands of vertices, hundreds
  of thousands of arcs) key-route extraction that previously took minutes now
  completes in seconds.
* `read_pajek()` now parses arcs in a single vectorised pass (the previous
  version was effectively quadratic in the number of arcs).
* New exported function `key_route_sweep()`, which computes the key-route main
  path for several values of k in one shared pass.
* `main_path()` gains an optional `k_range` argument (a slice of key routes by
  weight rank), mirroring Pajek's key-route range option.

No user-facing breaking changes: all exported signatures are unchanged except
`main_path()`, which gains one optional argument with a `NULL` default. The
rewritten internals were verified against the previous implementation across a
large randomized test suite, against Pajek on networks up to 50,000 vertices,
and by exhaustive source-to-sink path enumeration on small graphs.

## Test environments

* Local Windows 11, R 4.5.1 (2025-06-13 ucrt)
* Windows Server 2022 (win-builder), R-devel
* Windows Server 2022 (win-builder), R release
* Ubuntu 22.04 (R-hub), R-devel
* macOS (R-hub, Apple Silicon), R-devel

## R CMD check results

0 errors | 0 warnings | 0 notes

The incoming-check "Possibly misspelled words in DESCRIPTION" note, if it
appears again, refers to words that are correct as written: author surnames
from the cited reference (Hummon, Doreian), the method's abbreviation (MPA) and
its three traversal-weight measures (SPC, SPLC, SPNP), supported file formats
(Pajek, Gephi, gexf, graphml), and the imported package name (igraph).

## Downstream dependencies

Checked with revdepcheck::revdep_check(). There are currently no reverse
dependencies on CRAN.
