# CRAN submission comments

## Test environments

* Local Windows 11, R 4.4.x, win-builder build tools (RBuildTools 4.4)
* Windows Server 2022 (win-builder), R-devel (2026-06-17 r90169 ucrt)
* Windows Server 2022 (win-builder), R 4.6.0 (release)

## R CMD check results

0 errors | 0 warnings | 1 note

The single NOTE is expected for a first submission:

```
Maintainer: 'Paulo H Resende <paulo.resende@ttu.edu>'

New submission

Possibly misspelled words in DESCRIPTION:
  Doreian, Gephi, Hummon, MPA, Pajek, SPC, SPLC, SPNP, gexf, graphml, igraph
```

All flagged words are correct as written: author surnames from the cited
reference (Hummon, Doreian), the method's own abbreviation (MPA) and its
three traversal-weight measures (SPC, SPLC, SPNP), supported file formats
(Pajek, Gephi, gexf, graphml), and the imported package name (igraph).

## Downstream dependencies

This is a new package with no reverse dependencies.
