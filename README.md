---
title: ttmouse
emoji: 🐭
colorFrom: blue
colorTo: green
sdk: docker
app_port: 7860
pinned: false
license: mit
short_description: Circadian clock phase & dysfunction from mouse RNA-seq
---

# ttmouse

Engine for a public tool that projects raw **mouse RNA-seq count matrices** onto
pre-built **TimeTeller** circadian-clock models and returns the model results
(predicted phase and dysfunction). The Shiny front-end and Docker packaging are
later phases (not in this package yet).

Pipeline (runtime):
`read_counts()` → `preprocess_counts()` → `validate_genes()` → `project_test_data()`
→ `Test_Data$Results_df` (CSV export) + plots.


## Dependencies

Declared in `DESCRIPTION`. Notably `edgeR` + `DGEobj.utils` (used by
`counts_to_tpm`) and `TimeTeller` itself, pulled from GitHub via `Remotes`.
**Pin the TimeTeller commit** in `Remotes:` for reproducibility
(`VadimVasilyev1994/TimeTeller-v2@<commit>`).

## Build / document / test (full environment with all deps installed)

```r
devtools::document()   # regenerate NAMESPACE + man/ from roxygen
devtools::test()       # run tests/testthat
devtools::check()      # R CMD check
```

`data-raw/build_gene_lengths.R` is run **once, offline** to produce the
`gene_lengths` table; it is not part of the runtime package.

## Status

Engine functions are implemented and tested. Not yet built: the plot wrappers
and CSV export, the metadata-file reader, the Shiny app (`inst/app/` +
`run_app()`), and the Dockerfile. See `HANDOFF.md`.
