# CLAUDE.md — ens

`ens` is the **foundation** of a five-package paleogeoscience family (split out of the
monolithic geoChronR in June 2026). It holds the ensemble methods and the shared
uncertainty-propagation / null-hypothesis engine that the rest of the family builds on.

## Package family (dependency DAG: ens ← lipdViz ← geoChronR; actR & compositeR on top)

| Repo (`~/GitHub/...`) | GitHub | Branch | Role |
|---|---|---|---|
| **ens** (this repo) | nickmckay/ens | main | Ensemble methods + UQ engine |
| lipdViz | nickmckay/lipdViz | main | Plotting + mapping; imports ens |
| geoChronR-chronOnly | nickmckay/geoChronR-chronOnly | main | geoChronR 2.0: age modeling; re-exports ens+lipdViz |
| actR | **LinkedEarth/actR** | refactor | Abrupt-change detection on the ens engine |
| compositeR | nickmckay/compositeR | refactor | Record compositing on ens/lipdViz |

## What lives here

- **UQ engine** (`R/propagate.R`, `R/null-hypothesis.R`): `propagateUncertainty(time, vals, changeFun, ...)`, `testNullHypothesis(...)`, `kdePval()`. Contract: any `changeFun(time, vals, ...) -> one-row data.frame` gets ensemble propagation + surrogate significance for free. `summarize` takes a caller-supplied `function(propagated) -> data.frame` (generic, not analysis-specific).
- **Analysis**: `corEns`/`regressEns` (`R/correlation.regression.functions.R`), `pcaEns` (`R/pca.R`), `computeSpectraEns` (`R/spectra.R`), `fdr` (`R/fdr.R`). These return S3-classed lists (`corEns`/`regressEns`/`spectraEns`/`pcaEns`) with `print` methods (`R/print-methods.R`); `plot` methods live in lipdViz.
- **Binning**: `bin`/`binEns`/`binTs` (vectorized).
- **BAM core**: `simulateBam`/`bamCorrect` (`R/bam.R`) — pure R, moved here from geoChronR.
- **LiPD plumbing**: `selectData`, `pullTsVariable`, `mapAgeEnsembleToPaleoData` (`R/lipd-manipulation.R`).

## Conventions & gotchas

- **Two-layer results**: heavy numerics stay plain matrices/data.frames (so `$`-access and
  golden tests are unchanged); the outer object carries an S3 class + print/plot/summary.
  See the "Extending the ensemble stack" vignette.
- **Plain roxygen** (NO `Roxygen: list(markdown = TRUE)`). A literal `%` is an Rd comment
  char — write "percent". Keep param text escape-clean: downstream markdown packages
  `@inheritParams ens::foo` and re-emit it raw.
- **Golden tests** (`tests/testthat/`): regenerate with `tools/capture_reference.R`. They
  lock bin/corMatrix/corEns numerics bit-identically.
- CI: R CMD check on Windows/Linux/macOS with vignettes. Avoid introducing NEW dplyr calls
  in vignette-exercised code (macOS runner dplyr segfaults).

## Deferred TODO

- Re-express the UQ engine's parameter storage as a list column instead of an eval()-parsed
  string (a deliberate behavior change; would update actR goldens).

## Dev

`devtools::load_all()` · `devtools::document()` · `devtools::test()` · `devtools::check()`.
After changing exports, regenerate geoChronR's re-exports (`geoChronR-chronOnly/tools/gen_reexports.R`).
Commit work when complete. Co-author trailer: `Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>`.
