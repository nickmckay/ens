# ens

Ensemble methods and calculations for time-uncertain paleogeoscientific data.

`ens` provides the ensemble analysis engine that was previously part of
[geoChronR](https://github.com/nickmckay/GeoChronR):

- **Binning and alignment**: `bin()`, `binEns()`, `binTs()`, `alignTimeseriesBin()`
- **Correlation and regression across ensembles**: `corEns()`, `regressEns()`, `corMatrix()`
- **Ensemble PCA**: `pcaEns()`
- **Spectral analysis**: `computeSpectraEns()`, `ar1Surrogates()`, `createSyntheticTimeseries()`
- **False discovery rate**: `fdr()`
- **Utilities**: `gaussianize()`, `convertBP2AD()`/`convertAD2BP()`,
  `simulateAutoCorrelatedUncertainty()`, `generateEnsembleFromUncertainty()`
- **LiPD ensemble plumbing**: `selectData()`, `pullTsVariable()`,
  `mapAgeEnsembleToPaleoData()`

## Installation

```r
remotes::install_github("nickmckay/ens")
```

## Related packages

- [geoChronR](https://github.com/nickmckay/geoChronR-chronOnly) — age modeling (Bacon, Bchron, OxCal, BAM); imports this package
- [lipdViz](https://github.com/nickmckay/lipdViz) — visualization of LiPD data and ensemble output
- [lipdR](https://github.com/nickmckay/lipdR) — reading and writing LiPD files
