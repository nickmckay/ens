# ens 0.3.0

Uses the time scales lipdR (>= 0.8.1) puts on every time column, so time axes are converted, not guessed.

* `binTs()` gains `time.variable`, to bin any time variable by name (e.g. "age", "year", "calAge"), converted to `time.units`. `time.units` now defaults to NULL: the records' own scale if they all share one, and yr BP otherwise (`lipdR::commonTimeUnits()`), with a message saying which. Records whose scale was assumed get one warning per call. The result has a "timeUnits" attribute.
* `corEns()`, `regressEns()` and `alignTimeseriesBin()`: when both time variables carry a time scale (as variables read with lipdR >= 0.8.0 do) and the scales differ, the second is converted to the first one's scale, with a message. Radiocarbon years give an error. `corEns()` returns the scale as `timeUnits`.
* `binEns()` accepts variable lists (`selectData()` output) and returns their scale as `timeUnits`.
* `mapAgeEnsembleToPaleoData()` gives the paleo ensemble and median columns the chron ensemble's time scale.
* `heuristicUnits()` returns a variable's own time scale when it has one, and only guesses otherwise.

# ens 0.2.2

* `binTs()` and `pullTsVariable()` accept timeseries tibbles (`lipdR::ts2tibble()`) and plain data frames. `binTs()` used to pass tibbles to `lipdR::untidyTs()`, which expects the long `tidyTs()` format, so tibble input failed; `pullTsVariable()` refused tibbles.
* `binTs(time.var = "time")` bins each record's age ensemble, or otherwise its time axis, converted to the new `time.units` (default "yr BP") with `lipdR::getTsTime()`. Records in year CE and yr BP can then be binned together. Requires lipdR >= 0.7.3.

# ens 0.2.1

* Requires lipdR >= 0.7.0.
