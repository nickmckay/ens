# ens 0.2.2

* `binTs()` and `pullTsVariable()` accept timeseries tibbles (`lipdR::ts2tibble()`) and plain data frames. `binTs()` used to pass tibbles to `lipdR::untidyTs()`, which expects the long `tidyTs()` format, so tibble input failed; `pullTsVariable()` refused tibbles.
* `binTs(time.var = "time")` bins each record's age ensemble, or otherwise its time axis, converted to the new `time.units` (default "yr BP") with `lipdR::getTsTime()`. Records in year CE and yr BP can then be binned together. Requires lipdR >= 0.7.3.

# ens 0.2.1

* Requires lipdR >= 0.7.0.
