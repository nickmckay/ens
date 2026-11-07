# pullTsVariable() and binTs() accept timeseries tibbles, and binTs() can bin on time

makeTS <- function() {
  yrs <- seq(1005, 1995, by = 10)
  ce <- list(dataSetName = "ce", paleoData_TSid = "a", paleoData_values = yrs / 1000,
             year = yrs, time = yrs, timeUnits = "yr AD", timeDatum = 0,
             timeDirection = "prograde", timeExponent = 0)
  bp <- list(dataSetName = "bp", paleoData_TSid = "b", paleoData_values = yrs / 1000,
             age = 1950 - yrs, time = 1950 - yrs, timeUnits = "yr BP", timeDatum = 1950,
             timeDirection = "retrograde", timeExponent = 0)
  structure(list(ce, bp), class = c("lipd_ts", "list"))
}

test_that("pullTsVariable takes a timeseries tibble or data frame", {
  TS <- makeTS()
  tib <- lipdR::ts2tibble(TS)
  expect_equal(pullTsVariable(tib, "dataSetName"), c("ce", "bp"))
  expect_equal(pullTsVariable(as.data.frame(tib), "dataSetName"), c("ce", "bp"))
})

test_that("binTs on time puts CE and BP records on one scale, from a list or tibble", {
  TS <- makeTS()
  bv <- seq(0, 1000, by = 100)
  b <- suppressWarnings(binTs(TS, time.var = "time", bin.vec = bv))
  expect_equal(b[[1]]$matrix, b[[2]]$matrix)
  bt <- suppressWarnings(binTs(lipdR::ts2tibble(TS), time.var = "time", bin.vec = bv))
  expect_equal(bt[[1]]$matrix, b[[1]]$matrix)
  ce <- suppressWarnings(binTs(TS, time.var = "time", time.units = "CE", bin.vec = seq(1000, 2000, by = 100)))
  expect_equal(ce[[1]]$matrix, ce[[2]]$matrix)
})
