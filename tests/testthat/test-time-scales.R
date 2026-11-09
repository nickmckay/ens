# Time scales carried by variable lists (lipdR >= 0.8.0) and by timeseries

bpVar <- function(v) list(variableName = "age", units = "yr BP", values = v,
                          timeUnits = "yr BP", timeDatum = 1950, timeDirection = "retrograde",
                          timeExponent = 0, timeSource = "units")
ceVar <- function(v) list(variableName = "year", units = "CE", values = v,
                          timeUnits = "yr AD", timeDatum = 0, timeDirection = "prograde",
                          timeExponent = 0, timeSource = "units")

test_that("matchTimeScales converts the second variable to the first one's scale", {
  expect_message(m <- matchTimeScales(bpVar(c(0, 100)), ceVar(c(1950, 1850))), "from yr AD to yr BP")
  expect_equal(m[[2]]$values, c(0, 100))
  expect_equal(m[[2]]$timeUnits, "yr BP")
  expect_equal(m[[2]]$timeDirection, "retrograde")

  # nothing to do without scales, or when they already match
  expect_no_message(m <- matchTimeScales(c(0, 100), ceVar(c(1950, 1850))))
  expect_equal(m[[2]]$values, c(1950, 1850))
  expect_no_message(matchTimeScales(bpVar(1), bpVar(2)))

  rc <- bpVar(c(1000, 2000))
  rc$units <- "yr 14C BP"
  expect_error(matchTimeScales(bpVar(1), rc), "Radiocarbon")
})

test_that("corEns and binEns work across scales and report the scale", {
  set.seed(3)
  ages <- seq(0, 2000, by = 10)
  vals <- cumsum(rnorm(length(ages)))
  v <- list(values = vals, variableName = "x", units = "u")
  expect_message(ce <- corEns(bpVar(ages), v, ceVar(1950 - ages), v, bin.step = 50, percentiles = NA,
                              gaussianize = FALSE), "to yr BP")
  expect_equal(ce$timeUnits, "yr BP")
  expect_gt(min(ce$cor.df$r, na.rm = TRUE), 0.99)

  b <- binEns(bpVar(ages), v, bin.vec = seq(0, 2000, by = 100))
  expect_equal(b$timeUnits, "yr BP")
  expect_equal(nrow(b$matrix), 20)
})

test_that("binTs uses the records' shared scale by default, and a named time variable", {
  yrs <- seq(1005, 1995, by = 10)
  mk <- function(id) list(dataSetName = id, paleoData_TSid = id, paleoData_values = yrs / 1000,
                          year = yrs, yearUnits = "CE", time = yrs, timeUnits = "yr AD", timeDatum = 0,
                          timeDirection = "prograde", timeExponent = 0, timeVariableName = "year",
                          age = 1950 - yrs, ageUnits = "yr BP")
  TS <- structure(list(mk("a"), mk("b")), class = c("lipd_ts", "list"))
  expect_message(b <- binTs(TS, time.var = "time", bin.vec = seq(1000, 2000, by = 100)), "Binning on yr AD")
  expect_equal(attr(b, "timeUnits"), "yr AD")
  expect_false(all(is.na(b[[1]]$matrix)))

  bp <- suppressMessages(binTs(TS, time.variable = "age", time.units = "ka", bin.vec = seq(-0.05, 0.95, by = 0.1)))
  expect_equal(attr(bp, "timeUnits"), "ka")
  expect_equal(bp[[1]]$matrix, b[[1]]$matrix[rev(seq_len(nrow(b[[1]]$matrix))), , drop = FALSE],
               ignore_attr = TRUE)
})

test_that("heuristicUnits uses a stated time scale before guessing", {
  expect_equal(heuristicUnits(ceVar(c(1, 2, 3))), "yr AD")
  assumed <- ceVar(c(1, 2, 3))
  assumed$timeSource <- "assumed"
  expect_false(identical(heuristicUnits(assumed), "yr AD"))
})

test_that("mapAgeEnsembleToPaleoData carries the chron ensemble's time scale", {
  ens <- matrix(rep(c(10, 20, 30), 5) + rep(0:4, each = 3), nrow = 3)
  L <- list(dataSetName = "t",
            paleoData = list(list(measurementTable = list(list(
              depth = list(variableName = "depth", units = "cm", values = c(1, 2, 3)),
              x = list(variableName = "x", units = "u", values = c(5, 6, 7)))))),
            chronData = list(list(model = list(list(ensembleTable = list(list(
              depth = list(variableName = "depth", units = "cm", values = c(1, 2, 3)),
              ageEnsemble = c(list(variableName = "ageEnsemble", values = ens),
                              bpVar(NULL)[c("units", "timeUnits", "timeDatum", "timeDirection", "timeExponent")],
                              list(timeSource = "metadata")))))))))
  out <- suppressWarnings(utils::capture.output(L2 <- mapAgeEnsembleToPaleoData(L, max.ens = 5)))
  col <- L2$paleoData[[1]]$measurementTable[[1]]$ageEnsemble
  expect_equal(col$timeDatum, 1950)
  expect_equal(col$timeSource, "metadata")
  expect_equal(L2$paleoData[[1]]$measurementTable[[1]]$ageMedian$timeUnits, "yr BP")
})

test_that("pullTsVariable prefers the paleoData_ field among near matches", {
  TS <- list(list(paleoData_variableName = "d18O", timeVariableName = "age", paleoData_values = 1),
             list(paleoData_variableName = "Mg/Ca", timeVariableName = "age", paleoData_values = 2))
  expect_warning(v <- pullTsVariable(TS, "variableName"), "paleoData_variableName")
  expect_equal(v, c("d18O", "Mg/Ca"))
})
