library(ens)

# A minimal changeFun honoring the engine contract:
# changeFun(time, vals, ...) -> one-row tibble
cf <- function(time, vals, thresh = 0) {
  tibble::tibble(m = mean(vals, na.rm = TRUE),
                 s = sd(vals, na.rm = TRUE),
                 frac = mean(vals > thresh, na.rm = TRUE))
}

makeSeries <- function() {
  set.seed(1)
  list(time = seq(0, 500, by = 10), vals = rnorm(51))
}

test_that("propagateUncertainty maps changeFun across the ensemble", {
  d <- makeSeries()
  p <- propagateUncertainty(d$time, d$vals, changeFun = cf, n.ens = 20, thresh = 0)
  expect_s3_class(p, "tbl_df")
  expect_equal(nrow(p), 20)
  expect_true(all(c("m", "s", "frac", "nEns") %in% names(p)))
  expect_true(all(p$nEns == 20))
})

test_that("propagateUncertainty is reproducible given a seed", {
  d <- makeSeries()
  a <- propagateUncertainty(d$time, d$vals, changeFun = cf, n.ens = 15, thresh = 0, seed = 99)
  b <- propagateUncertainty(d$time, d$vals, changeFun = cf, n.ens = 15, thresh = 0, seed = 99)
  expect_equal(a, b)
})

test_that("summarize accepts a caller-supplied function (generic engine)", {
  d <- makeSeries()
  psum <- propagateUncertainty(d$time, d$vals, changeFun = cf, n.ens = 20, thresh = 0,
                               summarize = function(pp) tibble::tibble(meanM = mean(pp$m),
                                                                       n = nrow(pp)))
  expect_equal(nrow(psum), 1)
  expect_true(is.finite(psum$meanM))
  expect_equal(psum$n, 20)
})

test_that("legacy summarize = TRUE is guarded with a helpful error", {
  d <- makeSeries()
  expect_error(
    propagateUncertainty(d$time, d$vals, changeFun = cf, n.ens = 5, summarize = TRUE),
    "no longer supported"
  )
})

test_that("propagateUncertainty samples parameter ensembles passed via ...", {
  d <- makeSeries()
  # thresh as a vector becomes a parameter ensemble sampled across members
  p <- propagateUncertainty(d$time, d$vals, changeFun = cf, n.ens = 30,
                            thresh = c(-1, 0, 1))
  expect_equal(nrow(p), 30)
  expect_true(length(unique(p$frac)) > 1) # varying thresh changes frac
})

test_that("testNullHypothesis returns one propagation per surrogate", {
  d <- makeSeries()
  nh <- testNullHypothesis(d$time, d$vals, changeFun = cf, n.ens = 5, mc.ens = 4,
                           surrogate.method = "isospectral", thresh = 0)
  expect_length(nh, 4)
  expect_true(all(purrr::map_lgl(nh, ~ "m" %in% names(.x))))
})

test_that("kdePval returns a valid p-value and density", {
  set.seed(5)
  out <- kdePval(rnorm(200), real = 1)
  expect_true(is.finite(out$pval))
  expect_true(out$pval >= 0 && out$pval <= 1)
  expect_equal(length(out$x), length(out$y))
})
