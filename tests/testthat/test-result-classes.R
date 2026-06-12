library(ens)

makeCorData <- function() {
  set.seed(1)
  n <- 60; ne <- 8
  list(t1 = matrix(sort(runif(n, 0, 1000)), n, ne) + rnorm(n * ne, sd = 4),
       v1 = as.matrix(cumsum(rnorm(n))),
       t2 = matrix(sort(runif(n, 0, 1000)), n, ne) + rnorm(n * ne, sd = 4),
       v2 = as.matrix(cumsum(rnorm(n))))
}

test_that("corEns carries an S3 class without disturbing component access", {
  d <- makeCorData()
  suppressMessages(
    co <- corEns(d$t1, d$v1, d$t2, d$v2, bin.step = 50,
                 isospectral = FALSE, max.ens = 100)
  )
  expect_s3_class(co, "corEns")
  # two-layer convention: still a plain list underneath, $-access intact
  expect_true(is.list(co))
  expect_false(is.null(co$cor.df))
  expect_s3_class(co$cor.df, "data.frame")
  expect_output(print(co), "<corEns>")
})

test_that("regressEns carries an S3 class and prints", {
  d <- makeCorData()
  tx <- list(values = d$t1, units = "yr BP", variableName = "age")
  vx <- list(values = d$v1, units = "unitless", variableName = "x")
  ty <- list(values = d$t2, units = "yr BP", variableName = "age")
  vy <- list(values = d$v2, units = "unitless", variableName = "y")
  suppressMessages(
    re <- regressEns(tx, vx, ty, vy, bin.step = 50, max.ens = 30,
                     recon.bin.vec = seq(0, 1000, by = 50))
  )
  expect_s3_class(re, "regressEns")
  expect_false(is.null(re$m))
  expect_output(print(re), "<regressEns>")
})

test_that("print methods return their object invisibly", {
  d <- makeCorData()
  suppressMessages(
    co <- corEns(d$t1, d$v1, d$t2, d$v2, bin.step = 50,
                 isospectral = FALSE, max.ens = 50)
  )
  out <- withVisible(print(co))
  expect_false(out$visible)
  expect_identical(out$value, co)
})
