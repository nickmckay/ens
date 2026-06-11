#' Test a null hypothesis by propagating uncertainty across surrogate data
#'
#' @description Generic null-hypothesis testing engine. Generates mc.ens
#' surrogate datasets that share statistical properties with the input but
#' destroy the signal of interest, then runs propagateUncertainty() with the
#' supplied changeFun on each. Any analysis expressed as
#' changeFun(time, vals, ...) -> one-row tibble automatically gains
#' surrogate-based significance testing.
#'
#' @inheritParams propagateUncertainty
#' @param mc.ens How many Monte Carlo simulations to use for null hypothesis testing
#' @param surrogate.method What method to use to generate surrogate data. Options:
#' \itemize{
#' \item 'isospectral': (Default) Following Ebisuzaki (1997), generate surrogates by scrambling the phases of the data while preserving their power spectrum. Uses surrogateDataFun() (rEDM::SurrogateData).
#' \item 'isopersistent': Generates surrogates by simulating from an AR(1) process fit to the data. Uses createSyntheticTimeseries().
#' \item 'shuffle': Randomly shuffles the data. Uses surrogateDataFun().
#' }
#' @return a list (length mc.ens) of propagated results, one per surrogate
#' @family ensemble
#' @export
testNullHypothesis <- function(time,
                               vals,
                               changeFun,
                               n.ens = 100,
                               mc.ens = 100,
                               surrogate.method = "isospectral",
                               seed = round(sum(vals,na.rm=FALSE)),
                               progress = TRUE,
                               ...) {

  #set a seed.
  if(all(is.na(seed))){
    seed <- sample.int(1000,1)
  }
  set.seed(seed)

  ncp <- NCOL(vals)

  if(ncp == 1){
    vm <- matrix(rep(vals,times = mc.ens),ncol = mc.ens,byrow = FALSE)
    valList <- purrr::array_branch(vm,margin = 2)
  }else{
    valList <- purrr::array_branch(vals,margin = 2)
    if(length(valList) >= n.ens){
      valList <- valList[sample(seq_along(valList),size = mc.ens,replace = FALSE)]
    }else{
      valList <- valList[sample(seq_along(valList),size = mc.ens,replace = TRUE)]
    }
  }

  # generate some surrogates
  if(grepl(surrogate.method,pattern = "persis",ignore.case = T)){
    if (ncol(time)==0){
      cstv <- function(x,time=time, ...) {
        createSyntheticTimeseries(values = x, ...)
      }
    } else{
      cstv <- function(x,time=time, ...) {
        createSyntheticTimeseries(values = x, time = time[,sample(seq(1,ncol(time)),1)], ...)
      }
    }

    surVals <- purrr::map(valList,
                          cstv,
                          time = time,
                          sameTrend = TRUE,
                          n.ens = ncp)

  }else if(grepl(surrogate.method,pattern = "spectra",ignore.case = T)){

    cstv <- function(x,...){
      g <- is.finite(x)
      out <- surrogateDataFun(ts = x[g],...)
      nc <- ncol(out)
      om <- matrix(NA,nrow = NROW(g),ncol = nc)
      om[g,] <- out
      return(om)
    }

    surVals <- purrr::map(valList,
                          cstv,
                          method = 'ebisuzaki',
                          num_surr = ncp)
  }else if(grepl(surrogate.method,pattern = "shuffle",ignore.case = T)){
    cstv <- function(x,...){
      g <- is.finite(x)
      out <- surrogateDataFun(ts = x[g],...)
      nc <- ncol(out)
      om <- matrix(NA,nrow = NROW(g),ncol = nc)
      om[g,] <- out
      return(om)
    }

    surVals <- purrr::map(valList,
                          cstv,
                          method = 'random_shuffle',
                          num_surr = ncp)
  }

  #repeat the uncertainty propagation for EACH surrogate
  pucv <- function(x,...){propagateUncertainty(vals = x,...)}

  if(progress){
    progress <-  glue::glue("Testing null hypothesis with {mc.ens} simulations, each with {n.ens} ensemble members.")
  }

  out <- purrr::map(surVals,
                    pucv,
                    time = time,
                    changeFun = changeFun,
                    n.ens = n.ens,
                    .progress = progress,
                    ...)

  return(out)
}


#' Estimate a p-value from a null distribution using kernel density estimation
#'
#' @param nulls a vector of null hypothesis results
#' @param real a single value for the real data
#' @param xmin minimum value for the distribution
#' @param xmax maximum value for the distribution
#' @param h smoothing factor
#'
#' @importFrom ks kde
#' @family ensemble
#' @return a list with the p-value and the kde data (x, y)
#' @export
kdePval <- function(nulls,real,h = NA,xmin = NA, xmax = NA){
  datarange <- range(c(nulls,real),na.rm = TRUE)
  span <- abs(diff(datarange))

  if(span == 0){
    if(real == 0){
      xmin <- 0
      xmax <- 1
      span <- 1
    }else{
      xmin <- real - 0.5
      xmax <- real + 0.5
      span <- 1
    }
  }

  if(all(is.na(h))){
    h <- .03 * span
  }

  if(all(is.na(xmin))){
    xmin <- min(datarange,na.rm = TRUE) - span*.05
  }

  if(all(is.na(xmax))){
    xmax <- max(datarange,na.rm = TRUE) + span*.05
  }

  #if xmin is less than 0, make symmetric
  if(xmin < 0){
    xmin <- -max(abs(c(xmin,xmax)))
    xmax <- max(abs(c(xmin,xmax)))
  }

  #estimate kde
  kd <- ks::kde(nulls,h = h * span,xmin = xmin,xmax = xmax,density = TRUE,gridsize = 1000)
  if(real == xmax){
    real <- kd$eval.points[length(kd$eval.points)-1]
  }

  if(real == xmin){
    real <- kd$eval.points[2]
  }

  cdf <- cumsum(kd$estimate)/sum(kd$estimate)

  pval <- 1-approx(kd$eval.points,y = cdf,xout = real)$y

  out <- list(pval = pval,
              x = kd$eval.points,
              y = kd$estimate)

  return(out)
}
