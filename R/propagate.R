#' Propagate uncertainty across an ensemble through any analysis function
#'
#' @description Generic engine for ensemble uncertainty propagation. Given a
#' time vector (or ensemble) and a values vector (or ensemble), and a
#' changeFun that operates on a single (time, vals) pair, propagateUncertainty
#' maps changeFun across n.ens ensemble members, simulating time and/or
#' value uncertainty when an ensemble is not supplied. Any analysis expressed as
#' changeFun(time, vals, ...) -> one-row data.frame/tibble automatically gains
#' ensemble uncertainty propagation.
#'
#' @param time a time vector, or matrix of time ensemble members (ensembles in columns)
#' @param vals a values vector, or matrix of values ensemble members (ensembles in columns)
#' @param changeFun the analysis function to propagate across the ensemble. Must accept time and vals as its first two arguments and return a one-row data.frame/tibble.
#' @param simulate.time.uncertainty TRUE or FALSE. If an ensemble is not included, do you want to simulate time ensembles (default = TRUE)
#' @param simulate.paleo.uncertainty TRUE or FALSE. If an ensemble is not included, do you want to simulate paleo ensembles (default = TRUE)
#' @param n.ens How many ensembles to use for error propagation? (default = 100)
#' @param bam.model BAM Model parameters to use if simulating time uncertainty (default = list(ns = n.ens, name = "bernoulli", param = 0.05))
#' @param paleo.uncertainty Uncertainty to use if modelling uncertainty for paleo values. (default = sd(vals,na.rm = TRUE)/2)
#' @param paleo.ar1 Autocorrelation coefficient to use for modelling uncertainty on paleoData, what fraction of the uncertainties are autocorrelated? (default = sqrt(0.5); or 50 percent autocorrelated uncertainty)
#' @param paleo.arima.order Order to use for ARIMA model used in modelling uncertainty on paleoData (default = c(1,0,0))
#' @param summarize How to summarize the per-ensemble results. FALSE (default) returns every ensemble member. Alternatively, supply a function function(propagated) -> data.frame that collapses the propagated tibble into a summary; this replaces the previous hard-coded, analysis-specific summary so the engine stays generic across analyses.
#' @param seed set a seed for reproducibility
#' @param progress show a progress bar?
#' @param ... arguments to pass to changeFun. Vectors longer than 1 are treated as parameter ensembles and sampled across the n.ens members.
#'
#' @return a tibble of propagated results (one row per ensemble member, unless summarized)
#' @family ensemble
#' @export
propagateUncertainty <- function(time,
                                 vals,
                                 changeFun,
                                 simulate.time.uncertainty = TRUE,
                                 simulate.paleo.uncertainty = TRUE,
                                 n.ens = 100,
                                 bam.model = list(ns = n.ens, name = "bernoulli", param = 0.05),
                                 paleo.uncertainty = sd(vals,na.rm = TRUE)/2,
                                 paleo.ar1 = sqrt(0.5),
                                 paleo.arima.order = c(1,0,0),
                                 summarize = FALSE,
                                 seed = round(sum(time,na.rm = TRUE)),
                                 progress = TRUE,
                                 ...){

  #check inputs
  if(max(c(NCOL(time),NCOL(vals))) > 1 & n.ens <= 1){#at least one is an ensemble
    stop("To simulate uncertainty with an ensemble, increase n.ens to more than 1 (probably more than 50 at the minimum)")
  }

  nca <- NCOL(time)

  #set a seed.
  if(any(is.na(seed))){
    seed <- sample.int(1000,1)
  }
  try(set.seed(seed),silent = TRUE)

  #Prepare time ensemble
  if(nca == 1){#then it's not an ensemble
    #create ensemble?
    if(simulate.time.uncertainty){
      timeMat <- simulateBam(X = matrix(1,nrow = length(time)),
                             t = as.matrix(time),
                             model = bam.model,
                             ageEnsOut = TRUE)$ageEns
    }else{#replicate times up to n.ens
      timeMat <- matrix(rep(time,n.ens),ncol = n.ens,nrow = NROW(time))
    }
  }else{
    if(nca >= n.ens){
      timeMat <- time[,sample(seq_len(nca),size = n.ens,replace = FALSE)]
    }else if(nca < n.ens){
      timeMat <- time[,sample(seq_len(nca),size = n.ens,replace = TRUE)]
    }
  }

  #make into a list for purrr
  timeList <- purrr::array_branch(timeMat,margin = 2)

  #Now prep paleodata
  ncp <- NCOL(vals)

  if(ncp == 1){#then it's not an ensemble
    #create ensemble?
    if(simulate.paleo.uncertainty){
      paleoList <- purrr::map(seq_len(n.ens),
                              function(i) vals + simulateAutoCorrelatedUncertainty(sd = paleo.uncertainty,
                                                                                   n = NROW(vals),
                                                                                   ar = paleo.ar1,
                                                                                   arima.order = paleo.arima.order))
    }else{#replicate times up to n.ens
      paleoList <- matrix(rep(vals,n.ens),ncol = n.ens,nrow = NROW(vals)) %>%
        purrr::array_branch(margin = 2)
    }
  }else{
    if(ncp >= n.ens){
      paleoMat <- vals[,sample(seq_len(ncp),size = n.ens,replace = FALSE)]
    }else if(ncp < n.ens){
      paleoMat <- vals[,sample(seq_len(ncp),size = n.ens,replace = TRUE)]
    }
    paleoList <- purrr::array_branch(paleoMat,margin = 2)
  }

  # check to see if ... includes vectors of parameters
  dots <- list(...)

  dl <- purrr::map_dbl(dots,length)
  if(any(dl > 1)){#then we need to sample over
    #turn params into vectors that are n.ens long...
    dotsLong <- vector(mode = "list",length = length(dots))
    dotsSummaryString <- c()
    for(d in 1:length(dots)){
      if(length(dots[[d]]) == 1){
        dotsLong[[d]] <- rep(dots[[d]],n.ens)
        dotsSummaryString[d] <- glue::glue("{dots[[d]]}")
      }else{
        #sample the parameter ensemble across members; use replacement whenever
        #there are fewer supplied values than ensemble members
        dotsLong[[d]] <- sample(dots[[d]],size = n.ens,replace = length(dots[[d]]) < n.ens)
        dotsSummaryString[d] <- glue::glue("{round(mean(dotsLong[[d]]))} +/- {round(sd(dotsLong[[d]]))}")
      }
    }
    names(dotsLong) <- names(dots)
    tomaplist <- append(list(time = timeList,vals = paleoList),dotsLong)

    propagated <- purrr::pmap_dfr(tomaplist,changeFun)
  }else{
    propagated <- purrr::map2_dfr(timeList,paleoList,changeFun,...)
  }

  propagated$nEns <- n.ens

  #optionally collapse the per-ensemble results with a caller-supplied summary
  #function. The previous TRUE branch hard-coded analysis-specific column names
  #(time_start, eventDetected, ...) and so was not generic; callers now pass a
  #function describing how to summarize their own changeFun output.
  if(is.function(summarize)){
    propagated <- summarize(propagated)
  }else if(isTRUE(summarize)){
    stop("summarize = TRUE is no longer supported. Pass a function, e.g. summarize = function(propagated) ..., that returns a summary of your changeFun's output.")
  }

  return(propagated)
}
