# Time scales of LiPD variable lists (selectData() output). lipdR >= 0.8.0
# gives every time column timeUnits, timeDatum, timeDirection, timeExponent
# and timeSource when a file is read.

#' The time scale of a variable list, if it states one
#' @keywords internal
#' @param x a variable list (output of selectData()), or anything else
#' @return a lipdR::timeScale() list, or NULL if x doesn't carry a time scale
variableTimeScale <- function(x){
  if(!is.list(x) || is.data.frame(x)){
    return(NULL)
  }
  if(is.null(x$timeDatum) || is.null(x$timeDirection) || is.null(x$timeExponent)){
    return(NULL)
  }
  out <- tryCatch(lipdR::timeScale(x[intersect(c("timeUnits", "timeDatum", "timeDirection", "timeExponent"), names(x))]),
                  error = function(e) NULL)
  if(!is.null(out) && grepl("14c", tolower(paste(x$units, collapse = " ")))){
    out$radiocarbon <- TRUE
  }
  out
}

sameTimeScale <- function(a, b){
  a$timeDatum == b$timeDatum && a$timeDirection == b$timeDirection &&
    a$timeExponent == b$timeExponent && isTRUE(a$radiocarbon) == isTRUE(b$radiocarbon)
}

#' Put a time variable on another variable's time scale
#'
#' @description If both `time.1` and `time.2` are variable lists that carry
#'   time scales (as variables read with lipdR >= 0.8.0 do), and the scales
#'   differ, `time.2` is converted to the scale of `time.1`, with a message.
#'   Otherwise they are returned unchanged.
#' @keywords internal
#' @param time.1,time.2 variable lists (selectData() output), or vectors/matrices
#' @param names names of the two arguments, for messages
#' @return list(time.1, time.2)
matchTimeScales <- function(time.1, time.2, names = c("time.1", "time.2")){
  s1 <- variableTimeScale(time.1)
  s2 <- variableTimeScale(time.2)
  if(is.null(s1) || is.null(s2) || sameTimeScale(s1, s2)){
    return(list(time.1, time.2))
  }
  if(isTRUE(s1$radiocarbon) || isTRUE(s2$radiocarbon)){
    stop(names[1], " is on ", s1$timeUnits, " and ", names[2], " on ", s2$timeUnits,
         ". Radiocarbon years can't be converted to calendar years; calibrate them first.", call. = FALSE)
  }
  message("Converting ", names[2], " from ", s2$timeUnits, " to ", s1$timeUnits, " to match ", names[1], ".")
  time.2$values <- lipdR::convertTime(time.2$values, from = s2, to = s1)
  time.2$units <- s1$timeUnits
  time.2$timeUnits <- s1$timeUnits
  time.2$timeDatum <- s1$timeDatum
  time.2$timeDirection <- s1$timeDirection
  time.2$timeExponent <- s1$timeExponent
  list(time.1, time.2)
}

`%||%` <- function(x, y){
  if(is.null(x)) y else x
}

#' Copy a variable's time scale to another column
#' @keywords internal
#' @param col the column to give the scale to
#' @param from a variable list carrying a time scale
#' @return col, with from's timeUnits, timeDatum, timeDirection, timeExponent and timeSource (if from has them)
copyTimeScale <- function(col, from){
  if(is.null(variableTimeScale(from))){
    return(col)
  }
  for(f in c("timeUnits", "timeDatum", "timeDirection", "timeExponent", "timeSource")){
    col[[f]] <- from[[f]]
  }
  col
}
