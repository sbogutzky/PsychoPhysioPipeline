#' Calculates the instantaneous phase.
#' \code{CalculateInstantaneousPhases} returns the instantaneous phase.
#'
#' @param ts1. A numeric vector of time events.
#' @param ts2. A numeric vector of time events (onset of the cycle).
#' @return fi A numeric vector of instantaneous phase.

CalculateInstantaneousPhases <- function(ts1, ts2) {
  fis <- c()
  ts <- c()
  
  for (i in 1:(length(ts2) - 1)) {
    cycle <- c(ts2[i], ts2[i + 1])
    fi <- NA
    for (t in ts1) {
      if (cycle[1] <= t && t < cycle[2]) { 
        fi <- 2 * pi * i + 2 * pi * (t - cycle[1]) / diff(cycle)
        fis <- c(fis, fi)
        ts <- c(ts, cycle[1])
      }
    }
    if(is.na(fi)) {
      fis <- c(fis, fi)
      ts <- c(ts, cycle[1])
    }
  }
  return(data.frame(ts = ts, fi = fis))
}