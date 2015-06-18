#' Calculates the instantaneous phases of the time series.
#' \code{CalculateInstantaneousPhases} returns the instantaneous phases of the time series.
#'
#' @param times. A numeric vector of the times.
#' @param time.series. A numeric vector of times of appearance of kth event.
#' @return fi. A numeric vector of instantaneous phases.

CalculateInstantaneousPhases <- function(times, time.series) {
  fi <- c()
  
  for (t in times) {
    for (k in 1:(length(time.series) - 1)) {
      tk    <- time.series[k]
      tk.1  <- time.series[k + 1]
      if (tk <= t && t < tk.1) { 
        fi <- c(fi, 2 * pi * k + 2 * pi * (t - tk) / (tk.1 - tk))
      }
    }
    if (t > max(time.series) || t < min(time.series)) {
      fi <- c(fi, NA)
    }
  }
  return(fi)
}