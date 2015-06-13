#' Calculates the times of appearance of t of the first time series at the phase of the second time series.
#' \code{CalculatePhaseAppearances} returns times of appearance of t.
#'
#' @param of. A numeric vector of the first time series.
#' @param at. A numeric vector of the second time series.
#' @return fi. A numeric vector of times of appearance of t.

CalculatePhaseAppearances <- function(of, at) {
  fi <- c()
  
  for (k in 1:(length(at) - 1)) {
    tk    <- at[k]
    tk.1  <- at[k + 1]
    for (t in of) {
      if (tk < t && t < tk.1) { 
        fi <- c(fi, 2 * pi * k + 2 * pi * (t - tk) / (tk.1 - tk))
      }
    }
  }
  return(fi)
}