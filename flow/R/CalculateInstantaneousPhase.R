#' Calculates instantaneous phase of time series in another time series
#'
#' \code{CalculateInstantaneousPhase} returns a vector with fi
#'
#' @param t.1. First time series in a vector.
#' @param t.2. Second time series in a vector.
#' @return A vector with fi.

CalculateInstantaneousPhase <- function(t.1, t.2) {
  fi <- c()
  for (n in 1:(length(t.1) - 1)) {
    tn    <- t.1[n]
    tn.1  <- t.1[n + 1]
    for (t in t.2) {
      if (tn <= t && t < tn.1) {
        fi <- c(fi, 2 * pi * (t - tn) / (tn.1 - tn) + 2 * pi * n)
      }
    }
  }
  return(fi)
}