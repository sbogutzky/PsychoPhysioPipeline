#' Computes the phase coherence index of the relative phase of two oscillators
#'
#' \code{ComputePhaseCoherenceIndex} returns the index.
#' @param t a numerical vector of the times in second of the relative phases.
#' @param psi a numerical vector of the realive phase.
#' @param t.k is the point in time for calculting the index.
#' @param t.w is the time window around t.k in seconds (default = 10).
#' @return the index.

ComputePhaseCoherenceIndex <- function(t, psi, t.k, t.w = 10) {
  t.j <- t.k - t.w / 2 <= t & t < t.k + t.w / 2
  x <- psi[t.j]
  x <- x[!is.na(x)]
  n <- length(x)
  if(n > 0) {
    mean.re <- mean(cos(x * 2 * pi * 2 * pi), na.rm = T)
    mean.i  <- mean(sin(x * 2 * pi * 2 * pi), na.rm = T)
    return(mean.re^2 + mean.i^2)
  } else {
    return(0)
  }
}