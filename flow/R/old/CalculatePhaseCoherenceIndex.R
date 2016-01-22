#' Calculates the phase coherence index of the relative phase of two oscillators
#'
#' \code{CalculatePhaseCoherenceIndex} returns the index
#'
#' @param t t is a vector with the times of the relative phases.
#' @param psi psi is a vector with the realive phase in r (0 to 2pi).
#' @param t.k t.k is the point in time for calculting the index.
#' @param t.w t.w is the time window around t.k (default = 10).
#' @return the index.

CalculatePhaseCoherenceIndex <- function(t, psi, t.k, t.w = 10) {
  t.j     <- t.k - t.w / 2 <= t & t < t.k + t.w / 2
  mean.re <- mean(cos(psi[t.j]), na.rm = T)
  mean.i  <- mean(sin(psi[t.j]), na.rm = T)
  return(sqrt(mean.re^2 + mean.i^2))
}