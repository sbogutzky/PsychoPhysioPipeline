#' Computes the normalized shannon entropy index of the relative phase of two oscillators
#' 
#' \code{ComputeNormalizedShannonEntropyIndex} returns the index.
#' @param t t is a vector with the times of the relative phases.
#' @param psi psi is a vector with the realive phase.
#' @param t.k t.k is the point in time for calculting the index.
#' @param t.w t.w is the time window around t.k (default = 10).
#' @param bins bins is the number of bins (default = The optimal number of bins will be estimated as N = exp[0.626 + 0.4 ln(M - 1)], where M is the number of samples)
#' @return the index.

ComputeNormalizedShannonEntropyIndex <- function(t, psi, t.k, t.w = 10, bins = NA) {
  
  t.j <- t.k - t.w / 2 <= t & t < t.k + t.w / 2
  m <- length(psi[t.j])
  if(m > 1) {
    if(is.na(bins))
      bins <- round(exp(0.626 + 0.4 * log(m - 1)))
    #print(paste("Number of bins:", bins))
    h   <- hist(psi[t.j], breaks = seq(0, 1, 1 / bins), plot = F)
    p   <- h$counts / m
    h   <- -sum(p * log(p), na.rm = T)
    h.max <- log(bins)
    return((h.max - h) / h.max)
  } else {
    return(0)
  }
}