#' Computes the normalized shannon entropy index of the relative phase of two oscillators
#' 
#' \code{ComputeNormalizedShannonEntropyIndex} returns the index.
#' @param bins bins is the number of bins (default = The optimal number of bins will be estimated as N = exp[0.626 + 0.4 ln(M - 1)], where M is the number of samples)
#' @param t a numerical vector of the times in second of the relative phases.
#' @param psi a numerical vector of the realive phase.
#' @param t.k is the point in time for calculting the index.
#' @param t.w is the time window around t.k in seconds (default = 10).
#' @return the index.

ComputeNormalizedShannonEntropyIndex <- function(t, psi, t.k, t.w = 10, bins = NA) {
  
  t.j <- t.k - t.w / 2 <= t & t < t.k + t.w / 2
  
  x <- psi[t.j]
  x <- x[!is.na(x)]
  # print(x)
  
  n <- length(x)
  if(n > 1) {
    if(is.na(bins))
      bins <- round(exp(0.626 + 0.4 * log(n - 1)))
    
    # print(paste("Number of bins:", bins))
    histogram   <- hist(x, breaks = seq(0, 1, 1 / bins), plot = F)
      
    freqs <- histogram$counts
    # print(freqs)
      
    p <- freqs/n
    # print(p)
    p <- p[p != 0]
      
    h = -sum(p * log2(p))
    # print(paste("H: ", h))
      
    h.max <- log2(length(freqs))
    hn <- (h.max - h) / h.max
    # print(paste("Hn: ", hn))
    
    return(hn)
  
  } else {
    return(0)
  }
}