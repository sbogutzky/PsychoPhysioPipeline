#' Calculates the normalized shannon entropy indexes of ?
#'
#' \code{CalculateNormalizedShannonEntropyIndexes} returns a vector of indexes
#'
#' @param psi.m. ?.
#' @param bins. ?.
#' @return A vector of indexes.

CalculateNormalizedShannonEntropyIndexes <- function(psi.m, bins = 5) {
  mod <- bins %% 2
  pad <- floor(bins/2)
  add <- 0
  if(mod == 1) {
    add <- 1
  }
  
  indexes                 <- rep(NA, pad)
  length                <- 1:(length(psi.m)-bins)
  for (i in length) {
    h                   <- hist(psi.m[i:(i+(bins-1))], breaks = seq(0, 2 * pi, 2 * pi / bins), plot = F)
    counts              <- h$counts[h$counts != 0]
    entropy             <- sum(counts * log(counts / bins)) / -bins
    normalized.entropy  <- (log(bins) - entropy) / log(bins)
    indexes             <- c(indexes, normalized.entropy)
  }
  indexes               <- c(indexes, rep(NA, pad + add))
  return(indexes)
}