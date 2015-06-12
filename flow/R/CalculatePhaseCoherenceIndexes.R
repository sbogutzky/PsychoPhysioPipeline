#' Calculates the phase coherence indexes from ?
#'
#' \code{CalculatePhaseCoherenceIndex} returns a vector of indexes
#'
#' @param psi. ?.
#' @param n. ?.
#' @return A vector of indexes.

CalculatePhaseCoherenceIndexes <- function(psi, n = 5) {
  mod <- n %% 2
  pad <- floor(n/2)
  add <- 0
  if(mod == 1) {
    add <- 1
  }
  
  index   <- rep(NA, pad)
  length  <- 1:(length(psi)-n)
  for (i in length) {
    sum.re  <- sum(cos(psi[i:(i+(n-1))]))/n
    sum.i   <- sum(sin(psi[i:(i+(n-1))]))/n
    indexes <- c(indexes, sqrt(sum.re^2 + sum.i^2))
  }
  indexes     <- c(indexes, rep(NA, pad + add))
  return(indexes)
}