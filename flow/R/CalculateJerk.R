#' Calculates the jerk of an acceleration.
#'
#' \code{CalculateJerk} returns a vector with the jerk of the acceleration.
#'
#' @param t Vector with intervals
#' @param x The other vector with acceleration. t and x must have the same length, greater than one, with no missing values.
#' @return A vector with the jerk of the acceleration.

CalculateJerk <- function(t, x) {
  
#   jerk <- c()
#   for(i in 1:(length(t)-1)) {
#       j     <- (x[i] - x[i + 1]) / (t[i] - t[i + 1])
#       jerk  <- c(jerk, j)
#   }
  
  # Alternative mit Vektorlänge
  diff.t <- diff(t)
  diff.x <- diff(x)
  jerk   <- diff.x / diff.t
  return(jerk)
}