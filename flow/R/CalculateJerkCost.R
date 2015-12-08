#' Calculates the jerk cost of an acceleration in x-, y- and z-direction.
#'
#' \code{CalculateJerkCost} returns a vector with the jerk cost of the acceleration.
#'
#' @param t Vectors with intervals (s)
#' @param x The other vector with acceleration. t and x must have the same length, greater than one, with no missing values.
#' @param y The other vector with acceleration. t and y must have the same length, greater than one, with no missing values.
#' @param z The other vector with acceleration. t and z must have the same length, greater than one, with no missing values.
#' @param normalized Boolean. Normalized by cycle length.
#' @param plot Boolean. Plot cycle acceleration, cycle jerk and the time differential of jerk-cost
#' @return A vector with the jerk cost [m^2/s^4] of the acceleration or a vector with normalized jerk cost [m^2/s^5] of the acceleration.

CalculateJerkCost <- function(t.s, data, normalized = F) {
  
  x <- t.s
  n.col <- ncol(data)
  n.row <- length(x) - 1
  
  cycle.interval <- x[length(x)] - x[1]
  
  yi.all <- c()
  for(i in 1:n.col) {
    y <- data[, i]
    yi <- CalculateJerk(x, y)
    yi <- yi^2
    yi.all <- c(yi.all, yi)
  }
  
  m <- matrix(yi.all, n.row, n.col)
  
  # Intergal of the time differential
  jerk.cost <- pracma::trapz(x[-1], rowSums(m))
  
  if (normalized) {
    jerk.cost <- jerk.cost / cycle.interval
  }
  
  return(jerk.cost)
}