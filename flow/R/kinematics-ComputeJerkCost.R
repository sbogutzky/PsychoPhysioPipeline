# The MIT License (MIT)
# Copyright (c) 2016 University of Applied Sciences Bremen
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software
# and associated documentation files (the "Software"), to deal in the Software without restriction,
# including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
# DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
# ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#' Computes the jerk cost of an acceleration in x-, y- and z-direction.
#'
#' \code{ComputeJerkCost} returns a vector with the jerk cost of the acceleration.
#'
#' @param t a numeric of intervals (s)
#' @param x a numeric of accelerations. t and x must have the same length, greater than one, with no missing values
#' @param y a numeric of accelerations. t and y must have the same length, greater than one, with no missing values
#' @param z a numeric of accelerations. t and z must have the same length, greater than one, with no missing values
#' @param normalized a boolean. Normalized by cycle length
#' @return the numeric vector of jerk cost [m^2/s^4] of the acceleration or a vector with normalized jerk cost [m^2/s^5] of the acceleration

ComputeJerkCost <- function(t.s, data, normalized = F) {
  
  x <- t.s
  n.col <- ncol(data)
  n.row <- length(x) - 1
  
  cycle.interval <- x[length(x)] - x[1]
  
  yi.all <- c()
  for(i in 1:n.col) {
    y <- data[, i]
    yi <- ComputeJerk(x, y)
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