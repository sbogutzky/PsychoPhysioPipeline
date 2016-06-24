# The MIT License (MIT)
# Copyright (c) 2016 Simon Bogutzky
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

#' Searches for extrema in a signal.
#' 
#' \code{SearchExtrema} returns extrema of a signal.
#'
#' @param y a numeric vector of signal
#' @param which one of "both", "maxima", "minima". Default is "both"
#' @return the numeric vector of extrema indexes

SearchExtrema <- function(y, which = c("both", "maxima", "minima")) {
  
  maxima <- c()
  minima <- c()
  
  if(missing(which) | which == "both" | which == "maxima") {
    
    # Find locations of local maxima
    # p = 1 at maxima, p otherwise, end point maxima excluded
    n <- length(y) - 2
    p <- sign(sign(y[2:(n + 1)] - y[3:(n + 2)]) - sign(y[1:n] - y[2:(n + 1)]) -.1) + 1
    p <- c(0, p, 0)
    
    # Indices of maxima
    p <- as.logical(p) 
    i <- 1:length(p)
    maxima <- i[p]
  }
  
  if(missing(which) | which == "both" | which == "minima") {
    
    yn <- -y
    
    # Find locations of local minima
    # p = 1 at minima, p otherwise, end point minima excluded
    n <- length(yn) - 2
    p <- sign(sign(yn[2:(n + 1)] - yn[3:(n + 2)]) - sign(yn[1:n] - yn[2:(n + 1)]) -.1) + 1
    p <- c(0, p, 0)
    
    # Indices of minima
    p <- as.logical(p) 
    i <- 1:length(p)
    minima <- i[p]
  }
  return(c(maxima, minima))
}