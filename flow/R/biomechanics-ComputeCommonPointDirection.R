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

#' Computes the common direction of the relative phase of two oscillators
#' 
#' \code{ComputeCommonPointDirection} returns most common direction.
#' @param t a numerical vector of the times in second of the relative phases.
#' @param psi a numerical vector of the realive phase.
#' @param t.k the point in time for calculting the index.
#' @param t.w the time window around t.k in seconds (default = 10).
#' @return the most commen direction 1 = up, 2 = balance, 3 = down.

ComputeCommonPointDirection <- function(t, psi, t.k, t.w = 10) {
  
  t.j <- t.k - t.w / 2 <= t & t < t.k + t.w / 2
  
  x <- psi[t.j]
  x <- x[!is.na(x)]
  # print(x)
  
  n <- length(x)
  if(n > 1) {
    
    diffs <- diff(x)
    up <-  sum(diffs > 0)
    down <- sum(diffs < 0)
    balanced <- sum(diffs == 0)
    
    return(which.max(c(down, balanced, up)))
  
  } else {
    return(2)
  }
}