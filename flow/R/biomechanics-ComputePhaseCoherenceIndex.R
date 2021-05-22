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

#' Computes the phase coherence index of the relative phase of two oscillators
#'
#' \code{ComputePhaseCoherenceIndex} returns the index.
#' @param t a numerical vector of the times in second of the relative phases.
#' @param psi a numerical vector of the realive phase.
#' @param t.k the point in time for calculting the index.
#' @param t.w the time window around t.k in seconds (default = 10).
#' @return the index.

ComputePhaseCoherenceIndex <- function(t, psi, t.k, t.w = 10) {
  t.j <- t.k - t.w / 2 <= t & t < t.k + t.w / 2
  x <- psi[t.j]
  x <- x[!is.na(x)]
  n <- length(x)
  if(n > 0) {
    mean.re <- mean(cos(x * 2 * pi * 2 * pi), na.rm = T)
    mean.i  <- mean(sin(x * 2 * pi * 2 * pi), na.rm = T)
    return(mean.re^2 + mean.i^2)
  } else {
    return(0)
  }
}