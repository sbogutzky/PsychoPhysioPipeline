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

#' Detects anomaly of two variables.
#' 
#' \code{DetectAnomaly} returns outliers and a new epsilon for a new iteration.
#' 
#' @param x a numeric vector of the first variable.
#' @param y a numeric vector of the second variable.
#' @param epsilon a numeric that specifies a new epsilon (default = 0).
#' @param ... Arguments to be passed to methods, such as graphical parameters (see par). 
#' 
#' @return Epsilon "epsilon" and a numeric vector with indexes of outliers "outliers".

DetectAnomaly <- function(x, y, epsilon = 0, pch = 1, ...) {
  X <- matrix(data = c(x, y), nrow = length(y), ncol = 2)
  par(mfcol = c(1, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
  plot(X[,1], X[,2], pch = 1, ...)
  
  gl <- EstimateGaussian(X)
  p  <- ComputeMultivariateGaussian(X, gl$mu, gl$sigma2)
  
  if(epsilon == 0) {
    y <- zeros(nrow(X), 1)
    y[identify(X, plot = FALSE)]  <- 1
    bt              <- ComputeFScoreTreshold(y, p)
    epsilon         <- bt$epsilon
  }
  
  outliers <- which(p < epsilon)
  points(X[outliers, 1], X[outliers, 2], pch = pch, ...)
  
  return(list("epsilon" = epsilon, "outliers" = outliers))  
}