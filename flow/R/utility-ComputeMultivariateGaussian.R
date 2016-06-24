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

#' Computes the probability density function of the multivariate gaussian distribution.
#' 
#' \code{ComputeMultivariateGaussian} computes the probability density function of the multivariate gaussian distribution.
#' @param X a dataset with each n-dimensional data point in one row
#' @param mu the mean mu of the data set
#' @param Sigma2 the variances sigma^2, an n x 1 vector
#' @return an n x 1 vector of predictions

ComputeMultivariateGaussian <- function(X, mu, Sigma2) {
  
require(pracma)
  
k = length(mu)

if (nrow(matrix(Sigma2)) == 1 | ncol(matrix(Sigma2)) == 1)
  Sigma2 = diag(Sigma2)

mum = t(matrix(data = rep(mu, nrow(X)), nrow = length(mu), ncol = nrow(X)))
X = bsxfun("-", X, mum)

p = (2 * pi) ^ (- k / 2) * det(Sigma2) ^ (-0.5) * exp(-0.5 * rowSums(bsxfun("*", X %*% pinv(Sigma2), X)))
return(p)
}