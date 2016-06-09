#' Computes the probability density function of the multivariate gaussian distribution.
#' 
#' \code{ComputeMultivariateGaussian} computes the probability density function of the multivariate gaussian distribution.
#' @param X. A dataset with each n-dimensional data point in one row
#' @param mu. The mean mu of the data set
#' @param Sigma2. The variances sigma^2, an n x 1 vector
#' @return An n x 1 vector of predictions

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