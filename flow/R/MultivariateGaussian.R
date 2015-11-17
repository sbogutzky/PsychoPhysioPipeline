MultivariateGaussian <- function(X, mu, Sigma2) {
  #' \code{MultivariateGaussian} computes the probability density function of the multivariate gaussian distribution.
  #' density function of the examples X under the multivariate gaussian distribution with parameters mu and Sigma2. If Sigma2 is a matrix, it is treated as the covariance matrix. If Sigma2 is a vector, it is treated as the \sigma^2 values of the variances in each dimension (a diagonal covariance matrix)

require(pracma)
k = length(mu)

if (nrow(matrix(Sigma2)) == 1 | ncol(matrix(Sigma2)) == 1)
  Sigma2 = diag(Sigma2)

mum = t(matrix(data = rep(mu, nrow(X)), nrow = length(mu), ncol = nrow(X)))
X = bsxfun("-", X, mum)

p = (2 * pi) ^ (- k / 2) * det(Sigma2) ^ (-0.5) * exp(-0.5 * rowSums(bsxfun("*", X %*% pinv(Sigma2), X)))
return(p)
}