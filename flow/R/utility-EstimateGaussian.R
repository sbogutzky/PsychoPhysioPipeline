#' Estimates the parameters of a Gaussian distribution using the data in X.
#' 
#' \code{EstimateGaussian} estimates the parameters of a Gaussian distribution using the data in X.
#' 
#' @param X. A dataset with each n-dimensional data point in one row
#' @return The mean "mu" of the data set and the variances "sigma2", an n x 1 vector

EstimateGaussian <- function (X) {
  
  mu      = colMeans(X, na.rm = T)
  mum     = t(matrix(data = rep(mu, nrow(X)), nrow = length(mu), ncol = nrow(X)))
  sigma2  = colMeans((X - mum)^2, na.rm = T)
  
  return(list("mu" = mu, sigma2 = sigma2))
}
