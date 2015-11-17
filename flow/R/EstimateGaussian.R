EstimateGaussian <- function (X) {
#' \code{EstimateGaussian} estimates the parameters of a Gaussian distribution using the data in X
#' 
#' @param The input X is the dataset with each n-dimensional data point in one row
#' @return The output is an n-dimensional vector mu, the mean of the data set and the variances sigma^2, an n x 1 vector
  
  mu      = colMeans(X, na.rm = T)
  mum     = t(matrix(data = rep(mu, nrow(X)), nrow = length(mu), ncol = nrow(X)))
  sigma2  = colMeans((X - mum)^2, na.rm = T)
  
  return(list("mu" = mu, sigma2 = sigma2))
}
