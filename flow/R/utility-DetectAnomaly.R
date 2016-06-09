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