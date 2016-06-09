#' Detects anomaly of two variables.
#' 
#' \code{DetectAnomaly} returns outliers and a new epsilon for a new iteration.
#' 
#' @param x. A numeric vector of the first variable
#' @param y. A numeric vector of the second variable
#' @param x.lab. A character vector that specifies the x label of the plot
#' @param y.lab. A character vector that specifies the y label of the plot
#' @param x.lim. A two dimensional numeric vector that specifies x lim
#' @param y.lim. A two dimensional numeric vector that specifies y lim
#' @param epsilon. A numeric that specifies a new epsilon. Default is 0
#' @return Epsilon "epsilon" and a numeric vector with indexes of outliers "outliers"

DetectAnomaly <- function(x, y, x.lab, y.lab, x.lim, y.lim, epsilon = 0) {
  X <- matrix(data = c(x, y), nrow = length(y), ncol = 2)
  par(mfcol = c(1, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
  plot(X[,1], X[,2], xlab = x.lab, ylab = y.lab, pch = 21, xlim = x.lim, ylim = y.lim)
  
  gl <- EstimateGaussian(X)
  p  <- ComputeMultivariateGaussian(X, gl$mu, gl$sigma2)
  
  if(epsilon == 0) {
    y <- zeros(nrow(X), 1)
    y[identify(X, plot = FALSE)]  <- 1
    bt              <- ComputeFScoreTreshold(y, p)
    epsilon         <- bt$epsilon
  }
  
  outliers <- which(p < epsilon)
  points(X[outliers, 1], X[outliers, 2], xlab = x.lab, ylab = y.lab, pch = 21, bg = "tomato")
  
  return(list("epsilon" = epsilon, "outliers" = outliers))  
}