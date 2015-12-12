DetectAnomaly <- function(x, y, x.lab, y.lab, x.lim, y.lim, epsilon = 0) {
  X <- matrix(data = c(x, y), nrow = length(y), ncol = 2)
  par(mfcol = c(1, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
  plot(X[,1], X[,2], xlab = x.lab, ylab = y.lab, pch = 21, xlim = x.lim, ylim = y.lim)
  
  gl <- EstimateGaussian(X)
  p  <- MultivariateGaussian(X, gl$mu, gl$sigma2)
  
  if(epsilon == 0) {
    y <- zeros(nrow(X), 1)
    y[identify(X)]  <- 1
    bt              <- SelectThreshold(y, p)
    epsilon         <- bt$epsilon
  }
  
  outliers <- which(p < epsilon)
  points(X[outliers, 1], X[outliers, 2], xlab = x.lab, ylab = y.lab, pch = 21, bg = 2)
  
  return(list("epsilon" = epsilon, "outliers" = outliers))  
}