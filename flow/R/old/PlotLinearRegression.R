PlotLinearRegression <- function(x, y, ...) {
  linear.model <- lm(y ~ x)
  print(summary(linear.model))
  
  plot(x, y, pch = 21, cex = 1.2, ...)
  abline(linear.model)
  box()
}