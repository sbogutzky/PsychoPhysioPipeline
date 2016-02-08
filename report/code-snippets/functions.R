PlotFeatureOverview <- function(feature, ...) {
  par(mfrow = c(2, 2), xaxs = "r", yaxs = "r")
  qqnorm(feature)
  qqline(feature)
  plot(feature, ...)
  boxplot(feature, ...)
  hist(feature, ...)
}

PlotLinearRegression <- function(x, y, ...) {
  linear.model <- lm(y ~ x)
  print(summary(linear.model))
  
  plot(x, y, pch = 21, cex = 1.2, ...)
  abline(linear.model)
  box()
}