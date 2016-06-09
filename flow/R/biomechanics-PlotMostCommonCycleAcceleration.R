#' Plots cycles of acceleration with the most common length.
#' 
#' \code{PlotMostCommonCycleAcceleration} plots cycles of acceleration with the most common length.
#'
#' @param a. A numeric vector of acceleration
#' @param separator.indexes. A numerical vector of separator indexes
#' @return A numeric vector of the mean cycle with the most common length

PlotMostCommonCycleAcceleration <- function(a, separator.indexes, ...) {
  
  par(mfcol = c(3, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
  
  common.cycle.matrix <- ComputeMostCommonCycleMatrix(a, separator.indexes)
  
  x.p <- 1:length(common.cycle.matrix[, 1]) / length(common.cycle.matrix[, 1]) * 100
  plot(x.p, common.cycle.matrix[, 1], type = "l", xlab = expression("Time (% Stride)"), ylab = expression("Acceleration ("~m/s^2~")"), ...)
  
  for (j in 2:length(common.cycle.matrix[1, ])) {
    lines(x.p, common.cycle.matrix[,j], type = "l")
  }
  
  mean.cycle <- rowMeans(common.cycle.matrix)
  plot(x.p, mean.cycle, type = "l", xlab = expression("Time (% Stride)"), ylab = expression("Mean Acceleration ("~m/s^2~")"), ...)
  
  mean.cycle.jerk <- c(NA, ComputeJerk(x.p, mean.cycle))
  plot(x.p, mean.cycle.jerk, type = "l", xlab = expression("Time (% Stride)"), ylab = expression("Jerk ("~m/s^3~")"), ...)
}