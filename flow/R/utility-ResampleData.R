#' Resamples time-domain data by spline interpolation.
#' 
#' \code{ResampleData} resamples time-domain data by spline interpolation.
#'
#' @param data. A data frame with time domain data
#' @param fs. A numeric that specify the the new sampling rate
#' @param tm.s. A numerical vector of milliseconds with the same row size of data
#' @return A matrix with the time-domain data

ResampleData <- function(data, fs, t.ms) {
  
  x <- t.ms
  xi <- seq(x[1], x[length(x)], by = 1000/fs)
  n.col <- ncol(data)
  n.row <- length(xi)
  
  yi.all <- c()
  for(i in 1:n.col) {
    y <- data[, i]
    yi <- signal::interp1(x, y, xi, method = "spline")
    yi.all <- c(yi.all, yi)
  }
  
  m <- matrix(c(xi, yi.all), n.row, n.col + 1)
  
  return(m)
}