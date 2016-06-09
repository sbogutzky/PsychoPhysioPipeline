#' Computes the sampling rate of a time series in milliseconds.
#' 
#' \code{ComputeSamplingRate} returns the sampling in Hz.
#'
#' @param t.ms. A numerical vector of milliseconds
#' @return A numerical with the sampling rate in Hz

ComputeSamplingRate <- function(t.ms) {
  n <- length(t.ms)
  fs <- round(n / ((t.ms[n] - t.ms[1]) / 1000), 1)
  return(fs)
}