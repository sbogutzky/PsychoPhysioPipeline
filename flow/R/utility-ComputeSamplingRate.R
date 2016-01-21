#' ComputeSamplingRate
#' 
#' \code{ComputeSamplingRate} is ...

ComputeSamplingRate <- function(t.ms) {
  n <- length(t.ms)
  fs <- round(n / ((t.ms[n] - t.ms[1]) / 1000))
  return(fs)
}