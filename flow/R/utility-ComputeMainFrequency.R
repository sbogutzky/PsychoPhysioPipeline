#' Computes the main frequency of a cyclic signal.
#' 
#' \code{ComputeMainFrequency} returns the main frequency of a cyclic signal.
#' 
#' @param y. A numerical vector of values
#' @param fs. A numerical that specifies the sampling rate
#' @return A numerical of main frequency

ComputeMainFrequency <- function(y, fs) {
  
  periodogram <- TSA::periodogram(y, plot = F)
  freqs <- periodogram[[1]] * fs
  specs <- periodogram[[2]]
  index <- which.max(specs)
  main.freq <- freqs[index]
  
  return(main.freq)
}