#' ComputeMainFrequency
#' 
#' \code{ComputeMainFrequency} is ...

ComputeMainFrequency <- function(y, fs) {
  
  periodogram <- TSA::periodogram(y, plot = F)
  freqs <- periodogram[[1]] * fs
  specs <- periodogram[[2]]
  index <- which.max(specs)
  main.freq <- freqs[index]
  new.main.freq <- ceiling(main.freq)
  
  return(new.main.freq)
}