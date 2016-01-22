#' Searches for extrema in a signal.
#' 
#' \code{SearchExtrema} returns extrema of a signal.
#'
#' @param y. A numerical vector of signal
#' @param which. One of "both", "maxima", "minima". Default is "both"
#' @return A numerical vector of extrema indixes

SearchExtrema <- function(y, which = c("both", "maxima", "minima")) {
  
  maxima <- c()
  minima <- c()
  
  if(missing(which) | which == "both" | which == "maxima") {
    
    # Find locations of local maxima
    # p = 1 at maxima, p otherwise, end point maxima excluded
    n <- length(y) - 2
    p <- sign(sign(y[2:(n + 1)] - y[3:(n + 2)]) - sign(y[1:n] - y[2:(n + 1)]) -.1) + 1
    p <- c(0, p, 0)
    
    # Indices of maxima
    p <- as.logical(p) 
    i <- 1:length(p)
    maxima <- i[p]
  }
  
  if(missing(which) | which == "both" | which == "minima") {
    
    yn <- -y
    
    # Find locations of local minima
    # p = 1 at minima, p otherwise, end point minima excluded
    n <- length(yn) - 2
    p <- sign(sign(yn[2:(n + 1)] - yn[3:(n + 2)]) - sign(yn[1:n] - yn[2:(n + 1)]) -.1) + 1
    p <- c(0, p, 0)
    
    # Indices of minima
    p <- as.logical(p) 
    i <- 1:length(p)
    minima <- i[p]
  }
  return(c(maxima, minima))
}