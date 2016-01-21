#' Search for extrema in a vector.
#' 
#' \code{SearchExtrema} Search for extrema in a vector.
#'
#' @param v Vector of a function
#' @param which One of "both", "maxima", "minima". Default is "both"
#' @return Vector of extrema indices

SearchExtrema <- function(v, which = c("both", "maxima", "minima")) {
  
  maxima <- c()
  minima <- c()
  
  if(missing(which) | which == "both" | which == "maxima") {
    
    # Find locations of local maxima
    # p = 1 at maxima, p otherwise, end point maxima excluded
    n <- length(v) - 2
    p <- sign(sign(v[2:(n + 1)] - v[3:(n + 2)]) - sign(v[1:n] - v[2:(n + 1)]) -.1) + 1
    p <- c(0, p, 0)
    
    # Indices of maxima
    p <- as.logical(p) 
    i <- 1:length(p)
    maxima <- i[p]
  }
  
  if(missing(which) | which == "both" | which == "minima") {
    
    vn <- -v
    
    # Find locations of local minima
    # p = 1 at minima, p otherwise, end point minima excluded
    n <- length(vn) - 2
    p <- sign(sign(vn[2:(n + 1)] - vn[3:(n + 2)]) - sign(vn[1:n] - vn[2:(n + 1)]) -.1) + 1
    p <- c(0, p, 0)
    
    # Indices of minima
    p <- as.logical(p) 
    i <- 1:length(p)
    minima <- i[p]
  }
  return(c(maxima, minima))
}