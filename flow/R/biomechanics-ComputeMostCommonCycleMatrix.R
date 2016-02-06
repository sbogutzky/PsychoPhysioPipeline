#' Computes a matrix of cycles with the most common length.
#' 
#' \code{ComputeMostCommonCycleMatrix} return a matrix with cycles the most common length.
#'
#' @param y. A numeric vector of data
#' @param separator.indexes. A numerical vector of separator indexes
#' @return A matrix of cycles with the most common length

ComputeMostCommonCycleMatrix <- function(y, separator.indexes) {
  require(pracma)
  
  center <- Mode(diff(separator.indexes)) + 1
  cycles <- c()
  j = 0
  for(i in 1:(length(separator.indexes) - 1)) {
    m <- separator.indexes[i]
    n <- separator.indexes[(i+1)]
    cycle <- y[m:n]
    if(center == length(cycle)) {
      cycles <- c(cycles, cycle)
      j = j + 1
    }
  }
  cycle.matrix <- matrix(cycles, center, j)
  
  return(cycle.matrix)
}
