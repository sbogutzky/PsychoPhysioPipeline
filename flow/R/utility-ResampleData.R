#' ResampleData
#' 
#' \code{ResampleData} is ...

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