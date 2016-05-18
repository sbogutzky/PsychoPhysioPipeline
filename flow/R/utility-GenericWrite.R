#' Writes data with a write function in a file .
#' 
#' \code{GenericWrite} writes data with a write function in a file.
#'
#' @param x. The data to write
#' @param file. The File to write in
#' @param header. A extra header on top of the data
#' @param footer. A extra footer below the data
#' @param f. A write function
#' @param ... write options.

GenericWrite <- function(x, file, header, footer, f = write.csv, ...){
  # create and open the file connection
  datafile <- file(file, open = 'wt')
  # close on exit
  on.exit(close(datafile))
  # if a header is defined, write it to the file (@CarlWitthoft's suggestion)
  if(!missing(header)) writeLines(header,con = datafile)
  # write the file using the defined function and required addition arguments  
  f(x, datafile, ...)
  # if a footer is defined, write it to the file (@CarlWitthoft's suggestion)
  if(!missing(footer)) writeLines(footer, con = datafile)
}