#' Write subsets of data in csv files with incremented index.
#' 
#' \code{WriteDataSubsetInCSVFile} Writes subsets of data in csv files with incremented index.
#'
#' @param data Data. A data frame
#' @param times Times. A vector of the class 'POSIXct' with the same length row number than data
#' @param starts Start time of each subset. A vector of the class 'POSIXct' with start times  (must have the same length than 'ends')
#' @param ends End time of each subset. A vector of the class 'POSIXct' with end times (must have the same length than 'starts')
#' @param file.name File name. A vector of the class 'Character' with the file name
#' @param path.name Path name. A vector of the class 'Character' with the path name, where the csv files should be written

WriteDataSubsetInCSVFile <- function(data, times, starts, ends, file.name, file.path) {
  if(nrow(data) == length(times)) {
    if(length(starts) == length(ends)) {
      if(!missing(file.name) & !missing(file.path)) {
        for(i in 1:length(starts)) {
          x     <- data[starts[i] <= times & ends[i] > times, ]
          t     <- times[starts[i] <= times & ends[i] > times]
          start <- t[1]
          end   <- t[length(t)]
          
          file <- paste(file.path, file.name, "-", i, ".csv", sep="")
          write.csv(x, file=file, row.names=F)
          
          message(paste("Wrote data in ", file, " from ", start, " to ", end, " (", format(round(end-start, 2), nsmall=2), ")", sep=""))
        }
      } else {
        warning("File name OR file path is missing")
      }
    } else {
       warning("Starts and ends have not to be the same length")
    }
  } else {
    warning("Data and times have not to be the same length")
  }
}