#' Calculate the flow short scale results.
#'
#' \code{CalculateFlowShortScaleResults} returns a list of mean results, sd results and time properties.
#'
#' @param items Questionaire items. A numerical vector of the length 16 of items
#' @param start Start time. A scalar of the class 'POSIXct'
#' @param end End time. A scalar of the class 'POSIXct'
#' @return List of mean results, sd results and time properties.
#' @examples
#' CalculateFlowShortScaleResults(c(3,4,3,4,2,3,5,6,4,5,6,4,8,9,1,3))
#' CalculateFlowShortScaleResults(c(3,4,3,4,2,3,5,6,4,5,6,4,8,9,1,3))$results$mean$flow
#' CalculateFlowShortScaleResults(c(3,4,3,4,2,3,5,6,4,5,6,4,8,9,1,3))$results$sd$flow
#' 
#' start.time <- as.POSIXct(strptime("2013-10-31 18:11:26", "%Y-%m-%d %H:%M:%S"), tz="CET")
#' end.time   <- as.POSIXct(strptime("2013-10-31 18:26:26", "%Y-%m-%d %H:%M:%S"), tz="CET")
#' CalculateFlowShortScaleResults(c(3,4,3,4,2,3,5,6,4,5,6,4,8,9,1,3), start=start.time, end=end.time)$results$time.properties$time.taken

CalculateFlowShortScaleResults <- function(items, start, end) { 
  
  t <- list()
  if(!missing(start) & !missing(end)) {
    if(inherits(start, "POSIXct") & inherits(end, "POSIXct")) {
      t <- list(start=start, end=end, time.taken=end - start)
    } else {
      warning("Argument do not inherit from class 'POSIXct'")
    } 
  }
  
  list(results=list(mean=list(flow=mean(items[1:10]), fluency=mean(items[c(8,7,9,4,5,2)]), absorption=mean(items[c(6,1,10,3)]), anxiety=mean(items[11:13]), fit=mean(items[14:16])), sd=list(flow=sd(items[1:10]), fluency=sd(items[c(8,7,9,4,5,2)]), absorption=sd(items[c(6,1,10,3)]), anxiety=sd(items[11:13]), fit=sd(items[14:16])), time.properties=t))
}