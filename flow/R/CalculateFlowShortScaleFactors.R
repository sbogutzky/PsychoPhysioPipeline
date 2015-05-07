#' Calculate the flow short scale factors.
#'
#' \code{CalculateFlowShortScaleFactors} returns a data frame with flow factors and standard deviation.
#'
#' @param items Questionaire items. A numerical vector of the length 16 of items
#' @return Data frame with flow-factor and sd.
#' @examples
#' CalculateFlowShortScaleFactors(c(3,4,3,4,2,3,5,6,4,5,6,4,8,9,1,3))

CalculateFlowShortScaleFactors <- function(items) { 
  data.frame(flow = mean(items[1:10]), flow.sd = sd(items[1:10]), fluency = mean(items[c(8,7,9,4,5,2)]), fluency.sd = sd(items[c(8,7,9,4,5,2)]), absorption = mean(items[c(6,1,10,3)]), absorption.sd = sd(items[c(6,1,10,3)]), anxiety = mean(items[11:13]), anxiety.sd = sd(items[11:13]), fit = mean(items[14:16]), fit.sd = sd(items[14:16]))
}