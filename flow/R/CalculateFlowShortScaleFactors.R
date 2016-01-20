#' Calculate the flow short scale factors.
#'
#' \code{CalculateFlowShortScaleFactors} returns a data frame with flow factors and standard deviation.
#'
#' @param items Questionaire items. A numerical vector of the length 16 of items
#' @return Data frame with flow-factor and sd.
#' @examples
#' CalculateFlowShortScaleFactors(c(3,4,3,4,2,3,5,6,4,5,6,4,8,9,1,3))

CalculateFlowShortScaleFactors <- function(items) {
  
  daf = 0
  
  if(items[16] == 1 | items[16] == 9)
    daf = 1
  
  if(items[16] == 2 | items[16] == 8)
    daf = 2
  
  if(items[16] == 3 | items[16] == 7)
    daf = 3
  
  if(items[16] == 4 | items[16] == 6)
    daf = 4
  
  if(items[16] == 5)
    daf = 5
  
  data.frame(flow = mean(items[1:10]), flow.sd = sd(items[1:10]),
             fluency = mean(items[c(8,7,9,4,5,2)]), fluency.sd = sd(items[c(8,7,9,4,5,2)]), 
             absorption = mean(items[c(6,1,10,3)]), absorption.sd = sd(items[c(6,1,10,3)]), 
             anxiety = mean(items[11:13]), anxiety.sd = sd(items[11:13]), 
             fit = mean(items[14:16]), fit.sd = sd(items[14:16]),
             daf = daf
             )
}