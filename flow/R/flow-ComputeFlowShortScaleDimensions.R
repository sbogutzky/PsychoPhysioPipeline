# The MIT License (MIT)
# Copyright (c) 2016 Simon Bogutzky
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software
# and associated documentation files (the "Software"), to deal in the Software without restriction,
# including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
# DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
# ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#'Computes the flow short scale dimensions.
#'
#' \code{ComputeFlowShortScaleDimensions} returns a data frame with flow dimensions and standard deviations.
#'
#' @param items a numerical vector of the length 16 of questionaire items
#' @return the data frame with flow dimensions and their standard deviations
#' @examples
#' ComputeFlowShortScaleDimensions(c(3,4,3,4,2,3,5,6,4,5,6,4,8,9,1,3))

ComputeFlowShortScaleDimensions <- function(items) {
  
  fit.m = as.numeric(NA)
  
  if(!is.na(items[16])) {
  
    if(items[16] == 1 | items[16] == 9)
      fit.m = 1
    
    if(items[16] == 2 | items[16] == 8)
      fit.m = 2
    
    if(items[16] == 3 | items[16] == 7)
      fit.m = 3
    
    if(items[16] == 4 | items[16] == 6)
      fit.m = 4
    
    if(items[16] == 5)
      fit.m = 5
  }
  
  data.frame(flow = mean(items[1:10]), flow.sd = sd(items[1:10]),
             fluency = mean(items[c(8,7,9,4,5,2)]), fluency.sd = sd(items[c(8,7,9,4,5,2)]), 
             absorption = mean(items[c(6,1,10,3)]), absorption.sd = sd(items[c(6,1,10,3)]), 
             anxiety = mean(items[11:13]), anxiety.sd = sd(items[11:13]), 
             challenge = items[14], skill = items[15], fit = items[16],
             fit.m = fit.m
  )
}