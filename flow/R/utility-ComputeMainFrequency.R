# The MIT License (MIT)
# Copyright (c) 2016 University of Applied Sciences Bremen
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

#' Computes the main frequency of a cyclic signal.
#' 
#' \code{ComputeMainFrequency} returns the main frequency of a cyclic signal.
#' 
#' @param y a numeric vector of values
#' @param fs a numeric that specifies the sampling rate
#' @return the numeric of main frequency

ComputeMainFrequency <- function(y, fs) {
  
  periodogram <- TSA::periodogram(y, plot = F)
  freqs <- periodogram[[1]] * fs
  specs <- periodogram[[2]]
  index <- which.max(specs)
  main.freq <- freqs[index]
  
  return(main.freq)
}