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

#' Computes the instantaneous phase.
#' 
#' \code{ComputeInstantaneousPhases} returns the instantaneous phase.
#' @param ts1 a numeric vector of time events.
#' @param ts2 a numeric vector of time events (onset of the cycle).
#' @return fi a numeric vector of instantaneous phase.

ComputeInstantaneousPhases <- function(ts1, ts2) {
  fis <- c()
  ts <- c()
  
  for (i in 1:(length(ts2) - 1)) {
    cycle <- c(ts2[i], ts2[i + 1])
    fi <- NA
    for (t in ts1) {
      if (cycle[1] <= t && t < cycle[2]) { 
        fi <- 2 * pi * i + 2 * pi * (t - cycle[1]) / diff(cycle)
        fis <- c(fis, fi)
        ts <- c(ts, cycle[1])
      }
    }
    if(is.na(fi)) {
      fis <- c(fis, fi)
      ts <- c(ts, cycle[1])
    }
  }
  return(data.frame(ts = ts, fi = fis))
}