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

#' Detects mid swings based on angular velocity.
#' 
#' \code{DetectMidSwings} returns mid swings. Uses the angular velocity and filtering like in "Quasi real-time gait event detection using shank-attached gyroscopes" of Lee & Park (2011).
#' @param t.s a numerical vector of seconds
#' @param angular.velocity.deg.s a numerical vector of angular velocity in deg/s.
#' @param fs a numerical that specifies the sampling rate (default 100 Hz).
#' @param cf.1 a numerical that specifies the cut off frequency of the first low pass filter (default = 3 Hz).
#' @param cf.2 a numerical that specifies the cut off frequency of the second low pass filter (default = 1 Hz).
#' @param plot a boolean for control plot.
#' @return A numerical vector of mid swing indexes.

DetectMidSwings <- function(t.s, angular.velocity.deg.s, fs = 100, cf.1 = 3, cf.2 = 1, plot = FALSE) {
  
  require(signal)
  require(zoom)
  
  fn <- fs/2

  # Filter (1st level)
  W <- cf.1/fn
  n <- 2
  lp.1 <- butter(n, W)
  f.1 <- filter(lp.1, angular.velocity.deg.s)
  
  # Filter (2nd level)
  W <- cf.2/fn
  lp.2 <- butter(n, W)
  f.2 <- filter(lp.2, angular.velocity.deg.s)
  
  y <- angular.velocity.deg.s
  y.1 <- f.1
  y.2 <- f.2
  
  # Add maxima original data
  maxima    <- SearchExtrema(y, which = "maxima")
  maxima.f  <- rep(0, length(maxima))
  
  # Add maxima from the filtered signal (1st level)
  maxima.1  <- SearchExtrema(y.1, which = "maxima")
  maxima    <- c(maxima, maxima.1)
  maxima.f  <- c(maxima.f, rep(1, length(maxima.1)))
  rm(maxima.1)
  
  # Add maxima from the filtered signal (2nd level)
  maxima.2  <- SearchExtrema(y.2, which = "maxima")
  maxima    <- c(maxima, maxima.2)
  maxima.f  <- c(maxima.f, rep(2, length(maxima.2)))
  rm(maxima.2)
  
  # Create maxima data frame (sorted)
  maxima <- data.frame(maxima, maxima.f)[order(-maxima),]
  
  # Identify and sort mid swings
  mid.swing.indexes <- c()
  found.level.2   <- F
  found.level.1   <- F
  for(j in 1:nrow(maxima)) {
    if(!found.level.2)
      found.level.2 <- as.numeric(maxima$maxima.f[j]) == 2
    else {
      if(!found.level.1)
        found.level.1 <- as.numeric(maxima$maxima.f[j]) == 1
      else {
        if(as.numeric(maxima$maxima.f[j]) == 0) {
          mid.swing.indexes <- c(mid.swing.indexes, maxima$maxima[j])
          found.level.2   <- F
          found.level.1   <- F
        }
      }
    }
  }
  mid.swing.indexes <- sort(mid.swing.indexes)
  
  # Plot data 
  if(plot) {
    par(mfcol = c(1, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
    plot(t.s, angular.velocity.deg.s, type = "l", xlab = "Timestamp (s)", ylab = expression("Angular Velocity (" ~ deg%.%s^{-1} ~ ")"))
    lines(t.s, f.1, col = rgb(229/255, 66/255, 66/255))
    lines(t.s, f.2, col = rgb(155/255, 193/255, 54/255))
    zm()
  }
  
  return(mid.swing.indexes)
}