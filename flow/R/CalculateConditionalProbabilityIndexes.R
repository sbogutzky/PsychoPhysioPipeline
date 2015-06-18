#' Calculates conditional probability index for two interacting oscillators
#'
#' \code{CalculateConditionalProbabilityIndex} returns the conditional probability index.
#'
#' @param times. A numeric verctor of ti's for instantanous phase calculation.
#' @param time.series.1. A numeric verctor of time events of the first oscillator.
#' @param time.series.2. A numeric verctor ofime events of the second oscillator.
#' @param n. Phase locking integer for the first oscillator.
#' @param m. Phase locking integer for the second oscillator.
#' @param bins. Integer bins for dividing the interval of the first first oscillator.
#' @param plot. Logical if should plot phase in circles.
#' @return conditional probability index

CalculateConditionalProbabilityIndexes <- function(times, time.series.1, time.series.2, n, m, bins = 16, plot = F) {
  
  fi.1 <- CalculateInstantaneousPhases(times, time.series.1) %% (2*pi*n)
  
  bin.breaks <- seq(0, 2 * pi, 2 * pi / bins)
  
  if(plot) {
    circle <- function(x, y, rad = 1, nvert = 500, ...){
      rads <- seq(0,2*pi,length.out = nvert)
      xcoords <- cos(rads) * rad + x
      ycoords <- sin(rads) * rad + y
      polygon(xcoords, ycoords, ...)
    }
    
    par(mfcol=c(1, 1))
    plot(-1:1, type = "n", xlim = c(-1,1), ylim = c(-1,1), asp = 1, bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    circle(0,0,1)
    segments(rep(0,bins-1),rep(0,bins-1),sin(bin.breaks[-1]),cos(bin.breaks[-1]), lty = "dashed")
    segments(rep(0,length(fi.1)),rep(0,length(fi.1)),sin(fi.1),cos(fi.1))
    points(sin(fi.1), cos(fi.1), pch = 19)
    
    mtext(expression(pi), side = 1, line = 0)
    mtext(expression(pi/2), side = 4, line = 0)
    mtext(expression(0), side = 3, line = 0)
    mtext(expression(2*pi/3), side = 2, line = 0)
  }
  
  lambda <- c()
  for (l in 1:(length(bin.breaks)-1)) {
    t.l <- times[bin.breaks[l] <= fi.1 & fi.1 < bin.breaks[l+1]]
    t.l <- t.l[!is.na(t.l)]
    M.l <- length(t.l)
    
    fi.2 <- CalculateInstantaneousPhases(t.l, time.series.2)
    eta  <- fi.2 %% (2*pi*m)
    lambda.l <- M.l^-1 * sqrt(sum(cos(eta/m))^2 + sum(sin(eta/m))^2)
    
    if(plot) {
      par(mfcol=c(1, 1))
      plot(-1:1, type = "n", xlim = c(-1,1), ylim = c(-1,1), asp = 1, bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
      circle(0,0,1)
      segments(rep(0,length(eta)),rep(0,length(eta)),sin(eta/m),cos(eta/m))
      points(sin(eta/m), cos(eta/m), pch = 19)
    
      mtext(expression(pi), side = 1, line = 0)
      mtext(expression(pi/2), side = 4, line = 0)
      mtext(expression(0), side = 3, line = 0)
      mtext(expression(2*pi/3), side = 2, line = 0)
    
      print(paste("From", round(bin.breaks[l], 3), "to", round(bin.breaks[l+1], 3), "; Count:", M.l, "; dependent:", length(eta), "; lambda:", round(lambda.l, 3)))
    }
    lambda <- c(lambda, lambda.l)
  }
  
  return(mean(lambda, na.rm = T))
}