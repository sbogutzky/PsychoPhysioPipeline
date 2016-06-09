#' Computes optimal cut off frequency for a low pass butterworth filter
#' 
#' \code{ComputeOptimalCutoffFrequency} returns the index.
#' @param x a numerical vector.
#' @param fs sampling rate.
#' @param N filter order.
#' @param plot a boolean for control plot.
#' @return the optimal cut off frequency.

ComputeOptimalCutoffFrequency <- function(x, fs, N, plot = FALSE) {
  library(signal)
  
  fn <- fs/2
  rsmes <- c()
  fcs <- seq(0.5, fn, 0.5)
  for (fc in fcs) {
    W <- fc/fn
    lp <- butter(N, W)
    model <- filtfilt(lp, x)
    rsmes <- c(rsmes, sqrt(sum((model - x)^2, na.rm = T) / length(x)))
  }
  rm(fc, W, lp, model)
  
  fcb <- fs / 10
  fce <- fn - 5
  
  rsmeb <- rsmes[which.min(abs(fcs - fcb))]
  rsmee <- rsmes[which.min(abs(fcs - fce))]
  
  model <- lm(c(rsmee, rsmeb) ~ c(fce, fcb))
  noises <- model$coefficients[1] + fcs * model$coefficients[2]
  index <- which.min(abs(rsmes - noises[1]))
  fc <- fcs[index]
  
  if(plot) {
    par(mfcol = c(1, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
    plot(fcs, rsmes, type = "l", xlab = "Filter cutoff frequencies (Hz)", ylab = "RMS deviation", ylim = c(0, (max(max(rsmes), max(noises))) + 1), xaxs = "i")
    lines(fcs, noises)
    abline(v = c(fce, fcb))
    abline(h = noises[1])
    points(fc, rsmes[index])
    abline(v = fc)
  }
  
  return(fc)
}