#' Computes optimal cut off frequency for a low pass butterworth filter
#' 
#' \code{ComputeOptimalCutoffFrequency} returns the index.
#' @param x vector with data.
#' @param fs sampling rate.
#' @param N Order of the filter.
#' @return the optimal cut off frequency.

ComputeOptimalCutoffFrequency <- function(x, fs, N) {
  par(mfcol = c(1, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
  rsmes <- c()
  fcs <- seq(0.5, fs/2, 0.5)
  for (fc in fcs) {
    W <- fc/fn
    lp <- butter(N, W)
    model <- filtfilt(lp, x)
    rsmes <- c(rsmes, sqrt(sum((model - x)^2, na.rm = T) / length(x)))
  }
  rm(fc, W, lp, model)
  
  fcb <- fs / 10
  fce <- (fs / 2) - 5
  
  rsmeb <- rsmes[which.min(abs(fcs - fcb))]
  rsmee <- rsmes[which.min(abs(fcs - fce))]
  
  model <- lm(c(rsmee, rsmeb) ~ c(fce, fcb))
  noises <- model$coefficients[1] + fcs * model$coefficients[2]
  
  
  plot(fcs, rsmes, type = "l", xlab = "Filter cutoff frequencies (Hz)", ylab = "RMS deviation", ylim = c(0, (max(max(rsmes), max(noises))) + 1))
  lines(fcs, noises)
  abline(v = c(fce, fcb))
  abline(h = noises[1])
  index <- which.min(abs(rsmes - noises[1]))
  
  fc <- fcs[index]
  points(fc, rsmes[index])
  abline(v = fc)
  
  return(fc)
}