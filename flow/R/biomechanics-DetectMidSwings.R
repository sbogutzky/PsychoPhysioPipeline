#' DetectMidSwings
#' 
#' \code{DetectMidSwings} is ...

DetectMidSwings <- function(t.s, angular.velocity, fs = 102.4, ff.1 = 4, ff.2 = .5) {
  
  fn <- fs/2
  
  # Plot data (10s)
  range <- (30 * fs):(40 * fs)
  canPlot = !is.na(t.s[range[1]])
  if(canPlot) {
    par(mfcol = c(1, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
    plot(t.s[range], angular.velocity[range], type = "l", xlab = "Timestamp (s)", ylab = expression("Angular Velocity (" ~ deg/s ~ ")"))
  }
  
  # Compute main frequncy
  main.freq <- ComputeMainFrequency(angular.velocity, fs)
  
  # Filter (1nd level)
  fc <- main.freq * ff.1
  W <- fc/fn
  n <- 2
  lp.1 <- butter(n, W)
  f.1 <- filter(lp.1, angular.velocity)
  if(canPlot) {
    lines(t.s[range], f.1[range], col = 2)
  }
  
  # Filter (2nd level)
  fc <- main.freq * ff.2
  W <- fc/fn
  lp.2 <- butter(n, W)
  f.2 <- filter(lp.2, angular.velocity)
  if(canPlot) {
    lines(t.s[range], f.2[range], col = 3)
  }
  
  readline("Press return to continue > ")
  
  y <- angular.velocity
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
  
  return(mid.swing.indexes)
}