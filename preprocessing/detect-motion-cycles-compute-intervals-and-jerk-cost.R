# Remove all variables
rm(list = ls(all = T)) 

# Load libraries
require(TSA)
require(signal)
require(flow)

# Set root data directory path
root.data.directory.path <- ""
if(file.exists("/Volumes/flow/Documents/archiv/daten/2015/flow-gehen-und-laufen"))
  root.data.directory.path        <- "/Volumes/flow/Documents/archiv/daten/2015/flow-gehen-und-laufen/"
if(file.exists("//gangstore.ddns.net/flow/Documents/archiv/daten/2015/flow-gehen-und-laufen"))
  root.data.directory.path        <- "//gangstore.ddns.net/flow/Documents/archiv/daten/2015/flow-gehen-und-laufen/"
if(file.exists("C:/Users/Simon Bogutzky/Documents/Archiv/flow/data"))
  root.data.directory.path        <- "C:/Users/Simon Bogutzky/Documents/Archiv/flow/data/"

# Set preprocessed data directory path
preprocessed.data.directory.path <- "./data/preprocessed-data/"

# Set processed data directory path
processed.data.directory.path <- paste(root.data.directory.path, "processed-data/", sep = "")

# Set features directory path
features.directory.path <- paste(root.data.directory.path, "features/", sep = "")

# Read activity directory
activity.directory  <- readline("Type in activity directory and press return to continue (e. g. walking/) > ")

# Read user directory
user.directory      <- readline("Type in user directory and press return to continue (e. g. doe-john/) > ")

# Read in body position
body.position       <- readline("Type in body position and press return to continue (e. g. leg) > ")

# Read in subset length in seconds
length.s         <- as.numeric(readline("Type in subset size in seconds for visual control and press return to continue (e. g. 30) > "))

# Load fss features
fss.features        <- read.csv(paste(features.directory.path, activity.directory, user.directory, "fss-features.csv", sep = ""))

ComputeSamplingRate <- function(t.ms) {
  n <- length(t.ms)
  fs <- round(n / ((t.ms[n] - t.ms[1]) / 1000))
  new.fs <- 2^ceiling(log(fs)/log(2))
  return(new.fs)
}

ResampleData <- function(data, fs, t.ms) {
  
  x <- t.ms
  xi <- seq(x[1], x[length(x)], by = 1000/fs)
  n.col <- ncol(data)
  n.row <- length(xi)
  
  yi.all <- c()
  for(i in 1:n.col) {
    y <- data[, i]
    yi <- signal::interp1(x, y, xi, method = "spline")
    yi.all <- c(yi.all, yi)
  }
  
  m <- matrix(c(xi, yi.all), n.row, n.col + 1)
  
  return(m)
}

ComputeMainFrequency <- function(y, fs) {
  
  periodogram <- TSA::periodogram(y, plot = F)
  freqs <- periodogram[[1]] * fs
  specs <- periodogram[[2]]
  index <- which.max(specs)
  main.freq <- freqs[index]
  new.main.freq <- ceiling(main.freq)
  
  return(new.main.freq)
}

DetectMidSwings <- function(y, y.1, y.2) {
  
  # Add minima original data
  minima    <- SearchExtrema(y, which = "minima")
  minima.f  <- rep(0, length(minima))
  
  # Add minima from the filtered signal (1st level)
  minima.1  <- SearchExtrema(y.1, which = "minima")
  minima    <- c(minima, minima.1)
  minima.f  <- c(minima.f, rep(1, length(minima.1)))
  rm(minima.1)
  
  # Add minima from the filtered signal (2nd level)
  minima.2  <- SearchExtrema(y.2, which = "minima")
  minima    <- c(minima, minima.2)
  minima.f  <- c(minima.f, rep(2, length(minima.2)))
  rm(minima.2)
  
  # Create minima data frame (sorted)
  minima <- data.frame(minima, minima.f)[order(-minima),]
  rm(minima.f)
  
  # Identify and sort mid swings
  mid.swing.index <- c()
  found.level.2   <- F
  found.level.1   <- F
  for(j in 1:nrow(minima)) {
    if(!found.level.2)
      found.level.2 <- as.numeric(minima$minima.f[j]) == 2
    else {
      if(!found.level.1)
        found.level.1 <- as.numeric(minima$minima.f[j]) == 1
      else {
        if(as.numeric(minima$minima.f[j]) == 0) {
          mid.swing.index <- c(mid.swing.index, minima$minima[j])
          found.level.2   <- F
          found.level.1   <- F
        }
      }
    }
  }
  mid.swing.index <- sort(mid.swing.index)
  
  return(mid.swing.index)
}

DetectAnnomalies <- function(x, y, x.lab, y.lab, x.lim, y.lim, epsilon = 0) {
  X <- matrix(data = c(x, y), nrow = length(y), ncol = 2)
  plot(X[,1], X[,2], xlab = x.lab, ylab = y.lab, pch = 21, xlim = x.lim, ylim = y.lim)
  
  gl <- EstimateGaussian(X)
  p  <- MultivariateGaussian(X, gl$mu, gl$sigma2)
  
  if(epsilon == 0) {
    y <- zeros(nrow(X), 1)
    y[identify(X)]  <- 1
    bt              <- SelectThreshold(y, p)
    epsilon         <- bt$epsilon
  }
  
  outliers <- which(p < epsilon)
  points(X[outliers, 1], X[outliers, 2], xlab = x.lab, ylab = y.lab, pch = 21, bg = 2)
  
  return(list("epsilon" = epsilon, "outliers" = outliers))  
}

CheckMidSwingDetection <- function(t.ms, angular.velocity.deg.s, length.s, mid.swing.ms, mid.swing.deg.s) {
  
  current.t.ms <- 0
  while(current.t.ms < max(t.ms)) {
    
    plot(t.ms[t.ms >= current.t.ms & t.ms < current.t.ms + length.s * 1000] / 1000, angular.velocity.deg.s[t.ms >= current.t.ms & t.ms < current.t.ms + length.s * 1000], type = "l", xlab = expression("Time ("~s~")"), ylab = expression("Angular Velocity ("~deg/s~")"))
    points(mid.swing.ms[mid.swing.ms >= current.t.ms & mid.swing.ms < current.t.ms + length.s * 1000] / 1000, mid.swing.deg.s[mid.swing.ms >= current.t.ms & mid.swing.ms < current.t.ms + length.s * 1000], pch = 21, bg = "red")
    abline(h = mean(mid.swing.deg.s) + 4 * sd(mid.swing.deg.s), lty = "dashed", col = "darkgrey")
    title("Select to remove")
    
    remove  <- identify(mid.swing.ms / 1000, mid.swing.deg.s)
    
    # Remove selected mid swings
    if(length(remove) > 0) {
      mid.swing.ms <- mid.swing.ms[-remove]
      mid.swing.deg.s <- mid.swing.deg.s[-remove]
    }
    
    plot(t.ms[t.ms >= current.t.ms & t.ms < current.t.ms + length.s * 1000] / 1000, angular.velocity.deg.s[t.ms >= current.t.ms & t.ms < current.t.ms + length.s * 1000], type = "l", xlab = expression("Time ("~s~")"), ylab = expression("Angular Velocity ("~deg/s~")"))
    points(mid.swing.ms[mid.swing.ms >= current.t.ms & mid.swing.ms < current.t.ms + length.s * 1000] / 1000, mid.swing.deg.s[mid.swing.ms >= current.t.ms &  mid.swing.ms < current.t.ms + length.s * 1000], pch = 21, bg = "red")
    abline(h = mean(mid.swing.deg.s) + 4 * sd(mid.swing.deg.s), lty = "dashed", col = "darkgrey")
    title("Select to add")
    
    # Add mid swings and control
    add  <- identify(t.ms / 1000, angular.velocity.deg.s)
    if(length(add) > 0) {
      mid.swing.ms <- c(mid.swing.ms, t.ms[add])
      mid.swing.deg.s <- c(mid.swing.deg.s, angular.velocity.deg.s[add])
      
      mid.swings <- data.frame(t.ms = mid.swing.ms, angular.velocity.deg.s = mid.swing.deg.s)[order(mid.swing.ms),]
      
      plot(t.ms[t.ms >= current.t.ms & t.ms < current.t.ms + length.s * 1000] / 1000, angular.velocity.deg.s[t.ms >= current.t.ms & t.ms < current.t.ms + length.s * 1000], type = "l", xlab = expression("Time ("~s~")"), ylab = expression("Angular Velocity ("~deg/s~")"))
      points(mid.swings[, 1][mid.swings[, 1] >= current.t.ms & mid.swings[, 1] < current.t.ms + length.s * 1000] / 1000, mid.swings[, 2][mid.swings[, 1] >= current.t.ms & mid.swings[, 1] < current.t.ms + length.s * 1000], pch = 21, bg = "red")
      abline(h = mean(mid.swing.deg.s) + 4 * sd(mid.swing.deg.s), lty = "dashed", col = "darkgrey")
      
      readline("Press return to continue > ")
    }
    
    current.t.ms       <- current.t.ms + length.s * 1000
  }
  
  mid.swings <- data.frame(t.ms = mid.swing.ms, angular.velocity.deg.s = mid.swing.deg.s)[order(mid.swing.ms),]
  return(mid.swings)
}

ComputeCycleJerkCosts <- function(A, mid.swings) {
  jerk.costs <- c()
  for(l in 1:(nrow(mid.swings) - 1)) {
    
    # Compute jerk cost of each cycle
    in.cycle            <- mid.swings[l, 1] <= A[, 1] & A[, 1] < mid.swings[l + 1, 1]
    t.ms                <- A[, 1][in.cycle]
    # acceleration.x.ms.2 <- A[, 2][in.cycle]
    acceleration.y.ms.2 <- A[, 3][in.cycle]
    acceleration.z.ms.2 <- A[, 4][in.cycle]
    
    jerk.cost   <- CalculateJerkCost(t.ms / 1000, data.frame(acceleration.y.ms.2, acceleration.z.ms.2), normalized = T)
    jerk.costs  <- c(jerk.costs, jerk.cost)
  }
  
  return(jerk.costs)
}

saveData <- function(output.data, preprocessed.data.directory.path, activity.directory, user.directory, date.directory, body.position, activity.start, measurement) {
  
  # Create directory, if needed
  output.directory.path <- paste(processed.data.directory.path, activity.directory, user.directory, date.directory, sep="")
  if(!file.exists(substr(output.directory.path, 1, nchar(output.directory.path) - 1))) {
    dir.create(output.directory.path, recursive = TRUE)
  }
  
  # Write csv file
  output.file.path <- paste(output.directory.path, body.position, "-jerk-cost-data-", measurement, ".csv", sep = "")
  op <- options(digits.secs=3)
  con <- file(output.file.path, 'w') 
  writeLines(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y-%m-%d"), con = con)
  writeLines(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%H:%M:%OS"), con = con)
  write.csv(output.data, file = con, row.names = FALSE)
  close(con)
  options(op) #reset options
  print(paste("Wrote:", output.file.path))
}

for (i in 13:13) { # nrow(fss.features)) {
  
  properties      <- fss.features[i, c(6:12)]
  activity.start  <- properties[, 2]
  measurement     <- properties[, 5]
  if(measurement == 1) {
    date.directory  <- paste(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y-%m-%d--%H-%M-%S"), "/", sep ="")
  }
  motion.data.path <- paste(preprocessed.data.directory.path, activity.directory, user.directory, date.directory, body.position, "-motion-data-", measurement,  ".csv", sep="")
  
  if(file.exists(motion.data.path)) {
    
    # Load motion data
    motion.data <- read.csv(motion.data.path)
    x <- motion.data[, 1]
    y <- motion.data[, 5]
    
    # Compute fS
    fs <- ComputeSamplingRate(x)
    
    # Resample data
    M <- ResampleData(motion.data[2:7], fs, x)
    
    # Plot resampled data
    value.range <- (200 * 128):(210 * 128)
    plot(M[, 1][value.range]/1000, M[, 5][value.range], type = "l", xlab = expression("Time ("~s~")"), ylab = expression("Angular Velocity ("~deg/s~")"))
    
    # Compute main freq
    main.freq <- ComputeMainFrequency(M[, 5], fs)
    
    # Filter (1nd level)
    lp.1 <- butter(3, 1/(fs/2) * main.freq * 2, "low")
    f.1 <- filter(lp.1, M[, 5])
    lines(M[, 1][value.range]/1000, f.1[value.range], col = 2)
    
    # Filter (2nd level)
    lp.2 <- butter(3, 1/(fs/2) * main.freq * .5, "low")
    f.2 <- filter(lp.2, M[, 5])
    lines(M[, 1][value.range]/1000, f.2[value.range], col = 3)
    
    # Detect Midswings
    ms <- DetectMidSwings(M[, 5], f.1, f.2)
    mid.swing.ms <- M[, 1][ms]
    mid.swing.deg.s <- M[, 5][ms]
    points(mid.swing.ms/1000, mid.swing.deg.s, col = 4)
    
    # Remove changes below 10 deg per second
    mid.swing.ms <- mid.swing.ms[mid.swing.deg.s > 10 | mid.swing.deg.s < -10]
    mid.swing.deg.s <- mid.swing.deg.s[mid.swing.deg.s > 10 | mid.swing.deg.s < -10]
    
    # Detect Annomalies
    diff.x <- c(0, diff(mid.swing.ms / 1000))
    annomalies <- DetectAnnomalies(diff.x, mid.swing.deg.s/100, expression("Cycle Interval ("~s~")"), expression("Angular Velocity (x"~10^2~deg/s~")"), c(min(diff.x), max(diff.x)), c(min(mid.swing.deg.s/100), max(mid.swing.deg.s/100)), epsilon = 0)
    
    if(length(annomalies$outliers) > 0) {
      mid.swing.ms <- mid.swing.ms[-annomalies$outliers]
      mid.swing.deg.s <- mid.swing.deg.s[-annomalies$outliers]
    }
    
    # Check data
    mid.swings <- CheckMidSwingDetection(M[, 1], M[, 5], length.s, mid.swing.ms, mid.swing.deg.s)
    
    #TODO: save mid.swings
    
    # Plot all data
    plot(M[, 1] / 1000, M[, 5], type = "l", xlab = expression("Time ("~s~")"), ylab = expression("Angular Velocity ("~deg/s~")"))
    points(mid.swings[, 1] / 1000, mid.swings[, 2], pch = 21, bg = "red")
    
    # Smooth acceleration data
    A <- M[, 1:4]
    fn <- fs/2
    n <- 2
    current.cycle <- A[, 1] > mid.swings[50, 1] & A[, 1] < mid.swings[51, 1]
    # plot(A[, 1][current.cycle] / 1000, A[, 2][current.cycle], type = "l", xlab = expression("Time ("~s~")"), ylab = expression("Acceleration ("~m/s^2~")"), ylim = c(-40, 30))
    plot(A[, 1][current.cycle] / 1000, A[, 3][current.cycle], type = "l", xlab = expression("Time ("~s~")"), ylab = expression("Acceleration vertical ("~m/s^2~")"), ylim = c(-40, 30))
    plot(A[, 1][current.cycle] / 1000, A[, 4][current.cycle], type = "l", xlab = expression("Time ("~s~")"), ylab = expression("Acceleration horizontal ("~m/s^2~")"), ylim = c(-40, 30))
    fc <- 6.5
    W <- fc/fn
    lowpass.filter.vertical <- butter(n, W)
    # A[, 2] <- filtfilt(lowpass.filter, A[, 2])
    A[, 3] <- filtfilt(lowpass.filter.vertical, A[, 3])
    
    fc <- 7.5
    W <- fc/fn
    lowpass.filter.horizontal <- butter(n, W)
    A[, 4] <- filtfilt(lowpass.filter.horizontal, A[, 4])
    # plot(A[, 1][current.cycle] / 1000, A[, 2][current.cycle], type = "l", xlab = expression("Time ("~s~")"), ylab = expression("Acceleration ("~m/s^2~")"), ylim = c(-40, 30))
    plot(A[, 1][current.cycle] / 1000, A[, 3][current.cycle], type = "l", xlab = expression("Time ("~s~")"), ylab = expression("Acceleration vertical ("~m/s^2~")"), ylim = c(-40, 30))
    plot(A[, 1][current.cycle] / 1000, A[, 4][current.cycle], type = "l", xlab = expression("Time ("~s~")"), ylab = expression("Acceleration horizontal ("~m/s^2~")"), ylim = c(-40, 30))
    
    # Compute output data
    cycle.jerk.costs <- ComputeCycleJerkCosts(A, mid.swings)
    cycle.intervals <- diff(mid.swings[, 1] / 1000)
    output.data <- data.frame(t.s = mid.swings[(2:nrow(mid.swings)), 1] / 1000, cycle.interval.s = cycle.intervals, jerk.cost.m2s5 = cycle.jerk.costs)
    output.data <- output.data[output.data[, 2] < 1.5, ]
    
    # Compute mean to compare
    jerk.cost.by.all.accelerations  <- CalculateJerkCost(A[, 1] / 1000, A[, 3:4], normalized = T) 
    jerk.cost.by.cycle.mean         <- mean(output.data[, 3], na.rm = T)
    print(paste("Jerk Cost by Cycle:           :", jerk.cost.by.cycle.mean / 10^4))
    print(paste("Jerk Cost by all Accelerations:", jerk.cost.by.all.accelerations / 10^4))
    
    # Save data
    saveData(output.data, preprocessed.data.directory.path, activity.directory, user.directory, date.directory, body.position, activity.start, measurement)
    
    readline("Press return to continue > ")
    
  } else {
    print(paste("File not exits:", motion.data.path))
  }
}