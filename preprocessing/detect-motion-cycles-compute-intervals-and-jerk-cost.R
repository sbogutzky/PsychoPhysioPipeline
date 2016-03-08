# Remove all variables
rm(list = ls(all = T)) 

# Load libraries
require(TSA)
require(signal)
require(flow)

# Set root data directory path
if(file.exists("/Users/sbogutzky/Desktop/data"))
  root.data.directory.path        <- "/Users/sbogutzky/Desktop/data/"

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
  
  # Identify and sort mid swings
  mid.swing.indexes <- c()
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
          mid.swing.indexes <- c(mid.swing.indexes, minima$minima[j])
          found.level.2   <- F
          found.level.1   <- F
        }
      }
    }
  }
  mid.swing.indexes <- sort(mid.swing.indexes)
  
  return(mid.swing.indexes)
}

CheckMidSwingDetection <- function(t.ms, angular.velocity.deg.s, length.s, mid.swing.indexes) {
  
  par(mfcol = c(2, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
  plot(t.ms / 1000, angular.velocity.deg.s, type = "l", xlab = expression("Time ("~s~")"), ylab = expression("Angular Velocity ("~deg/s~")"))
  points(t.ms[mid.swing.indexes] / 1000, angular.velocity.deg.s[mid.swing.indexes], pch = 21, bg = "red")
  cycle.interval.t.ms <- diff(t.ms[mid.swing.indexes])
  plot(t.ms[mid.swing.indexes] / 1000, c(mean(cycle.interval.t.ms), cycle.interval.t.ms) / 1000, xlab = expression("Time ("~s~")"), ylab = expression("Cycle Interval ("~s~")"))
  
  answer <- readline("Are you finish? Type Y and press return > ")
  if(answer != "Y") {
    answer <- "N"
    current.t.ms <- 0
  }
  
  while(answer == "N") {
    while(current.t.ms < max(t.ms)) {
      
      par(mfcol = c(1, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
      in.loop.line <- t.ms >= current.t.ms & t.ms < current.t.ms + length.s * 1000
      in.loop.points <- t.ms[mid.swing.indexes] >= current.t.ms & t.ms[mid.swing.indexes] < current.t.ms + length.s * 1000
      
      plot(t.ms[in.loop.line] / 1000, angular.velocity.deg.s[in.loop.line], type = "l", xlab = expression("Time ("~s~")"), ylab = expression("Angular Velocity ("~deg/s~")"))
      points(t.ms[mid.swing.indexes][in.loop.points] / 1000, angular.velocity.deg.s[mid.swing.indexes][in.loop.points], pch = 21, bg = "red")
      title("Select to remove")
      
      remove  <- identify(t.ms[mid.swing.indexes] / 1000, angular.velocity.deg.s[mid.swing.indexes])
      
      # Remove selected mid swings
      if(length(remove) > 0) {
        mid.swing.indexes <- mid.swing.indexes[-remove]
      }
      
      in.loop.line <- t.ms >= current.t.ms & t.ms < current.t.ms + length.s * 1000
      in.loop.points <- t.ms[mid.swing.indexes] >= current.t.ms & t.ms[mid.swing.indexes] < current.t.ms + length.s * 1000
      
      plot(t.ms[in.loop.line] / 1000, angular.velocity.deg.s[in.loop.line], type = "l", xlab = expression("Time ("~s~")"), ylab = expression("Angular Velocity ("~deg/s~")"))
      points(t.ms[mid.swing.indexes][in.loop.points] / 1000, angular.velocity.deg.s[mid.swing.indexes][in.loop.points], pch = 21, bg = "red")
      title("Select to add")
      
      # Add mid swings and control
      add  <- identify(t.ms / 1000, angular.velocity.deg.s)
      if(length(add) > 0) {
        mid.swing.indexes <- c(mid.swing.indexes, add)
        mid.swing.indexes <- sort(mid.swing.indexes)
        
        in.loop.line <- t.ms >= current.t.ms & t.ms < current.t.ms + length.s * 1000
        in.loop.points <- t.ms[mid.swing.indexes] >= current.t.ms & t.ms[mid.swing.indexes] < current.t.ms + length.s * 1000
        
        plot(t.ms[in.loop.line] / 1000, angular.velocity.deg.s[in.loop.line], type = "l", xlab = expression("Time ("~s~")"), ylab = expression("Angular Velocity ("~deg/s~")"))
        points(t.ms[mid.swing.indexes][in.loop.points] / 1000, angular.velocity.deg.s[mid.swing.indexes][in.loop.points], pch = 21, bg = "red")
        
        readline("Press return to continue > ")
      }
      
      current.t.ms       <- current.t.ms + length.s * 1000
    }
    
    par(mfcol = c(2, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
    plot(t.ms / 1000, angular.velocity.deg.s, type = "l", xlab = expression("Time ("~s~")"), ylab = expression("Angular Velocity ("~deg/s~")"))
    points(t.ms[mid.swing.indexes] / 1000, angular.velocity.deg.s[mid.swing.indexes], pch = 21, bg = "red")
    cycle.interval.t.ms <- diff(t.ms[mid.swing.indexes])
    plot(t.ms[mid.swing.indexes] / 1000, c(mean(cycle.interval.t.ms), cycle.interval.t.ms) / 1000, xlab = expression("Time ("~s~")"), ylab = expression("Cycle Interval ("~s~")"))
    
    answer <- readline("Are you finish? Type Y and press return > ")
    if(answer != "Y") {
      answer <- "N"
      current.t.ms <- 0
    }
  }
  
  return(mid.swing.indexes)
}

ComputeCycleJerkCosts <- function(A, mid.swing.indexes) {
  jerk.costs <- c()
  for(i in 1:(length(mid.swing.indexes) - 1)) {
    
    m <- mid.swing.indexes[i]
    n <- mid.swing.indexes[(i+1)]
    # Compute jerk cost of each cycle
    t.ms                <- A[, 1][m:n]
    # acceleration.x.ms.2 <- A[, 2][m:n]
    acceleration.y.ms.2 <- A[, 3][m:n]
    acceleration.z.ms.2 <- A[, 4][m:n]
    
    jerk.cost   <- CalculateJerkCost(t.ms / 1000, data.frame(acceleration.y.ms.2, acceleration.z.ms.2), normalized = T)
    jerk.costs  <- c(jerk.costs, jerk.cost)
  }
  
  return(jerk.costs)
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

ComputeCycleMeanMatrix <- function(M, col, mid.swing.indexes) {
  center <- Mode(diff(mid.swing.indexes))
  values <- c()
  j = 0
  for(i in 1:(length(mid.swing.indexes) - 1)) {
    m <- mid.swing.indexes[i]
    n <- mid.swing.indexes[(i+1)]
    values.temp <- M[, col][m:n]
    if(center == length(values.temp)) {
      values <- c(values, values.temp)
      j = j + 1
    }
  }
  M <- matrix(values, center, j)
  
  return(M)
}

ComputeOptimalCutoffFrequency <- function(x, fn, N) {
  par(mfcol = c(1, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
  rsmes <- c()
  fcs <- seq(0.1, 20, 0.1)
  for (fc in fcs) {
    W <- fc/fn
    lp <- butter(N, W)
    model <- filtfilt(lp, x)
    rsmes <- c(rsmes, sqrt(sum((model - x)^2, na.rm = T) / length(x)))
  }
  rm(fc, W, lp, model)
  
  plot(fcs, rsmes, type = "l", xlab = "Filter cutoff frequencies (Hz)", ylab = "RMS deviation")
  m <- length(fcs) / 2
  n <- length(fcs)
  model <- lm(rsmes[m:n] ~ fcs[m:n])
  noises <- model$coefficients[1] + fcs * model$coefficients[2]
  lines(fcs, noises)
  signals <- rsmes - noises
  fc <- fcs[which.min(abs(noises[1:m] - 5 * signals[1:m]))]
  abline(v = fc)
  
  rm(m, n, model)
  
  return(fc)
}

saveData <- function(output.data, file.name, preprocessed.data.directory.path, activity.directory, user.directory, date.directory, activity.start) {
  
  # Create directory, if needed
  output.directory.path <- paste(processed.data.directory.path, activity.directory, user.directory, date.directory, sep="")
  if(!file.exists(substr(output.directory.path, 1, nchar(output.directory.path) - 1))) {
    dir.create(output.directory.path, recursive = TRUE)
  }
  
  # Write csv file
  output.file.path <- paste(output.directory.path, file.name, sep = "")
  op <- options(digits.secs=3)
  con <- file(output.file.path, 'w') 
  writeLines(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y-%m-%d"), con = con)
  writeLines(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%H:%M:%OS"), con = con)
  write.csv(output.data, file = con, row.names = FALSE)
  close(con)
  options(op) #reset options
  print(paste("Wrote:", output.file.path))
}

fcs <- c()
for (i in 1:nrow(fss.features)) {
  
  properties      <- fss.features[i, c(7:13)]
  activity.start  <- properties[, 2]
  measurement     <- properties[, 5]
  if(measurement == 1) {
    date.directory  <- paste(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y-%m-%d--%H-%M-%S"), "/", sep ="")
  }
  motion.data.path <- paste(preprocessed.data.directory.path, activity.directory, user.directory, date.directory, body.position, "-motion-data-", measurement,  ".csv", sep="")
  mid.swing.indexes.path <- paste(processed.data.directory.path, activity.directory, user.directory, date.directory, "mid-swing-indexes-", measurement,  ".csv", sep="")
  if(file.exists(motion.data.path)) {
    
    # Load motion data
    motion.data <- read.csv(motion.data.path)
    t.ms <- motion.data[, 1]
    
    # Compute FS and FN
    fs <- 512 # ComputeSamplingRate(t.ms)
    fn <- fs/2
    
    # Resample data
    M <- ResampleData(motion.data[, 2:7], fs, t.ms)
    
    # Check for mid swing indexes
    mid.swing.indexes <- c()
    if(file.exists(mid.swing.indexes.path)) {
      mid.swing.indexes <- read.csv(mid.swing.indexes.path, skip = 2)[, 1]
    }
    
    if(length(mid.swing.indexes) == 0) {
      
      # Plot resampled data
      value.range <- (200 * fs):(210 * fs)
      par(mfcol = c(1, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
      plot(M[, 1][value.range]/1000, M[, 5][value.range], type = "l", xlab = expression("Time ("~s~")"), ylab = expression("Angular Velocity ("~deg/s~")"))
      
      # Compute main freq
      main.freq <- ComputeMainFrequency(M[, 5], fs)
      
      # Filter (1nd level)
      fc <- main.freq * 4
      W <- fc/fn
      n <- 2
      lp.1 <- butter(n, W)
      f.1 <- filter(lp.1, M[, 5])
      lines(M[, 1][value.range]/1000, f.1[value.range], col = 2)
      
      # Filter (2nd level)
      fc <- main.freq * .5
      W <- fc/fn
      lp.2 <- butter(n, W)
      f.2 <- filter(lp.2, M[, 5])
      lines(M[, 1][value.range]/1000, f.2[value.range], col = 3)
      readline("Press return to continue > ")
      
      # Detect Midswings
      mid.swing.indexes <- DetectMidSwings(M[, 5], f.1, f.2)
      
      # Remove changes below 50 deg per second
      mid.swing.indexes <- mid.swing.indexes[M[, 5][mid.swing.indexes] > 50 | M[, 5][mid.swing.indexes] < -50]
      
      # Detect Annomaly
      temp.diff <- diff(M[, 1][mid.swing.indexes] / 1000)
      interval.t.s <- c(mean(temp.diff), temp.diff)
      angular.velocity.deg.s <- M[, 5][mid.swing.indexes] / 100
      annomaly <- DetectAnomaly(interval.t.s, angular.velocity.deg.s, expression("Cycle Interval ("~s~")"), expression("Angular Velocity (x"~10^2~deg/s~")"), c(min(interval.t.s), max(interval.t.s)), c(min(angular.velocity.deg.s), max(angular.velocity.deg.s)), epsilon = 0)
      
      if(length(annomaly$outliers) > 0) {
        mid.swing.indexes <- mid.swing.indexes[-annomaly$outliers]
      }
    }
    
    # Check data
    #mid.swing.indexes <- CheckMidSwingDetection(M[, 1], M[, 5], length.s, mid.swing.indexes)
    
    # Save Midswing indexes
    #saveData(mid.swing.indexes, paste("mid-swing-indexes-", measurement, ".csv", sep = ""), preprocessed.data.directory.path, activity.directory, user.directory, date.directory, activity.start)
    
    # Smooth acceleration data
    A <- M[, 1:4]
    fn <- fs/2
    n <- 4
    
    fc <- 1.75 # ComputeOptimalCutoffFrequency(A[, 2], fn, n)
    print(paste("Optimal Cutoff Frequency:", fc))
    
    fcs <- c(fcs, fc)

    W <- fc/fn
    lp <- butter(n, W)
    lp.freg <- freqz(lp, Fs = fs)
    #plot(lp.freg$f, abs(lp.freg$h), type = "l", xlim = c(0, 30), xlab = "Frequency (Hz)", ylab = "Magnitude Response")
    A[, 2] <- filtfilt(lp, A[, 2])

    par(mfcol = c(3, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
    mean.x <- ComputeCycleMeanMatrix(A, 2, mid.swing.indexes)
    x.p <- 1:length(mean.x[, 1]) / length(mean.x[, 1]) * 100
    plot(x.p,  mean.x[, 1], type = "l", xlab = expression("Time ( % Stride )"), ylab = expression("Accelerations X ("~m/s^2~")"))
    for (j in 2:length(mean.x[1, ])) {
      lines(x.p, mean.x[,j], type = "l")
    }
    mean.a.x <- rowMeans(mean.x)
    plot(x.p, mean.a.x, type = "l", xlab = expression("Time ( % Stride )"), ylab = expression("Mean Acceleration X ("~m/s^2~")"))
    mean.j.x <- c(NA, CalculateJerk(x.p, mean.a.x))
    plot(x.p, mean.j.x, type = "l", xlab = expression("Time ( % Stride )"), ylab = expression("Mean Jerk X ("~m/s^3~")"))
    
    fc <- 2.5 # ComputeOptimalCutoffFrequency(A[, 3], fn, n)
    print(paste("Optimal Cutoff Frequency:", fc))
    
    fcs <- c(fcs, fc)
    
    W <- fc/fn
    lp <- butter(n, W)
    lp.freg <- freqz(lp, Fs = fs)
    #plot(lp.freg$f, abs(lp.freg$h), type = "l", xlim = c(0, 30), xlab = "Frequency (Hz)", ylab = "Magnitude Response")
    A[, 3] <- filtfilt(lp, A[, 3])
    
    par(mfcol = c(3, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
    mean.x <- ComputeCycleMeanMatrix(A, 3, mid.swing.indexes)
    x.p <- 1:length(mean.x[, 1]) / length(mean.x[, 1]) * 100
    plot(x.p,  mean.x[, 1], type = "l", xlab = expression("Time ( % Stride )"), ylab = expression("Accelerations Y ("~m/s^2~")"))
    for (j in 2:length(mean.x[1, ])) {
      lines(x.p, mean.x[,j], type = "l")
    }
    mean.a.x <- rowMeans(mean.x)
    plot(x.p, mean.a.x, type = "l", xlab = expression("Time ( % Stride )"), ylab = expression("Mean Acceleration Y ("~m/s^2~")"))
    mean.j.x <- c(NA, CalculateJerk(x.p, mean.a.x))
    plot(x.p, mean.j.x, type = "l", xlab = expression("Time ( % Stride )"), ylab = expression("Mean Jerk Y ("~m/s^3~")"))
    
    fc <- 6 # ComputeOptimalCutoffFrequency(A[, 4], fn, n)
    print(paste("Optimal Cutoff Frequency:", fc))
    
    fcs <- c(fcs, fc)
    
    W <- fc/fn
    lp <- butter(n, W)
    lp.freg <- freqz(lp, Fs = fs)
    #plot(lp.freg$f, abs(lp.freg$h), type = "l", xlim = c(0, 30), xlab = "Frequency (Hz)", ylab = "Magnitude Response")
    A[, 4] <- filtfilt(lp, A[, 4])
    
    par(mfcol = c(3, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
    mean.x <- ComputeCycleMeanMatrix(A, 4, mid.swing.indexes)
    x.p <- 1:length(mean.x[, 1]) / length(mean.x[, 1]) * 100
    plot(x.p,  mean.x[, 1], type = "l", xlab = expression("Time ( % Stride )"), ylab = expression("Accelerations Z ("~m/s^2~")"))
    for (j in 2:length(mean.x[1, ])) {
      lines(x.p, mean.x[,j], type = "l")
    }
    mean.a.x <- rowMeans(mean.x)
    plot(x.p, mean.a.x, type = "l", xlab = expression("Time ( % Stride )"), ylab = expression("Mean Acceleration Z ("~m/s^2~")"))
    mean.j.x <- c(NA, CalculateJerk(x.p, mean.a.x))
    plot(x.p, mean.j.x, type = "l", xlab = expression("Time ( % Stride )"), ylab = expression("Mean Jerk Z ("~m/s^3~")"))
       
    # Compute output data
    cycle.jerk.costs <- ComputeCycleJerkCosts(A, mid.swing.indexes)
    cycle.intervals <- diff(M[, 1][mid.swing.indexes] / 1000)
    output.data <- data.frame(t.s = M[, 1][mid.swing.indexes[2:length(mid.swing.indexes)]] / 1000, cycle.interval.s = cycle.intervals, jerk.cost.m2s5 = cycle.jerk.costs)
    output.data <- output.data[output.data[, 2] < 1.5, ]

    # Compute mean to compare
    jerk.cost.by.all.accelerations <- CalculateJerkCost(A[, 1] / 1000, A[, 3:4], normalized = T)
    jerk.cost.by.cycle.mean <- mean(output.data[, 3], na.rm = T)
    print(paste("Jerk Cost by Cycle:            ", jerk.cost.by.cycle.mean / 10^4))
    print(paste("Jerk Cost by all Accelerations:", jerk.cost.by.all.accelerations / 10^4))
    
    # Save data
    saveData(output.data, paste(body.position, "-jerk-cost-data-", measurement, ".csv", sep = ""), preprocessed.data.directory.path, activity.directory, user.directory, date.directory, activity.start)

    # readline("Press return to continue > ")
    
  } else {
    print(paste("File not exits:", motion.data.path))
  }
}

# fcs[fcs == 10] <- NA
# rowMeans(matrix(fcs, 3, 72/3), na.rm = T)