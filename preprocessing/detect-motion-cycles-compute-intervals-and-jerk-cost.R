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

# Read in subset size in seconds
subset.size         <- as.numeric(readline("Type in subset size in seconds for visual control and press return to continue (e. g. 30) > "))

# Load fss features
fss.features        <- read.csv(paste(features.directory.path, activity.directory, user.directory, "fss-features.csv", sep = ""), stringsAsFactors = F)

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


for (i in 1:nrow(fss.features)) {
  properties      <- fss.features[i, c(6:12)]
  activity.start  <- properties[, 2]
  measurement     <- properties[, 5]
  if(measurement == 1) {
    date.directory  <- paste(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y-%m-%d--%H-%M-%S"), "/", sep ="")
  }
  
  # Read motion data
  motion.data.path <- paste(preprocessed.data.directory.path, activity.directory, user.directory, date.directory, body.position, "-motion-data-", measurement,  ".csv", sep="")
  if(file.exists(motion.data.path)) {
  
    # Load motion data
    motion.data <- read.csv(motion.data.path)
    n           <- nrow(motion.data)
    
    # Upsampling
    fs <- 2000
    x <- seq(motion.data[1, 1], motion.data[n, 1], by = 1000/fs)
    y <- signal::interp1(motion.data[, 1], motion.data[, 5], x, method = "spline")
    
    # Add minima original data
    minima    <- SearchExtrema(y, which = "minima")
    minima.f  <- rep(0, length(minima))
    
    # Determine filter frequency
    periodogram <- TSA::periodogram(y, plot = F)
    freqs       <- periodogram[[1]] * fs
    specs       <- periodogram[[2]]
    index       <- which.max(specs)
    main.freq   <- freqs[index]
    filt.freq   <- ceiling(main.freq)
    rm(periodogram, freqs, specs, index)
    
    # Low pass signal
    lp  <- butter(1, 1/(fs/2) * filt.freq, "low")
    y.1 <- filter(lp, y)
    
    # Add minima from the filtered signal (1st level)
    minima.1  <- SearchExtrema(y.1, which = "minima")
    minima    <- c(minima, minima.1)
    minima.f  <- c(minima.f, rep(1, length(minima.1)))
    rm(minima.1)
    
    # Low pass signal
    lp  <- butter(1, 1/(fs/2) * (filt.freq / 10), "low")
    y.2 <- filter(lp, y)
    
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
    
    # Get time and value for mid swing
    mid.swing.x <- x[mid.swing.index]
    mid.swing.y <- y[mid.swing.index]
    
    # Remove changes below 10 deg per second
    mid.swing.x <- mid.swing.x[mid.swing.y > 10 | mid.swing.y < -10]
    mid.swing.y <- mid.swing.y[mid.swing.y > 10 | mid.swing.y < -10]
    
    diff.x <- c(0, diff(mid.swing.x / 1000))
    
    da <- DetectAnnomalies(diff.x, mid.swing.y/400, expression("Cycle Interval ("~s~")"), expression("Angular Velocity ("~rad/s~")"), c(min(diff.x), max(diff.x)), c(min(mid.swing.y/400), max(mid.swing.y/400)), epsilon = 0)
    # Remove selected mid swings
    if(length(da$outliers) > 0) {
      mid.swing.x <- mid.swing.x[-da$outliers]
      mid.swing.y <- mid.swing.y[-da$outliers]
    }
    
    # Visual control
    k       <- 0
    while(k < max(motion.data[, 1])) {
      plot(motion.data[, 1][motion.data[, 1] >= k & motion.data[, 1] < k + subset.size * 1000] / 1000, motion.data[, 5][motion.data[, 1] >= k & motion.data[, 1] < k + subset.size * 1000], type = "l", xlab = "t [s]", ylab = "Rotation Rate X [deg/s]")
      points(mid.swing.x[mid.swing.x >= k & mid.swing.x < k + subset.size * 1000] / 1000, mid.swing.y[mid.swing.x >= k &  mid.swing.x < k + subset.size * 1000], pch = 21, bg = "red")
      abline(h = mean(mid.swing.y) + 4 * sd(mid.swing.y), lty = "dashed", col = "darkgrey")
      title("Select to remove")
      
      remove  <- identify(mid.swing.x / 1000, mid.swing.y)
      
      # Remove selected mid swings
      if(length(remove) > 0) {
        mid.swing.x <- mid.swing.x[-remove]
        mid.swing.y <- mid.swing.y[-remove]
      }
      
      plot(motion.data[, 1][motion.data[, 1] >= k & motion.data[, 1] < k + subset.size * 1000] / 1000, motion.data[, 5][motion.data[, 1] >= k & motion.data[, 1] < k + subset.size * 1000], type = "l", xlab = "t [s]", ylab = "Rotation Rate X [deg/s]")
      points(mid.swing.x[mid.swing.x >= k & mid.swing.x < k + subset.size * 1000] / 1000, mid.swing.y[mid.swing.x >= k &  mid.swing.x < k + subset.size * 1000], pch = 21, bg = "red")
      abline(h = mean(mid.swing.y) + 4 * sd(mid.swing.y), lty = "dashed", col = "darkgrey")
      title("Select to add")
      
      # Add mid swings and control
      add  <- identify(motion.data[, 1] / 1000, motion.data[, 5])
      if(length(add) > 0) {
        mid.swing.x <- c(mid.swing.x, motion.data[add, 1])
        mid.swing.y <- c(mid.swing.y, motion.data[add, 5])
        
        mid.swings <- data.frame(t.ms = mid.swing.x, rotation.rate.x.deg.s = mid.swing.y)[order(mid.swing.x),]
        
        plot(motion.data[, 1][motion.data[, 1] >= k & motion.data[, 1] < k + subset.size * 1000] / 1000, motion.data[, 5][motion.data[, 1] >= k & motion.data[, 1] < k + subset.size * 1000], type = "l", xlab = "t [s]", ylab = "Rotation Rate X [deg/s]")
        points(mid.swings[, 1][mid.swings[, 1] >= k & mid.swings[, 1] < k + subset.size * 1000] / 1000, mid.swings[, 2][mid.swings[, 1] >= k & mid.swings[, 1] < k + subset.size * 1000], pch = 21, bg = "red")
        abline(h = mean(mid.swing.y) + 4 * sd(mid.swing.y), lty = "dashed", col = "darkgrey")
        
        readline("Press return to continue > ")
      }
      
      k       <- k + subset.size * 1000
    }
    
    mid.swings <- data.frame(t.ms = mid.swing.x, rotation.rate.x.deg.s = mid.swing.y)[order(mid.swing.x),]
    
    plot(motion.data[, 1] / 1000, motion.data[, 5], type = "l", xlab = "t [s]", ylab = "Rotation Rate X [deg/s]")
    points(mid.swings[, 1] / 1000, mid.swings[, 2], pch = 21, bg = "red")
    
    # Isolate gravity from acceleration 
    butterworth.filter              <- butter(1, .2, "high")
    motion.data$motion.accel.x.ms.2 <- filtfilt(butterworth.filter, motion.data$motion.accel.x.ms.2)
    motion.data$motion.accel.y.ms.2 <- filtfilt(butterworth.filter, motion.data$motion.accel.y.ms.2)
    motion.data$motion.accel.z.ms.2 <- filtfilt(butterworth.filter, motion.data$motion.accel.z.ms.2)
        
    jerk.costs <- c()
    for(l in 1:(nrow(mid.swings) - 1)) {
          
        # Compute jerk cost of each cycle
        in.cycle                    <- mid.swings[l, 1] <= motion.data$t.ms & motion.data$t.ms < mid.swings[l + 1, 1]
        t.ms.subset                 <- motion.data$t.ms[in.cycle]
        motion.accel.x.ms.2.subset  <- motion.data$motion.accel.x.ms.2[in.cycle]
        motion.accel.y.ms.2.subset  <- motion.data$motion.accel.y.ms.2[in.cycle]
        motion.accel.z.ms.2.subset  <- motion.data$motion.accel.z.ms.2[in.cycle]
          
        jerk.cost   <- CalculateJerkCost(t.ms.subset / 1000, motion.accel.x.ms.2.subset, motion.accel.y.ms.2.subset, motion.accel.z.ms.2.subset, normalized = T, plot = F)
        jerk.costs  <- c(jerk.costs, jerk.cost)
    }
    
    cycle.intervals <- diff(mid.swings[, 1] / 1000)
    
    result.data <- data.frame(t.s = mid.swings[(2:nrow(mid.swings)), 1] / 1000, cycle.interval.s = cycle.intervals, jerk.cost.m2s5 = jerk.costs)
    result.data <- result.data[result.data[,2] < 1.5, ]
    
    # Compute mean to compare
    jerk.cost.by.all.accelerations  <- CalculateJerkCost(motion.data$t.ms / 1000, motion.data$motion.accel.x.ms.2, motion.data$motion.accel.y.ms.2, motion.data$motion.accel.z.ms.2, normalized = T, plot = F)
    jerk.cost.by.cycle.mean         <- mean(result.data[, 3], na.rm = T)
    print(paste("Jerk Cost by Cycle:           :", jerk.cost.by.cycle.mean))
    print(paste("Jerk Cost by all Accelerations:", jerk.cost.by.all.accelerations))
    
    # Create directory, if needed
    output.directory.path <- paste(processed.data.directory.path, activity.directory, user.directory, date.directory, sep="")
    if(!file.exists(output.directory.path)) {
      dir.create(output.directory.path, recursive = TRUE)
    }

    # Write csv file
    output.file.path <- paste(output.directory.path, body.position, "-jerk-cost-data-", measurement, ".csv", sep = "")
    op <- options(digits.secs=3)
    con <- file(output.file.path, 'w') 
    writeLines(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y-%m-%d"), con = con)
    writeLines(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%H:%M:%OS"), con = con)
    write.csv(result.data, file = con, row.names = FALSE)
    close(con)
    options(op) #reset options
    print(paste("Wrote:", output.file.path))
    readline("Press return to continue > ")
    
  } else {
    print("No Motion data")
  }
}
