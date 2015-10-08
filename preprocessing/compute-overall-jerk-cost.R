# Remove all variables
rm(list = ls(all = T)) 

# Load libraries
require(flow)
require(signal)

# Set root data directory path
root.data.directory.path <- ""
if(file.exists("C:/Users/Simon Bogutzky/Documents/flow/data"))
  root.data.directory.path <- "C:/Users/Simon Bogutzky/Documents/flow/data/"
if(file.exists("/Volumes/flow/Documents/archiv/daten/2015/flow-gehen-und-laufen"))
  root.data.directory.path <- "/Volumes/flow/Documents/archiv/daten/2015/flow-gehen-und-laufen/"
if(file.exists("//gangstore.ddns.net/flow/Documents/archiv/daten/2015/flow-gehen-und-laufen"))
  root.data.directory.path <- "//gangstore.ddns.net/flow/Documents/archiv/daten/2015/flow-gehen-und-laufen/"

# Set preprocessed data directory path
preprocessed.data.directory.path <- "./data/preprocessed-data/"

# Set processed data directory path
processed.data.directory.path <- paste(root.data.directory.path, "processed-data/", sep = "")

# Set features directory path
features.directory.path <- paste(root.data.directory.path, "features/", sep = "")

# Load fss features
fss.features <- read.csv(paste(features.directory.path, "fss-features-grueter.csv", sep = ""), stringsAsFactors = F)

# Set body position
body.position       <- "leg"

# Set measures in ms
ranges <- seq(0,900000,300000)

total.jerk.cost         <- c()
jerk.cost.by.cycle.mean <- c()
activity.starts         <- c()

detectMidSwings <- function(motion.data) {
  
  # Number of data row
  n <- nrow(motion.data)
  
  # Find cyclic movement   
  rotation.rate.x.spectrum <- TSA::periodogram(motion.data[, 5], plot = F)
  rotation.rate.y.spectrum <- TSA::periodogram(motion.data[, 6], plot = F)
  rotation.rate.z.spectrum <- TSA::periodogram(motion.data[, 7], plot = F)
  
  axis <- which.max(c(max(rotation.rate.x.spectrum[[2]]), max(rotation.rate.y.spectrum[[2]]), max(rotation.rate.z.spectrum[[2]])))
  
  # Upsampling
  fs            <- 2000
  t             <- seq(motion.data[1, 1], motion.data[n, 1], by = 1000/fs)
  rotation.rate <- interp1(motion.data[, 1], motion.data[, axis + 4], t, method = "spline")
  
  # Determine filter frequency
  periodogram   <- TSA::periodogram(rotation.rate, plot = F)
  freqs         <- periodogram[[1]] * fs
  specs         <- periodogram[[2]]
  index         <- which.max(specs)
  main.freq     <- freqs[index]
  filt.freq     <- ceiling(main.freq)
  rm(periodogram, freqs, specs, index)
  
  # Low pass signal
  low.pass.filter         <- butter(3, 1/(fs/2) * filt.freq, "low")
  filtered.rotation.rate  <- filtfilt(low.pass.filter, rotation.rate)
  
  # Search for minima
  minima <- SearchExtrema(filtered.rotation.rate, which = "minima")
  t.ms               <- t[minima]
  return(t.ms)
}

for (i in 1:nrow(fss.features)) {
  
  properties      <- fss.features[i, c(6:12)]
  activity        <- properties[, 1]
  activity.start  <- properties[, 2]
  measurement     <- properties[, 5]
  last.name       <- properties[, 6]
  first.name      <- properties[, 7]
  if(measurement == 1) {
    date.directory  <- paste(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y-%m-%d--%H-%M-%S"), "/", sep ="")
  }
  
  # Read motion data
  motion.data.path <- paste(preprocessed.data.directory.path, tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/", date.directory, body.position, "-motion-data-", measurement,  ".csv", sep="")
  if(file.exists(motion.data.path)) {
  
    # Load motion data
    motion.data <- read.csv(motion.data.path)
    
    # Isolate gravity from acceleration 
    butterworth.filter              <- butter(1, .2, "high")
    motion.data$motion.accel.x.ms.2 <- filtfilt(butterworth.filter, motion.data$motion.accel.x.ms.2)
    motion.data$motion.accel.y.ms.2 <- filtfilt(butterworth.filter, motion.data$motion.accel.y.ms.2)
    motion.data$motion.accel.z.ms.2 <- filtfilt(butterworth.filter, motion.data$motion.accel.z.ms.2)
    
    for(k in 1:(length(ranges)-1)) {
      
      # Subset motion data in measure ranges
      motion.data.subset  <- motion.data[ranges[k] < motion.data$t.ms & motion.data$t.ms <= ranges[k+1],]
      
      # Detect Midswings
      mid.swings <- detectMidSwings(motion.data.subset)
      #print(mean(diff(mid.swings)))
      
      jerk.costs <- c()
      for(l in 1:(length(mid.swings) - 1)) {
        
        # Compute jerk cost of each cycle
        in.cycle                    <- mid.swings[l] <= motion.data.subset$t.ms & motion.data.subset$t.ms <= mid.swings[l + 1]
        t.ms.subset                 <- motion.data.subset$t.ms[in.cycle]
        motion.accel.x.ms.2.subset  <- motion.data.subset$motion.accel.x.ms.2[in.cycle]
        motion.accel.y.ms.2.subset  <- motion.data.subset$motion.accel.y.ms.2[in.cycle]
        motion.accel.z.ms.2.subset  <- motion.data.subset$motion.accel.z.ms.2[in.cycle]
        
        jerk.cost   <- CalculateJerkCost(t.ms.subset / 1000, motion.accel.x.ms.2.subset, motion.accel.y.ms.2.subset, motion.accel.z.ms.2.subset, normalized = T, plot = F)
        jerk.costs  <- c(jerk.costs, jerk.cost)
      }
      
      # Compute total jerk cost
      jerk.cost.by.cycle.mean <- c(jerk.cost.by.cycle.mean, mean(jerk.costs, na.rm = T))
      motion.data.subset.new  <- motion.data.subset[mid.swings[1] <= motion.data.subset$t.ms & motion.data.subset$t.ms <= mid.swings[length(mid.swings)], ]
      activity.starts         <- c(activity.starts, activity.start)
      total.jerk.cost         <- c(total.jerk.cost, CalculateJerkCost(motion.data.subset.new$t.ms / 1000, motion.data.subset.new$motion.accel.x.ms.2, motion.data.subset.new$motion.accel.y.ms.2, motion.data.subset.new$motion.accel.z.ms.2, normalized = T, plot = F))
    }
  } else {
    print("No Motion data")
  }
}

par(mfrow = c(3,1), mgp = c(2, 1, 0)) 

plot(total.jerk.cost, col = c(1,2,3))
abline(v = c(12,24,36,48,60,72,84))

plot(jerk.cost.by.cycle.mean, col = c(1,2,3))
abline(v = c(12,24,36,48,60,72,84))
sample = rep(c(1,2,3), length(total.jerk.cost)/3)

boxplot(total.jerk.cost ~ sample)

write.csv(data.frame(activity.start = activity.starts, total.jerk.cost, sample), "jerk-cost.csv")