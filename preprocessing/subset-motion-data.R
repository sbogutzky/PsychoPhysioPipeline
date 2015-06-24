# Remove all variables
rm(list = ls(all = T))  

# Set working directory
setwd("~/Entwicklung/bogutzky/repositories/non-disruptive-flow-measures/preprocessing")

# Load libraries
library(signal)

# Set paths
root.data.path    <- "../data/cleaned-data/"

# How many minutes before the activity end the measurement started?
measurement.started.before  <- 5

# Where do you measure the data?
body.position <- "leg"

# # Which sampling rate do you wish?
# sampling.rate  <- 64

# Load sampling.rates features
fss.features        <- read.csv("../data/features/fss-features.csv", stringsAsFactors = F)

for (i in 1:nrow(fss.features)) {
  
  properties      <- fss.features[i, c(6:13)]
  activity        <- properties[, 1]
  activity.start  <- as.POSIXct(properties[, 2])
  activity.end    <- as.POSIXct(properties[, 3])
  measurement     <- properties[, 5]
  last.name       <- properties[, 6]
  first.name      <- properties[, 7]
  date.of.birth   <- properties[, 8]
  
  if(measurement == 1) {
    motion.data        <- data.frame()
    
    # Read data, if needed
    motion.data.path <- paste(root.data.path, tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/", format(activity.start, format="%Y-%m-%d--%H-%M-%S", tz="CET"), "/", body.position, "-motion-data.csv", sep="")
    if(file.exists(motion.data.path)) {
      motion.data      <- read.csv(motion.data.path)
      
      # Set motion data timestamp to zero 
      motion.data[,1] <- (motion.data[,1] - motion.data[1,1]) / 1000
      
      # Correct motion data system time
      motion.data[,8] <- motion.data[1,8] / 1000 + motion.data[,1] 
      
      # Create directory, if needed
      output.directory <- paste("../data/preprocessed-data/", tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/", format(activity.start, format="%Y-%m-%d--%H-%M-%S", tz="CET"), "/", sep="")
      if(!file.exists(output.directory)) {
        dir.create(output.directory, recursive = TRUE)
      }
      
      par(mfcol=c(2, 1))
      plot(motion.data[,1], motion.data[,5], type = "l", xlab = "t [s]", ylab = "Rotation Rate X [deg/s]")
      title(format(activity.start, format="%Y-%m-%d--%H-%M-%S", tz="CET"))
      plot(motion.data[motion.data[,1] > 1800 & motion.data[,1] < 1810,1], motion.data[motion.data[,1] > 1800 & motion.data[,1] < 1810,5], type = "l", xlab = "t [s]", ylab = "Rotation Rate X [deg/s]")
      
    } else {
      print("No motion data")
    }
  }
  
  # Subset motion data
  if(nrow(motion.data) > 0) {
    
    start.time         <- activity.end - measurement.started.before * 60
    motion.data.subset <- motion.data[start.time <= as.POSIXct(motion.data[,8], origin = "1970-01-01") & as.POSIXct(motion.data[,8], origin = "1970-01-01") <= activity.end,]
    time.difference    <- as.POSIXct(motion.data.subset[1,8], origin = "1970-01-01") - (activity.end - measurement.started.before * 60)
    print(time.difference)
    print(as.POSIXct(motion.data.subset[nrow(motion.data.subset),8], origin = "1970-01-01") - as.POSIXct(motion.data.subset[1,8], origin = "1970-01-01") + time.difference)
    
    # Set subset timestamp to zero
    motion.data.subset[,1] <- motion.data.subset[,1] - motion.data.subset[1, 1]
    
#     # Interpolate
#     t                       <- seq(motion.data.subset[1, 1], motion.data.subset[nrow(motion.data.subset), 1], by = 1/sampling.rate)
#     motion.acceleration.x   <- interp1(motion.data.subset[,1], motion.data.subset[,2], t, method = "spline")
#     motion.acceleration.y   <- interp1(motion.data.subset[,1], motion.data.subset[,3], t, method = "spline")
#     motion.acceleration.z   <- interp1(motion.data.subset[,1], motion.data.subset[,4], t, method = "spline")
#     motion.rotation.rate.x  <- interp1(motion.data.subset[,1], motion.data.subset[,5], t, method = "spline")
#     motion.rotation.rate.y  <- interp1(motion.data.subset[,1], motion.data.subset[,6], t, method = "spline")
#     motion.rotation.rate.z  <- interp1(motion.data.subset[,1], motion.data.subset[,7], t, method = "spline")
#     t                       <- t + time.difference
    
    t                       <- motion.data.subset[,1] + time.difference
    motion.acceleration.x   <- motion.data.subset[,2]
    motion.acceleration.y   <- motion.data.subset[,3]
    motion.acceleration.z   <- motion.data.subset[,4]
    motion.rotation.rate.x  <- motion.data.subset[,5]
    motion.rotation.rate.y  <- motion.data.subset[,6]
    motion.rotation.rate.z  <- motion.data.subset[,7]
  
    # Write csv file
    output.file.path <- paste(output.directory, body.position, "-motion-data-", measurement, ".csv", sep = "")
    write.csv(data.frame(t, motion.acceleration.x, motion.acceleration.y, motion.acceleration.z, motion.rotation.rate.x, motion.rotation.rate.y, motion.rotation.rate.z), output.file.path, row.names = FALSE)
    print(output.file.path)
  }
}