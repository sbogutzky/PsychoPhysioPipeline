# Remove all variables
rm(list = ls(all = T))  

# Set working directory
setwd("~/Entwicklung/bogutzky/repositories/non-disruptive-flow-measures/preprocessing")

# Load libraries
library(signal)

# Set properties
feature.path      <- "../data/features/fss-features.csv"
root.data.path    <- "../data/cleaned-data/"
file.name.prefix  <- "leg"
subset.length     <- 5 # in minutes
fs                <- 64

# Read feature data
fss.features    <- read.csv(feature.path)
fss.features    <- fss.features[order(as.Date(fss.features$activity.start, format = "%Y-%m-%d %H:%M:%S")), , drop = FALSE]

for (i in 1:nrow(fss.features)) {
  activity        <- as.character(fss.features[i, 11])
  activity.start  <- as.POSIXct(fss.features[i, 12])
  activity.end    <- as.POSIXct(fss.features[i, 13])
  last.name       <- as.character(fss.features[i, 16])
  first.name      <- as.character(fss.features[i, 17])
  measurement     <- fss.features[i, 15]
  
  if(measurement == 1) {
    motion.data        <- data.frame()
    
    # Read data, if needed
    motion.data.path <- paste(root.data.path, tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/", format(activity.start, format="%Y-%m-%d--%H-%M-%S", tz="CET"), "/", file.name.prefix, "-data.csv", sep="")
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
      
    } else {
      print("No motion data")
    }
  }
  
  # Subset motion data
  if(nrow(motion.data) > 0) {
    
    start.time         <- activity.end - 5 * 60
    motion.data.subset <- motion.data[start.time <= as.POSIXct(motion.data[,8], origin = "1970-01-01") & as.POSIXct(motion.data[,8], origin = "1970-01-01") <= activity.end,]
    time.difference    <- as.POSIXct(motion.data.subset[1,8], origin = "1970-01-01") - (activity.end - 5 * 60)
    print(time.difference)
    print(as.POSIXct(motion.data.subset[nrow(motion.data.subset),8], origin = "1970-01-01") - as.POSIXct(motion.data.subset[1,8], origin = "1970-01-01") + time.difference)
    
    # Set subset timestamp to zero
    motion.data.subset[,1] <- motion.data.subset[,1] - motion.data.subset[1, 1]
    
    # Interpolate
    t                       <- seq(motion.data.subset[1, 1], motion.data.subset[nrow(motion.data.subset), 1], by = 1/fs)
    motion.acceleration.x   <- interp1(motion.data.subset[,1], motion.data.subset[,2], t, method = "spline")
    motion.acceleration.y   <- interp1(motion.data.subset[,1], motion.data.subset[,3], t, method = "spline")
    motion.acceleration.z   <- interp1(motion.data.subset[,1], motion.data.subset[,4], t, method = "spline")
    motion.rotation.rate.x  <- interp1(motion.data.subset[,1], motion.data.subset[,5], t, method = "spline")
    motion.rotation.rate.y  <- interp1(motion.data.subset[,1], motion.data.subset[,6], t, method = "spline")
    motion.rotation.rate.z  <- interp1(motion.data.subset[,1], motion.data.subset[,7], t, method = "spline")
    t                       <- t + time.difference
  
    # Write csv file
    output.file.path <- paste(output.directory, file.name.prefix, "-data-", measurement, ".csv", sep = "")
    write.csv(data.frame(t, motion.acceleration.x, motion.acceleration.y, motion.acceleration.z, motion.rotation.rate.x, motion.rotation.rate.y, motion.rotation.rate.z), output.file.path, row.names = FALSE)
    print(output.file.path)
  }
}