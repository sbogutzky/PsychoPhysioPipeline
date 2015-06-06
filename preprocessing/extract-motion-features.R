# Remove all variables
rm(list = ls(all = T))  

# Set working directory
setwd("~/Entwicklung/bogutzky/repositories/non-disruptive-flow-measures/preprocessing")

# Set paths
feature.path            <- "../data/features/"
preprocessed.data.path  <- "../data/preprocessed-data/"

# How many minutes before the activity end the measurement started?
measurement.started.before  <- 5

# How many minutes before the activity end the measurement ended?
measurement.ended.before  <- 0

# Where do you measure the data?
body.position <- "leg"

# Load fss features
fss.features        <- read.csv("../data/features/fss-features.csv", stringsAsFactors = F)

# Create feature data frame as result
motion.features  <- data.frame()

for (i in 1:nrow(fss.features)) {
  properties      <- fss.features[i, c(6:13)]
  activity        <- properties[, 1]
  activity.start  <- properties[, 2]
  activity.end    <- properties[, 3]
  measurement     <- properties[, 5]
  last.name       <- properties[, 6]
  first.name      <- properties[, 7]
  date.of.birth   <- properties[, 8]
  
  if(measurement == 1) {
    current.data.path   <- paste(preprocessed.data.path, tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/", strftime(activity.start, format = "%Y-%m-%d--%H-%M-%S"), "/",  sep = "")
  }
  current.file.name.1   <- paste(body.position, "-motion-time-data-", measurement, ".csv", sep = "")
  current.file.path.1   <- paste(current.data.path, current.file.name.1, sep = "")
  current.file.name.2   <- paste(body.position, "-motion-jerk-cost-data-", measurement, ".csv", sep = "")
  current.file.path.2   <- paste(current.data.path, current.file.name.2, sep = "")
  
  if(file.exists(current.file.path.1) & file.exists(current.file.path.1)) {
    
    motion.time.data        <- read.csv(current.file.path.1)
    motion.jerk.cost.data   <- read.csv(current.file.path.2)
    
    mean.jc             <- round(mean(motion.jerk.cost.data$jerk.costs, na.rm = T), 2)
    mean.cycle.interval <- round(mean(motion.time.data$cycle.interval, na.rm = T), 3)
    
    # Compute start and end of the measurement
    measurement.start <- strftime(as.POSIXct(activity.end) - measurement.started.before * 60, format = "%Y-%m-%d %H:%M:%S")
    measurement.end   <- strftime(as.POSIXct(activity.end) - measurement.started.before * 60, format = "%Y-%m-%d %H:%M:%S")
    
    # Add parameter to feature vector
    motion.features    <- rbind(motion.features, data.frame(mean.jc, mean.cycle.interval, activity, activity.start, activity.end, measurement.start, measurement.end, measurement, last.name, first.name, date.of.birth))
  }
}

# Write to csv file
if(file.exists(paste("../data/features/", body.position, "-motion-features.csv", sep = ""))) {
  features <- read.csv(paste("../data/features/", body.position, "-motion-features.csv", sep = ""), stringsAsFactors = F)
  write.csv(rbind(features, motion.features), paste("../data/features/", body.position, "-motion-features.csv", sep = ""), row.names = F)
} else {
  write.csv(motion.features, paste("../data/features/", body.position, "-motion-features.csv", sep = ""), row.names = F)
}