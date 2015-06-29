# Remove all variables
rm(list = ls(all = T)) 

# Load libraries
require(signal)
require(flow)

# Set root data directory path
root.data.directory.path <- ""
if(file.exists("C:/Users/Simon Bogutzky/Documents/flow/data"))
  root.data.directory.path <- "C:/Users/Simon Bogutzky/Documents/flow/data/"
if(file.exists("/Volumes/flow/Documents/simon-bogutzky/data"))
  root.data.directory.path <- "/Volumes/flow/Documents/simon-bogutzky/data/"
if(file.exists("//gangstore.ddns.net/flow/Documents/simon-bogutzky/data"))
  root.data.directory.path <- "//gangstore.ddns.net/flow/Documents/simon-bogutzky/data/"

# Set preprocessed data directory path
preprocessed.data.directory.path <- "./data/preprocessed-data/"

# Set processed data directory path
processed.data.directory.path <- paste(root.data.directory.path, "processed-data/", sep = "")

# Set features directory path
features.directory.path <- paste(root.data.directory.path, "features/", sep = "")

# Load fss features
fss.features <- read.csv(paste(features.directory.path, "fss-features.csv", sep = ""), stringsAsFactors = F)

# Set body position
body.position       <- "leg"

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
    
    # Number of data row
    n <- nrow(motion.data)
  
    # Load motion time data
    motion.time.data.path <- paste(processed.data.directory.path, tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/", date.directory, body.position, "-motion-time-data-", measurement, ".csv", sep="")
    motion.time.data      <- read.csv(motion.time.data.path, skip = 2)
    
    fs <- n / ((motion.data[n, 1] - motion.data[1, 1]) / 1000) 
    # Compute jerk cost per cycle
    # Isolate gravity from acceleration 
    butterworth.filter  <- butter(1, 1/(fs/2) * 0.8, "low")
    gravity.x           <- filtfilt(butterworth.filter, motion.data[, 2])
    gravity.y           <- filtfilt(butterworth.filter, motion.data[, 3])
    gravity.z           <- filtfilt(butterworth.filter, motion.data[, 4])
  
    # Compute linear acceleration
    linear.acceleration.x <- motion.data[, 2] - gravity.x
    linear.acceleration.y <- motion.data[, 3] - gravity.y
    linear.acceleration.z <- motion.data[, 4] - gravity.z
  
    if (nrow(motion.time.data) > 1) {
        jerk.costs    <- c()
        for(i in 1:(nrow(motion.time.data) - 1)) {
      
        in.cycle <- motion.data[,1] / 1000 >= motion.time.data[,1][i] & motion.data[,1] / 1000 <= motion.time.data[,1][i+1]
      
        # Compute jerk cost of each cycle
        t.subset                      <- motion.data[, 1][in.cycle]
        linear.acceleration.x.subset  <- linear.acceleration.x[in.cycle]
        linear.acceleration.y.subset  <- linear.acceleration.y[in.cycle]
        linear.acceleration.z.subset  <- linear.acceleration.z[in.cycle]
      
        jerk.cost   <- CalculateJerkCost(t.subset / 1000, linear.acceleration.x.subset, linear.acceleration.y.subset, linear.acceleration.z.subset, normalized = T, plot = F)
        jerk.costs  <- c(jerk.costs, jerk.cost)
      }
    } else {
      print("Has no cycle indicators")
    }
  
    if (length(jerk.costs) > 0) {
      jerk.costs <- c(NA, jerk.costs)
      print(summary(jerk.costs))
      par(mfcol=c(2, 1))
      hist(jerk.costs)
      t.s <- motion.time.data[,1]
      plot(t.s, jerk.costs, type="l", xlab="t [s]", ylab="Jerk Cost [m^2 s^-5]")
    
      # Create directory, if needed
      output.directory.path <- paste(processed.data.directory.path, tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/", date.directory, sep="")
      if(!file.exists(output.directory.path)) {
        dir.create(output.directory.path, recursive = TRUE)
      }
    
      # Write csv file
      output.file.path <- paste(output.directory.path, body.position, "-motion-jerk-cost-data-", measurement, ".csv", sep = "")
      op <- options(digits.secs=3)
      con <- file(output.file.path, 'w') 
      writeLines(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y-%m-%d"), con = con)
      writeLines(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%H:%M:%OS"), con = con)
      write.csv(data.frame(t.s, jerk.cost.m.2.s..5 = jerk.costs), file = con, row.names = FALSE)
      close(con)
      options(op) #reset options
      print(paste("Wrote:", output.file.path))
    }
  }
}
