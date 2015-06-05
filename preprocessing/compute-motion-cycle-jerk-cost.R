# Remove all variables
rm(list = ls(all = T))  

# Set working directory
setwd("~/Entwicklung/bogutzky/repositories/non-disruptive-flow-measures/preprocessing")

# Load libraries
require(signal)

# Set properties
root.data.path          <- "../data/preprocessed-data/"
first.name              <- "Patrick"
last.name               <- "Buse"
activity                <- "Running"
file.name.prefix        <- "leg"
sampling.rate           <- 64

# Load all file names
data.path               <- paste(root.data.path, tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/",  sep = "")
motion.file.names       <- list.files(path = data.path, pattern = paste(file.name.prefix, "-motion-data-[1-9].csv", sep=""), recursive = T)

for (motion.file.name in motion.file.names) {
  
  # Load motion data
  motion.data           <- read.csv(paste(data.path, motion.file.name, sep=""))
  
  # Extract properties
  activity.start    <- as.POSIXct(strptime(regmatches(motion.file.name, regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}--[0-9]{2}-[0-9]{2}-[0-9]{2}", motion.file.name)), "%Y-%m-%d--%H-%M-%S"))
  measurement       <- substr(regmatches(motion.file.name, regexpr("[1-9]{1}.csv", motion.file.name)), 1, 1)
  
  # Create directory, if needed
  output.directory <- paste("../data/preprocessed-data/", tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/", format(activity.start, format="%Y-%m-%d--%H-%M-%S", tz="CET"), "/", sep="")
  if(!file.exists(output.directory)) {
    dir.create(output.directory, recursive = TRUE)
  }
  
  # Load motion time data
  motion.time.data  <- read.csv(paste(output.directory, file.name.prefix, "-motion-time-data-", measurement, ".csv", sep = ""))
  
  # Compute jerk cost per cycle
  # Isolate gravity from acceleration 
  butterworth.filter  <- butter(1, 1/sampling.rate * 0.8, "low")
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
      
      in.cycle <- motion.data[,1] >= motion.time.data[,1][i] & motion.data[,1] <= motion.time.data[,1][i+1]
      
      # Compute jerk cost of each cycle
      t.subset                      <- motion.data[, 1][in.cycle]
      linear.acceleration.x.subset  <- linear.acceleration.x[in.cycle]
      linear.acceleration.y.subset  <- linear.acceleration.y[in.cycle]
      linear.acceleration.z.subset  <- linear.acceleration.z[in.cycle]
      
      jerk.cost   <- CalculateJerkCost(t.subset, linear.acceleration.x.subset, linear.acceleration.y.subset, linear.acceleration.z.subset, normalized = T, plot = F)
      jerk.costs  <- c(jerk.costs, jerk.cost)
  
    }
  } else {
    print("Has no cycle indicators")
  }
  
  if (length(jerk.costs) > 0) {
    print(summary(jerk.costs))
    par(mfcol=c(2, 1))
    hist(jerk.costs)
    t <- motion.time.data[,1][-1]
    plot(t, jerk.costs, type="l", xlab="t [s]", ylab="Jerk Cost [m^2 s^-5]")
    
    # Extract properties
    activity.start    <- as.POSIXct(strptime(regmatches(motion.file.name, regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}--[0-9]{2}-[0-9]{2}-[0-9]{2}", motion.file.name)), "%Y-%m-%d--%H-%M-%S"))
    measurement       <- substr(regmatches(motion.file.name, regexpr("[1-9]{1}.csv", motion.file.name)), 1, 1)
    
    # Create directory, if needed
    output.directory <- paste("../data/preprocessed-data/", tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/", format(activity.start, format="%Y-%m-%d--%H-%M-%S", tz="CET"), "/", sep="")
    if(!file.exists(output.directory)) {
      dir.create(output.directory, recursive = TRUE)
    }
    
    # Write csv file
    output.file.path <- paste(output.directory, file.name.prefix, "-motion-jerk-cost-data-", measurement, ".csv", sep = "")
    write.csv(data.frame(t, jerk.costs), output.file.path, row.names = FALSE)
    print(output.file.path)
  }
}
