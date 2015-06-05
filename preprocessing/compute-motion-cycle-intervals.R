# Remove all variables
rm(list = ls(all = T))  

# Set working directory
setwd("~/Entwicklung/bogutzky/repositories/non-disruptive-flow-measures/preprocessing")

# Load libraries
require(TSA)
require(signal)
require(flow)

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
  
  # Find main frequency   
  par(mfcol=c(3, 1))
  rotation.rate.x.spectrum <- TSA::periodogram(motion.data[, 5], plot = T, main = "Rotation Rate X")
  rotation.rate.y.spectrum <- TSA::periodogram(motion.data[, 6], plot = T, main = "Rotation Rate Y")
  rotation.rate.z.spectrum <- TSA::periodogram(motion.data[, 7], plot = T, main = "Rotation Rate Z")
  
  axis <- which.max(c(max(rotation.rate.x.spectrum[[2]]), max(rotation.rate.y.spectrum[[2]]), max(rotation.rate.z.spectrum[[2]])))
  if(axis == 1) {
    axis.label      <- "X-Axis"
    index           <- which.max(rotation.rate.x.spectrum[[2]])
    main.frequency  <- rotation.rate.x.spectrum[[1]][index] * sampling.rate
  }

  if(axis == 2) {
    axis.label      <- "Y-Axis"
    index           <- which.max(rotation.rate.y.spectrum[[2]])
    main.frequency  <- rotation.rate.y.spectrum[[1]][index] * sampling.rate
  }

  if(axis == 3) {
    axis.label      <- "Z-Axis"
    index           <- which.max(rotation.rate.z.spectrum[[2]])
    main.frequency  <- rotation.rate.z.spectrum[[1]][index] * sampling.rate
  }
  
  # Low pass signal
  #main.frequency         <- .5
  main.frequency          <- floor(main.frequency * 10) / 10
  butterworth.filter      <- butter(1, 1/sampling.rate * main.frequency, "low")
  filtered.rotation.rate  <- filtfilt(butterworth.filter, motion.data[, axis + 4])
  
  # Search for minima
  minima <- SearchExtrema(filtered.rotation.rate, which = "minima")
  
  # Remove some minima
  m <- mean(filtered.rotation.rate[minima])
  s <- sd(filtered.rotation.rate[minima])
  minima <- minima[m + s * 1 > filtered.rotation.rate[minima]]
  
  par(mfcol=c(2, 1))
  plot(motion.data[, 1], motion.data[, axis + 4], type="l", xlab="t [s]", ylab=paste("Rotation Rate [deg/s] around", axis.label))
  abline(v = motion.data[minima, 1])
  plot(motion.data[, 1], filtered.rotation.rate, type="l", xlab="t [s]", ylab=paste("Filtered Rotation Rate [deg/s] around", axis.label))
  abline(v = motion.data[minima, 1])

  t               <- motion.data[minima, 1]
  cycle.interval  <- c(NA, diff(t))

  print(summary(cycle.interval))
  par(mfcol=c(2, 1))
  hist(cycle.interval)
  plot(t, cycle.interval, type="l", xlab="t [s]", ylab="Motion Cycle Interval [s]")
  
  # Extract properties
  activity.start    <- as.POSIXct(strptime(regmatches(motion.file.name, regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}--[0-9]{2}-[0-9]{2}-[0-9]{2}", motion.file.name)), "%Y-%m-%d--%H-%M-%S"))
  measurement       <- substr(regmatches(motion.file.name, regexpr("[1-9]{1}.csv", motion.file.name)), 1, 1)

  # Create directory, if needed
  output.directory <- paste("../data/preprocessed-data/", tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/", format(activity.start, format="%Y-%m-%d--%H-%M-%S", tz="CET"), "/", sep="")
  if(!file.exists(output.directory)) {
    dir.create(output.directory, recursive = TRUE)
  }

  # Write csv file
  output.file.path <- paste(output.directory, file.name.prefix, "-motion-time-data-", measurement, ".csv", sep = "")
  write.csv(data.frame(t, cycle.interval), output.file.path, row.names = FALSE)
  print(output.file.path)
}
