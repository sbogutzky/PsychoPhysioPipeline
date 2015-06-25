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

# Load all file names
data.path               <- paste(root.data.path, tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/",  sep = "")
motion.file.names       <- list.files(path = data.path, pattern = paste(file.name.prefix, "-motion-data-[1-9].csv", sep=""), recursive = T)

for (motion.file.name in motion.file.names) {
  
  # Load motion data
  motion.data <- read.csv(paste(data.path, motion.file.name, sep=""))
  
  # Number of data row
  n <- nrow(motion.data)
  
  # Find cyclic movement   
  rotation.rate.x.spectrum <- TSA::periodogram(motion.data[, 5], plot = F)
  rotation.rate.y.spectrum <- TSA::periodogram(motion.data[, 6], plot = F)
  rotation.rate.z.spectrum <- TSA::periodogram(motion.data[, 7], plot = F)
  
  axis <- which.max(c(max(rotation.rate.x.spectrum[[2]]), max(rotation.rate.y.spectrum[[2]]), max(rotation.rate.z.spectrum[[2]])))
  rm(rotation.rate.x.spectrum, rotation.rate.y.spectrum, rotation.rate.z.spectrum)
  
  # Upsampling
  fs            <- 2000
  t             <- seq(motion.data[1, 1], motion.data[n, 1], by = 1/fs)
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
  low.pass.filter         <- butter(6, 1/(fs/2) * filt.freq, "low")
  filtered.rotation.rate  <- filtfilt(low.pass.filter, rotation.rate)
  
  # Search for minima
  minima <- SearchExtrema(filtered.rotation.rate, which = "minima")
  
  # Remove some minima
  m <- mean(filtered.rotation.rate[minima])
  s <- sd(filtered.rotation.rate[minima])
  minima <- minima[m + s * 4 > filtered.rotation.rate[minima]]
  rm(m, s)
  
  # 15 secs
  par(mfcol=c(1, 1))
  plot(t, filtered.rotation.rate, type="l", xlab="t [s]", ylab=paste("Filtered Rotation Rate [deg/s]"), xlim = c(0,50))
  abline(v = t[minima])

  # Compute time and intervals
  t.1             <- t[minima]
  cycle.interval  <- c(NA, diff(t.1))

  # Plot barplot and tachogramm
  par(mfcol=c(2, 1))
  hist(cycle.interval, xlab="Motion Cycle Interval [s]", main="")
  plot(t.1, cycle.interval, type="l", xlab="t [s]", ylab="Motion Cycle Interval [s]")
  
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
  write.csv(data.frame(t.1, cycle.interval), output.file.path, row.names = FALSE)
  print(paste("Wrote:", output.file.path))
}
