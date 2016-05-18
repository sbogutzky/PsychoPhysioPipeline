# Version 2.0

# Remove all variables
rm(list = ls(all = T))

# Load library
library(flow)

# Set input variables
input.directory.path <- "C:/Users/sbogutzky/Desktop/2013-10-03/2013-10-03-t17-39-20-"
root.directory.path <- "C:/Users/sbogutzky/Desktop/data (lokal)/2013/raw-data/"
user.directory <- "buse-patrick/"
activity.directory <- "laufen/"
resample.rate <- 205

# Read data
fss.data <- read.csv(paste(input.directory.path, "fss-data.csv", sep = ""))
ecg.data <- read.csv(paste(input.directory.path, "ecg-data.csv", sep = "")) 
leg.data <- read.csv(paste(input.directory.path, "leg-data.csv", sep = "")) 

# Log start times 
print(paste("FSS start time:", as.POSIXlt(fss.data[1, 17] / 1000, "Europe/Berlin", origin = "1970-01-01")))
print(paste("ECG start time:", as.POSIXlt(ecg.data[1, 4] / 1000, "Europe/Berlin", origin = "1970-01-01")))
print(paste("LEG start time:", as.POSIXlt(leg.data[1, 8] / 1000, "Europe/Berlin", origin = "1970-01-01")))

# Compute time differences
time.difference.fss.ecg <- fss.data[1, 17] - ecg.data[1, 4]
time.difference.fss.leg <- fss.data[1, 17] - leg.data[1, 8]

# Log time differences
print(paste("FSS - ECG time difference:", time.difference.fss.ecg, "ms"))
print(paste("FSS - LEG time difference:", time.difference.fss.leg, "ms"))
offset <- max(time.difference.fss.ecg, time.difference.fss.leg)
rm(time.difference.fss.ecg, time.difference.fss.leg)

# With baseline measurement
if(nrow(fss.data) == 5) {
  
  # baseline
  
  # Set variables
  start.time <- as.POSIXlt((fss.data[1, 17] - offset) / 1000, "Europe/Berlin", origin = "1970-01-01")
  stop.time <- as.POSIXlt(fss.data[1, 18] / 1000, "Europe/Berlin", origin = "1970-01-01")
  date.directory <- gsub(" ", "--", gsub(":", "-", start.time))
  header.comment <- gsub("-", "/", paste("# StartTime:", start.time))
  footer.comment <- gsub("-", "/", paste("# StopTime:", stop.time))
  
  # Set self report data
  self.report.baseline <- data.frame(c(offset, offset, offset + fss.data[1, 18] - fss.data[1, 17], fss.data[1, 1:16]))
  names(self.report.baseline) <- c("timestamp.show.ms", "timestamp.start.ms", "timestamp.stop.ms", "item.01", "item.02", "item.03", "item.04", "item.05", "item.06", "item.07", "item.08", "item.09", "item.10", "item.11", "item.12", "item.13", "item.14", "item.15", "item.16")
  
  # Create directory, if needed
  output.directory.path <- paste(root.directory.path, "baseline/", user.directory, date.directory, "/", sep = "")
  if(!file.exists(substr(output.directory.path, 1, nchar(output.directory.path) - 1))) {
    dir.create(output.directory.path, recursive = TRUE)
  }
  
  # Write self report data
  GenericWrite(self.report.baseline, paste(output.directory.path, "self-report.csv", sep = ""), header.comment, footer.comment, quote = FALSE, row.names = FALSE)
  
  # Set ecg data
  ecg.data.1 <- ecg.data
  ecg.data.1[, 1] <- ecg.data[, 1] - ecg.data[1, 1]
  ecg.data.baseline <- ecg.data.1[ecg.data.1[, 1] < self.report.baseline[1, 3], 1:3]
  names(ecg.data.baseline) <- c("timestamp.ms", "ecg.ra.ll.mv", "ecg.la.ll.mv")
  
  # Resample ecg data
  resampled.data <- ResampleData(ecg.data.baseline[, 2:3], resample.rate, ecg.data.baseline[, 1])
  resampled.data <- data.frame(resampled.data)
  colnames(resampled.data) <- colnames(ecg.data.baseline)
  ecg.data.baseline <- resampled.data
  
  # Write ecg data
  GenericWrite(ecg.data.baseline, paste(output.directory.path, "imu-rn42-bd38.csv", sep = ""), header.comment, footer.comment, quote = FALSE, row.names = FALSE)
  
  # Set leg data
  leg.data.1 <- leg.data
  leg.data.1[, 1] <- leg.data[, 1] - leg.data[1, 1]
  leg.data.baseline <- leg.data.1[leg.data.1[, 1] < self.report.baseline[1, 3], 1:7]
  names(leg.data.baseline) <- c("timestamp.ms", "acceleration.x.ms.2", "acceleration.y.ms.2", "acceleration.z.ms.2", "angular.velocity.x.deg.s", "angular.velocity.y.deg.s", "angular.velocity.z.deg.s")
  
  # Switch axes
  leg.data.baseline[, 2] <- -leg.data.baseline[, 2]
  leg.data.baseline[, 4] <- -leg.data.baseline[, 4]
  leg.data.baseline[, 5] <- -leg.data.baseline[, 5]
  leg.data.baseline[, 7] <- -leg.data.baseline[, 7]
  
  # Write leg data
  GenericWrite(leg.data.baseline, paste(output.directory.path, "imu-rn42-3b70.csv", sep = ""), header.comment, footer.comment, quote = FALSE, row.names = FALSE)
  
  rm(ecg.data.1, leg.data.1, resampled.data, date.directory, footer.comment, header.comment, offset, start.time, stop.time, output.directory.path)
  
  # Activity
  
  # Set variables
  start.time <- as.POSIXlt(fss.data[1, 18] / 1000, "Europe/Berlin", origin = "1970-01-01")
  stop.time <- as.POSIXlt(fss.data[5, 18] / 1000, "Europe/Berlin", origin = "1970-01-01")
  date.directory <- gsub(" ", "--", gsub(":", "-", start.time))
  header.comment <- gsub("-", "/", paste("# StartTime:", start.time))
  footer.comment <- gsub("-", "/", paste("# StopTime:", stop.time))
  
  # Set self report data
  offset <- abs(fss.data[1, 18] - fss.data[2, 17])
  self.report.activity.1 <- data.frame(c(offset, offset, offset + fss.data[2, 18] - fss.data[2, 17], fss.data[2, 1:16]))
  names(self.report.activity.1) <- c("timestamp.show.ms", "timestamp.start.ms", "timestamp.stop.ms", "item.01", "item.02", "item.03", "item.04", "item.05", "item.06", "item.07", "item.08", "item.09", "item.10", "item.11", "item.12", "item.13", "item.14", "item.15", "item.16")
  offset <- offset + fss.data[2, 18] - fss.data[2, 17] + abs(fss.data[2, 18] - fss.data[3, 17])
  self.report.activity.2 <- data.frame(c(offset, offset, offset + fss.data[3, 18] - fss.data[3, 17], fss.data[3, 1:16]))
  names(self.report.activity.2) <- c("timestamp.show.ms", "timestamp.start.ms", "timestamp.stop.ms", "item.01", "item.02", "item.03", "item.04", "item.05", "item.06", "item.07", "item.08", "item.09", "item.10", "item.11", "item.12", "item.13", "item.14", "item.15", "item.16")
  offset <- offset + fss.data[3, 18] - fss.data[3, 17] + abs(fss.data[3, 18] - fss.data[4, 17])
  self.report.activity.3 <- data.frame(c(offset, offset, offset + fss.data[4, 18] - fss.data[4, 17], fss.data[4, 1:16]))
  names(self.report.activity.3) <- c("timestamp.show.ms", "timestamp.start.ms", "timestamp.stop.ms", "item.01", "item.02", "item.03", "item.04", "item.05", "item.06", "item.07", "item.08", "item.09", "item.10", "item.11", "item.12", "item.13", "item.14", "item.15", "item.16")
  offset <- offset + fss.data[4, 18] - fss.data[4, 17] + abs(fss.data[4, 18] - fss.data[5, 17])
  self.report.activity.4 <- data.frame(c(offset, offset, offset + fss.data[5, 18] - fss.data[5, 17], fss.data[5, 1:16]))
  names(self.report.activity.4) <- c("timestamp.show.ms", "timestamp.start.ms", "timestamp.stop.ms", "item.01", "item.02", "item.03", "item.04", "item.05", "item.06", "item.07", "item.08", "item.09", "item.10", "item.11", "item.12", "item.13", "item.14", "item.15", "item.16")
  
  self.report.activity <- rbind(self.report.activity.1, self.report.activity.2, self.report.activity.3, self.report.activity.4)
  rm(self.report.activity.1, self.report.activity.2, self.report.activity.3, self.report.activity.4)
  
  # Create directory, if needed
  output.directory.path <- paste(root.directory.path, activity.directory, user.directory, date.directory, "/", sep = "")
  if(!file.exists(substr(output.directory.path, 1, nchar(output.directory.path) - 1))) {
    dir.create(output.directory.path, recursive = TRUE)
  }
  
  # Set ecg data
  ecg.data.1 <- ecg.data
  ecg.data.1[, 1] <- ecg.data[, 1] - ecg.data[1, 1]
  ecg.data.activity <- ecg.data.1[ecg.data.1[, 1] >= self.report.baseline[1, 3], 1:3]
  names(ecg.data.activity) <- c("timestamp.ms", "ecg.ra.ll.mv", "ecg.la.ll.mv")
  ecg.data.activity[, 1] <- ecg.data.activity[, 1] - ecg.data.activity[1, 1]
  ecg.data.activity <- ecg.data.activity[ecg.data.activity[, 1] < self.report.activity[4, 3], ]
  
  # Resample ecg data
  resampled.data <- ResampleData(ecg.data.activity[, 2:3], resample.rate, ecg.data.activity[, 1])
  resampled.data <- data.frame(resampled.data)
  colnames(resampled.data) <- colnames(ecg.data.activity)
  ecg.data.activity <- resampled.data
  
  # Write ecg data
  GenericWrite(ecg.data.activity, paste(output.directory.path, "imu-rn42-bd38.csv", sep = ""), header.comment, footer.comment, quote = FALSE, row.names = FALSE)
  
  # Set leg data
  leg.data.1 <- leg.data
  leg.data.1[, 1] <- leg.data[, 1] - leg.data[1, 1]
  leg.data.activity <- leg.data.1[leg.data.1[, 1] >= self.report.baseline[1, 3], 1:7]
  names(leg.data.activity) <- c("timestamp.ms", "acceleration.x.ms.2", "acceleration.y.ms.2", "acceleration.z.ms.2", "angular.velocity.x.deg.s", "angular.velocity.y.deg.s", "angular.velocity.z.deg.s")
  leg.data.activity[, 1] <- leg.data.activity[, 1] - leg.data.activity[1, 1]
  leg.data.activity <- leg.data.activity[leg.data.activity[, 1] < self.report.activity[4, 3], ]
  
  # Switch axes
  leg.data.activity[, 2] <- -leg.data.activity[, 2]
  leg.data.activity[, 4] <- -leg.data.activity[, 4]
  leg.data.activity[, 5] <- -leg.data.activity[, 5]
  leg.data.activity[, 7] <- -leg.data.activity[, 7]
  
  # Write leg data
  GenericWrite(leg.data.activity, paste(output.directory.path, "imu-rn42-3b70.csv", sep = ""), header.comment, footer.comment, quote = FALSE, row.names = FALSE)
  
} else {
}

steps <- seq(0, nrow(leg.data.activity), 10)
plot(leg.data.activity[steps, 1] / 1000, leg.data.activity[steps, 5], type = "l", xlab = "Time (s)", ylab = "Angular Velocity X (deg/s)")
abline(v = self.report.activity[, 1] / 1000, col = "tomato", lty = "dashed")
title(gsub("-", "/", start.time))
start.pos <- identify(x = leg.data.activity[steps, 1] / 1000, leg.data.activity[steps, 5], plot = FALSE, n = 4)

if(length(start.pos) == 4) {
  self.report.activity[, 2] <- leg.data.activity[steps, 1][start.pos]
  
  # Write self report data
  GenericWrite(self.report.activity, paste(output.directory.path, "self-report.csv", sep = ""), header.comment, footer.comment, quote = FALSE, row.names = FALSE)
}

rm(ecg.data.1, leg.data.1, resampled.data, date.directory, footer.comment, header.comment, offset, stop.time, output.directory.path, steps, start.pos)
