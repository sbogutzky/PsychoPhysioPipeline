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

# Which sampling rate do you wish?
sampling.rate  <- 256

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
    ecg.data        <- data.frame()
    
    # Read data, if needed
    ecg.data.path <- paste(root.data.path, tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/", format(activity.start, format="%Y-%m-%d--%H-%M-%S", tz="CET"), "/ecg-data.csv", sep="")
    if(file.exists(ecg.data.path)) {
      ecg.data      <- read.csv(ecg.data.path)
      
      # Set ecg data timestamp to zero 
      ecg.data[,1] <- (ecg.data[,1] - ecg.data[1,1]) / 1000
      
      # Correct ecg data system time
      ecg.data[,4] <- ecg.data[1,4] / 1000 + ecg.data[,1] 
      
      # Create directory, if needed
      output.directory <- paste("../data/preprocessed-data/", tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/", format(activity.start, format="%Y-%m-%d--%H-%M-%S", tz="CET"), "/", sep="")
      if(!file.exists(output.directory)) {
        dir.create(output.directory, recursive = TRUE)
      }
      
      par(mfcol=c(2, 1))
      plot(ecg.data[,1], ecg.data[,3], type = "l", xlab = "t [s]", ylab = "ECG LALL [mV]")
      title(activity.start)
      if(activity == "Running") {
        plot(ecg.data[ecg.data[,1] > 1800 & ecg.data[,1] < 1810,1], ecg.data[ecg.data[,1] > 1800 & ecg.data[,1] < 1810,3], type = "l", xlab = "t [s]", ylab = "ECG LALL [mV]")
      } else {
        plot(ecg.data[ecg.data[,1] > 600 & ecg.data[,1] < 610,1], ecg.data[ecg.data[,1] > 600 & ecg.data[,1] < 610,3], type = "l", xlab = "t [s]", ylab = "ECG LALL [mV]")
      }
    } else {
      print("No ecg data")
    }
  }
  
  # Subset ecg data
  if(nrow(ecg.data) > 0) {
    
    start.time        <- activity.end - measurement.started.before * 60
    ecg.data.subset   <- ecg.data[start.time <= as.POSIXct(ecg.data[,4], origin = "1970-01-01") & as.POSIXct(ecg.data[,4], origin = "1970-01-01") <= activity.end,]
    time.difference   <- as.POSIXct(ecg.data.subset[1,4], origin = "1970-01-01") - (activity.end - measurement.started.before * 60)
    print(time.difference)
    print(as.POSIXct(ecg.data.subset[nrow(ecg.data.subset),4], origin = "1970-01-01") - as.POSIXct(ecg.data.subset[1,4], origin = "1970-01-01") + time.difference)
    
    # Set subset timestamp to zero
    ecg.data.subset[,1] <- ecg.data.subset[,1] - ecg.data.subset[1, 1]
    
    # Interpolate
    t         <- seq(ecg.data.subset[1, 1], ecg.data.subset[nrow(ecg.data.subset), 1], by = 1/sampling.rate)
    ecg.rall  <- interp1(ecg.data.subset[,1], ecg.data.subset[, 2], t, method = "spline")
    ecg.lall  <- interp1(ecg.data.subset[,1], ecg.data.subset[, 3], t, method = "spline")
    t         <- t + time.difference
  
    # Write csv file
    output.file.path <- paste(output.directory, "ecg-data-", measurement, ".csv", sep = "")
    write.csv(data.frame(t, ecg.rall, ecg.lall), output.file.path, row.names = FALSE)
    print(output.file.path)
  }
}