# Remove all variables
rm(list = ls(all = T))  

# Set working directory
setwd("~/Entwicklung/bogutzky/repositories/non-disruptive-flow-measures/preprocessing")

# Load libraries
library(flow)
library(signal)

# Set properties
feature.path      <- "../data/features/fss-features.csv"
root.data.path    <- "../data/cleaned-data/"
subset.length     <- 5 # in minutes
fs                <- 256

# Read feature data
fss.features    <- read.csv(feature.path)
ecg.data        <- data.frame()

for (i in 1:nrow(fss.features)) {

  activity        <- as.character(fss.features[i, 11])
  activity.start  <- as.POSIXct(fss.features[i, 12])
  activity.end    <- as.POSIXct(fss.features[i, 13])
  last.name       <- as.character(fss.features[i, 16])
  first.name      <- as.character(fss.features[i, 17])
  measurement     <- fss.features[i, 15]
  
  if(measurement == 1) {
    
    # Read data, if needed
    ecg.data.path <- paste(root.data.path, tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/", format(activity.start, format="%Y-%m-%d--%H-%M-%S", tz="CET"), "/ecg-data.csv", sep="")
    if(file.exists(ecg.data.path)) {
      ecg.data      <- read.csv(ecg.data.path)
      
      # Create directory, if needed
      output.directory <- paste("../data/preprocessed-data/", tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/", format(activity.start, format="%Y-%m-%d--%H-%M-%S", tz="CET"), "/", sep="")
      if(!file.exists(output.directory)) {
        dir.create(output.directory, recursive = TRUE)
      }
      
    } else {
      print("No ecg data")
    }
  }
  
  # Subset ecg data
  if(nrow(ecg.data) > 0) {
    
    ecg.data.subset <- ecg.data[activity.end - 5 * 60 <= as.POSIXct(ecg.data[,4]/1000, origin = "1970-01-01") & as.POSIXct(ecg.data[,4]/1000, origin = "1970-01-01") <= activity.end,]
    print(as.POSIXct(ecg.data.subset[nrow(ecg.data.subset),4]/1000, origin = "1970-01-01") - as.POSIXct(ecg.data.subset[1,4]/1000, origin = "1970-01-01"))
    
    # Set timestamp to zero
    ecg.data.subset[,1] <- (ecg.data.subset[,1] - ecg.data.subset[1, 1]) / 1000
    
    # Interpolate
    t         <- seq(ecg.data.subset[1, 1], ecg.data.subset[nrow(ecg.data.subset), 1], by = 1/fs)
    ecg.rall  <- interp1(ecg.data.subset[,1], ecg.data.subset[, 2], t, method = "spline")
    ecg.lall  <- interp1(ecg.data.subset[,1], ecg.data.subset[, 3], t, method = "spline")
  
    # Write csv file
    output.file.path <- paste(output.directory, "ecg-data-", measurement, ".csv", sep = "")
    write.csv(data.frame(t, ecg.rall, ecg.lall), output.file.path, row.names = FALSE)
    print(output.file.path)
  }
}