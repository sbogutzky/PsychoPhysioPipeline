# Remove all variables
rm(list = ls(all = T))

# Load libraries
require(signal)

# Set root data directory path
if(file.exists("/Users/sbogutzky/Desktop/data"))
  root.data.directory.path        <- "/Users/sbogutzky/Desktop/data/"

# Set cleaned data directory path
cleaned.data.directory.path       <- paste(root.data.directory.path, "cleaned-data/", sep = "")

# Set features directory path
features.directory.path           <- paste(root.data.directory.path, "features/", sep = "")

# Set preprocessed data directory path
preprocessed.data.directory.path  <- "./data/preprocessed-data/"

# Read activity directory
activity.directory  <- readline("Type in activity directory and press return to continue (e. g. walking/) > ")

# Read user directory
user.directory      <- readline("Type in user directory and press return to continue (e. g. doe-john/) > ")

# Load fss features
fss.features        <- read.csv(paste(features.directory.path, activity.directory, user.directory, "fss-features.csv", sep = ""), stringsAsFactors = F)

for (i in 1:nrow(fss.features)) {
  
  properties      <- fss.features[i, c(7:14)]
  activity.start  <- properties[, 2]
  activity.end    <- properties[, 3]
  measurement     <- properties[, 5]
  date.directory  <- paste(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y-%m-%d--%H-%M-%S"), "/", sep = "")
  
  if(measurement == 1) {
    ecg.data        <- data.frame()
    n               <- 0
    
    # Read data, if needed
    ecg.data.path <- paste(cleaned.data.directory.path, activity.directory, user.directory, date.directory, "ecg-data.csv", sep="")
    if(file.exists(ecg.data.path)) {
      ecg.data      <- read.csv(ecg.data.path)
      
      # Number of data rows
      n <- nrow(ecg.data)
      
      # Set ecg data timestamp to zero 
      ecg.data[,1] <- ecg.data[,1] - ecg.data[1,1]
      
      # Set first timestamp
      first.timestamp <- ecg.data[1,4]
      
      # Create output directory, if needed
      output.directory.path <- paste(preprocessed.data.directory.path, activity.directory, user.directory, date.directory, sep="")
      if(!file.exists(substr(output.directory.path, 1, nchar(output.directory.path) - 1))) {
        dir.create(output.directory.path, recursive = TRUE)
      }
    } else {
      print("No ecg data")
    }
  }
  
  if(n > 0) {
    
    # Subset ecg data
    ecg.data.subset   <- ecg.data[activity.start <= first.timestamp + ecg.data[,1] & first.timestamp + ecg.data[,1] < activity.end,]
    n.subset <- nrow(ecg.data.subset)
    if(n.subset > 0) {
      time.difference   <- first.timestamp + ecg.data.subset[1,1] - activity.start
      print(paste("Time difference (ms):", round(time.difference, 3)))
      print(paste("Total time      (ms):", round(ecg.data.subset[nrow(ecg.data.subset),1] - ecg.data.subset[1,1] + time.difference, 3)))
      
      output.file.path <- paste(output.directory.path, "ecg-data-", measurement, ".csv", sep = "")
      
      duplicates <- which(duplicated(ecg.data.subset))
      if(length(duplicates) > 0) {
        
        t <- (ecg.data.subset$Timestamp - ecg.data.subset$Timestamp[1]) / 1000
        plot(t, t, xlab = "t [s]", ylab = "t [s]")
        points(t[duplicates], t[duplicates], col = "red")
        
        ecg.data.subset <- ecg.data.subset[-duplicates, ]
        
        print(paste(length(duplicates), "removed duplicates in ", output.file.path))
        n.subset <- nrow(ecg.data.subset)
        
        #readline("Press return to continue > ")
      }
      
      # Interpolate for Kubios HRV
      n.subset / ((ecg.data.subset[n.subset,1] - ecg.data.subset[1,1]) / 1000)
      
      # Determine fs
      x   <- round(n.subset / ((ecg.data.subset[n.subset,1] - ecg.data.subset[1,1]) / 1000))
      fs   <- 205
      print(paste("Sampling rate   (Hz):", fs))
      
      t.ms      <- seq(ecg.data.subset[1, 1], ecg.data.subset[n.subset, 1], by = 1000/fs)
      lead.1.mv <- signal::interp1(ecg.data.subset[, 1], ecg.data.subset[,3] - ecg.data.subset[,2], t.ms, method = "spline")
      lead.2.mv <- signal::interp1(ecg.data.subset[, 1], ecg.data.subset[,2], t.ms, method = "spline")
      lead.3.mv <- signal::interp1(ecg.data.subset[, 1], ecg.data.subset[,3], t.ms, method = "spline")
      t.ms      <- t.ms - t.ms[1] + time.difference
      
      # Plot data
      par(mfcol=c(3, 1))
      plot(t.ms[0:1024] / 1000, lead.1.mv[0:1024], type = "l", xlab = "t [s]", ylab = "Lead I [mV]")
      title(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y/%m/%d %H:%M"))
      plot(t.ms[0:1024] / 1000, lead.2.mv[0:1024], type = "l", xlab = "t [s]", ylab = "Lead II [mV]")
      plot(t.ms[0:1024] / 1000, lead.3.mv[0:1024], type = "l", xlab = "t [s]", ylab = "Lead III [mV]")
      
      # Write csv file
      write.csv(data.frame(t.ms, lead.1.mv, lead.2.mv, lead.3.mv), output.file.path, row.names = F)
      print(paste("Wrote:", output.file.path))
    } else {
      print(paste("No data in time range:", date.directory, measurement))
    }
  }
}