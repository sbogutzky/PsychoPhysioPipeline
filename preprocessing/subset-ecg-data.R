# Remove all variables
rm(list = ls(all = T)) 

require(signal)

# Set network directory
network.directory <- "//gangstore.ddns.net/flow/Documents/simon-bogutzky/data/"
if(file.exists("/Volumes/flow/Documents/simon-bogutzky/data"))
  network.directory <- "/Volumes/flow/Documents/simon-bogutzky/data/"

# Set network cleaned data directory
cleaned.data.directory    <- paste(network.directory, "cleaned-data/", sep = "")

# Set local processed data directory
processed.data.directory <- "./data/preprocessed-data/"

# Load fss features
fss.features        <- read.csv(paste(network.directory, "features/fss-features.csv", sep = ""), stringsAsFactors = F)

for (i in 1:nrow(fss.features)) {
  
  properties      <- fss.features[i, c(6:13)]
  activity        <- properties[, 1]
  activity.start  <- properties[, 2]
  activity.end    <- properties[, 3]
  measurement     <- properties[, 5]
  last.name       <- properties[, 6]
  first.name      <- properties[, 7]
  date.of.birth   <- properties[, 8]
  date.directory  <- strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y-%m-%d--%H-%M-%S")
  
  if(measurement == 1) {
    ecg.data        <- data.frame()
    n               <- 0
    
    # Read data, if needed
    ecg.data.path <- paste(cleaned.data.directory, tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/", date.directory, "/ecg-data.csv", sep="")
    if(file.exists(ecg.data.path)) {
      ecg.data      <- read.csv(ecg.data.path)
      
      # Number of data rows
      n <- nrow(ecg.data)
      
      # Set ecg data timestamp to zero 
      ecg.data[,1] <- ecg.data[,1] - ecg.data[1,1]
      
      # Set first timestamp
      first.timestamp <- ecg.data[1,4]
      
      # Create output directory, if needed
      output.directory <- paste(processed.data.directory, tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/", date.directory, sep="")
      if(!file.exists(output.directory)) {
        dir.create(output.directory, recursive = TRUE)
      }
    } else {
      print("No ecg data")
    }
  }
  
  if(n > 0) {
    
    # Subset ecg data
    ecg.data.subset   <- ecg.data[activity.start <= first.timestamp + ecg.data[,1] & first.timestamp + ecg.data[,1] < activity.end,]
    time.difference   <- first.timestamp + ecg.data.subset[1,1] - activity.start
    print(paste("Time difference (ms):", round(time.difference, 3)))
    print(paste("Total time      (ms):", round(ecg.data.subset[nrow(ecg.data.subset),1] - ecg.data.subset[1,1] + time.difference, 3)))
    
    # Interpolate for Kubios HRV
    n.subset <- nrow(ecg.data.subset)
    n.subset / ((ecg.data.subset[n.subset,1] - ecg.data.subset[1,1]) / 1000)
    fs <- 512 #round(n.subset / ((ecg.data.subset[n.subset,1] - ecg.data.subset[1,1]) / 1000))
    
    t.ms      <- seq(ecg.data.subset[1, 1], ecg.data.subset[n.subset, 1], by = 1000/fs)
    lead.1.mv <- interp1(ecg.data.subset[, 1], ecg.data.subset[,3] - ecg.data.subset[,2], t.ms, method = "spline")
    lead.2.mv <- interp1(ecg.data.subset[, 1], ecg.data.subset[,2], t.ms, method = "spline")
    lead.3.mv <- interp1(ecg.data.subset[, 1], ecg.data.subset[,3], t.ms, method = "spline")
    t.ms      <- t.ms - t.ms[1] + time.difference
    
    # Plot data
    par(mfcol=c(3, 1))
    plot(t.ms[0:1024] / 1000, lead.1.mv[0:1024], type = "l", xlab = "t [s]", ylab = "Lead I [mV]")
    title(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y/%m/%d %H:%M"))
    plot(t.ms[0:1024] / 1000, lead.2.mv[0:1024], type = "l", xlab = "t [s]", ylab = "Lead II [mV]")
    plot(t.ms[0:1024] / 1000, lead.3.mv[0:1024], type = "l", xlab = "t [s]", ylab = "Lead III [mV]")
    
    # Write csv file
    output.file.path <- paste(output.directory, "/ecg-data-", measurement, ".csv", sep = "")
    write.csv(data.frame(t.ms, lead.1.mv, lead.2.mv, lead.3.mv), output.file.path, row.names = FALSE)
    print(paste("Wrote:", output.file.path))
  }
}