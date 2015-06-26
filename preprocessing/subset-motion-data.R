# Remove all variables
rm(list = ls(all = T))  

# Set network directory
network.directory <- "//gangstore.ddns.net/flow/Documents/simon-bogutzky/data"
if(file.exists("/Volumes/flow/Documents/simon-bogutzky/data"))
  network.directory <- "/Volumes/flow/Documents/simon-bogutzky/data"

# Set network cleaned data directory
cleaned.data.directory    <- paste(network.directory, "/cleaned-data/", sep = "")

# Set local processed data directory
processed.data.directory <- "./data/preprocessed-data/"

# Load fss features
fss.features <- read.csv(paste(network.directory, "/features/fss-features.csv", sep = ""), stringsAsFactors = F)

# Set body position
body.position <- "leg"

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
    motion.data        <- data.frame()
    n               <- 0
    
    # Read data, if needed
    motion.data.path <- paste(cleaned.data.directory, tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/", date.directory, "/", body.position, "-motion-data.csv", sep="")
    if(file.exists(motion.data.path)) {
      motion.data      <- read.csv(motion.data.path)
      
      # Number of data rows
      n <- nrow(motion.data)
      
      # Set motion data timestamp to zero 
      motion.data[,1] <- motion.data[,1] - motion.data[1,1]
      
      # Set first timestamp
      first.timestamp <- motion.data[1,8]
      
      # Create output directory, if needed
      output.directory <- paste(processed.data.directory, tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/", date.directory, sep="")
      if(!file.exists(output.directory)) {
        dir.create(output.directory, recursive = TRUE)
      }
    } else {
      print("No motion data")
    }
  }
  
  if(n > 0) {
    
    # Subset motion data
    motion.data.subset   <- motion.data[activity.start <= first.timestamp + motion.data[,1] & first.timestamp + motion.data[,1] < activity.end,]
    time.difference   <- first.timestamp + motion.data.subset[1,1] - activity.start
    print(paste("Time difference (ms):", round(time.difference, 3)))
    print(paste("Total time      (ms):", round(motion.data.subset[nrow(motion.data.subset),1] - motion.data.subset[1,1] + time.difference, 3)))
    
    # Extract data
    t.ms                     <- motion.data.subset[,1] - motion.data.subset[1,1] + time.difference
    motion.accel.x.ms.2      <- motion.data.subset[,2]
    motion.accel.y.ms.2      <- motion.data.subset[,3]
    motion.accel.z.ms.2      <- motion.data.subset[,4]
    motion.rot.rate.x.deg.s  <- motion.data.subset[,5]
    motion.rot.rate.y.deg.s  <- motion.data.subset[,6]
    motion.rot.rate.z.deg.s  <- motion.data.subset[,7]
    
    # Plot data
    plot(t.ms / 1000, motion.rot.rate.x.deg.s, type = "l", xlab = "t [s]", ylab = "Rotation Rate X [deg/s]")
    title(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y/%m/%d %H:%M"))
    
    # Write csv file
    output.file.path <- paste(output.directory, "/", body.position, "-motion-data-", measurement, ".csv", sep = "")
    write.csv(data.frame(t.ms, motion.accel.x.ms.2, motion.accel.y.ms.2, motion.accel.z.ms.2, motion.rot.rate.x.deg.s, motion.rot.rate.y.deg.s, motion.rot.rate.z.deg.s), output.file.path, row.names = FALSE)
    print(paste("Wrote:", output.file.path))
  }
}