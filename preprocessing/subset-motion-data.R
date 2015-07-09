# Remove all variables
rm(list = ls(all = T)) 

# Load libraries
require(TSA)

# Set root data directory path
root.data.directory.path <- ""
if(file.exists("C:/Users/Simon Bogutzky/Documents/flow/data"))
  root.data.directory.path <- "C:/Users/Simon Bogutzky/Documents/flow/data/"
if(file.exists("/Volumes/flow/Documents/simon-bogutzky/data"))
  root.data.directory.path <- "/Volumes/flow/Documents/simon-bogutzky/data/"
if(file.exists("//gangstore.ddns.net/flow/Documents/simon-bogutzky/data"))
  root.data.directory.path <- "//gangstore.ddns.net/flow/Documents/simon-bogutzky/data/"

# Set cleaned data directory path
cleaned.data.directory.path <- paste(root.data.directory.path, "cleaned-data/", sep = "")

# Set preprocessed data directory path
preprocessed.data.directory.path <- "./data/preprocessed-data/"

# Set features directory path
features.directory.path <- paste(root.data.directory.path, "features/", sep = "")

# Load fss features
fss.features <- read.csv(paste(features.directory.path, "fss-features.csv", sep = ""), stringsAsFactors = F)

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
  date.directory  <- paste(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y-%m-%d--%H-%M-%S"), sep = "")
  
  if(measurement == 1) {
    motion.data     <- data.frame()
    n               <- 0
    
    if(body.position == "arm") {
      acceleration.data.path <- paste(cleaned.data.directory.path, tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/", date.directory, "/", body.position, "-accelerometer-data.csv", sep="")
      gyroscope.data.path    <- paste(cleaned.data.directory.path, tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/", date.directory, "/", body.position, "-gyroscope-data.csv", sep="")
    
      if(file.exists(acceleration.data.path) & file.exists(gyroscope.data.path)) {
        acceleration.data <- read.csv(acceleration.data.path)
        gyroscope.data    <- read.csv(gyroscope.data.path)
        gyroscope.data    <- gyroscope.data[,1:4] 
        
        motion.data <- merge(acceleration.data, gyroscope.data, by.x = "Timestamp", by.y = "Timestamp")
        motion.data <- motion.data[, c(1:4, 6:8, 5)]
        motion.data[,1] <- motion.data[,1] / (10^6)
      }
    } else {
      motion.data.path <- paste(cleaned.data.directory.path, tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/", date.directory, body.position, "-motion-data.csv", sep="")
      if(file.exists(motion.data.path)) {
        motion.data      <- read.csv(motion.data.path)
      }
    }
      
    # Number of data rows
    n <- nrow(motion.data)
    
    if(n > 0) {
      # Set motion data timestamp to zero 
      motion.data[,1] <- motion.data[,1] - motion.data[1,1]
      
      # Set first timestamp
      first.timestamp <- motion.data[1,8]
      
      # Create output directory, if needed
      output.directory.path <- paste(preprocessed.data.directory.path, tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/", date.directory, sep="")
      if(!file.exists(output.directory.path)) {
        dir.create(output.directory.path, recursive = TRUE)
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
    
    motion.rot.rate.deg.s <- motion.rot.rate.x.deg.s
    
    # Find cyclic movement   
    rotation.rate.x.spectrum <- TSA::periodogram(motion.rot.rate.x.deg.s, plot = F)
    rotation.rate.y.spectrum <- TSA::periodogram(motion.rot.rate.y.deg.s, plot = F)
    rotation.rate.z.spectrum <- TSA::periodogram(motion.rot.rate.z.deg.s, plot = F)
    
    axis <- which.max(c(max(rotation.rate.x.spectrum[[2]]), max(rotation.rate.y.spectrum[[2]]), max(rotation.rate.z.spectrum[[2]])))
    
    if(mean(motion.accel.y.ms.2) < 0 & axis == 1) {
      motion.accel.x.ms.2      <- -motion.accel.x.ms.2 
      motion.accel.y.ms.2      <- -motion.accel.y.ms.2
      motion.rot.rate.x.deg.s  <- -motion.rot.rate.x.deg.s
      motion.rot.rate.y.deg.s  <- -motion.rot.rate.y.deg.s
      
      print("Axis switched cyclic motion around x")
      
      motion.rot.rate.deg.s <- motion.rot.rate.x.deg.s
    }
    
    if(mean(motion.accel.y.ms.2) < 0 & axis == 3) {
      motion.accel.z.ms.2      <- -motion.accel.z.ms.2 
      motion.accel.y.ms.2      <- -motion.accel.y.ms.2
      motion.rot.rate.z.deg.s  <- -motion.rot.rate.z.deg.s
      motion.rot.rate.y.deg.s  <- -motion.rot.rate.y.deg.s
      
      print("Axis switched cyclic motion around z")
      
      motion.rot.rate.deg.s <- motion.rot.rate.z.deg.s
    } 
    
    # Plot data
    plot(t.ms / 1000, motion.rot.rate.deg.s, type = "l", xlab = "t [s]", ylab = "Rotation Rate [deg/s]", xlim = c(140, 150))
    title(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y/%m/%d %H:%M"))
    
    # Write csv file
    output.file.path <- paste(output.directory.path, body.position, "-motion-data-", measurement, ".csv", sep = "")
    write.csv(data.frame(t.ms, motion.accel.x.ms.2, motion.accel.y.ms.2, motion.accel.z.ms.2, motion.rot.rate.x.deg.s, motion.rot.rate.y.deg.s, motion.rot.rate.z.deg.s), output.file.path, row.names = FALSE)
    print(paste("Wrote:", output.file.path))
  }
}