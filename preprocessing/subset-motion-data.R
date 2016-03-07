# Remove all variables
rm(list = ls(all = T)) 

# Load libraries
require(TSA)

# Set root data directory path
root.data.directory.path <- ""
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

# Read in body position
body.position       <- readline("Type in body position and press return to continue (e. g. leg) > ")

# Load fss features
fss.features        <- read.csv(paste(features.directory.path, activity.directory, user.directory, "fss-features.csv", sep = ""), stringsAsFactors = F)

for (i in 1:nrow(fss.features)) {
  
  properties      <- fss.features[i, c(7:14)]
  activity        <- properties[, 1]
  activity.start  <- properties[, 2]
  activity.end    <- properties[, 3]
  measurement     <- properties[, 5]
#   last.name       <- properties[, 6]
#   first.name      <- properties[, 7]
#   date.of.birth   <- properties[, 8]
  date.directory  <- paste(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y-%m-%d--%H-%M-%S"), "/", sep ="")
  if(measurement == 1) {
    print(date.directory)
    motion.data     <- data.frame()
    n               <- 0
    
#     if(body.position == "arm") {
#       acceleration.data.path <- paste(cleaned.data.directory.path, tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/", date.directory, "/", body.position, "-accelerometer-data.csv", sep="")
#       gyroscope.data.path    <- paste(cleaned.data.directory.path, tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/", date.directory, "/", body.position, "-gyroscope-data.csv", sep="")
#     
#       if(file.exists(acceleration.data.path) & file.exists(gyroscope.data.path)) {
#         acceleration.data <- read.csv(acceleration.data.path)
#         gyroscope.data    <- read.csv(gyroscope.data.path)
#         gyroscope.data    <- gyroscope.data[,1:4] 
#         
#         motion.data <- merge(acceleration.data, gyroscope.data, by.x = "Timestamp", by.y = "Timestamp")
#         motion.data <- motion.data[, c(1:4, 6:8, 5)]
#         motion.data[,1] <- motion.data[,1] / (10^6)
#       }
#     } else {
      motion.data.path <- paste(cleaned.data.directory.path, activity.directory, user.directory, date.directory, body.position, "-motion-data.csv", sep="")
      if(file.exists(motion.data.path)) {
        motion.data      <- read.csv(motion.data.path)
      # }
    }
      
    # Number of data rows
    n <- nrow(motion.data)
    
    if(n > 0) {
      # Set motion data timestamp to zero 
      motion.data[,1] <- motion.data[,1] - motion.data[1,1]
      
      # Set first timestamp
      first.timestamp <- motion.data[1,8]
      
      # Create output directory, if needed
      output.directory.path <- paste(preprocessed.data.directory.path, activity.directory, user.directory, date.directory, sep="")
      if(!file.exists(substr(output.directory.path, 1, nchar(output.directory.path) - 1))) {
        dir.create(output.directory.path, recursive = TRUE)
      }
    } else {
      print("No motion data")
    }
  }
  
  if(n > 0) {
    
    # Subset motion data
    motion.data.subset  <- motion.data[activity.start <= first.timestamp + motion.data[,1] & first.timestamp + motion.data[,1] < activity.end,]
    motion.data.subset <- motion.data.subset[complete.cases(motion.data.subset), ]
    n.subset <- nrow(motion.data.subset)
    if(n.subset > 0) {
      time.difference     <- first.timestamp + motion.data.subset[1,1] - activity.start
      print(paste("Time difference (ms):", round(time.difference, 3)))
      print(paste("Total time      (ms):", round(motion.data.subset[nrow(motion.data.subset),1] - motion.data.subset[1,1] + time.difference, 3)))
      
      output.file.path <- paste(output.directory.path, body.position, "-motion-data-", measurement, ".csv", sep = "")
      
      duplicates <- which(duplicated(motion.data.subset))
      if(length(duplicates) > 0) {
        
        t <- (motion.data.subset$Timestamp - motion.data.subset$Timestamp[1]) / 1000
        plot(t, t, xlab = "t [s]", ylab = "t [s]")
        points(t[duplicates], t[duplicates], col = "red")
        
        motion.data.subset <- motion.data.subset[-duplicates, ]
        
        print(paste(length(duplicates), "removed duplicates in ", output.file.path))
        
        readline("Press return to continue > ")
      }
      
      # Extract data
      t.ms                <- motion.data.subset[,1] - motion.data.subset[1,1] + time.difference
      acceleration.ms.2   <- t(matrix(c(motion.data.subset[, 2], motion.data.subset[, 3], motion.data.subset[, 4]), nrow(motion.data.subset), 3))
      rotation.rate.deg.s <- t(matrix(c(motion.data.subset[, 5], motion.data.subset[, 6], motion.data.subset[, 7]), nrow(motion.data.subset), 3))
      
      # Find cyclic movement   
      rotation.rate.x.spectrum <- TSA::periodogram(rotation.rate.deg.s[1, ], plot = F)
      rotation.rate.y.spectrum <- TSA::periodogram(rotation.rate.deg.s[2, ], plot = F)
      rotation.rate.z.spectrum <- TSA::periodogram(rotation.rate.deg.s[3, ], plot = F)
      
      axis <- which.max(c(max(rotation.rate.x.spectrum[[2]]), max(rotation.rate.y.spectrum[[2]]), max(rotation.rate.z.spectrum[[2]])))
      
      if(axis == 3) {
        print("90 deg around y")
        
        # Rotation
        rotation.matrix     <- matrix(c(cos(pi/2), 0, sin(pi/2), 0, 1, 0, -sin(pi/2), 0, cos(pi/2)), 3, 3, byrow = T)
        acceleration.ms.2   <- rotation.matrix %*% acceleration.ms.2
        rotation.rate.deg.s <- rotation.matrix %*% rotation.rate.deg.s
      }
      
      if(mean(acceleration.ms.2[2, ]) < 0) {
        print("180 deg around x")
        
        # Rotation
        rotation.matrix     <- matrix(c(cos(pi), -sin(pi), 0, sin(pi), cos(pi), 0, 0, 0, 1), 3, 3, byrow = T)
        acceleration.ms.2   <- rotation.matrix %*% acceleration.ms.2  
        rotation.rate.deg.s <- rotation.matrix %*% rotation.rate.deg.s
      } 
      
#       if(i > 4) {
#         acceleration.ms.2[3,]   <- -acceleration.ms.2[3,]
#         rotation.rate.deg.s[1,] <- -rotation.rate.deg.s[1,]
#       }
      
      # Plot data
      #     plot(t.ms / 1000, acceleration.ms.2[1,], type = "l", xlab = "t [s]", ylab = "Acceleration X [ms^2]", xlim = c(140, 150))
      #     plot(t.ms / 1000, acceleration.ms.2[2,], type = "l", xlab = "t [s]", ylab = "Acceleration Y [ms^2]", xlim = c(140, 150))
      #     plot(t.ms / 1000, acceleration.ms.2[3,], type = "l", xlab = "t [s]", ylab = "Acceleration Z [ms^2]", xlim = c(140, 150))
      plot(t.ms / 1000, rotation.rate.deg.s[1,], type = "l", xlab = "t [s]", ylab = "Rotation Rate X [deg/s]", xlim = c(140, 150))
      #     plot(t.ms / 1000, rotation.rate.deg.s[2,], type = "l", xlab = "t [s]", ylab = "Rotation Rate Y [deg/s]", xlim = c(140, 150))
      #     plot(t.ms / 1000, rotation.rate.deg.s[3,], type = "l", xlab = "t [s]", ylab = "Rotation Rate Z [deg/s]", xlim = c(140, 150))
      title(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y/%m/%d %H:%M"))
      
      # Write csv file
      write.csv(data.frame(t.ms, motion.accel.x.ms.2 = acceleration.ms.2[1, ], motion.accel.y.ms.2 = acceleration.ms.2[2, ], motion.accel.z.ms.2  = acceleration.ms.2[3, ], motion.rot.rate.x.deg.s = rotation.rate.deg.s[1, ], motion.rot.rate.y.deg.s  = rotation.rate.deg.s[2, ], motion.rot.rate.z.deg.s = rotation.rate.deg.s[3, ]), output.file.path, row.names = FALSE)
      print(paste("Wrote:", output.file.path))
    } else {
      print(paste("No data in time range:", date.directory, measurement))
    }
    readline("Press return to continue > ")
  }
}