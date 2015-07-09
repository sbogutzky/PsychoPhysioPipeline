# Remove all variables
rm(list = ls(all = T)) 

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

# Set processed data directory path
processed.data.directory.path <- "processed-data/"

# Set features directory path
features.directory.path <- paste(root.data.directory.path, "features/", sep = "")

# Load fss features
fss.features <- read.csv(paste(features.directory.path, "fss-features.csv", sep = ""), stringsAsFactors = F)

for (i in 1:nrow(fss.features)) {
  
  properties      <- fss.features[i, c(6:13)]
  activity        <- properties[, 1]
  activity.start  <- properties[, 2]
  activity.end    <- properties[, 3]
  measurement     <- properties[, 5]
  last.name       <- properties[, 6]
  first.name      <- properties[, 7]
  date.of.birth   <- properties[, 8]
  date.directory  <- paste(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y-%m-%d--%H-%M-%S"), "/", sep = "")
  
  if(measurement == 1) {
    gps.data        <- data.frame()
    n               <- 0
    
    # Read data, if needed
    gps.data.path <- paste(cleaned.data.directory.path, tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/", date.directory, "gps-data.csv", sep="")
    if(file.exists(gps.data.path)) {
      gps.data      <- read.csv(gps.data.path)
      
      # Number of data rows
      n <- nrow(gps.data)
      
      # Create output directory, if needed
      output.directory.path <- paste(root.data.directory.path, processed.data.directory.path, tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/", date.directory, sep="")
      if(!file.exists(output.directory.path)) {
        dir.create(output.directory.path, recursive = TRUE)
      }
    } else {
      print("No gps data")
    }
  }
  
  if(n > 0) {
    
    # Subset gps data
    gps.data.subset   <- gps.data[activity.start <= gps.data[,1] & gps.data[,1] < activity.end,]
    time.difference   <- gps.data.subset[1,1] - activity.start
    print(paste("Time difference (ms):", round(time.difference, 3)))
    print(paste("Total time      (ms):", round(gps.data.subset[nrow(gps.data.subset),1] - gps.data.subset[1,1] + time.difference, 3)))
    
    
    date.time <- as.POSIXct(gps.data.subset[,1] / 1000, origin = "1970-01-01", tz = "CET")
    date.time <- format(date.time, "%Y-%m-%d %H:%M:%OS3")
    latitude  <- gps.data.subset[,2]
    longitude <- gps.data.subset[,3]
    altitude  <- gps.data.subset[,4]
    
    # Write csv file
    output.file.path <- paste(output.directory.path, "gps-data-", measurement, ".csv", sep = "")
    write.csv(data.frame(date.time, latitude, longitude, altitude), output.file.path, row.names = FALSE)
    print(paste("Wrote:", output.file.path))
  }
}