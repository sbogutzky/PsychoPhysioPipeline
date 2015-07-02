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

# Set processed data directory path
processed.data.directory.path <- paste(root.data.directory.path, "processed-data/", sep = "")

# Set features directory path
features.directory.path <- paste(root.data.directory.path, "features/", sep = "")

# Load fss features
fss.features <- read.csv(paste(features.directory.path, "fss-features.csv", sep = ""), stringsAsFactors = F)

# Create cps feature data frame
cps.features  <- data.frame()

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
  if(measurement == 1) {
    date.directory  <- paste(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y-%m-%d--%H-%M-%S"), "/", sep ="")
  }
  
  current.data.path <- paste(processed.data.directory.path, tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/", date.directory, sep="")
  
  current.file.name.1   <- paste(body.position, "-cps-indexes-", measurement, ".csv", sep = "")
  current.file.path.1   <- paste(current.data.path, current.file.name.1, sep = "")
  
  if(file.exists(current.file.path.1)) {
    
    cps.indexes        <- read.csv(current.file.path.1, skip = 2)
    
    mean.phase.coherence.index             <- round(mean(cps.indexes$phase.coherence.index, na.rm = T), 3)
    mean.normalized.shannon.entropy.index <- round(mean(cps.indexes$normalized.shannon.entropy.index, na.rm = T), 3)
    
    # Add parameter to feature vector
    cps.features    <- rbind(cps.features, data.frame(mean.phase.coherence.index, mean.normalized.shannon.entropy.index, activity, activity.start, activity.end, measurement, last.name, first.name, date.of.birth))
  }
}

# Create output directory, if needed
output.directory.path <- features.directory.path
if(!file.exists(output.directory.path)) {
  dir.create(output.directory.path, recursive = TRUE)
}

# Write to csv file
output.file.path <- paste(output.directory.path, body.position, "-cps-features.csv", sep = "")
if(file.exists(output.file.path)) {
  features <- read.csv(output.file.path, stringsAsFactors = FALSE)
  write.csv(unique(rbind(features, cps.features)), output.file.path, row.names = FALSE)
} else {
  write.csv(cps.features, output.file.path, row.names = FALSE)
}