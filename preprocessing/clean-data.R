# Remove all variables
rm(list = ls(all = T)) 

# Set root data directory path
root.data.directory.path <- ""
if(file.exists("/Volumes/flow/Documents/archiv/daten/2015/flow-gehen-und-laufen"))
  root.data.directory.path        <- "/Volumes/flow/Documents/archiv/daten/2015/flow-gehen-und-laufen/"
if(file.exists("C:/Users/Simon Bogutzky/Documents/Archiv/flow/data"))
  root.data.directory.path        <- "C:/Users/Simon Bogutzky/Documents/Archiv/flow/data/"

# Set raw data directory path
raw.data.directory.path <- paste(root.data.directory.path, "raw-data/", sep = "")

# Set clean data directory path
cleaned.data.directory.path <- paste(root.data.directory.path, "cleaned-data/", sep = "")

# Read activity directory
activity.directory      <- "walking/" #readline("Type in activity directory and press return to continue (e. g. walking/) > ")

# Read user directory
user.directory          <- "grueter-barbara/" #readline("Type in user directory and press return to continue (e. g. doe-john/) > ")

# List fss file paths
fss.file.path.list      <- list.files(paste(raw.data.directory.path, activity.directory, user.directory, sep = ""), pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}-t[0-9]{2}-[0-9]{2}-[0-9]{2}-fss-data.csv", recursive = T)

for (fss.file.path in fss.file.path.list) {
  
  # Load fss data
  fss.data <- read.csv(paste(raw.data.directory.path, activity.directory, user.directory, fss.file.path, sep = ""))
  fss.data <- cbind(fss.data[, 17:18], fss.data[1:16])
  colnames(fss.data) <- c("System Timestamp 01","System Timestamp 02","Item 01","Item 02","Item 03","Item 04","Item 05","Item 06","Item 07","Item 08","Item 09","Item 10","Item 11","Item 12","Item 13","Item 14","Item 15","Item 16")
  fss.data <- fss.data[c(T, diff(fss.data$`System Timestamp 01`) / 1000 / 60 > 15), ]
  
  # Load ecg data
  ecg.file.path <- paste(substr(fss.file.path, 1, 32), "ecg-data.csv", sep = "")
  ecg.data      <- read.csv(paste(raw.data.directory.path, activity.directory, user.directory, ecg.file.path, sep = ""))
  colnames(ecg.data) <- c("Timestamp","ECG RA-LL","ECG LA-LL","System Timestamp")
  if(anyDuplicated(ecg.data)) {
    print(paste("Found duplicates in", ecg.file.path))
  }
  
  # Load motion data -- leg
  leg.motion.data.file.path <- paste(substr(fss.file.path, 1, 32), "leg-data.csv", sep = "")
  leg.motion.data           <- read.csv(paste(raw.data.directory.path, activity.directory, user.directory, leg.motion.data.file.path, sep = ""))
  colnames(leg.motion.data) <- c("Timestamp","Accelerometer X","Accelerometer Y","Accelerometer Z","Gyroscope X","Gyroscope Y","Gyroscope Z","System Timestamp")
  if(anyDuplicated(leg.motion.data)) {
    print(paste("Found duplicates in", leg.motion.data.file.path))
  }
    
  # Compute actitity start
  activity.start  <- fss.data[1, 1] - 15 * 60000
  date.directory  <- paste(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y-%m-%d--%H-%M-%S"), "/", sep ="")
  
  # Create output directory, if needed
  output.directory.path <- paste(cleaned.data.directory.path, activity.directory, user.directory, date.directory, sep="")
  if(!file.exists(substr(output.directory.path, 1, nchar(output.directory.path) - 1))) {
    dir.create(output.directory.path, recursive = TRUE)
  }
  
  # Save fss file
  write.csv(fss.data, paste(output.directory.path, "fss-data.csv", sep = ""), row.names = F)
  
  # Save ecg data file
  write.csv(ecg.data, paste(output.directory.path, "ecg-data.csv", sep = ""), row.names = F)
  
  # Save motion data file leg
  write.csv(leg.motion.data, paste(output.directory.path, "leg-motion-data.csv", sep = ""), row.names = F)
}
