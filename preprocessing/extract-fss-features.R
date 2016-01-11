# Remove all variables
rm(list = ls(all = T))  

# Load libraries
library(flow)

# Set root data directory path
if(file.exists("/Volumes/OSX/flow/data"))
  root.data.directory.path        <- "/Volumes/OSX/flow/data/"

# Set cleaned data directory path
cleaned.data.directory.path <- paste(root.data.directory.path, "cleaned-data/", sep = "")

# Set features directory path
features.directory.path <- paste(root.data.directory.path, "features/", sep = "")

# Set properties
first.name        <- "Patrick"
last.name         <- "Buse"
date.of.birth     <- "1984-05-05"
activity          <- "Running"

activity.directory <- paste(tolower(activity), "/",  sep = "")
user.directory <- paste(tolower(last.name), "-", tolower(first.name), "/",  sep = "")

# Load all file names
input.data.directory.path <- paste(cleaned.data.directory.path, activity.directory, user.directory, sep = "")
fss.data.file.names <- list.files(path = input.data.directory.path, pattern = "fss-data.csv", recursive = T)

# Create fss feature data frame
fss.features  <- data.frame()
fss.measurements  <- data.frame()

# Fill fss features data frame
for (fss.data.file.name in fss.data.file.names) {
  
  # Load fss data
  fss.data  <- read.csv(paste(input.data.directory.path, fss.data.file.name, sep = ""), header = T) 
  
  # Loop measurements
  for(i in 1:nrow(fss.data)) {
    
    # Extract times
    if(i == 1) {
      activity.start  <- fss.data[i, 1] - 15 * 60000
    } else {
      activity.start  <- inquiry.end
    }
    
    activity.end <- fss.data[i, 1]
    inquiry.end  <- fss.data[i, 2]
    
    # Calculate fss factors
    fss.factors     <- CalculateFlowShortScaleFactors(as.numeric(fss.data[i, 3:18]))
    
    # Add fss fss features
    fss.features    <- rbind(fss.features, data.frame(round(fss.factors[c(1, 3, 5, 7, 9, 11)], 2), activity, activity.start, activity.end, inquiry.end, measurement = i, last.name, first.name, date.of.birth))
    fss.measurements    <- rbind(fss.measurements, data.frame(fss.data[i, 3:18]))
  }
}

print(paste("Cronbach's Alpha Flow", multilevel::cronbach(fss.measurements[, 1:10])$Alpha))
print(paste("Cronbach's Alpha Fluency", multilevel::cronbach(fss.measurements[, c(8,7,9,4,5,2)])$Alpha))
print(paste("Cronbach's Alpha Absorption", multilevel::cronbach(fss.measurements[, c(6,1,10,3)])$Alpha))
print(paste("Cronbach's Alpha Anxienty", multilevel::cronbach(fss.measurements[, 11:13])$Alpha))

# Create output directory, if needed
output.directory.path <- paste(features.directory.path, activity.directory, user.directory, sep = "")
if(!file.exists(substr(output.directory.path, 1, nchar(output.directory.path) - 1))) {
  dir.create(output.directory.path, recursive = TRUE)
}

# Write to csv file
output.file.path <- paste(output.directory.path, "fss-features.csv", sep = "")
if(file.exists(output.file.path)) {
  features <- read.csv(output.file.path, stringsAsFactors = FALSE)
  write.csv(unique(rbind(features, fss.features)), output.file.path, row.names = FALSE)
  print(paste("Appended to", output.file.path))
} else {
  write.csv(fss.features, output.file.path, row.names = FALSE)
  print(paste("Worte to", output.file.path))
}