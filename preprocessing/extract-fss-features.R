# Remove all variables
rm(list = ls(all = T))  

# Load libraries
library(flow)

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

# Set features directory path
features.directory.path <- paste(root.data.directory.path, "features/", sep = "")

# Set properties
first.name        <- "Barbara"
last.name         <- "Grueter"
date.of.birth     <- "1950-04-15"
activity          <- "Walking"

# Load all file names
input.data.directory.path <- paste(cleaned.data.directory.path, tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/",  sep = "")
fss.data.file.names <- list.files(path = input.data.directory.path, pattern = "fss-data.csv", recursive = T)

# Create fss feature data frame
fss.features  <- data.frame()

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
    fss.features    <- rbind(fss.features, data.frame(round(fss.factors[c(1, 3, 5, 7, 9)], 2), activity, activity.start, activity.end, inquiry.end, measurement = i, last.name, first.name, date.of.birth))
  }
}

# Create output directory, if needed
output.directory.path <- features.directory.path
if(!file.exists(output.directory.path)) {
  dir.create(output.directory.path, recursive = TRUE)
}

# Write to csv file
output.file.path <- paste(output.directory.path, "fss-features.csv", sep = "")
if(file.exists(output.file.path)) {
  features <- read.csv(output.file.path, stringsAsFactors = FALSE)
  write.csv(unique(rbind(features, fss.features)), output.file.path, row.names = FALSE)
} else {
  write.csv(fss.features, output.file.path, row.names = FALSE)
}