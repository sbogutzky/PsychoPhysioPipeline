# Remove all variables
rm(list = ls(all = T))  

# Set working directory
if(file.exists("/Volumes/flow/Documents/simon-bogutzky/data")) {
  setwd("/Volumes/flow/Documents/simon-bogutzky/data")
} else {
  setwd("//gangstore.ddns.net/flow/flow/Documents/simon-bogutzky/data")
}

# Load libraries
library(flow)

# Set properties
root.data.path    <- "./cleaned-data/"
first.name        <- "Barbara"
last.name         <- "Grueter"
date.of.birth     <- "1950-04-15"
activity          <- "Walking"

# Load all file names
data.path           <- paste(root.data.path, tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/",  sep = "")
fss.data.file.names <- list.files(path = data.path, pattern = "fss-data.csv", recursive = T)

# Create fss feature data frame
fss.features  <- data.frame()

# Fill fss features data frame
for (fss.data.file.name in fss.data.file.names) {
  
  # Load fss data
  fss.data  <- read.csv(paste(data.path, fss.data.file.name, sep = ""), header = T) 
  
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
output.directory <- "./features"
if(!file.exists(output.directory)) {
  dir.create(output.directory, recursive = TRUE)
}

# Write to csv file
if(file.exists("./features/fss-features.csv")) {
  features <- read.csv("./features/fss-features.csv", stringsAsFactors = FALSE)
  write.csv(unique(rbind(features, fss.features)), "../data/features/fss-features.csv", row.names = FALSE)
} else {
  write.csv(fss.features, "./features/fss-features.csv", row.names = FALSE)
}