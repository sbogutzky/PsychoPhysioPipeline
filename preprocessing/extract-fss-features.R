# Remove all variables
rm(list = ls(all = T))  

# Set working directory
setwd("~/Entwicklung/bogutzky/repositories/non-disruptive-flow-measures/preprocessing")

# Load libraries
library(flow)

# Set properties
root.data.path    <- "../data/cleaned-data/"
first.name        <- "Patrick"
last.name         <- "Buse"
date.of.birth     <- "1984-05-05"
activity          <- "Baseline" # Running

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
      activity.start  <- as.POSIXct(strptime(substr(fss.data.file.name, 1, 20), "%Y-%m-%d--%H-%M-%S"), tz="CET")
      activity.end    <- as.POSIXct(fss.data[i, 1]/1000, origin="1970-01-01", tz="CET")
      inquiry.end     <- as.POSIXct(fss.data[i, 2]/1000, origin="1970-01-01", tz="CET")
    } else {
      activity.start  <- inquiry.end
      activity.end    <- as.POSIXct(fss.data[i, 1]/1000, origin="1970-01-01", tz="CET")
      inquiry.end     <- as.POSIXct(fss.data[i, 2]/1000, origin="1970-01-01", tz="CET")
    }
    
    # Calculate fss factors
    fss.factors     <- CalculateFlowShortScaleFactors(as.numeric(fss.data[i, 3:18]))
    
    # Add fss fss features
    fss.features    <- rbind(fss.features, data.frame(fss.factors, activity, activity.start, activity.end, inquiry.end, measurement = i, last.name, first.name, date.of.birth))
  }
}

# Write to csv file
if(file.exists("../data/features/fss-features.csv")) {
  features <- read.csv("../data/features/fss-features.csv", stringsAsFactors = FALSE)
  write.csv(rbind(features, fss.features), "../data/features/fss-features.csv", row.names = FALSE)
} else {
  write.csv(fss.features, "../data/features/fss-features.csv", row.names = FALSE)
}