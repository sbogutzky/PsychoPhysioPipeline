# Version 2.0

# Remove all variables
rm(list = ls(all = T))  

# Load libraries
library(flow)
library(multilevel)

# Read root data directory
root.directory <- readline("Type in root directory and press return to continue (with: /) > ")

# Read properties
first.name <- readline("Type in first name and press return to continue > ")
last.name <- readline("Type in last name and press return to continue > ")
date.of.birth <- readline("Type in date of birth and press return to continue (format: YYYY-MM-dd) > ")
activity <- readline("Type in activity and press return to continue > ")

# Set directories
raw.data.directory <- paste(root.directory, "raw-data/", sep = "")
feature.directory <- paste(root.directory, "features/", sep = "")
activity.directory <- paste(tolower(activity), "/",  sep = "")
user.directory <- paste(tolower(last.name), "-", tolower(first.name), "/",  sep = "")
input.data.directory <- paste(raw.data.directory, activity.directory, user.directory, sep = "")

# Load all self report file names
self.report.file.names <- list.files(path = input.data.directory, pattern = "self-report.csv", recursive = T)

# Create fss feature and measurement data frame
fss.features <- data.frame()
fss.measurements <- data.frame()

for (self.report.file.name in self.report.file.names) {
  
  # Load self report data
  self.report.data <- read.csv(paste(input.data.directory, self.report.file.name, sep = ""), comment.char = "#") 
  
  # Loop measurements
  for(i in 1:nrow(self.report.data)) {
    
    # Extract times
    if(i == 1) {
      activity.start.ms <- 0
    } else {
      activity.start.ms <- self.report.stop.ms
    }
    
    activity.end.stop.ms <- self.report.data$timestamp.start.ms[i]
    self.report.stop.ms <- self.report.data$timestamp.stop.ms[i]
    
    # Calculate fss factors
    fss.measurement <- c(as.numeric(self.report.data[i, 7:15]), 0, 0, 0, as.numeric(self.report.data[i, 16:18]))
    fss.factors <- CalculateFlowShortScaleFactors(fss.measurement)
    
    # Add fss features
    fss.features <- rbind(fss.features, data.frame(round(fss.factors[c(1, 3, 5, 7, 9, 11)], 2), activity, activity.start.ms, activity.end.stop.ms, self.report.stop.ms, measurement = i, last.name, first.name, date.of.birth))
    fss.measurements <- rbind(fss.measurements, fss.measurement)
  }
}

print(paste("Cronbach's Alpha Flow: ", multilevel::cronbach(fss.measurements[, 1:10])$Alpha))
print(paste("Cronbach's Alpha Fluency: ", multilevel::cronbach(fss.measurements[, c(8,7,9,4,5,2)])$Alpha))
print(paste("Cronbach's Alpha Absorption: ", multilevel::cronbach(fss.measurements[, c(6,1,10,3)])$Alpha))

# Write to csv file
output.directory <- paste(feature.directory, activity.directory, user.directory, sep = "")
if(!file.exists(substr(output.directory, 1, nchar(output.directory) - 1))) {
  dir.create(output.directory, recursive = T)
}
output.directory <- paste(output.directory, "fss-features.csv", sep = "")
write.csv(fss.features, output.directory, row.names = F)
print(paste("Worte to", output.directory))
