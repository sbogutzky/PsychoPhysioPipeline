# Version 2.0

# Remove all variables
rm(list = ls(all = T))  

# Load libraries
library(flow)
library(multilevel)

source("./code-snippets/read-set-load.R")

date.of.birth <- readline("Type in date of birth and press return to continue (format: YYYY-MM-dd) > ")

# Create fss feature and measurement data frame
fss.features <- data.frame()
fss.measurements <- data.frame()

for (self.report.file.name in self.report.file.names) {
  
  source("./code-snippets/extract-session-start.R")
  
  # Load self report data
  self.report.data <- read.csv(paste(input.data.directory, self.report.file.name, sep = ""), comment.char = "#") 
  
  # Loop measurements
  for(i in 1:nrow(self.report.data)) {
    
    source("./code-snippets/extract-self-report-times.R")
    
    # Calculate fss factors
    fss.measurement <- c(as.numeric(self.report.data[i, 7:16]), 0, 0, 0, as.numeric(self.report.data[i, 17:19]))
    fss.factors <- CalculateFlowShortScaleFactors(fss.measurement)
    
    # Add fss features
    fss.features <- rbind(fss.features, data.frame(round(fss.factors[c(1, 3, 5, 7, 9, 11)], 2), session.start, activity, activity.start.ms, activity.end.ms, self.report.end.ms, measurement = i, last.name, first.name, date.of.birth))
    fss.measurements <- rbind(fss.measurements, fss.measurement)
  }
}

print(paste("Cronbach's Alpha Flow: ", round(multilevel::cronbach(fss.measurements[, 1:10])$Alpha, 2)))
print(paste("Cronbach's Alpha Fluency: ", round(multilevel::cronbach(fss.measurements[, c(8,7,9,4,5,2)])$Alpha, 2)))
print(paste("Cronbach's Alpha Absorption: ", round(multilevel::cronbach(fss.measurements[, c(6,1,10,3)])$Alpha, 2)))

# Write to csv file
output.directory <- paste(feature.directory, activity.directory, user.directory, sep = "")
if(!file.exists(substr(output.directory, 1, nchar(output.directory) - 1))) {
  dir.create(output.directory, recursive = T)
}
output.directory <- paste(output.directory, "fss-features.csv", sep = "")
write.csv(fss.features, output.directory, row.names = F)
print(paste("Worte:", output.directory))