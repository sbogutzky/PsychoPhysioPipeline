# Version 2.0

# Remove all variables
rm(list = ls(all = T))  

# Load libraries
library(flow)
library(multilevel)

# Load user properties
source("./code-snippets/load-user-properties.R")

# Set activities
activities <- c("Baseline", "Laufen")

# Loop activities
for (activity in activities) {
  
  # Create fss measurement data frame
  fss.measurements <- data.frame()

  # Loop user properties
  for (i in 1:length(first.names)) {
  
    first.name <- first.names[i]
    last.name <- last.names[i]
    date.of.birth <- dates.of.birth[i]
  
    source("./code-snippets/load-directories.R")
    
    # Load all self report file names
    self.report.file.names <- list.files(path = input.data.directory, pattern = "self-report.csv", recursive = T)

    # Create fss feature data frame
    fss.features <- data.frame()
    
    # Loop self reports
    for (self.report.file.name in self.report.file.names) {
  
      source("./code-snippets/extract-session-start.R")
  
      # Load self report data
      self.report.data <- read.csv(paste(input.data.directory, self.report.file.name, sep = ""), comment.char = "#") 
  
      # Loop measurements
      for(j in 1:nrow(self.report.data)) {
    
        source("./code-snippets/extract-self-report-times.R")
    
        # Calculate fss dimensions
        fss.measurement <- as.numeric(self.report.data[j, 4:19])
        fss.dimensions <- ComputeFlowShortScaleDimensions(fss.measurement)
    
        # Add fss features
        fss.features <- rbind(fss.features, data.frame(round(fss.dimensions[c(1, 3, 5, 7, 9, 11)], 2), session.start, activity, activity.start.ms, activity.end.ms, self.report.end.ms, measurement = j, last.name, first.name, date.of.birth))
        fss.measurements <- rbind(fss.measurements, fss.measurement)
        
        print(paste(last.name, ", ", first.name, " / ", activity, " - Flow ", ": ", round(fss.dimensions[1], 2), sep = ""))
        print(paste(last.name, ", ", first.name, " / ", activity, " - Fluency ", ": ", round(fss.dimensions[3], 2), sep = ""))
        print(paste(last.name, ", ", first.name, " / ", activity, " - Absorption ", ": ", round(fss.dimensions[5], 2), sep = ""))
        readline("Press return to continue > ")
        
      } # measurements
    } # self reports

    # Write to csv file
    output.directory <- paste(feature.directory, activity.directory, user.directory, sep = "")
    if(!file.exists(substr(output.directory, 1, nchar(output.directory) - 1))) {
      dir.create(output.directory, recursive = T)
    }
    output.directory <- paste(output.directory, "fss-features.csv", sep = "")
    write.csv(fss.features, output.directory, row.names = F)
    print(paste("Wrote:", output.directory))
  } # user properties
  # Print Cronbach's Alpha
  print(paste(activity, "- Cronbach's Alpha Flow", round(multilevel::cronbach(fss.measurements[, 1:10])$Alpha, 2)))
  print(paste(activity, "- Cronbach's Alpha Fluency:", round(multilevel::cronbach(fss.measurements[, c(8,7,9,4,5,2)])$Alpha, 2)))
  print(paste(activity, "- Cronbach's Alpha Absorption:", round(multilevel::cronbach(fss.measurements[, c(6,1,10,3)])$Alpha, 2)))
  readline("Press return to continue > ")
} # activities

