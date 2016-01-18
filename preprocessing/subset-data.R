# Version 2.0

# Remove all variables
rm(list = ls(all = T))  

# Load libraries
library(flow)
library(multilevel)
library(zoom)

source("./code-snippets/read-set-load.R")

data.file.name <- readline("Type in data file name and press return to continue > ")

for (self.report.file.name in self.report.file.names) {
  
  source("./code-snippets/extract-session-start.R")
  
  # Load self report data
  self.report.data <- read.csv(paste(input.data.directory, self.report.file.name, sep = ""), comment.char = "#")
  
  # Load data
  data <- read.csv(paste(input.data.directory, date.directory, data.file.name, sep = ""), comment.char = "#")
  
  # Loop measurements
  for(i in 1:nrow(self.report.data)) {
    
    source("./code-snippets/extract-self-report-times.R")
  
    # Subset data
    data.subset <- data[activity.start.ms <= data$timestamp.ms & data$timestamp.ms < activity.end.ms, ]
    
    # Check for dublicates
    duplicates <- which(duplicated(data.subset))
    if(length(duplicates) > 0) {
      data.subset <- data.subset[-duplicates, ]
      print(paste(length(duplicates), "removed duplicates in ", paste(input.data.directory, date.directory, data.file.name, sep = ""), i))
      readline("Press return to continue > ")
    }
    
    # Check sampling rate
    duration.s <- ((data.subset$timestamp.ms[nrow(data.subset)] - data.subset$timestamp.ms[1]) / 1000)
    fs <- nrow(data.subset) / duration.s
    print(paste("Sampling rate:", round(fs, 2), "Hz"))
    print(paste("Duration:", round(duration.s, 2), "s"))
    plot(data.subset$timestamp.ms[-1] / 1000, diff(data.subset$timestamp.ms / 1000), xlab = "Timestamp (s)", ylab = "Interval (s)")
    title(paste(strftime(session.start, format="%Y/%m/%d %H:%M"), " #", i, sep = ""))
    session.zoom()

    # Check data
    for(j in 2:ncol(data.subset)) {
      plot(data.subset$timestamp.ms / 1000, data.subset[, j], type = "l", xlab = "Timestamp (s)", ylab = GetDataLabel(colnames(data.subset)[j]))
      title(paste(strftime(session.start, format="%Y/%m/%d %H:%M"), " #", i, sep = ""))
      session.zoom()
    }
    
    # Write to csv file
    output.directory <- paste(preprocessed.data.directory, activity.directory, user.directory, date.directory, sep = "")
    if(!file.exists(substr(output.directory, 1, nchar(output.directory) - 1))) {
      dir.create(output.directory, recursive = T)
    }
    output.file.name <- gsub(pattern = ".csv", replacement = paste("-", i, ".csv", sep = ""), x = data.file.name, ignore.case = T)
    output.directory <- paste(output.directory, output.file.name, sep = "")
    write.csv(data.subset, output.directory, row.names = F)
    print(paste("Worte:", output.directory))
  }
}