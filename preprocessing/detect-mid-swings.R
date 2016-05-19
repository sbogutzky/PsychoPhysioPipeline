# Version 2.0

# Remove all variables
rm(list = ls(all = T))  

# Load libraries
require(flow)

# Set working directory
setwd("~/psychophysiopipeline/preprocessing")

source("./code-snippets/read-set-load.R")

# Set input variables
data.file.name <- readline("Type in data file name and press return to continue > ")
sampling.rate <- as.numeric(readline("Type in sampling rate and press return to continue > "))
angular.velocity.offset <- as.numeric(readline("Type in angular velocity offset and press return to continue > "))

for (self.report.file.name in self.report.file.names) {
  
  source("./code-snippets/extract-session-start.R")
  
  # Load self report data
  self.report.data <- read.csv(paste(input.data.directory, self.report.file.name, sep = ""), comment.char = "#")
  
  # Loop measurements
  for(i in 1:nrow(self.report.data)) {
    
    source("./code-snippets/extract-self-report-times.R")
    
    file.name <- as.POSIXct(gsub(pattern = "# StartTime: ", replacement = "", x = start.time.line, ignore.case = T))
    data.path <- paste(preprocessed.data.directory, activity.directory, user.directory, date.directory, data.file.name, "-", i,  ".csv", sep = "")
    
    if(file.exists(data.path)) {
      
      # Load motion data
      data.1 <- read.csv(data.path)
      
      # Detect Midswings
      mid.swing.indexes <- DetectMidSwings(data.1[, 1] / 1000, data.1[, 5], fs = sampling.rate)
      
      # Remove changes below 50 deg per second
      mid.swing.indexes <- mid.swing.indexes[data.1[mid.swing.indexes, 5] > angular.velocity.offset | data.1[mid.swing.indexes, 5] < -angular.velocity.offset]
      
      if(length(mid.swing.indexes) > 0) {
      
        # Detect outliers
        interval.t.s <- diff(data.1[mid.swing.indexes, 1] / 1000)
        interval.t.s <- c(mean(interval.t.s), interval.t.s)
        anomaly <- DetectAnomaly(interval.t.s, data.1[mid.swing.indexes, 5] / 100 , "Cycle Interval (s)", expression("Angular Velocity (x"~10^2~deg/s~")"), c(min(interval.t.s), max(interval.t.s)), c(min(data.1[mid.swing.indexes, 5] / 100), max(data.1[mid.swing.indexes, 5] / 100)), epsilon = 0)
        if(length(anomaly$outliers) > 0) {
          mid.swing.indexes <- mid.swing.indexes[-anomaly$outliers]
        }
        readline("Press return to continue > ")
      
        # Visual check
        mid.swing.indexes <- CheckMidSwings(data.1[, 1] / 1000, data.1[, 5], mid.swing.indexes)
      
        # Write to csv file
        output.directory <- paste(preprocessed.data.directory, activity.directory, user.directory, date.directory, sep = "")
        if(!file.exists(substr(output.directory, 1, nchar(output.directory) - 1))) {
          dir.create(output.directory, recursive = T)
        }
        
        output.file.name <- paste(data.file.name, "-mid-swing-indexes-", i, ".csv", sep = "")
        output.directory <- paste(output.directory, output.file.name, sep = "")
        output.data <- data.frame(timestamp.ms = data.1[mid.swing.indexes, 1], row = mid.swing.indexes)
        write.csv(output.data, output.directory, row.names = F)
        
        print(paste("Wrote:", output.directory))
        
        
        first.step <- round(min(output.data[, 1] / 1000))
        last.step <- round(max(output.data[, 1] / 1000))
        step.duration <- last.step - first.step
        print(paste("First step: ", sprintf("%02d", trunc(first.step / 60)), ":", sprintf("%02d", trunc(first.step %% 60)), " (", first.step, " s)", sep = ""))
        print(paste("Duration  : ", sprintf("%02d", trunc(step.duration / 60)), ":", sprintf("%02d", trunc(step.duration %% 60)), " (", step.duration, " s)", sep = ""))
        print(paste("Last step : ", sprintf("%02d", trunc(last.step / 60)), ":", sprintf("%02d", trunc(last.step %% 60)), " (", last.step, " s)", sep = ""))
      
      } else {
        print("No mid swings detected")
      } 
      
    } else {
      print(paste("No file found:", data.path))
    }
  }
}
