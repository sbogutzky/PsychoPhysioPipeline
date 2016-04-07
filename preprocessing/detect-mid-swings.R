# Version 2.0

# Remove all variables
rm(list = ls(all = T))  

# Load libraries
require(flow)

# Load user properties
source("./code-snippets/load-user-properties.R")

data.file.names <- c("imu-rn42-3b70", "imu-rn42-bc98")
activity <- "Laufen"

# Loop user properties
for (i in 1:length(first.names)) {
  
  first.name <- first.names[i]
  last.name <- last.names[i]
  source("./code-snippets/load-directories.R")
  
  # Loop data files
  for (data.file.name in data.file.names) {
    
    # Load all self report file names
    self.report.file.names <- list.files(path = input.data.directory, pattern = "self-report.csv", recursive = T)
    
    # Loop self reports
    for (self.report.file.name in self.report.file.names) {
      source("./code-snippets/extract-session-start.R")
  
      # Load self report data
      self.report.data <- read.csv(paste(input.data.directory, self.report.file.name, sep = ""), comment.char = "#")
  
      # Loop measurements
      for(j in 1:nrow(self.report.data)) {
        source("./code-snippets/extract-self-report-times.R")
        file.name <- as.POSIXct(gsub(pattern = "# StartTime: ", replacement = "", x = start.time.line, ignore.case = T))
        data.path <- paste(preprocessed.data.directory, activity.directory, user.directory, date.directory, data.file.name, "-", j,  ".csv", sep = "")
        if(file.exists(data.path)) {
      
          # Load motion data
          data <- read.csv(data.path)
      
          # # Resample data
          # fs <- 512
          # M <- ResampleData(data[, 2:7], fs, data$timestamp.ms)
          # data.1 <- data.frame(M)
          # colnames(data.1) <- colnames(data)
          # data <- data.1
      
          # Detect Midswings
          mid.swing.indexes <- DetectMidSwings(data$timestamp.ms/1000, data$angular.velocity.x.deg.s)
      
          # Remove changes below 50 deg per second
          mid.swing.indexes <- mid.swing.indexes[data$angular.velocity.x.deg.s[mid.swing.indexes] > 50 | data$angular.velocity.x.deg.s[mid.swing.indexes] < -50]
      
          if(length(mid.swing.indexes) > 0) {
      
            # Detect outliers
            interval.t.s <- diff(data$timestamp.ms[mid.swing.indexes] / 1000)
            interval.t.s <- c(mean(interval.t.s), interval.t.s)
            anomaly <- DetectAnomaly(interval.t.s, data$angular.velocity.x.deg.s[mid.swing.indexes] / 100 , "Cycle Interval (s)", expression("Angular Velocity (x"~10^2~deg/s~")"), c(min(interval.t.s), max(interval.t.s)), c(min(data$angular.velocity.x.deg.s[mid.swing.indexes] / 100), max(data$angular.velocity.x.deg.s[mid.swing.indexes] / 100)), epsilon = 0)
            if(length(anomaly$outliers) > 0) {
              mid.swing.indexes <- mid.swing.indexes[-anomaly$outliers]
            }
            readline("Press return to continue > ")
      
            # Visual check
            mid.swing.indexes <- CheckMidSwings(data$timestamp.ms / 1000, data$angular.velocity.x.deg.s, mid.swing.indexes)
      
            # Write to csv file
            output.directory <- paste(preprocessed.data.directory, activity.directory, user.directory, date.directory, sep = "")
            if(!file.exists(substr(output.directory, 1, nchar(output.directory) - 1))) {
              dir.create(output.directory, recursive = T)
            }
            output.file.name <- paste(data.file.name, "-mid-swing-indexes-", j, ".csv", sep = "")
            output.directory <- paste(output.directory, output.file.name, sep = "")
            output.data <- data.frame(timestamp.ms = data$timestamp.ms[mid.swing.indexes], row = mid.swing.indexes)
            write.csv(output.data, output.directory, row.names = F)
        
            print(paste(last.name, ", ", first.name, " / First step: ", round(min(output.data$timestamp.ms / 1000)), "s", sep = ""))
            print(paste(last.name, ", ", first.name, " / Last step: ", round(max(output.data$timestamp.ms / 1000)), "s", sep = ""))
            print(paste("Wrote:", output.directory))
            
          } else {
            print("No mid swings detected")
          } 
          readline("Press return to continue > ")
        } else {
          print(paste("No file found:", data.path))
        }
      } # measurements
    } # self reports
  } # data files
} # user properties
