# Version 2.0

# Remove all variables
rm(list = ls(all = T))  

# Load libraries
library(flow)
library(zoom)
library(signal)

# Set working directory
setwd("~/psychophysiopipeline/processing")

# User input
root.directory.path <- readline("Quellverzeichnis > ") 
first.name <- readline("Vorname der Untersuchungsperson > ")
last.name <- readline("Nachname der Untersuchungsperson > ")
activity <- readline("Aktivität der Untersuchung > ")
kinematic.data.file.name <- readline("Dateiname der Datei mit kinematischen Daten (ohne .csv) > ")
cut.off.frequency.x <- as.numeric(readline("Grenzfrequenz für die Beschleunigung entlang der X-Achse (Hz) > "))
cut.off.frequency.y <- as.numeric(readline("Grenzfrequenz für die Beschleunigung entlang der Y-Achse (Hz) > "))
cut.off.frequency.z <- as.numeric(readline("Grenzfrequenz für die Beschleunigung entlang der Z-Achse (Hz) > "))

# Set directory paths
source("./code-snippets/set-directory-paths.R")

# List self report names
self.report.file.names <- list.files(path = raw.data.directory.path, pattern = "self-report.csv", recursive = TRUE)

for (self.report.file.name in self.report.file.names) {
  
  source("./code-snippets/get-session-start.R")
  
  # Load self report data
  self.report.data <- read.csv(paste(raw.data.directory.path, self.report.file.name, sep = ""), comment.char = "#")
  
  # Loop measurements
  for(i in 1:nrow(self.report.data)) {
    
    source("./code-snippets/get-self-report-times.R")
    
    kinematic.data.file.path <- paste(processed.data.directory.path, date.directory, kinematic.data.file.name, "-", i,  ".csv", sep = "")
    
    stride.data.file.path <- paste(processed.data.directory.path, date.directory, kinematic.data.file.name, "-stride-data-", i,  ".csv", sep = "")
    
    if(file.exists(kinematic.data.file.path) & file.exists(stride.data.file.path)) {
      
      # Load kinematic data and stride data
      kinematic.data <- read.csv(kinematic.data.file.path)
      stride.data <- read.csv(stride.data.file.path)
      
      # Compute sampling rate
      fs <- ComputeSamplingRate(kinematic.data[, 1])
      
      # Smooth acceleration
      fn <- fs/2
      n <- 4
      
      fc <- cut.off.frequency.x
      W <- fc/fn
      lp <- butter(n, W)
      acceleration.x <- filtfilt(lp, kinematic.data[, 2])
      
      fc <- cut.off.frequency.y
      W <- fc/fn
      lp <- butter(n, W)
      acceleration.y <- filtfilt(lp, kinematic.data[, 3])
      
      fc <- cut.off.frequency.z
      W <- fc/fn
      lp <- butter(n, W)
      acceleration.z <- filtfilt(lp, kinematic.data[, 4])
      rm(fs, fn, n, fc, W, lp)
      
      jerk.cost <- c(NA)
      for(j in 1:(length(stride.data[, 1]) - 1)) {
        stride.indexes <- which(stride.data[j, 1] <= kinematic.data[, 1] & kinematic.data[, 1] < stride.data[j + 1, 1])
        if(stride.data[j + 1, 2] < 13000) {
          jerk.cost <- c(jerk.cost, ComputeJerkCost(kinematic.data[, 1][stride.indexes] / 1000, data.frame(acceleration.x[stride.indexes], acceleration.y[stride.indexes], acceleration.z[stride.indexes]), normalized = T))
        } else {
          jerk.cost <- c(jerk.cost, NA)
        }
      }
      jerk.cost.data <- data.frame(timestamp.ms = stride.data[, 1], jerk.cost.m2s5 = round(jerk.cost, 3))
      rm(j, acceleration.x, acceleration.y, acceleration.z)
      
      # Compute stride features
      mean.stride <- mean(60 / (stride.data[, 2] / 1000), na.rm = TRUE)
      mean.jerk.cost <- mean(jerk.cost.data[, 2], na.rm = TRUE)
      
      # Print stride features
      print("---")
      print(paste("Mittlerer Doppelschritt:", round(mean.stride, 2), "1/min"))
      print(paste("Mittlerer Bewegungsaufwand:", round(mean.jerk.cost, 2), "m^2/s^-5"))
      
      # Write to csv file
      if(!dir.exists(paste(processed.data.directory.path, date.directory, sep =""))) {
        dir.create(paste(processed.data.directory.path, date.directory, sep =""), recursive = T)
      }
      write.csv(jerk.cost.data, paste(processed.data.directory.path, date.directory, kinematic.data.file.name, "-jerk-cost-data-", i, ".csv", sep = ""), row.names = F)
      
      print("---")
      print(paste(paste(kinematic.data.file.name, "-jerk-cost-data-", i, ".csv", sep = ""), "in", paste(processed.data.directory.path, date.directory, sep =""), "geschrieben."))
  
    } else {
      print("---")
      print(paste("Datei nicht gefunden:", kinematic.data.file.path))
      print("oder")
      print(paste("Datei nicht gefunden:", stride.data.file.path))
    }
  }
}