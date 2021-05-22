# The MIT License (MIT)
# Copyright (c) 2016 University of Applied Sciences Bremen
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software
# and associated documentation files (the "Software"), to deal in the Software without restriction,
# including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
# DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
# ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

# Version 2.0

# !!! Set working directory to file directory

# Remove all variables
rm(list = ls(all = T))  

# Load libraries
library(flow)
library(zoom)
library(signal)

# User input
root.directory.path <- readline("Source data directory (with / at the end) > ") 
first.name <- readline("First name of the participant > ")
last.name <- readline("Last name of the participant > ")
activity <- readline("Activity of the session > ")
kinematic.data.file.name <- readline("Filename of the file with kinematic data (without .csv) > ")
cut.off.frequency.x <- as.numeric(readline("Cutoff frequency for the acceleration along the X-axis (Hz) > "))
cut.off.frequency.y <- as.numeric(readline("Cutoff frequency for the acceleration along the Y-axis (Hz) > "))
cut.off.frequency.z <- as.numeric(readline("Cutoff frequency for the acceleration along the Z-axis (Hz) > "))

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
      print(paste("Mean stride:", round(mean.stride, 2), "1/min"))
      print(paste("Mean jerk-cost:", round(mean.jerk.cost, 2), "m^2/s^-5"))
      
      # Write to csv file
      if(!dir.exists(paste(processed.data.directory.path, date.directory, sep =""))) {
        dir.create(paste(processed.data.directory.path, date.directory, sep =""), recursive = T)
      }
      write.csv(jerk.cost.data, paste(processed.data.directory.path, date.directory, kinematic.data.file.name, "-jerk-cost-data-", i, ".csv", sep = ""), row.names = F)
      
      print("---")
      print(paste("Wrote", paste(kinematic.data.file.name, "-jerk-cost-data-", i, ".csv", sep = ""), "in", paste(processed.data.directory.path, date.directory, sep ="")))
  
    } else {
      print("---")
      print(paste("File not found:", kinematic.data.file.path))
      print("or")
      print(paste("File not found:", stride.data.file.path))
    }
  }
}