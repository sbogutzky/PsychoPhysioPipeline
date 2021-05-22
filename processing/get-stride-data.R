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

# User input
root.directory.path <- readline("Source data directory (with / at the end) > ") 
first.name <- readline("First name of the participant > ")
last.name <- readline("Last name of the participant > ")
activity <- readline("Activity of the session > ")
kinematic.data.file.name <- readline("Filename of the file with kinematic data (without .csv) > ")
angular.velocity.offset <- as.numeric(readline("Threshold for impossible detection (+/- deg/s) > "))

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
    
    if(file.exists(kinematic.data.file.path)) {
      
      # Load motion data
      kinematic.data <- read.csv(kinematic.data.file.path)
      
      fs <- ComputeSamplingRate(kinematic.data[, 1])
      print("---")
      print(paste("Sampling rate:", round(fs, 2), "Hz"))
      
      # Detect Midswings
      cf <- ComputeOptimalCutoffFrequency(kinematic.data[, 5], fs, 4)
      mid.swing.indexes <- DetectMidSwings(kinematic.data[, 1] / 1000, kinematic.data[, 5], fs = fs, cf.1 = cf, cf.2 = cf/3)
      
      # Remove changes below offset
      mid.swing.indexes <- mid.swing.indexes[kinematic.data[mid.swing.indexes, 5] > angular.velocity.offset | kinematic.data[mid.swing.indexes, 5] < -angular.velocity.offset]
      
      if(length(mid.swing.indexes) > 0) {

        # Detect outliers
        source("./code-snippets/translate.R")
        stride.per.minute <- 60 / diff(kinematic.data[mid.swing.indexes, 1] / 1000)
        stride.per.minute <- c(mean(stride.per.minute), stride.per.minute)
        anomaly <- DetectAnomaly(stride.per.minute, kinematic.data[mid.swing.indexes, 5], epsilon = 0, xlab = "Mean Stride (1/min)", ylab = ReturnFieldLabels(colnames(kinematic.data)[5]), xlim = c(min(stride.per.minute, na.rm = TRUE), max(stride.per.minute, na.rm = TRUE)), ylim = c(min(kinematic.data[mid.swing.indexes, 5]), max(kinematic.data[mid.swing.indexes, 5])), pch = 21, bg = rgb(229/255, 66/255, 66/255))
        if(length(anomaly$outliers) > 0) {
          mid.swing.indexes <- mid.swing.indexes[-anomaly$outliers]
        }
        readline("Next > ")
        
        # Compute mean stride
        stride.per.minute <- c(NA, 60 / diff(kinematic.data[mid.swing.indexes, 1] / 1000))
        mean.nn <- mean(stride.per.minute, na.rm = TRUE)
        print("---")
        print(paste("Mean stride:", round(mean.nn, 2), "1/min"))
        sd.nn <- sd(stride.per.minute, na.rm = TRUE)
        print(paste("Mean stride (SD):", round(sd.nn, 2), "1/min"))
       
        # Check outliers manual
        outliers <- which(stride.per.minute < mean.nn - sd.nn * 2.5)
        n.outlier <- length(outliers)
        new.indexes <- c()
        if (n.outlier > 0) {
          for (j in 1:n.outlier) {
            outlier <- outliers[j]
            m <- outlier - 7; if (m < 0) m <- 0
            n <- outlier + 7; if (n > length(mid.swing.indexes)) n <- length(mid.swing.indexes)
            par(mfcol = c(2, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
            plot(kinematic.data[min(mid.swing.indexes[m:n]):max(mid.swing.indexes[m:n]), 1] / 1000, kinematic.data[min(mid.swing.indexes[m:n]):max(mid.swing.indexes[m:n]), 5], xlab = "Time (s)", ylab = ReturnFieldLabels(colnames(kinematic.data)[5]), xaxs = "i", type = "l", main = paste("Add control", j, "of", n.outlier))
            points(kinematic.data[mid.swing.indexes[m:n], 1] / 1000, kinematic.data[mid.swing.indexes[m:n], 5], pch = 23, bg = rgb(0/255, 152/255, 199/255))
            # Add mid swings
            
            add <- identify(kinematic.data[, 1] / 1000, kinematic.data[, 5], plot = FALSE)
            if(length(add) > 0) {
              new.indexes <- c(new.indexes, add)
              plot(kinematic.data[min(mid.swing.indexes[m:n]):max(mid.swing.indexes[m:n]), 1] / 1000, kinematic.data[min(mid.swing.indexes[m:n]):max(mid.swing.indexes[m:n]), 5], xlab = "Time (s)", ylab = ReturnFieldLabels(colnames(kinematic.data)[5]), xaxs = "i", type = "l", main = "added")
              points(kinematic.data[mid.swing.indexes[m:n], 1] / 1000, kinematic.data[mid.swing.indexes[m:n], 5], pch = 23, bg = rgb(0/255, 152/255, 199/255))
              points(kinematic.data[add, 1] / 1000, kinematic.data[add, 5], pch = 23, bg = rgb(0/255, 152/255, 199/255))
              readline("Next > ")
            } else {
              frame()
            }
          }
          if(length(new.indexes) > 0) mid.swing.indexes <- sort(c(mid.swing.indexes, new.indexes))
          rm(outlier, m, n, add, j)
        }
        rm(outliers, n.outlier, new.indexes)
        
        stride.per.minute <- c(NA, 60 / diff(kinematic.data[mid.swing.indexes, 1] / 1000))
        mean.nn <- mean(stride.per.minute, na.rm = TRUE)
        sd.nn <- sd(stride.per.minute, na.rm = TRUE)
        
        outliers <- which(stride.per.minute > mean.nn + sd.nn * 2.5)
        n.outlier <- length(outliers)
        real.outlier <- c()
        if (n.outlier > 0) {
          for (j in 1:n.outlier) {
            outlier <- outliers[j]
            m <- outlier - 7; if (m < 0) m <- 0
            n <- outlier + 7; if (n > length(mid.swing.indexes)) n <- length(mid.swing.indexes)
            par(mfrow = c(2, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
            plot(kinematic.data[min(mid.swing.indexes[m:n]):max(mid.swing.indexes[m:n]), 1] / 1000, kinematic.data[min(mid.swing.indexes[m:n]):max(mid.swing.indexes[m:n]), 5], xlab = "Time (s)", ylab = ReturnFieldLabels(colnames(kinematic.data)[5]), xaxs = "i", type = "l", main = paste("Remove control", j, "of", n.outlier))
            points(kinematic.data[mid.swing.indexes[m:n], 1] / 1000, kinematic.data[mid.swing.indexes[m:n], 5], pch = 23, bg = rgb(0/255, 152/255, 199/255))
            
            # Remove selected mid swings
            remove <- identify(kinematic.data[mid.swing.indexes, 1] / 1000, kinematic.data[mid.swing.indexes, 5], plot = FALSE)
            if(length(remove) > 0) {
              plot(kinematic.data[min(mid.swing.indexes[m:n]):max(mid.swing.indexes[m:n]), 1] / 1000, kinematic.data[min(mid.swing.indexes[m:n]):max(mid.swing.indexes[m:n]), 5], xlab = "Time (s)", ylab = ReturnFieldLabels(colnames(kinematic.data)[5]), xaxs = "i", type = "l", main = "removed")
              points(kinematic.data[mid.swing.indexes[-remove], 1] / 1000, kinematic.data[mid.swing.indexes[-remove], 5], pch = 23, bg = rgb(0/255, 152/255, 199/255))
              real.outlier <- c(real.outlier, remove)
              readline("Next > ")
            } else {
              frame()
            }
          }
          if(length(real.outlier) > 0) mid.swing.indexes <- mid.swing.indexes[-real.outlier]
          rm(outlier, m, n, remove, j)
        }
        rm(outliers, n.outlier, real.outlier)
        
        # Compute mean stride
        stride.per.minute <- c(NA, 60 / diff(kinematic.data[mid.swing.indexes, 1] / 1000))
        mean.nn <- mean(stride.per.minute, na.rm = TRUE)
        print("---")
        print(paste("Mean stride:", round(mean.nn, 2), "1/min"))
        sd.nn <- sd(stride.per.minute, na.rm = TRUE)
        print(paste("Mean stride (SD):", round(sd.nn, 2), "1/min"))
        
        # Plot kinematic data
        par(mfcol = c(1, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
        source("./code-snippets/translate.R")
        plot(kinematic.data[, 1], kinematic.data[, 5], xlab = "Time (s)", ylab = ReturnFieldLabels(colnames(kinematic.data)[5]), xaxs = "i", type = "l")
        points(kinematic.data[mid.swing.indexes, 1], kinematic.data[mid.swing.indexes , 5], pch = 23, bg = rgb(0/255, 152/255, 199/255))
        grid()
        zm()
        
        # Remove pauses
        stride.data <- data.frame(timestamp.ms = round(kinematic.data[mid.swing.indexes, 1], 3), nn.interval.ms = c(NA, round(diff(kinematic.data[mid.swing.indexes, 1]), 3)))
        par(mfrow = c(2, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
        plot(stride.data[, 1] / 1000, 60 / (stride.data[, 2] / 1000), xlab = "Time (s)", ylab = "Mean Stride (1/min)", xaxs = "i", type = "l")
        grid()
        stride.data <- stride.data[stride.data[, 2] < 1300 | is.na(stride.data[, 2]), ]
        plot(stride.data[, 1] / 1000, 60 / (stride.data[, 2] / 1000), xlab = "Time (s)", ylab = "Mean Stride (1/min)", xaxs = "i", type = "l")
        grid()
        zm()

        first.step <- round(min(stride.data[, 1] / 1000))
        last.step <- round(max(stride.data[, 1] / 1000))
        step.duration <- last.step - first.step
        
        print("---")
        print(paste("First stride: ", sprintf("%02d", trunc(first.step / 60)), ":", sprintf("%02d", trunc(first.step %% 60)), " (", first.step, " s)", sep = ""))
        print(paste("Duration: ", sprintf("%02d", trunc(step.duration / 60)), ":", sprintf("%02d", trunc(step.duration %% 60)), " (", step.duration, " s)", sep = ""))
        print(paste("Last stride: ", sprintf("%02d", trunc(last.step / 60)), ":", sprintf("%02d", trunc(last.step %% 60)), " (", last.step, " s)", sep = ""))

        # Write to csv file
        if(!dir.exists(paste(processed.data.directory.path, date.directory, sep =""))) {
          dir.create(paste(processed.data.directory.path, date.directory, sep =""), recursive = T)
        }
        write.csv(stride.data, paste(processed.data.directory.path, date.directory, kinematic.data.file.name, "-stride-data-", i, ".csv", sep = ""), row.names = F)
        
        print("---")
        print(paste("Wrote", paste(kinematic.data.file.name, "-stride-data-", i, ".csv", sep = ""), "in", paste(processed.data.directory.path, date.directory, sep ="")))

      } else {
        print("---")
        print("No 'MS' events found.")
      } 
      
    } else {
      print("---")
      print(paste("File not found:", kinematic.data.file.path))
    }
  }
}
