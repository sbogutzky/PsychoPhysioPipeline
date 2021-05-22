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

# User input
root.directory.path <- readline("Source data directory (with / at the end) > ") 
activity <- readline("Activity of the session > ")
kinematic.data.file.name <- readline("Filename of the file with kinematic data (without .csv) > ")

kinematic.data.file.paths <- list.files(path = paste(root.directory.path, "processed-data/", sep = ""), pattern = paste(kinematic.data.file.name, "-[0-9].csv", sep = ""), full.names = TRUE, recursive = TRUE)
optimal.cutoff.frequency.data <- c()
n <- length(kinematic.data.file.paths)

print("---")
for (i in 1:n) {
  
  kinematic.data.file.path <- kinematic.data.file.paths[i]
  
  # Load kinematic data
  kinematic.data <- read.csv(kinematic.data.file.path)
  
  # Compute sampling rate
  fs <- ComputeSamplingRate(kinematic.data[, 1])
  f.n <- 4

  # Compute optimal cut off frequencies
  optimal.cutoff.frequency.data <- c(optimal.cutoff.frequency.data, ComputeOptimalCutoffFrequency(kinematic.data[, 2], fs, f.n), ComputeOptimalCutoffFrequency(kinematic.data[, 3], fs, f.n), ComputeOptimalCutoffFrequency(kinematic.data[, 4], fs, f.n))

  print(paste("File", i, "of", n, "processed."))
}

optimal.cutoff.frequency.data <- matrix(optimal.cutoff.frequency.data, 3, i)
optimal.cutoff.frequencies <- rowMeans(optimal.cutoff.frequency.data, na.rm = T)

print("---")
print(paste("Cut off frequency for the acceleration along the X-axis:", round(optimal.cutoff.frequencies[1], 1), "Hz"))
print(paste("Cut off frequency for the acceleration along the Y-axis:", round(optimal.cutoff.frequencies[2], 1), "Hz"))
print(paste("Cut off frequency for the acceleration along the Z-axis:", round(optimal.cutoff.frequencies[3], 1), "Hz"))