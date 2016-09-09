# The MIT License (MIT)
# Copyright (c) 2016 Simon Bogutzky
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

# Remove all variables
rm(list = ls(all = T))  

# Load libraries
library(flow)

# Set working directory
setwd("~/psychophysiopipeline/processing")

# User input
root.directory.path <- readline("Quellverzeichnis > ")
first.name <- readline("Vorname der Untersuchungsperson > ")
last.name <- readline("Nachname der Untersuchungsperson > ")
date.of.birth <- readline("Geburtsdatum der Untersuchungsperson (Format: YYYY-MM-dd) > ")
activity <- readline("Aktivität der Untersuchung > ")

# Set directory paths
source("./code-snippets/set-directory-paths.R")

# Create fss feature and measurement data frame
fss.features <- data.frame()
fss.measurements <- data.frame()

# List self report names
self.report.file.names <- list.files(path = raw.data.directory.path, pattern = "self-report.csv", recursive = TRUE)

for (self.report.file.name in self.report.file.names) {
  
  source("./code-snippets/get-session-start.R")
  
  # Load self report data
  self.report.data <- read.csv(paste(raw.data.directory.path, self.report.file.name, sep = ""), comment.char = "#")
  
  # Loop measurements
  for(i in 1:nrow(self.report.data)) {
    
    source("./code-snippets/get-self-report-times.R")
    
    # Calculate fss dimensions
    fss.measurement <- as.numeric(self.report.data[i, 4:19])
    fss.dimensions <- ComputeFlowShortScaleDimensions(fss.measurement)
    
    # Add fss features
    fss.features <- rbind(fss.features, data.frame(round(fss.dimensions[c(1, 3, 5, 7, 9:12)], 2), session.start, activity, activity.start.ms, activity.end.ms, self.report.end.ms, measurement = i, last.name, first.name, date.of.birth))
    fss.measurements <- rbind(fss.measurements, fss.measurement)
  }
}

# Write to csv file
if(!dir.exists(feature.directory.path)) {
  dir.create(feature.directory.path, recursive = T)
}
write.csv(fss.features, paste(feature.directory.path, "fss-features.csv", sep = ""), row.names = F)

print("---")
print(paste("fss-features.csv in", feature.directory.path, "geschrieben."))

# Clean up
rm(fss.dimensions, self.report.data, activity, activity.end.ms, activity.start.ms, date.of.birth, first.name, fss.measurement, i, last.name, self.report.end.ms, self.report.file.name, self.report.file.names, session.start)