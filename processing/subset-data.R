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
library(zoom)

# Set working directory
setwd("~/psychophysiopipeline/processing")

# User input
root.directory.path <- readline("Quellverzeichnis > ") 
first.name <- readline("Vorname der Untersuchungsperson > ")
last.name <- readline("Nachname der Untersuchungsperson > ")
activity <- readline("Aktivität der Untersuchung > ")
data.file.name <- readline("Dateiname der Datendatei (ohne .csv) > ")
is.gps.data <- "JA" == readline("Sind die Daten GPS-Daten (JA/NEIN) > ")

# Set directory paths
source("./code-snippets/set-directory-paths.R")

# List self report names
self.report.file.names <- list.files(path = raw.data.directory.path, pattern = "self-report.csv", recursive = TRUE)

for (self.report.file.name in self.report.file.names) {
  
  source("./code-snippets/get-session-start.R")
  
  # Load self report data
  self.report.data <- read.csv(paste(raw.data.directory.path, self.report.file.name, sep = ""), comment.char = "#")
  
  # Load data
  data.1 <- read.csv(paste(raw.data.directory.path, date.directory, data.file.name, ".csv", sep = ""), comment.char = "#")
    
  # Loop measurements
  for(i in 1:nrow(self.report.data)) {
      
    source("./code-snippets/get-self-report-times.R")
      
    # Subset data
    data.1.subset <- data.1[activity.start.ms <= data.1[, 1] & data.1[, 1] < activity.end.ms, ]
      
    if(!is.gps.data) {
        
      # Check sampling rate
      duration.s <- (max(data.1.subset[, 1]) - min(data.1.subset[, 1])) / 1000
      fs <- ComputeSamplingRate(data.1.subset[, 1])
      print("---")
      print(paste("Abtastrate:", round(fs, 2), "Hz"))
      print(paste("Länge:", round(duration.s, 2), "s"))
      
      # Check data
      j <- 2
      if(ncol(data.1.subset) > 4) {
        j <- 5
      }
      source("./code-snippets/translate.R")
      par(mfcol = c(1, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
      plot(data.1.subset[, 1] / 1000, data.1.subset[, j], type = "l", xlab = "Zeit (s)", ylab = ReturnFieldLabels(colnames(data.1.subset)[j]), xaxs = "i")
      title(paste(strftime(session.start + activity.start.ms / 1000, format="%d.%m.%Y %H:%M"), " #", i, sep = ""))
      zm()
      readline("Weiter > ")
      
      # Write to csv file
      if(!dir.exists(paste(processed.data.directory.path, date.directory, sep =""))) {
        dir.create(paste(processed.data.directory.path, date.directory, sep =""), recursive = T)
      }
      write.csv(data.1.subset, paste(processed.data.directory.path, date.directory, data.file.name, "-", i, ".csv", sep = ""), row.names = F)
      
      print("---")
      print(paste(paste(data.file.name, "-", i, ".csv", sep = ""), "in", paste(processed.data.directory.path, date.directory, sep =""), "geschrieben."))
      
      # Clean up
      rm(j)
      
    } else {
      
      date.time <- session.start + data.1.subset[, 1] / 1000
      date.time <- format(date.time, "%Y-%m-%d %H:%M:%OS3")
      latitude  <- data.1.subset[, 2]
      longitude <- data.1.subset[, 3]
      altitude  <- data.1.subset[, 4]
      
      # Write to csv file
      if(!dir.exists(paste(processed.data.directory.path, date.directory, sep =""))) {
        dir.create(paste(processed.data.directory.path, date.directory, sep =""), recursive = T)
      }
      
      header <- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><kml xmlns=\"http://earth.google.com/kml/2.2\"><!-- TimeStamp is recommended for Point. Each Point represents a sample from a GPS. --><Document><name>Points with TimeStamps</name><Style id=\"paddle-a\"><IconStyle><Icon><href>http://maps.google.com/mapfiles/kml/paddle/A.png</href></Icon><hotSpot x=\"32\" y=\"1\" xunits=\"pixels\" yunits=\"pixels\"/></IconStyle></Style><Style id=\"paddle-b\"><IconStyle><Icon><href>http://maps.google.com/mapfiles/kml/paddle/B.png</href></Icon><hotSpot x=\"32\" y=\"1\" xunits=\"pixels\" yunits=\"pixels\"/></IconStyle></Style><Style id=\"hiker-icon\"><IconStyle><Icon><href>http://maps.google.com/mapfiles/ms/icons/hiker.png</href></Icon><hotSpot x=\"0\" y=\".5\" xunits=\"fraction\" yunits=\"fraction\"/></IconStyle></Style><Style id=\"check-hide-children\"><ListStyle><listItemType>checkHideChildren</listItemType></ListStyle></Style>"
      footer <- "</Document></kml>"
      
      write(header, paste(processed.data.directory.path, date.directory, data.file.name, "-", i, ".kml", sep = ""))
      write(paste("<Placemark><TimeStamp><when>", strftime(date.time, format = "%F", tz = "CET"), "T", strftime(date.time, format = "%T"), "Z</when></TimeStamp><styleUrl>#hiker-icon</styleUrl><Point><coordinates>", longitude, ",", latitude, ",", altitude, "</coordinates></Point></Placemark>", sep = ""), paste(processed.data.directory.path, date.directory, data.file.name, "-", i, ".kml", sep = ""), append = T)
      write(footer, paste(processed.data.directory.path, date.directory, data.file.name, "-", i, ".kml", sep = ""), append = T)
      
      print("---")
      print(paste(paste(data.file.name, "-", i, ".kml", sep = ""), "in", paste(processed.data.directory.path, date.directory, sep =""), "geschrieben."))
    
      # Clean up
      rm(header, footer)
    }
  }
}

# Clean up
rm(activity, activity.end.ms, activity.start.ms, first.name, i, last.name, self.report.end.ms, self.report.file.name, self.report.file.names, session.start)
