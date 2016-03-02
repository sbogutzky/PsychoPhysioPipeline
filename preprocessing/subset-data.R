# Version 2.0

# Remove all variables
rm(list = ls(all = T))  

# Load libraries
library(flow)
library(zoom)

source("./code-snippets/read-set-load.R")

data.file.name <- readline("Type in data file name and press return to continue > ")

for (self.report.file.name in self.report.file.names) {
  
  source("./code-snippets/extract-session-start.R")
  
  # Load self report data
  self.report.data <- read.csv(paste(input.data.directory, self.report.file.name, sep = ""), comment.char = "#")
  self.report.data <- self.report.data[-1,]
  
  data.path <- paste(input.data.directory, date.directory, data.file.name, ".csv", sep = "")
  
  if(file.exists(data.path)) {
  
    # Load data
    data <- read.csv(data.path, comment.char = "#")
    
    # Loop measurements
    for(i in 1:nrow(self.report.data)) {
      
      source("./code-snippets/extract-self-report-times.R")
      
      # Subset data
      data.subset <- data[activity.start.ms <= data$timestamp.ms & data$timestamp.ms < activity.end.ms, ]
      
      # Check for dublicates
      duplicates <- which(duplicated(data.subset))
      if(length(duplicates) > 0) {
        data.subset <- data.subset[-duplicates, ]
        print(paste(length(duplicates), "removed duplicates in ", paste(input.data.directory, date.directory, data.file.name, ".csv", sep = ""), i))
        readline("Press return to continue > ")
      }
      
      if(data.file.name != "gps-position") {
        
        # Check sampling rate
        duration.s <- ((data.subset$timestamp.ms[nrow(data.subset)] - data.subset$timestamp.ms[1]) / 1000)
        fs <- ComputeSamplingRate(data.subset$timestamp.ms)
        print(paste("Sampling rate:", round(fs, 2), "Hz"))
        print(paste("Duration:", round(duration.s, 2), "s"))
        plot(diff(data.subset$timestamp.ms / 1000), xlab = "#", ylab = "Interval (s)") # data.subset$timestamp.ms[-1] / 1000, -- Timestamp (s)
        title(paste(strftime(session.start, format="%Y/%m/%d %H:%M"), " #", i, sep = ""))
        session.zoom()
        
        # Check data
        for(j in 2:ncol(data.subset)) {
          par(mfcol = c(1, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
          plot(data.subset$timestamp.ms / 1000, data.subset[, j], type = "l", xlab = "Timestamp (s)", ylab = ReturnFieldLabels(colnames(data.subset)[j]))
          title(paste(strftime(session.start, format="%Y/%m/%d %H:%M"), " #", i, sep = ""))
          session.zoom()
        }
        
        # Write to csv file
        output.directory <- paste(preprocessed.data.directory, activity.directory, user.directory, date.directory, sep = "")
        if(!file.exists(substr(output.directory, 1, nchar(output.directory) - 1))) {
          dir.create(output.directory, recursive = T)
        }
        output.file.name <- paste(data.file.name, "-", i, ".csv", sep = "")
        output.directory <- paste(output.directory, output.file.name, sep = "")
        write.csv(data.subset, output.directory, row.names = F)
        print(paste("Worte:", output.directory))
      } else {
        
        date.time <- session.start + data.subset$timestamp.ms / 1000
        date.time <- format(date.time, "%Y-%m-%d %H:%M:%OS3")
        latitude  <- data.subset[, 2]
        longitude <- data.subset[, 3]
        altitude  <- data.subset[, 4]
        
        # Write to kml file
        output.directory <- paste(preprocessed.data.directory, activity.directory, user.directory, date.directory, sep = "")
        if(!file.exists(substr(output.directory, 1, nchar(output.directory) - 1))) {
          dir.create(output.directory, recursive = T)
        }
        
        header <- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><kml xmlns=\"http://earth.google.com/kml/2.2\"><!-- TimeStamp is recommended for Point. Each Point represents a sample from a GPS. --><Document><name>Points with TimeStamps</name><Style id=\"paddle-a\"><IconStyle><Icon><href>http://maps.google.com/mapfiles/kml/paddle/A.png</href></Icon><hotSpot x=\"32\" y=\"1\" xunits=\"pixels\" yunits=\"pixels\"/></IconStyle></Style><Style id=\"paddle-b\"><IconStyle><Icon><href>http://maps.google.com/mapfiles/kml/paddle/B.png</href></Icon><hotSpot x=\"32\" y=\"1\" xunits=\"pixels\" yunits=\"pixels\"/></IconStyle></Style><Style id=\"hiker-icon\"><IconStyle><Icon><href>http://maps.google.com/mapfiles/ms/icons/hiker.png</href></Icon><hotSpot x=\"0\" y=\".5\" xunits=\"fraction\" yunits=\"fraction\"/></IconStyle></Style><Style id=\"check-hide-children\"><ListStyle><listItemType>checkHideChildren</listItemType></ListStyle></Style>"
        footer <- "</Document></kml>"
        
        output.directory <- paste(output.directory, "gps-position-", i, ".kml", sep = "")
        write(header, output.directory)
        write(paste("<Placemark><TimeStamp><when>", strftime(date.time, format = "%F", tz = "CET"), "T", strftime(date.time, format = "%T"), "Z</when></TimeStamp><styleUrl>#hiker-icon</styleUrl><Point><coordinates>", longitude, ",", latitude, ",", altitude, "</coordinates></Point></Placemark>", sep = ""), output.directory, append = T)
        write(footer, output.directory, append = T)
        print(paste("Wrote:", output.directory))
      }
    }
  } else {
    print(paste("No file found:", data.path))
  }
}