# Version 2.0

# Remove all variables
rm(list = ls(all = T))  

# Load libraries
library(flow)
#library(zoom)

# Load user properties
source("./code-snippets/load-user-properties.R")

data.file.names <- c("imu-rn42-3b70", "imu-rn42-bc98", "linear-acceleration", "acceleration", "angular-velocity", "gps-position")
# data.file.names <- c("imu-rn42-bd38")
data.file.count <- length(data.file.names)
activity <- "Laufen"
range.s <- c(NA, NA)

# Loop user properties
for (i in 1:length(first.names)) {
  first.name <- first.names[i]
  last.name <- last.names[i]
  source("./code-snippets/load-directories.R")
  
  # Load all self report file names
  self.report.file.names <- list.files(path = input.data.directory, pattern = "self-report.csv", recursive = T)
  
  # Loop data files
  for (data.file.name in data.file.names) {
    if(data.file.count == 1) {
      range.s <- as.numeric(c(readline("Type in start time in seconds and press return to continue > "), readline("Type in stop time in seconds and press return to continue > ")))
    }
    # Loop self reports
    for (self.report.file.name in self.report.file.names) {
      source("./code-snippets/extract-session-start.R")
  
      # Load self report data
      self.report.data <- read.csv(paste(input.data.directory, self.report.file.name, sep = ""), comment.char = "#")
      data.path <- paste(input.data.directory, date.directory, data.file.name, ".csv", sep = "")
  
      if(file.exists(data.path)) {
  
        # Load data
        data <- read.csv(data.path, comment.char = "#")
    
        # Loop measurements
        for(j in 1:nrow(self.report.data)) {
          source("./code-snippets/extract-self-report-times.R")
      
          # Subset data
          if(is.na(range.s[1]) & is.na(range.s[2])) {
            data.subset <- data[activity.start.ms <= data$timestamp.ms & data$timestamp.ms < activity.end.ms, ]
          } else {
            data.subset <- data[range.s[1] * 1000 <= data$timestamp.ms & data$timestamp.ms < range.s[2] * 1000, ]
          }
      
          # Check for dublicates
          duplicates <- which(duplicated(data.subset))
          if(length(duplicates) > 0) {
            data.subset <- data.subset[-duplicates, ]
            print(paste(length(duplicates), "removed duplicates in ", paste(input.data.directory, date.directory, data.file.name, ".csv", sep = ""), j))
            readline("Press return to continue > ")
          }
          if(data.file.name != "gps-position") {
        
            # Check sampling rate
            duration.s <- ((data.subset$timestamp.ms[nrow(data.subset)] - data.subset$timestamp.ms[1]) / 1000)
            fs <- ComputeSamplingRate(data.subset$timestamp.ms)
            print(paste("Sampling rate:", round(fs, 2), "Hz"))
            print(paste("Duration:", round(duration.s, 2), "s"))
          
            # par(mfcol = c(1, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
            # plot(diff(data.subset$timestamp.ms / 1000), xlab = "#", ylab = "Interval (s)") # data.subset$timestamp.ms[-1] / 1000, -- Timestamp (s)
            # title(paste(strftime(session.start, format="%Y/%m/%d %H:%M"), " #", j, sep = ""))
            # session.zoom()
        
            # # Check data
            # for(k in 2:ncol(data.subset)) {
            #   k <- 2
            #   if(ncol(data.subset) > 4) {
            #     k <- 5
            #   }
            #   par(mfcol = c(1, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
            #   plot(data.subset$timestamp.ms / 1000, data.subset[, k], type = "l", xlab = "Timestamp (s)", ylab = ReturnFieldLabels(colnames(data.subset)[k]))
            #   title(paste(strftime(session.start, format="%Y/%m/%d %H:%M"), " #", j, sep = ""))
            #   session.zoom()
            # } # cols
        
            # Write to csv file
            output.directory <- paste(preprocessed.data.directory, activity.directory, user.directory, date.directory, sep = "")
            if(!file.exists(substr(output.directory, 1, nchar(output.directory) - 1))) {
              dir.create(output.directory, recursive = T)
            }
            output.file.name <- paste(data.file.name, "-", j, ".csv", sep = "")
            output.directory <- paste(output.directory, output.file.name, sep = "")
            write.csv(data.subset, output.directory, row.names = F)
            print(paste("Wrote:", output.directory))
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
        
            output.directory <- paste(output.directory, "gps-position-", j, ".kml", sep = "")
            write(header, output.directory)
            write(paste("<Placemark><TimeStamp><when>", strftime(date.time, format = "%F", tz = "CET"), "T", strftime(date.time, format = "%T"), "Z</when></TimeStamp><styleUrl>#hiker-icon</styleUrl><Point><coordinates>", longitude, ",", latitude, ",", altitude, "</coordinates></Point></Placemark>", sep = ""), output.directory, append = T)
            write(footer, output.directory, append = T)
            print(paste("Wrote:", output.directory))
          }
        } # measurement
      } else {
        print(paste("No file found:", data.path))
      }
    } # self report
  } # data file names
} # user properties