# Remove all variables
rm(list = ls(all = T)) 

# Set root data directory path
root.data.directory.path <- ""
if(file.exists("/Volumes/flow/Documents/archiv/daten/2015/flow-gehen-und-laufen"))
  root.data.directory.path        <- "/Volumes/flow/Documents/archiv/daten/2015/flow-gehen-und-laufen/"
if(file.exists("//gangstore.ddns.net/flow/Documents/archiv/daten/2015/flow-gehen-und-laufen"))
  root.data.directory.path        <- "//gangstore.ddns.net/flow/Documents/archiv/daten/2015/flow-gehen-und-laufen/"

# Set cleaned data directory path
cleaned.data.directory.path       <- paste(root.data.directory.path, "cleaned-data/", sep = "")

# Set features directory path
features.directory.path           <- paste(root.data.directory.path, "features/", sep = "")

# Set processed data directory path
processed.data.directory.path  <- "processed-data/"

# Read activity directory
activity.directory  <- readline("Type in activity directory and press return to continue (e. g. walking/) > ")

# Read user directory
user.directory      <- readline("Type in user directory and press return to continue (e. g. doe-john/) > ")

# Load fss features
fss.features        <- read.csv(paste(features.directory.path, activity.directory, user.directory, "fss-features.csv", sep = ""), stringsAsFactors = F)

for (i in 1:nrow(fss.features)) {
  
  properties      <- fss.features[i, c(6:13)]
  activity.start  <- properties[, 2]
  activity.end    <- properties[, 3]
  measurement     <- properties[, 5]
  date.directory  <- paste(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y-%m-%d--%H-%M-%S"), "/", sep = "")
  
  if(measurement == 1) {
    gps.data        <- data.frame()
    n               <- 0
    
    # Read data, if needed
    gps.data.path <- paste(cleaned.data.directory.path, activity.directory, user.directory, date.directory, "gps-data.csv", sep="")
    if(file.exists(gps.data.path)) {
      gps.data      <- read.csv(gps.data.path)
      
      # Number of data rows
      n <- nrow(gps.data)
      
      # Create output directory, if needed
      output.directory.path <- paste(root.data.directory.path, processed.data.directory.path, activity.directory, user.directory, date.directory, sep="")
      if(!file.exists(output.directory.path)) {
        dir.create(output.directory.path, recursive = TRUE)
      }
    } else {
      print("No gps data")
    }
  }
  
  if(n > 0) {
    
    # Subset gps data
    gps.data.subset   <- gps.data[activity.start <= gps.data[,1] & gps.data[,1] < activity.end,]
    time.difference   <- gps.data.subset[1,1] - activity.start
    print(paste("Time difference (ms):", round(time.difference, 3)))
    print(paste("Total time      (ms):", round(gps.data.subset[nrow(gps.data.subset),1] - gps.data.subset[1,1] + time.difference, 3)))
    
    
    date.time <- as.POSIXct(gps.data.subset[,1] / 1000, origin = "1970-01-01", tz = "CET")
    date.time <- format(date.time, "%Y-%m-%d %H:%M:%OS3")
    latitude  <- gps.data.subset[,2]
    longitude <- gps.data.subset[,3]
    altitude  <- gps.data.subset[,4]
    
    # Write kml file
    header <- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><kml xmlns=\"http://earth.google.com/kml/2.2\"><!-- TimeStamp is recommended for Point. Each Point represents a sample from a GPS. --><Document><name>Points with TimeStamps</name><Style id=\"paddle-a\"><IconStyle><Icon><href>http://maps.google.com/mapfiles/kml/paddle/A.png</href></Icon><hotSpot x=\"32\" y=\"1\" xunits=\"pixels\" yunits=\"pixels\"/></IconStyle></Style><Style id=\"paddle-b\"><IconStyle><Icon><href>http://maps.google.com/mapfiles/kml/paddle/B.png</href></Icon><hotSpot x=\"32\" y=\"1\" xunits=\"pixels\" yunits=\"pixels\"/></IconStyle></Style><Style id=\"hiker-icon\"><IconStyle><Icon><href>http://maps.google.com/mapfiles/ms/icons/hiker.png</href></Icon><hotSpot x=\"0\" y=\".5\" xunits=\"fraction\" yunits=\"fraction\"/></IconStyle></Style><Style id=\"check-hide-children\"><ListStyle><listItemType>checkHideChildren</listItemType></ListStyle></Style>"
    footer <- "</Document></kml>"
    output.file.path <- paste(output.directory.path, "kml-path-", measurement, ".kml", sep = "")
    write(header, output.file.path)
    write(paste("<Placemark><TimeStamp><when>", strftime(date.time, format = "%F", tz = "CET"), "T", strftime(date.time, format = "%T"), "Z</when></TimeStamp><styleUrl>#hiker-icon</styleUrl><Point><coordinates>", longitude, ",", latitude, ",", altitude, "</coordinates></Point></Placemark>", sep = ""), output.file.path, append = T)
    write(footer, output.file.path, append = T)
    print(paste("Wrote:", output.file.path))
  }
}