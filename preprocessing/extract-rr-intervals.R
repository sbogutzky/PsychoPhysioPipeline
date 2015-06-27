# Remove all variables
rm(list = ls(all = T)) 

# Set root data directory path
root.data.directory.path <- ""
if(file.exists("C:/Users/Simon Bogutzky/Documents/flow/data"))
  root.data.directory.path <- "C:/Users/Simon Bogutzky/Documents/flow/data/"
if(file.exists("/Volumes/flow/Documents/simon-bogutzky/data"))
  root.data.directory.path <- "/Volumes/flow/Documents/simon-bogutzky/data/"
if(file.exists("//gangstore.ddns.net/flow/Documents/simon-bogutzky/data"))
  root.data.directory.path <- "//gangstore.ddns.net/flow/Documents/simon-bogutzky/data/"

# Set processed data directory path
processed.data.directory.path <- paste(root.data.directory.path, "processed-data/", sep = "")

# Set features directory path
features.directory.path <- paste(root.data.directory.path, "features/", sep = "")

# Load fss features
fss.features <- read.csv(paste(features.directory.path, "fss-features.csv", sep = ""), stringsAsFactors = F)

for (i in 1:nrow(fss.features)) {
  
  properties      <- fss.features[i, c(6:12)]
  activity        <- properties[, 1]
  activity.start  <- properties[, 2]
  measurement     <- properties[, 5]
  last.name       <- properties[, 6]
  first.name      <- properties[, 7]
  if(measurement == 1) {
    date.directory  <- paste(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y-%m-%d--%H-%M-%S"), "/", sep ="")
  }
    
  # Read hrv data
  hrv.data.path <- paste(processed.data.directory.path, tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/", date.directory, "/ecg-data-", measurement,  "_hrv.txt", sep="")
  if(file.exists(hrv.data.path)) {
  
    # Load hrv data
    hrv.data <- read.csv(hrv.data.path, header = F, na.strings = "", fill = T, skip = 117, stringsAsFactors = F, col.names = c("", "t.s", "rr.interval.s", "", "", "", "", "", "", "", ""))[,2:3]
  
    # Create directory, if needed
    output.directory.path <- paste(processed.data.directory.path, tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/", date.directory, sep="")
    if(!file.exists(output.directory.path)) {
      dir.create(paste(output.directory.path, "/", sep = ""), recursive = TRUE)
    }
    
    # Write csv file
    output.file.path <- paste(output.directory.path, "hrv-time-data-", measurement, ".csv", sep = "")
    op <- options(digits.secs=3)
    con <- file(output.file.path, 'w') 
    writeLines(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y-%m-%d"), con = con)
    writeLines(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%H:%M:%OS"), con = con)
    write.csv(hrv.data, file = con, row.names = FALSE)
    close(con)
    options(op) #reset options
    print(paste("Wrote:", output.file.path))
    
  } else {
    print("No HRV data")
  }
}