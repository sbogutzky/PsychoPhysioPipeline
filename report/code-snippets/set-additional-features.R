# Set additional features
additional.features <- fss.features[i, c(6:13)]
activity.start      <- additional.features[, 2]
measurement         <- additional.features[, 5]

if(measurement == 1) {
  date <- as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET")
}