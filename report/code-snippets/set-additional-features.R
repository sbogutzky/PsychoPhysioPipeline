# Set additional features
additional.features <- fss.features[i, c(7:14)]
activity.start      <- additional.features[, 2]
measurement         <- additional.features[, 5]

if(measurement == 1) {
  date.directory <- paste(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y-%m-%d--%H-%M-%S"), "/", sep ="")
}