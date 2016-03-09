# Set additional features
additional.features <- fss.features[i, c(7:14)]
if(colnames(additional.features)[1] == "session.start") {
  measurement <- additional.features[, 6]
  date <- activity.start <- as.POSIXct(additional.features[, 1], origin = "1970-01-01", tz="CET")
  hrv.file.name.prefix <- "heart-"
  jc.file.name.prefix <- "motion-cycle-intervals-jerk-costs-"
  m <- 1
} else {
  activity.start <- additional.features[, 2]
  measurement <- additional.features[, 5]
  hrv.file.name.prefix <- "ecg-data-"
  jc.file.name.prefix <- "leg-jerk-cost-data-"
  m <- 1
  if(measurement == 1) {
      date <- as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET")
  }
}
