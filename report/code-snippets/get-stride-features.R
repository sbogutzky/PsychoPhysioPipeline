stride.features <- data.frame()
for (i in 1:nrow(fss.features)) {
  
  session.start <- fss.features[i, 9]
  measurement <- fss.features[i, 14]
  
  stride.data.path <- paste(root.path, processed.data.directory, activity.directory, user.directory, strftime(session.start, format="%Y-%m-%d--%H-%M-%S"), "/", kinematic.data.file.name, "-stride-data-", measurement, ".csv", sep = "")
  rm(measurement)
  
  if(file.exists(stride.data.path)) {
    
    stride.data <- read.csv(stride.data.path)
    stride.data <- stride.data[max(stride.data[, 1]) - min.before.end * 60000 < stride.data[, 1], ]
    stride.features <- rbind(stride.features, data.frame(mean.ms.interval = mean(stride.data[, 2], na.rm = TRUE)))
    rm(stride.data)
  } 

  else {
    stride.features <- rbind(stride.features, data.frame(mean.ms.interval = NA))
  }
  rm(stride.data.path)
}
rm(i, session.start)
