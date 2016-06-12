jc.features <- data.frame()
for (i in 1:nrow(fss.features)) {
  
  session.start <- fss.features[i, 9]
  measurement <- fss.features[i, 14]
  
  jc.data.path <- paste(root.path, processed.data.directory, activity.directory, user.directory, strftime(session.start, format="%Y-%m-%d--%H-%M-%S"), "/", kinematic.data.file.name, "-jerk-cost-data-", measurement, ".csv", sep = "")
  rm(measurement)
  
  if(file.exists(jc.data.path)) {
    
    jc.data <- read.csv(jc.data.path)
    jc.data <- jc.data[max(jc.data[, 1]) - min.before.end * 60000 < jc.data[, 1], ]
    jc.features <- rbind(jc.features, data.frame(mean.jerk.cost = mean(jc.data[, 2], na.rm = TRUE)))
    rm(jc.data)
  } 

  else {
    jc.features <- rbind(jc.features, data.frame(mean.jerk.cost = NA))
  }
  rm(jc.data.path)
}
rm(i, session.start)
