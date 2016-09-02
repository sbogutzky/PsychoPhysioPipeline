hrv.features <- data.frame()
hrv.feature.names <- c("mean.hr","rmssd")
for (i in 1:nrow(fss.features)) {
  
  session.start <- fss.features[i, 9]
  measurement <- fss.features[i, 14]
  
  kubios.hrv.data.file.path <- paste(root.path, processed.data.directory, activity.directory, user.directory, strftime(session.start, format="%Y-%m-%d--%H-%M-%S"), "/", "imu-rn42-bd38-", measurement, "_hrv.txt", sep = "")
  rm(measurement)
  if(file.exists(kubios.hrv.data.file.path)) {
    
    # Load heart beat times
    source("../processing/code-snippets/get-kubios-hrv-data.R")
    
    kubios.hrv.data <- kubios.hrv.data[max(kubios.hrv.data[, 1], na.rm = T) - min.before.end * 60 < kubios.hrv.data[, 1], ]
    heart.beat.times <- c(kubios.hrv.data[1, 1] - kubios.hrv.data[1, 2], kubios.hrv.data[, 1])
    
    rr <- diff(heart.beat.times * 1000)
    mean.hr <- mean(60000 / rr, na.rm = T)
    rmssd <- sqrt(sum(diff(rr)^2, na.rm = T)/(length(rr)-1))
    
    hrv.feature.row <- data.frame(mean.hr, rmssd)
    names(hrv.feature.row) <- hrv.feature.names
    hrv.features <- rbind(hrv.features, hrv.feature.row)
  } 
  
  else {
    hrv.feature.row <- data.frame(t(rep(NA, 2)))
    names(hrv.feature.row) <- hrv.feature.names
    hrv.features <- rbind(hrv.features, hrv.feature.row)
  }
  rm(kubios.hrv.data.file.path, hrv.feature.row)
}
rm(hrv.feature.names, i, session.start)