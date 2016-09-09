cls.features  <- data.frame()
cls.feature.names <- c("mean.pcoi", "mean.nsei", "direction")
for (i in 1:nrow(fss.features)) {
  
  session.start <- fss.features[i, 9]
  measurement <- fss.features[i, 14]
  
  cls.data.path <- paste(root.path, processed.data.directory, activity.directory, user.directory, strftime(session.start, format="%Y-%m-%d--%H-%M-%S"), "/", "cls-index-data-", measurement, ".csv", sep = "")
  rm(measurement)
  
  if(file.exists(cls.data.path)) {
    cls.data <- read.csv(cls.data.path, skip = 2)
    cls.data <- cls.data[max(cls.data[, 1], na.rm = T) - min.before.end * 60000 < cls.data[, 1], ]
    cls.feature.row <- data.frame(t(colMeans(cls.data[, 2:3], na.rm = T)), as.numeric(which.max(table(as.factor(c(cls.data[, 4], levels = c(1,2,3)))))))
    names(cls.feature.row) <- cls.feature.names
    cls.features <- rbind(cls.features, cls.feature.row)
    rm(cls.data)
  } 
  
  else {
    cls.feature.row <- data.frame(t(rep(NA, 3)))
    names(cls.feature.row) <- cls.feature.names
    cls.features <- rbind(cls.features, cls.feature.row)
  }
  rm(cls.data.path, cls.feature.row)
}
rm(cls.feature.names, i, session.start)