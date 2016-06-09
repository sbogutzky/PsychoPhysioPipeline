cls.features  <- data.frame()
cls.feature.names <- c("mean.pcoi", "mean.nsei")
for (i in 1:nrow(fss.features)) {
  
  source("code-snippets/set-additional-features.R")
  cls.data.path <- paste(root.path, preprocessed.data.directory, activity.directory, user.directory, strftime(date, format="%Y-%m-%d--%H-%M-%S"), "/", "cls-indexes-", measurement, ".csv", sep = "")
  rm(measurement)
  
  if(file.exists(cls.data.path)) {
    cls.data <- read.csv(cls.data.path, skip = 2)
    cls.feature.row <- data.frame(t(colMeans(cls.data[, 2:3], na.rm = T)))
    names(cls.feature.row) <- cls.feature.names
    cls.features <- rbind(cls.features, cls.feature.row)
    rm(cls.data)
  } 
  
  else {
    cls.feature.row <- data.frame(t(rep(NA, 2)))
    names(cls.feature.row) <- cls.feature.names
    cls.features <- rbind(cls.features, cls.feature.row)
  }
  rm(cls.data.path, cls.feature.row)
}
rm(cls.feature.names, i, date)