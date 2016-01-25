cls.features <- data.frame()
for (i in 1:nrow(fss.features)) {
  
  source("code-snippets/set-additional-features.R")
  
  directory.path        <- paste(root.data.directory.path, "processed-data/", activity.directory, user.directory, date.directory, sep = "")
  file.name             <- paste("leg-cls-indexes-", measurement, ".csv", sep = "")
  col.names             <- c("mean.pcoi", "mean.nsei", "activity","activity.start","activity.end","inquiry.end","measurement","last.name","first.name","date.of.birth")
  range.intervals       <- seq(0, 900, 300)
  
  feature.data.frame <- data.frame()
  if(file.exists(paste(directory.path, file.name, sep = ""))) {
    data        <- read.csv(paste(directory.path, file.name, sep = ""), skip = 2)
    #plot(data[, 2], data[, 3], xlab ="PCoI", ylab = "NSEI", xlim = c(0, 1), ylim = c(0, 1))  
    for (j in 1:(length(range.intervals) - 1)) {
      
      estimated.features <- c(mean(data[, 2][data[, 1] >= range.intervals[j] & data[, 1] < range.intervals[j + 1]], na.rm = T), mean(data[, 3][data[, 1] >= range.intervals[j] & data[, 1] < range.intervals[j + 1]], na.rm = T))
      feature.vector        <- c(estimated.features, additional.features)
      names(feature.vector) <- col.names
      feature.data.frame    <- rbind(feature.data.frame, feature.vector)
    }
  } else {
    for (j in 1:(length(range.intervals) - 1)) {
      feature.vector        <- c(rep(NA, 2), additional.features)
      names(feature.vector) <- col.names
      feature.data.frame    <- rbind(feature.data.frame, feature.vector)
    }
  }
  cls.features        <- rbind(cls.features, feature.data.frame)
}

rm(additional.features, data, feature.data.frame, activity.start, col.names, date.directory, estimated.features, feature.vector, file.name, i, j, measurement, range.intervals)
