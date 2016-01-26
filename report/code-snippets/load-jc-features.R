jc.features  <- data.frame()
for (i in 1:nrow(fss.features)) {
  
  source("code-snippets/set-additional-features.R")
  
  directory.path        <- paste(root.data.directory.path, "processed-data/", activity.directory, user.directory, date.directory, sep = "")
  file.name             <- paste("leg-jerk-cost-data-", measurement, ".csv", sep = "")
  col.names             <- jerk.cost.feature.names   <- c("mean.cycle.interval", "mean.jerk.cost", "activity","activity.start","activity.end","inquiry.end","measurement","last.name","first.name","date.of.birth")
  range.intervals       <- seq(0, 900, 300)
  
  feature.data.frame <- data.frame()
  if(file.exists(paste(directory.path, file.name, sep = ""))) {
    data.1        <- read.csv(paste(directory.path, file.name, sep = ""), skip = 2)
    data.1[, 3]   <- data.1[, 3] / 10^4
    
    for (j in 1:(length(range.intervals) - 1)) {
      
      data.subset <- data.1[data.1[, 1] >= range.intervals[j] & data.1[, 1] < range.intervals[j + 1], ]
      # n <- nrow(data.subset)
      data.subset <- data.subset[data.subset[, 2] < 1.25, ]
      # print(n - nrow(data.subset))
      
      estimated.features <- colMeans(data.subset[, 2:3], na.rm = T)
      feature.vector <- c(estimated.features, additional.features)
      names(feature.vector) <- col.names
      feature.data.frame <- rbind(feature.data.frame, feature.vector)
    }
  } else {
    for (j in 1:(length(range.intervals) - 1)) {
      feature.vector        <- c(rep(NA, 2), additional.features)
      names(feature.vector) <- col.names
      feature.data.frame    <- rbind(feature.data.frame, feature.vector)
    }
  }
  jc.features        <- rbind(jc.features, feature.data.frame)
}

rm(additional.features, data.1, data.subset, feature.data.frame, activity.start, col.names, date.directory, estimated.features, feature.vector, file.name, i, j, jerk.cost.feature.names, measurement, n, range.intervals)
