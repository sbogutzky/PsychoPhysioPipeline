jc.features <- data.frame()
jc.feature.names <- c("mean.cycle.interval", "mean.jerk.cost")
for (i in 1:nrow(fss.features)) {
  
  source("code-snippets/set-additional-features.R")
  jc.data.path <- paste(root.path, "processed-data", "/", activity, "/", user, "/", strftime(date, format="%Y-%m-%d--%H-%M-%S"), "/", "leg-jerk-cost-data-", measurement, ".csv", sep = "")
  rm(measurement)
  
  if(file.exists(jc.data.path)) {
    jc.data <- read.csv(jc.data.path, skip = 2)
    
    jc.data[, 3] <- jc.data[, 3] / 10^5
    jc.data <- jc.data[jc.data[, 2] < 1.25, ]
    
    jc.feature.row <- data.frame(t(colMeans(jc.data[, 2:3], na.rm = T)))
    names(jc.feature.row) <- jc.feature.names
    jc.features <- rbind(jc.features, jc.feature.row)
    rm(jc.data)
  } 

  else {
    jc.feature.row <- data.frame(t(rep(NA, 2)))
    names(jc.feature.row) <- jc.feature.names
    jc.features <- rbind(jc.features, jc.feature.row)
  }
  rm(jc.data.path, jc.feature.row)
}
rm(jc.feature.names, i, date)
