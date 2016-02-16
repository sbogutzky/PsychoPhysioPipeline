jc.features  <- data.frame()
for (i in 1:nrow(fss.features)) {
  source("code-snippets/set-additional-features.R")
  jc.data.path <- paste(root.path, "processed-data", "/", activity, "/", user, "/", strftime(date, format="%Y-%m-%d--%H-%M-%S"), "/", jc.file.name.prefix, measurement, ".csv", sep = "")
  
  print(jc.data.path)
  if(file.exists(jc.data.path)) {
    jc.data <- read.csv(jc.data.path)
    jc.data[, 3] <- jc.data[, 3] / 10^4
    jc.data <- jc.data[jc.data[, 2] < 1.25, ]
    jc.feature.row <- data.frame(t(colMeans(jc.data[, 2:3], na.rm = T)))
    names(jc.feature.row) <- c("mean.cycle.interval", "mean.jerk.cost")
    jc.features <- rbind(jc.features, jc.feature.row)
    rm(jc.data, jc.feature.row)
  } else {
    jc.feature.row <- data.frame(t(rep(NA, 2)))
    names(jc.feature.row) <- c("mean.cycle.interval", "mean.jerk.cost")
    jc.features <- rbind(jc.features, jc.feature.row)
    rm(jc.feature.row)
  }
}
rm(jc.data.path, i,additional.features, activity.start, date, measurement, m, hrv.file.name.prefix, jc.file.name.prefix)