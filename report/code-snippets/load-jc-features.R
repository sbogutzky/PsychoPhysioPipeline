jc.features  <- data.frame()
for (i in 1:nrow(fss.features)) {
  source("code-snippets/set-additional-features.R")
  jc.data.path <- paste(root.path, "processed-data", "/", activity, "/", user, "/", strftime(date, format="%Y-%m-%d--%H-%M-%S"), "/", "leg-jerk-cost-data-", measurement, ".csv", sep = "")
  range.intervals <- seq(0, 900, 300)
  if(file.exists(jc.data.path)) {
    jc.data <- read.csv(jc.data.path, skip = 2)
    jc.data[, 3] <- jc.data[, 3] / 10^4
    for (j in 1:(length(range.intervals) - 1)) {
      jc.data.subset <- jc.data[jc.data[, 1] >= range.intervals[j] & jc.data[, 1] < range.intervals[j + 1], ]
      jc.data.subset <- jc.data.subset[jc.data.subset[, 2] < 1.25, ]
      jc.feature.row <- colMeans(jc.data.subset[, 2:3], na.rm = T)
      jc.features <- rbind(jc.features, jc.feature.row)
    }
    rm(jc.data, jc.data.subset, jc.feature.row, j)
  } else {
    for (j in 1:(length(range.intervals) - 1)) {
      jc.feature.row <- rep(NA, 2)
      jc.features <- rbind(jc.features, jc.feature.row)
    }
    rm(jc.feature.row, j)
  }
}
names(jc.features) <- c("mean.cycle.interval", "mean.jerk.cost")
rm(jc.data.path, range.intervals, i, additional.features, activity.start, date, measurement)
