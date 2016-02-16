jc.features  <- data.frame()
for (i in 1:nrow(fss.features)) {
  source("code-snippets/set-additional-features.R")
  jc.data.path <- paste(root.path, "processed-data", "/", activity, "/", user, "/", strftime(date, format="%Y-%m-%d--%H-%M-%S"), "/", jc.file.name.prefix, measurement, ".csv", sep = "")
  range.intervals <- seq(0, 900, 300)
  if(file.exists(jc.data.path)) {
    jc.data <- read.csv(jc.data.path, skip = 2)
    jc.data[, 3] <- jc.data[, 3] / 10^4
    for (j in 1:(length(range.intervals) - 1)) {
      jc.data.subset <- jc.data[jc.data[, 1] >= range.intervals[j] & jc.data[, 1] < range.intervals[j + 1], ]
      jc.data.subset <- jc.data.subset[jc.data.subset[, 2] < 1.25, ]
      jc.feature.row <- colMeans(jc.data.subset[, 2:3], na.rm = T)
      names(jc.feature.row) <- c("mean.cycle.interval", "mean.jerk.cost")
      jc.features <- rbind(jc.features, jc.feature.row)
    }
    rm(jc.data, jc.data.subset, jc.feature.row, j)
  } else {
    for (j in 1:(length(range.intervals) - 1)) {
      jc.feature.row <- data.frame(t(rep(NA, 2)))
      names(jc.feature.row) <- c("mean.cycle.interval", "mean.jerk.cost")
      jc.features <- rbind(jc.features, jc.feature.row)
    }
    rm(jc.feature.row, j)
  }
}
rm(jc.data.path, range.intervals, i, additional.features, activity.start, date, measurement, m, hrv.file.name.prefix, jc.file.name.prefix)
