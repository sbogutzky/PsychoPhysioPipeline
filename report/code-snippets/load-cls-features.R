cls.features  <- data.frame()
for (i in 1:nrow(fss.features)) {
  source("code-snippets/set-additional-features.R")
  cls.data.path <- paste(root.path, "processed-data", "/", activity, "/", user, "/", strftime(date, format="%Y-%m-%d--%H-%M-%S"), "/", "leg-cls-indexes-", measurement, ".csv", sep = "")
  range.intervals <- seq(0, 900, 300)
  if(file.exists(cls.data.path)) {
    cls.data <- read.csv(cls.data.path, skip = 2)
    for (j in 1:(length(range.intervals) - 1)) {
      cls.data.subset <- cls.data[cls.data[, 1] >= range.intervals[j] & cls.data[, 1] < range.intervals[j + 1], ]
      cls.feature.row <- colMeans(cls.data.subset[, 2:3], na.rm = T)
      cls.features <- rbind(cls.features, cls.feature.row)
    }
    rm(cls.data, cls.data.subset, cls.feature.row, j)
  } else {
    for (j in 1:(length(range.intervals) - 1)) {
      cls.feature.row <- rep(NA, 2)
      cls.features <- rbind(cls.features, cls.feature.row)
    }
    rm(cls.feature.row, j)
  }
}
names(cls.features) <- c("mean.pcoi", "mean.nsei")
rm(cls.data.path, range.intervals, i, additional.features, activity.start, date, measurement)
