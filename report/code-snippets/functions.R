CreateFeatureDataFrame <- function(directory.path, filename, cols, rows, col.names, additional.features, skip = skip) {
  feature.data.frame <- data.frame()
  if(file.exists(paste(directory.path, file.name, sep = ""))) {
    loaded.features <- read.csv(paste(directory.path, file.name, sep = ""), skip = skip)
    for (i in rows) {
      feature.vector        <- c(loaded.features[i, cols], additional.features)
      names(feature.vector) <- col.names
      feature.data.frame    <- rbind(feature.data.frame, feature.vector)
    }
  } else {
    for (i in rows) {
      feature.vector        <- c(rep(NA, length(cols)), additional.features)
      names(feature.vector) <- col.names
      feature.data.frame    <- rbind(feature.data.frame, feature.vector)
    }
  }
  return(feature.data.frame)
}