motion.data <- read.csv("./data/tests/detect-gait-events.csv")

subset <- function(x, select) {
  
  # Column selection
  selection <- base::subset(x, select = select)
  plot(selection)
  final.bounds <- c()
  
  # Iterative subsetting
  while(T) {
    cat("Options: \n1. Select two values for subsetting \n2. Select nothing for quitting \n\n")
    bounds <- identify(selection, labels = row.names(selection))
    if(length(bounds) == 2) {
      cat(paste(select[1], "from", selection[min(bounds), 1], "to", selection[max(bounds), 1], "\n\n"))
      plot(selection, xlim = c(selection[min(bounds), 1], selection[max(bounds), 1]))
      final.bounds <- bounds
    } 
    if (length(bounds) == 0 & length(final.bounds) > 0) {
      return(x[min(final.bounds):max(final.bounds), ])
    } else {
      return(x)
    }
  }
}

motion.data.subset <- subset(motion.data, select = c("Timestamp", "Gyroscope.X"))

plot(motion.data.subset, type = "l")
x <- motion.data.subset$Timestamp
y <- motion.data.subset$Gyroscope.X

slope <- 0
for(i in 1:(length(y)-1)) {
    if(slope * (y[i] - y[i+1]) < 0) {
      points(x[i], y[i])
  }
  slope <- y[i] - y[i+1]
}
