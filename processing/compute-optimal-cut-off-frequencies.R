# Version 2.0

# Remove all variables
rm(list = ls(all = T))  

# Load libraries
library(flow)

# Set working directory
setwd("~/psychophysiopipeline/processing")

# User input
root.directory.path <- readline("Quellverzeichnis > ") 
activity <- readline("Aktivität der Untersuchung > ")
kinematic.data.file.name <- readline("Dateiname der Datei mit kinematischen Daten (ohne .csv) > ")

kinematic.data.file.paths <- list.files(path = paste(root.directory.path, "processed-data/", sep = ""), pattern = paste(kinematic.data.file.name, "-[0-9].csv", sep = ""), full.names = TRUE, recursive = TRUE)
optimal.cutoff.frequency.data <- c()
n <- length(kinematic.data.file.paths)

print("---")
for (i in 1:n) {
  
  kinematic.data.file.path <- kinematic.data.file.paths[i]
  
  # Load kinematic data
  kinematic.data <- read.csv(kinematic.data.file.path)
  
  # Compute sampling rate
  fs <- ComputeSamplingRate(kinematic.data[, 1])
  f.n <- 4

  # Compute optimal cut off frequencies
  optimal.cutoff.frequency.data <- c(optimal.cutoff.frequency.data, ComputeOptimalCutoffFrequency(kinematic.data[, 2], fs, f.n), ComputeOptimalCutoffFrequency(kinematic.data[, 3], fs, f.n), ComputeOptimalCutoffFrequency(kinematic.data[, 4], fs, f.n))

  print(paste("Datei", i, "von", n, "verarbeitet."))
}

optimal.cutoff.frequency.data <- matrix(optimal.cutoff.frequency.data, 3, i)
optimal.cutoff.frequencies <- rowMeans(optimal.cutoff.frequency.data, na.rm = T)

print("---")
print(paste("Grenzfrequenz für die Beschleunigung entlang der X-Achse:", round(optimal.cutoff.frequencies[1], 1), "Hz"))
print(paste("Grenzfrequenz für die Beschleunigung entlang der Y-Achse:", round(optimal.cutoff.frequencies[2], 1), "Hz"))
print(paste("Grenzfrequenz für die Beschleunigung entlang der Z-Achse:", round(optimal.cutoff.frequencies[3], 1), "Hz"))