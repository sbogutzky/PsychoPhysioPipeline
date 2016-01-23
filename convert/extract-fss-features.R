# Version 2.0

# Remove all variables
rm(list = ls(all = T))

source("./code-snippets/read-and-set.R")

file.names <- list.files(path = input.data.directory, pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}--[0-9]{2}-[0-9]{2}-[0-9]{2}-questionaire.csv", recursive = T)

# Create fss feature and measurement data frame
fss.features <- data.frame()

for (file.name in file.names) {
  
  # Load self report results
  self.report.results <- read.csv(paste(input.data.directory, file.name, sep = ""), comment.char = "#", skip = 10)
  
  source("./code-snippets/extract-session-start.R")
  
  date.directory <- paste(strftime(session.start, format="%Y-%m-%d--%H-%M-%S"), "/", sep ="")
  
  fss.dimensions <- data.frame(self.report.results[, c(3, 5, 7, 9, 11)], NA, session.start, activity, 0, self.report.results[, 1] * 1000, NA, 1, NA, first.name, NA)
  colnames(fss.dimensions) <- c("flow","fluency","absorption","anxiety","fit","daf","session.start","activity","activity.start.ms","activity.end.ms","self.report.end.ms","measurement","last.name","first.name","date.of.birth")
  fss.features <- rbind(fss.features, fss.dimensions)
  
  
  self.report.data <- data.frame(NA, self.report.results[, 1] * 1000, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
  colnames(self.report.data) <- c("timestamp.show.ms","timestamp.start.ms","timestamp.stop.ms","item.01","item.02","item.03","item.04","item.05","item.06","item.07","item.08","item.09","item.10","item.11","item.12","item.13","item.14","item.15","item.16")

  # Write to csv file
  output.directory <- paste(raw.data.directory, activity.directory, user.directory, date.directory, sep = "")
  if(!file.exists(substr(output.directory, 1, nchar(output.directory) - 1))) {
    dir.create(output.directory, recursive = T)
  }
  
  output.directory <- paste(output.directory, "self-report.csv", sep = "")
  con <- file(output.directory, 'w') 
  writeLines(paste("# StartTime:", strftime(session.start, format="%Y/%m/%d %H:%M:%S")), con = con)
  write.csv(self.report.data, file = con, row.names = FALSE)
  writeLines(paste("# StopTime:", strftime(session.start + self.report.results[, 1], format="%Y/%m/%d %H:%M:%S")), con = con)
  close(con)
  print(paste("Worte:", output.directory))
  
  # Delete file
  file.remove(paste(input.data.directory, file.name, sep = ""))
}

# Write to csv file
output.directory <- paste(feature.directory, activity.directory, user.directory, sep = "")
if(!file.exists(substr(output.directory, 1, nchar(output.directory) - 1))) {
  dir.create(output.directory, recursive = T)
}
output.directory <- paste(output.directory, "fss-features.csv", sep = "")
write.csv(fss.features, output.directory, row.names = F)
print(paste("Worte:", output.directory))