# Version 2.0

# Remove all variables
rm(list = ls(all = T))

source("./code-snippets/read-and-set.R")

file.names <- list.files(path = input.data.directory, pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}--[0-9]{2}-[0-9]{2}-[0-9]{2}-motion.csv", recursive = T)

for (file.name in file.names) {
  
  # Load motion data
  motion.data <- read.csv(paste(input.data.directory, file.name, sep = ""), comment.char = "#", skip = 10)
  
  source("./code-snippets/extract-session-start.R")
  
  date.directory <- paste(strftime(session.start, format="%Y-%m-%d--%H-%M-%S"), "/", sep ="")
  
  new.moton.data <- data.frame(motion.data[, 1] * 1000, (motion.data[, 2] + motion.data[, 5]) * 9.81, (motion.data[, 3] + motion.data[, 6]) * 9.81, (motion.data[, 4] + motion.data[, 7]) * 9.81, motion.data[, 8] * 180/pi, motion.data[, 9] * 180/pi, motion.data[, 10] * 180/pi)
  colnames(new.moton.data) <- c("timestamp.ms","acceleration.x.ms.2","acceleration.y.ms.2","acceleration.z.ms.2","angular.velocity.x.deg.s","angular.velocity.y.deg.s","angular.velocity.z.deg.s")

  # Write to csv file
  output.directory <- paste(raw.data.directory, activity.directory, user.directory, date.directory, sep = "")
  if(!file.exists(substr(output.directory, 1, nchar(output.directory) - 1))) {
    dir.create(output.directory, recursive = T)
  }
  
  output.directory <- paste(output.directory, "motion.csv", sep = "")
  con <- file(output.directory, 'w') 
  writeLines(paste("# StartTime:", strftime(session.start, format="%Y/%m/%d %H:%M:%S")), con = con)
  write.csv(new.moton.data, file = con, row.names = FALSE)
  writeLines(paste("# StopTime:", strftime(session.start + motion.data[, 1], format="%Y/%m/%d %H:%M:%S")), con = con)
  close(con)
  print(paste("Worte:", output.directory))
  
  # Delete file
  file.remove(paste(input.data.directory, file.name, sep = ""))
}