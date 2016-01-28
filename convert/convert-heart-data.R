# Version 2.0

# Remove all variables
rm(list = ls(all = T))

root.directory <- readline("Type in root directory and press return to continue (with: /) > ")
first.names <- c("anke", "birthe", "danilo", "hilke", "kiana", "linde", "maike", "max", "susanna", "timo", "tobias", "vreni")
last.name <- ""
activity <- "walking"

for(first.name in first.names) {

source("./code-snippets/read-and-set.R")

file.paths <- list.files(path = input.data.directory, pattern = "([0-9]{4}-[0-9]{2}-[0-9]{2}--[0-9]{2}-[0-9]{2}-[0-9]{2}-)?heart.csv", full.names = T, recursive = T)

for (file.path in file.paths) {
  
  # Load motion data
  heart.data  <- read.csv(file.path, comment.char = "#", skip = 10)
  
  source("./code-snippets/extract-session-start.R")
  
  date.directory <- paste(strftime(session.start, format="%Y-%m-%d--%H-%M-%S"), "/", sep ="")
  
  new.heart.data  <- data.frame(heart.data [, 1], heart.data [, 2])
  colnames(new.heart.data ) <- c("timestamp.ms","rr.interval.s")

  # Write to csv file
  output.directory <- paste(raw.data.directory, activity.directory, user.directory, date.directory, sep = "")
  if(!file.exists(substr(output.directory, 1, nchar(output.directory) - 1))) {
    dir.create(output.directory, recursive = T)
  }
  
  # Delete file
  # file.remove(file.path)
  
  output.directory <- paste(output.directory, "heart.csv", sep = "")
  con <- file(output.directory, 'w') 
  writeLines(paste("# StartTime:", strftime(session.start, format="%Y/%m/%d %H:%M:%S")), con = con)
  write.csv(new.heart.data , file = con, row.names = FALSE)
  writeLines(paste("# StopTime:", strftime(session.start + heart.data [, 1], format="%Y/%m/%d %H:%M:%S")), con = con)
  close(con)
  print(paste("Worte:", output.directory))
  
}
}