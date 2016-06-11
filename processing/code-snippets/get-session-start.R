# Extract session start
start.time.line <- readLines(paste(raw.data.directory.path, self.report.file.name, sep = ""), n = 1)
session.start <- as.POSIXct(gsub(pattern = "# StartTime: ", replacement = "", x = start.time.line, ignore.case = T))
date.directory <- paste(strftime(session.start, format="%Y-%m-%d--%H-%M-%S"), "/", sep ="")

# Clean up
rm(start.time.line)
