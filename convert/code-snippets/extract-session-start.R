comment.lines <- readLines(paste(input.data.directory, file.name, sep = ""), n = 3)
comment.lines <- gsub(pattern = "[A-Za-z]+: ", replacement = "", x = comment.lines, ignore.case = T)
comment.lines <- gsub(pattern = " ", replacement = "", x = comment.lines, ignore.case = T)

session.start <- as.POSIXct(paste(comment.lines[1], comment.lines[2]), format = "%d-%m-%Y %H:%M:%OS")

#   duration <- gsub(pattern = "[A-Za-z]", replacement = ",", x = comment.lines[3], ignore.case = T)
#   duration <- as.numeric(strsplit(duration, ",")[[1]])
#   duration <- duration[1] * 60 * 60 + duration[2] * 60 + duration[3]