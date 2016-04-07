# Set directories
root.directory <- "/Users/sbogutzky/Desktop/data/2016/"
raw.data.directory <- paste(root.directory, "raw-data/", sep = "")
feature.directory <- paste(root.directory, "features/", sep = "")
preprocessed.data.directory <- paste(root.directory, "preprocessed/", sep = "")
activity.directory <- paste(tolower(activity), "/",  sep = "")
if(last.name != "" & first.name != "") {
  user.directory <- paste(tolower(last.name), "-", tolower(first.name), "/",  sep = "")
}
if(last.name == "" & first.name != "") {
  user.directory <- paste(tolower(first.name), "/",  sep = "")
}
if(last.name != "" & first.name == "") {
  user.directory <- paste(tolower(last.name), "/",  sep = "")
}
input.data.directory <- paste(raw.data.directory, activity.directory, user.directory, sep = "")