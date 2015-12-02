# Remove all variables
rm(list = ls(all = T)) 

# Set root data directory path
root.data.directory.path <- ""
if(file.exists("/Volumes/flow/Documents/archiv/daten/2015/flow-gehen-und-laufen"))
  root.data.directory.path        <- "/Volumes/flow/Documents/archiv/daten/2015/flow-gehen-und-laufen/"
if(file.exists("//gangstore.ddns.net/flow/Documents/archiv/daten/2015/flow-gehen-und-laufen"))
  root.data.directory.path        <- "//gangstore.ddns.net/flow/Documents/archiv/daten/2015/flow-gehen-und-laufen/"

# Set raw data directory path
raw.data.directory.path <- paste(root.data.directory.path, "raw-data/", sep = "")

# Set clean data directory path
features.directory.path <- paste(root.data.directory.path, "cleaned-data/", sep = "")

# Read activity directory
activity.directory      <- "walking/" #readline("Type in activity directory and press return to continue (e. g. walking/) > ")

# Read user directory
user.directory          <- "grueter-barbara/" #readline("Type in user directory and press return to continue (e. g. doe-john/) > ")

# List al
fss.file.path.list      <- list.files(paste(raw.data.directory.path, activity.directory, user.directory, sep = ""), pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}-t[0-9]{2}-[0-9]{2}-[0-9]{2}-fss-data.csv", recursive = T)
