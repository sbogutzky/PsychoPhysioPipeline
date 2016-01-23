# Read root data directory
root.directory <- readline("Type in root directory and press return to continue (with: /) > ")

# Read properties
first.name <- readline("Type in first name and press return to continue > ")
activity <- readline("Type in activity and press return to continue > ")

# Set directories
raw.data.directory <- paste(root.directory, "raw-data/", sep = "")
feature.directory <- paste(root.directory, "features/", sep = "")
preprocessed.data.directory <- paste(root.directory, "preprocessed/", sep = "")
activity.directory <- paste(tolower(activity), "/",  sep = "")
user.directory <- paste(tolower(first.name), "/",  sep = "")
input.data.directory <- paste(raw.data.directory, activity.directory, user.directory, sep = "")