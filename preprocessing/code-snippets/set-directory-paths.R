# set directories
activity.directory <- paste(tolower(activity), "/",  sep = "")
user.directory <- paste(tolower(last.name), "-", tolower(first.name), "/",  sep = "")

# Set directory paths
raw.data.directory.path <- paste(root.directory.path, "raw-data/", activity.directory, user.directory, sep = "")
processed.data.directory.path <- paste(root.directory.path, "processed-data/", activity.directory, user.directory, sep = "")
feature.directory.path <- paste(root.directory.path, "features/", activity.directory, user.directory, sep = "")