# Read root data directory
root.directory <- readline("Type in root directory and press return to continue (with: /) > ")

# Read properties
first.name <- readline("Type in first name and press return to continue > ")
last.name <- readline("Type in last name and press return to continue > ")
activity <- readline("Type in activity and press return to continue > ")

# Set directories
raw.data.directory <- paste(root.directory, "raw-data/", sep = "")
feature.directory <- paste(root.directory, "features/", sep = "")
preprocessed.data.directory <- paste(root.directory, "preprocessed/", sep = "")
activity.directory <- paste(tolower(activity), "/",  sep = "")
user.directory <- paste(tolower(last.name), "-", tolower(first.name), "/",  sep = "")
input.data.directory <- paste(raw.data.directory, activity.directory, user.directory, sep = "")

# Load all self report file names
self.report.file.names <- list.files(path = input.data.directory, pattern = "self-report.csv", recursive = T)

# Load functions
GetDataLabel <- function(col.name) {
  data.label = col.name
  if(col.name == "acceleration.x.ms.2") {
    data.label = expression("Acceleration X (" ~ m/s^2 ~ ")")
  }
  if(col.name == "acceleration.y.ms.2") {
    data.label = expression("Acceleration Y (" ~ m/s^2 ~ ")")
  }
  if(col.name == "acceleration.z.ms.2") {
    data.label = expression("Acceleration Z (" ~ m/s^2 ~ ")")
  }
  if(col.name == "angular.velocity.x.deg.s") {
    data.label = expression("Angular Velocity X (" ~ deg/s ~ ")")
  }
  if(col.name == "angular.velocity.y.deg.s") {
    data.label = expression("Angular Velocity Y (" ~ deg/s ~ ")")
  }
  if(col.name == "angular.velocity.z.deg.s") {
    data.label = expression("Angular Velocity Z (" ~ deg/s ~ ")")
  }
  return(data.label)
}