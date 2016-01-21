#' GetDataLabel 
#' 
#' \code{GetDataLabel} is ...

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