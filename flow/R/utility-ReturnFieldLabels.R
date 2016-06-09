#' Returns label for the fields.
#' 
#' \code{ReturnFieldLabels} returns the field label by column
#' 
#' @param col.name. A character vector that specifies the column name
#' @return A expression for the field label

ReturnFieldLabels <- function(col.name) {
  field.label = col.name
  if(col.name == "acceleration.x.ms.2") {
    field.label = expression("Acceleration X (" ~ m/s^2 ~ ")")
  }
  if(col.name == "acceleration.y.ms.2") {
    field.label = expression("Acceleration Y (" ~ m/s^2 ~ ")")
  }
  if(col.name == "acceleration.z.ms.2") {
    field.label = expression("Acceleration Z (" ~ m/s^2 ~ ")")
  }
  if(col.name == "angular.velocity.x.deg.s") {
    field.label = expression("Angular Velocity X (" ~ deg/s ~ ")")
  }
  if(col.name == "angular.velocity.y.deg.s") {
    field.label = expression("Angular Velocity Y (" ~ deg/s ~ ")")
  }
  if(col.name == "angular.velocity.z.deg.s") {
    field.label = expression("Angular Velocity Z (" ~ deg/s ~ ")")
  }
  return(field.label)
}