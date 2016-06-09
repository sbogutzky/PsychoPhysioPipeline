#' Returns label for the fields.
#' 
#' \code{ReturnFieldLabels} returns the field label by column
#' 
#' @param col.name. A character vector that specifies the column name
#' @return A expression for the field label

ReturnFieldLabels <- function(col.name) {
  field.label = col.name
  if(col.name == "acceleration.x.ms.2") {
    field.label = expression("Beschleunigung X (" ~ m%.%s^{-2} ~ ")")
  }
  if(col.name == "acceleration.y.ms.2") {
    field.label = expression("Beschleunigung Y (" ~ m%.%s^{-2} ~ ")")
  }
  if(col.name == "acceleration.z.ms.2") {
    field.label = expression("Beschleunigung Z (" ~ m%.%s^{-2} ~ ")")
  }
  if(col.name == "angular.velocity.x.deg.s") {
    field.label = expression("Winkelgeschwindigkeit X (" ~ deg%.%s^{-1} ~ ")")
  }
  if(col.name == "angular.velocity.y.deg.s") {
    field.label = expression("Winkelgeschwindigkeit Y (" ~ deg%.%s^{-1} ~ ")")
  }
  if(col.name == "angular.velocity.z.deg.s") {
    field.label = expression("Winkelgeschwindigkeit Z (" ~ deg%.%s^{-1} ~ ")")
  }
  return(field.label)
}