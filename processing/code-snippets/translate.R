# The MIT License (MIT)
# Copyright (c) 2016 Simon Bogutzky
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software
# and associated documentation files (the "Software"), to deal in the Software without restriction,
# including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
# DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
# ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

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