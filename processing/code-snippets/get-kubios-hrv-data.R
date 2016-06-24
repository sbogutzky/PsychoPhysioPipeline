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

kubios.hrv.data <- read.csv(kubios.hrv.data.file.path, header = F, na.strings = "", fill = T, skip = 117, stringsAsFactors = F)

kubios.hrv.data <- kubios.hrv.data[, 3:ncol(kubios.hrv.data)-1]

samples <- ncol(kubios.hrv.data) / 9

data <- kubios.hrv.data[, 1:2]
if(samples > 1) {
  for (k in 2:samples) {
    m <- ((k-1) * 9) + 1
    n <- m + 1
    
    data.cols <- kubios.hrv.data[, m:n]
    names(data.cols) <- names(data) 
    data <- rbind(data, data.cols)
    
    m <- ((k-1) * 2) + 1
    n <- m + 1
    
    o <- which(kubios.hrv.data[, 3] == 2) + 1
    p <- nrow(kubios.hrv.data)
    
    data.cols <- kubios.hrv.data[o:p, m:n]
    names(data.cols) <- names(data) 
    data <- rbind(data, data.cols)
    
  }
  rm(samples, k, m, n, o, p, data.cols)
  data <- data[complete.cases(data), ]
}

kubios.hrv.data <- data
rm(data)
names(kubios.hrv.data) <- c("timestamp.s", "rr.interval.s")
kubios.hrv.data <- kubios.hrv.data[order(kubios.hrv.data[, 1]),]