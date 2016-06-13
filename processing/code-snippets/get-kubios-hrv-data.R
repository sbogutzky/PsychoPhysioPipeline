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