kubios.hrv.data.time.data <- read.csv(kubios.hrv.data.file.path, header = F, na.strings = "", fill = T, skip = 117, stringsAsFactors = F)

kubios.hrv.data.time.data <- kubios.hrv.data.time.data[, 3:ncol(kubios.hrv.data.time.data)-1]

samples <- ncol(kubios.hrv.data.time.data) / 9

data <- kubios.hrv.data.time.data[, 1:2]
if(samples > 1) {
  for (k in 2:samples) {
    m <- ((k-1) * 9) + 1
    n <- m + 1
    
    data.1 <- kubios.hrv.data.time.data[, m:n]
    names(data.1) <- names(data) 
    data <- rbind(data, data.1)
    
    m <- ((k-1) * 2) + 1
    n <- m + 1
    
    o <- which(kubios.hrv.data.time.data[, 3] == 2) + 1
    p <- nrow(kubios.hrv.data.time.data)
    
    data.1 <- kubios.hrv.data.time.data[o:p, m:n]
    names(data.1) <- names(data) 
    data <- rbind(data, data.1)
    
  }
  rm(samples, k, m, n, o, p, data.1)
  data <- data[complete.cases(data), ]
}

kubios.hrv.data.time.data <- data
rm(data)
names(kubios.hrv.data.time.data) <- c("t.s", "rr.interval.s")
kubios.hrv.data.time.data <- kubios.hrv.data.time.data[order(kubios.hrv.data.time.data$t.s),]