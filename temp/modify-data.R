my.write <- function(x, file, header, footer, f = write.csv, ...){
  # create and open the file connection
  datafile <- file(file, open = 'wt')
  # close on exit
  on.exit(close(datafile))
  # if a header is defined, write it to the file (@CarlWitthoft's suggestion)
  if(!missing(header)) writeLines(header,con = datafile)
  # write the file using the defined function and required addition arguments  
  f(x, datafile, ...)
  # if a footer is defined, write it to the file (@CarlWitthoft's suggestion)
  if(!missing(footer)) writeLines(footer, con = datafile)
}

fss.data <- read.csv("C:/Users/sbogutzky/Desktop/2013-10-03/2013-10-03-t17-39-20-fss-data.csv")
ecg.data <- read.csv("C:/Users/sbogutzky/Desktop/2013-10-03/2013-10-03-t17-39-20-ecg-data.csv")
leg.data <- read.csv("C:/Users/sbogutzky/Desktop/2013-10-03/2013-10-03-t17-39-20-leg-data.csv")

as.POSIXlt(fss.data$SystemTime[1] / 1000, "Europe/Berlin", origin = "1970-01-01")
as.POSIXlt(ecg.data$SystemTime[1] / 1000, "Europe/Berlin", origin = "1970-01-01")
as.POSIXlt(leg.data$SystemTime[1] / 1000, "Europe/Berlin", origin = "1970-01-01")

time.difference.1 <- fss.data$SystemTime[1] - ecg.data$SystemTime[1] - 900000
time.difference.2 <- fss.data$SystemTime[1] - leg.data$SystemTime[1] - 900000

if(nrow(fss.data) == 5) {
  start.time <- as.POSIXlt((fss.data$SystemTime[1] - 900000) / 1000, "Europe/Berlin", origin = "1970-01-01")
  stop.time <- as.POSIXlt(fss.data$EndTime[1] / 1000, "Europe/Berlin", origin = "1970-01-01")
  
  header.comment <- gsub("-", "/", paste("# StartTime:", start.time))
  footer.comment <- gsub("-", "/", paste("# StopTime:", stop.time))
  
  self.report.baseline <- data.frame(c(900000, 900000, 900000 + fss.data[1, 18] - fss.data[1, 17], fss.data[1, 1:16]))
  names(self.report.baseline) <- c("timestamp.show.ms", "timestamp.start.ms", "timestamp.stop.ms", "item.01", "item.02", "item.03", "item.04", "item.05", "item.06", "item.07", "item.08", "item.09", "item.10", "item.11", "item.12", "item.13", "item.14", "item.15", "item.16")
  
  my.write(self.report.baseline, "C:/Users/sbogutzky/Desktop/data (lokal)/2013/raw-data/baseline/buse-patrick/2013-10-03--17-39-21/self-report.csv", header.comment, footer.comment, quote = FALSE, row.names = FALSE)
  
  ecg.data.1 <- ecg.data
  ecg.data.1$SensorTime <- ecg.data$SensorTime - ecg.data$SensorTime[1] + time.difference.1
  ecg.data.baseline <- ecg.data.1[ecg.data.1$SensorTime < self.report.baseline$timestamp.stop.ms, 1:3]
  names(ecg.data.baseline) <- c("timestamp.ms", "ecg.ra.ll.mv", "ecg.la.ll.mv")
  
  # Resample data
  fs <- 205
  M <- ResampleData(ecg.data.baseline[, 2:3], fs, ecg.data.baseline$timestamp.ms)
  data.1 <- data.frame(M)
  colnames(data.1) <- colnames(ecg.data.baseline)
  ecg.data.baseline <- data.1

  my.write(ecg.data.baseline, "C:/Users/sbogutzky/Desktop/data (lokal)/2013/raw-data/baseline/buse-patrick/2013-10-03--17-39-21/imu-rn42-bd38.csv", header.comment, footer.comment, quote = FALSE, row.names = FALSE)
  
  leg.data.1 <- leg.data
  leg.data.1$SensorTime <- leg.data$SensorTime - leg.data$SensorTime[1] + time.difference.2
  leg.data.baseline <- leg.data.1[leg.data.1$SensorTime < self.report.baseline$timestamp.stop.ms, 1:7]
  names(leg.data.baseline) <- c("timestamp.ms", "acceleration.x.ms.2", "acceleration.y.ms.2", "acceleration.z.ms.2", "angular.velocity.x.deg.s", "angular.velocity.y.deg.s", "angular.velocity.z.deg.s")
  
  leg.data.baseline$acceleration.x.ms.2 <- -leg.data.baseline$acceleration.x.ms.2
  leg.data.baseline$acceleration.z.ms.2 <- -leg.data.baseline$acceleration.z.ms.2
  leg.data.baseline$angular.velocity.x.deg.s <- -leg.data.baseline$angular.velocity.x.deg.s
  leg.data.baseline$angular.velocity.z.deg.s <- -leg.data.baseline$angular.velocity.z.deg.s
  
  my.write(leg.data.baseline, "C:/Users/sbogutzky/Desktop/data (lokal)/2013/raw-data/baseline/buse-patrick/2013-10-03--17-39-21/imu-rn42-3b70.csv", header.comment, footer.comment, quote = FALSE, row.names = FALSE)
  
  start.time <- as.POSIXlt(fss.data$EndTime[1] / 1000, "Europe/Berlin", origin = "1970-01-01")
  stop.time <- as.POSIXlt(fss.data$EndTime[5] / 1000, "Europe/Berlin", origin = "1970-01-01")
  
  header.comment <- gsub("-", "/", paste("# StartTime:", start.time))
  footer.comment <- gsub("-", "/", paste("# StopTime:", stop.time))
  
  self.report.laufen.1 <- data.frame(c(900000, 900000, 900000 + fss.data[2, 18] - fss.data[2, 17], fss.data[2, 1:16]))
  names(self.report.laufen.1) <- c("timestamp.show.ms", "timestamp.start.ms", "timestamp.stop.ms", "item.01", "item.02", "item.03", "item.04", "item.05", "item.06", "item.07", "item.08", "item.09", "item.10", "item.11", "item.12", "item.13", "item.14", "item.15", "item.16")
  self.report.laufen.2 <- data.frame(c(self.report.laufen.1$timestamp.stop.ms + 900000, self.report.laufen.1$timestamp.stop.ms + 900000, self.report.laufen.1$timestamp.stop.ms + 900000 + fss.data[3, 18] - fss.data[3, 17], fss.data[3, 1:16]))
  names(self.report.laufen.2) <- c("timestamp.show.ms", "timestamp.start.ms", "timestamp.stop.ms", "item.01", "item.02", "item.03", "item.04", "item.05", "item.06", "item.07", "item.08", "item.09", "item.10", "item.11", "item.12", "item.13", "item.14", "item.15", "item.16")
  self.report.laufen.3 <- data.frame(c(self.report.laufen.2$timestamp.stop.ms + 900000, self.report.laufen.2$timestamp.stop.ms + 900000, self.report.laufen.2$timestamp.stop.ms + 900000 + fss.data[4, 18] - fss.data[4, 17], fss.data[4, 1:16]))
  names(self.report.laufen.3) <- c("timestamp.show.ms", "timestamp.start.ms", "timestamp.stop.ms", "item.01", "item.02", "item.03", "item.04", "item.05", "item.06", "item.07", "item.08", "item.09", "item.10", "item.11", "item.12", "item.13", "item.14", "item.15", "item.16")
  self.report.laufen.4 <- data.frame(c(self.report.laufen.3$timestamp.stop.ms + 900000, self.report.laufen.3$timestamp.stop.ms + 900000, self.report.laufen.3$timestamp.stop.ms + 900000 + fss.data[5, 18] - fss.data[5, 17], fss.data[5, 1:16]))
  names(self.report.laufen.4) <- c("timestamp.show.ms", "timestamp.start.ms", "timestamp.stop.ms", "item.01", "item.02", "item.03", "item.04", "item.05", "item.06", "item.07", "item.08", "item.09", "item.10", "item.11", "item.12", "item.13", "item.14", "item.15", "item.16")
  
  self.report.laufen <- rbind(self.report.laufen.1, self.report.laufen.2, self.report.laufen.3, self.report.laufen.4)
  
  my.write(self.report.laufen, "C:/Users/sbogutzky/Desktop/data (lokal)/2013/raw-data/laufen/buse-patrick/2013-10-03--17-55-54/self-report.csv", header.comment, footer.comment, quote = FALSE, row.names = FALSE)
  
  ecg.data.1 <- ecg.data
  ecg.data.1$SensorTime <- ecg.data$SensorTime - ecg.data$SensorTime[1] + time.difference.1
  ecg.data.laufen <- ecg.data.1[ecg.data.1$SensorTime >= self.report.baseline$timestamp.stop.ms, 1:3]
  names(ecg.data.laufen) <- c("timestamp.ms", "ecg.ra.ll.mv", "ecg.la.ll.mv")
  ecg.data.laufen$timestamp.ms <- ecg.data.laufen$timestamp.ms - ecg.data.laufen$timestamp.ms[1]
  ecg.data.laufen <- ecg.data.laufen[ecg.data.laufen$timestamp.ms < self.report.laufen.4$timestamp.stop.ms, ]
  
  # Resample data
  fs <- 205
  M <- ResampleData(ecg.data.laufen[, 2:3], fs, ecg.data.laufen$timestamp.ms)
  data.1 <- data.frame(M)
  colnames(data.1) <- colnames(ecg.data.laufen)
  ecg.data.laufen <- data.1
  
  my.write(ecg.data.laufen, "C:/Users/sbogutzky/Desktop/data (lokal)/2013/raw-data/laufen/buse-patrick/2013-10-03--17-55-54/imu-rn42-bd38.csv", header.comment, footer.comment, quote = FALSE, row.names = FALSE)
  
  leg.data.1 <- leg.data
  leg.data.1$SensorTime <- leg.data$SensorTime - leg.data$SensorTime[1] + time.difference.2
  leg.data.laufen <- leg.data.1[leg.data.1$SensorTime >= self.report.baseline$timestamp.stop.ms, 1:7]
  names(leg.data.laufen) <- c("timestamp.ms", "acceleration.x.ms.2", "acceleration.y.ms.2", "acceleration.z.ms.2", "angular.velocity.x.deg.s", "angular.velocity.y.deg.s", "angular.velocity.z.deg.s")
  leg.data.laufen$timestamp.ms <- leg.data.laufen$timestamp.ms - leg.data.laufen$timestamp.ms[1]
  leg.data.laufen <- leg.data.laufen[leg.data.laufen$timestamp.ms < self.report.laufen.4$timestamp.stop.ms, ]
  
  leg.data.laufen$acceleration.x.ms.2 <- -leg.data.laufen$acceleration.x.ms.2
  leg.data.laufen$acceleration.z.ms.2 <- -leg.data.laufen$acceleration.z.ms.2
  leg.data.laufen$angular.velocity.x.deg.s <- -leg.data.laufen$angular.velocity.x.deg.s
  leg.data.laufen$angular.velocity.z.deg.s <- -leg.data.laufen$angular.velocity.z.deg.s
  
  my.write(leg.data.laufen, "C:/Users/sbogutzky/Desktop/data (lokal)/2013/raw-data/laufen/buse-patrick/2013-10-03--17-55-54/imu-rn42-3b70.csv", header.comment, footer.comment, quote = FALSE, row.names = FALSE)
  
  } else {
  }


# s <- seq(0, nrow(data), 10)
# plot(data$timestamp.ms[s], data$angular.velocity.x.deg.s[s], type = "l")
# abline(v = self.report.data$timestamp.start.ms, col = "red")
# session.zoom()
