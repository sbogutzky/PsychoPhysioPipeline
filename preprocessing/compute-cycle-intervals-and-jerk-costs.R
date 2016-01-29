# Version 2.0

# Remove all variables
rm(list = ls(all = T))  

# Load libraries
require(flow)

source("./code-snippets/read-set-load.R")

data.file.name <- readline("Type in data file name and press return to continue > ")

for (self.report.file.name in self.report.file.names) {
  
  source("./code-snippets/extract-session-start.R")
  
  # Load self report data
  self.report.data <- read.csv(paste(input.data.directory, self.report.file.name, sep = ""), comment.char = "#")
  
  # Loop measurements
  for(i in 1:nrow(self.report.data)) {
    
    source("./code-snippets/extract-self-report-times.R")
    
    file.name <- as.POSIXct(gsub(pattern = "# StartTime: ", replacement = "", x = start.time.line, ignore.case = T))
    data.path <- paste(preprocessed.data.directory, activity.directory, user.directory, date.directory, data.file.name, "-", i,  ".csv", sep = "")
    mid.swing.path <- paste(preprocessed.data.directory, activity.directory, user.directory, date.directory, data.file.name,"-mid-swing-indexes-", i,  ".csv", sep = "")
    
    if(file.exists(data.path) & file.exists(mid.swing.path)) {
      
      # Load motion data and midswing indexes
      data <- read.csv(data.path)
      mid.swing.indexes <- read.csv(mid.swing.path)[, 1]
      A <- data[, 1:4]
      
      # Plot raw acceleration
      PlotMostCommonCycleAcceleration(A[, 3], mid.swing.indexes, main = "Vertical")
      PlotMostCommonCycleAcceleration(A[, 4], mid.swing.indexes, main = "Horizontal")
      
      # Smooth acceleration
      fs <- 102.4
      fn <- fs/2
      n <- 2
      
      fc <- 6.5
      W <- fc/fn
      lp.vertical <- butter(n, W)
      lp.vertical.freg <- freqz(lp.vertical, Fs = fs)
      par(mfcol = c(2, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
      plot(lp.vertical.freg$f, abs(lp.vertical.freg$h), type = "l", xlim = c(0, 30), xlab = "Frequency (Hz)", ylab = "Magnitude Response", main = "Vertical acceleration filter")
      A[, 3] <- filtfilt(lp.vertical, A[, 3])
      
      fc <- 7.5
      W <- fc/fn
      lp.horizontal <- butter(n, W)
      lp.horizontal.freg <- freqz(lp.horizontal, Fs = fs)
      plot(lp.horizontal.freg$f, abs(lp.horizontal.freg$h), type = "l", xlim = c(0, 30), xlab = "Frequency (Hz)", ylab = "Magnitude Response", main = "Horizontal acceleration filter")
      A[, 4] <- filtfilt(lp.horizontal, A[, 4])
      
      # Plot filtered acceleration
      PlotMostCommonCycleAcceleration(A[, 3], mid.swing.indexes, main = "Vertical")
      PlotMostCommonCycleAcceleration(A[, 4], mid.swing.indexes, main = "Horizontal")
      
      # Compute output data
      t.s <- A[, 1][mid.swing.indexes] / 1000
      cycle.interval.s <- diff(t.s)
      t.s <- t.s[-1]
      
      jerk.cost.m2s5 <- c()
      for(j in 1:(length(mid.swing.indexes) - 1)) {
        
        m <- mid.swing.indexes[j]
        n <- mid.swing.indexes[(j+1)]
        
        # Compute jerk cost of each cycle
        t.ms                <- A[, 1][m:n]
        # acceleration.x.ms.2 <- A[, 2][m:n]
        acceleration.y.ms.2 <- A[, 3][m:n]
        acceleration.z.ms.2 <- A[, 4][m:n]
        
        jerk.cost   <- ComputeJerkCost(t.ms / 1000, data.frame(acceleration.y.ms.2, acceleration.z.ms.2), normalized = T)
        jerk.cost.m2s5  <- c(jerk.cost.m2s5, jerk.cost)
      }
      
      output.data <- data.frame(t.s, cycle.interval.s, jerk.cost.m2s5)
      output.data <- output.data[cycle.interval.s < 1.25,]
      
      # Detect outliers
      anomaly <- DetectAnomaly(cycle.interval.s, jerk.cost.m2s5 / 10^4, "Cycle Interval (s)", expression("JC (x"~10^4~m^2*s^{-5}~")"), c(min(cycle.interval.s), max(cycle.interval.s)), c(min(jerk.cost.m2s5 / 10^4), max(jerk.cost.m2s5 / 10^4)))
      if(length(anomaly$outliers) > 0) {
        output.data <- output.data[-anomaly$outliers, ]
      }
      
      # Compute mean to compare
      jerk.cost.by.all.accelerations <- ComputeJerkCost(A[, 1] / 1000, A[, 3:4], normalized = T) 
      jerk.cost.by.cycle.mean <- mean(output.data[, 3], na.rm = T)
      print(paste("Jerk Cost by Cycle:            ", jerk.cost.by.cycle.mean / 10^4))
      print(paste("Jerk Cost by all Accelerations:", jerk.cost.by.all.accelerations / 10^4))
      
      # Create directory, if needed
      output.directory <- paste(preprocessed.data.directory, activity.directory, user.directory, date.directory, sep="")
      if(!file.exists(substr(output.directory, 1, nchar(output.directory) - 1))) {
        dir.create(output.directory, recursive = TRUE)
      }
      
      # Write csv file
      output.file.name <- paste(data.file.name, "-cycle-intervals-jerk-costs-", i, ".csv", sep = "")
      output.directory <- paste(output.directory, output.file.name, sep = "")
      write.csv(output.data, output.directory, row.names = F)
      print(paste("Wrote:", output.directory))
  
    } else {
      print(paste("No files found:", data.path))
    }
    readline("Press return to continue > ")
  }
}