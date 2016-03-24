# Version 2.0

# Remove all variables
rm(list = ls(all = T))  

# Load libraries
require(flow)
require(signal)

source("./code-snippets/read-set-load.R")
input.data.directory <- paste(raw.data.directory, activity.directory, sep = "")
self.report.file.names <- list.files(path = input.data.directory, pattern = "self-report.csv", recursive = T)

data.file.names <- c("imu-rn42-bc98", "imu-rn42-3b70") # readline("Type in data file name and press return to continue > ")

optimal.cutoff.frequencies <- c()
count <- 0
for (data.file.name in data.file.names) {
  for (self.report.file.name in self.report.file.names) {
    
    source("./code-snippets/extract-session-start.R")
    
    # Load self report data
    self.report.data <- read.csv(paste(input.data.directory, self.report.file.name, sep = ""), comment.char = "#")
    
    # Loop measurements
    for(i in 1:nrow(self.report.data)) {
      
      source("./code-snippets/extract-self-report-times.R")
      user.directory <- paste(strsplit(self.report.file.name, "/")[[1]][1], "/", sep = "")
      data.path <- paste(preprocessed.data.directory, activity.directory, user.directory, date.directory, data.file.name, "-", i,  ".csv", sep = "")
      
      # Compute optimal cutoff frequencies
      if(file.exists(data.path)) {
        data <- read.csv(data.path)
        
        A <- data[, 1:4]
        fs <- 102.4
        n <- 4
        count <- count + 1
        optimal.cutoff.frequencies <- c(optimal.cutoff.frequencies, ComputeOptimalCutoffFrequency(A[, 2], fs, n), ComputeOptimalCutoffFrequency(A[, 3], fs, n), ComputeOptimalCutoffFrequency(A[, 4], fs, n))
        }
    }
  }
}

optimal.cutoff.frequencies <- rowMeans(matrix(optimal.cutoff.frequencies, 3, count), na.rm = T)

source("./code-snippets/read-set-load.R")
for (data.file.name in data.file.names) {
  for (self.report.file.name in self.report.file.names) {
    
    source("./code-snippets/extract-session-start.R")
    
    # Load self report data
    self.report.data <- read.csv(paste(input.data.directory, self.report.file.name, sep = ""), comment.char = "#")
    
    #Loop measurements
    for(i in 1:nrow(self.report.data)) {
      
      source("./code-snippets/extract-self-report-times.R")
      
      data.path <- paste(preprocessed.data.directory, activity.directory, user.directory, date.directory, data.file.name, "-", i,  ".csv", sep = "")
      mid.swing.path <- paste(preprocessed.data.directory, activity.directory, user.directory, date.directory, data.file.name,"-mid-swing-indexes-", i,  ".csv", sep = "")
      
      if(file.exists(data.path) & file.exists(mid.swing.path)) {
        
        # Load motion data and midswing indexes
        data <- read.csv(data.path)
        mid.swing.indexes <- read.csv(mid.swing.path)[, 2]
        A <- data[, 1:4]
        
        # Plot raw acceleration
        PlotMostCommonCycleAcceleration(A[, 2], mid.swing.indexes, main = "Coronary")
        PlotMostCommonCycleAcceleration(A[, 3], mid.swing.indexes, main = "Sagittal")
        PlotMostCommonCycleAcceleration(A[, 4], mid.swing.indexes, main = "Axial")
        
        # Smooth acceleration
        fs <- 102.4
        fn <- fs/2
        n <- 4
        
        par(mfcol = c(3, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
        
        fc <- optimal.cutoff.frequencies[1]
        W <- fc/fn
        lp.coronary <- butter(n, W)
        lp.coronary.freg <- freqz(lp.coronary, Fs = fs)
        plot(lp.coronary.freg$f, abs(lp.coronary.freg$h), type = "l", xlim = c(0, 30), xlab = "Frequency (Hz)", ylab = "Magnitude Response", main = "Coronary acceleration filter")
        A[, 2] <- filtfilt(lp.coronary, A[, 2])
        
        fc <- optimal.cutoff.frequencies[2]
        W <- fc/fn
        lp.sagittal <- butter(n, W)
        lp.sagittal.freg <- freqz(lp.sagittal, Fs = fs)
        plot(lp.sagittal.freg$f, abs(lp.sagittal.freg$h), type = "l", xlim = c(0, 30), xlab = "Frequency (Hz)", ylab = "Magnitude Response", main = "Sagittal acceleration filter")
        A[, 3] <- filtfilt(lp.sagittal, A[, 3])
        
        fc <- optimal.cutoff.frequencies[3]
        W <- fc/fn
        lp.axial <- butter(n, W)
        lp.axial.freg <- freqz(lp.axial, Fs = fs)
        plot(lp.axial.freg$f, abs(lp.axial.freg$h), type = "l", xlim = c(0, 30), xlab = "Frequency (Hz)", ylab = "Magnitude Response", main = "Axial acceleration filter")
        A[, 4] <- filtfilt(lp.axial, A[, 4])
        
        # Plot filtered acceleration
        PlotMostCommonCycleAcceleration(A[, 2], mid.swing.indexes, main = "Coronary")
        PlotMostCommonCycleAcceleration(A[, 3], mid.swing.indexes, main = "Sagittal")
        PlotMostCommonCycleAcceleration(A[, 4], mid.swing.indexes, main = "Axial")
        
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
          acceleration.x.ms.2 <- A[, 2][m:n]
          acceleration.y.ms.2 <- A[, 3][m:n]
          acceleration.z.ms.2 <- A[, 4][m:n]
          
          jerk.cost   <- ComputeJerkCost(t.ms / 1000, data.frame(acceleration.x.ms.2, acceleration.y.ms.2, acceleration.z.ms.2), normalized = T)
          jerk.cost.m2s5  <- c(jerk.cost.m2s5, jerk.cost)
        }
        
        output.data <- data.frame(t.s, cycle.interval.s, jerk.cost.m2s5)
        # output.data <- output.data[cycle.interval.s < 1.25,]
        
        # Detect outliers
        # anomaly <- DetectAnomaly(cycle.interval.s, jerk.cost.m2s5 / 10^5, "Cycle Interval (s)", expression("JC (x"~10^5~m^2*s^{-5}~")"), c(min(cycle.interval.s), max(cycle.interval.s)), c(min(jerk.cost.m2s5 / 10^5), max(jerk.cost.m2s5 / 10^5)))
        # if(length(anomaly$outliers) > 0) {
        #   output.data <- output.data[-anomaly$outliers, ]
        # }
        
        # Compute mean to compare
        jerk.cost.by.all.accelerations <- ComputeJerkCost(A[, 1] / 1000, A[, 2:4], normalized = T) 
        jerk.cost.by.cycle.mean <- mean(output.data[, 3], na.rm = T)
        print(paste("Jerk Cost by Cycle:            ", jerk.cost.by.cycle.mean / 10^5))
        print(paste("Jerk Cost by all Accelerations:", jerk.cost.by.all.accelerations / 10^5))
        
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
      #readline("Press return to continue > ")
    }
  }
  readline("Press return to continue > ")
}