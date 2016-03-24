# Version 2.0

# Remove all variables
rm(list = ls(all = T))  

# Load libraries
require(flow)

source("./code-snippets/read-set-load.R")

data.file.name.1 <- readline("Type in data file name of the kubios file and press return to continue > ")
data.file.name.2 <- readline("Type in data file name of the first motion file and press return to continue > ")
data.file.name.3 <- readline("Type in data file name of the second motion file and press return to continue > ")

# Set time range
time.range.s  <- c(as.numeric(readline("Type in start for visualisation in seconds and press return to continue (e. g. 0) > ")), as.numeric(readline("Type in end for visualisation in seconds and press return to continue (e. g. 900) > ")))

# Set time window
time.window.s <- 10 # as.numeric(readline("Type in time window in seconds and press return to continue (e. g. 30) > "))

for (self.report.file.name in self.report.file.names) {
  
  source("./code-snippets/extract-session-start.R")
  
  # Load self report data
  self.report.data <- read.csv(paste(input.data.directory, self.report.file.name, sep = ""), comment.char = "#")
  
  # Loop measurements
  for(i in 1:nrow(self.report.data)) {
    
    source("./code-snippets/extract-self-report-times.R")
    
    mid.swing.data.file.path.1 <- paste(preprocessed.data.directory, activity.directory, user.directory, date.directory, data.file.name.2, "-mid-swing-indexes-", i, ".csv", sep="")
    mid.swing.data.file.path.2 <- paste(preprocessed.data.directory, activity.directory, user.directory, date.directory, data.file.name.3, "-mid-swing-indexes-", i, ".csv", sep="")
    kubios.hrv.data.file.path <- paste(preprocessed.data.directory, activity.directory, user.directory, date.directory, data.file.name.1, "-", i, "_hrv.txt", sep="")
  
    if(file.exists(mid.swing.data.file.path.1) & file.exists(kubios.hrv.data.file.path)) {
    
      # Load data
      mid.swing.data.1            <- read.csv(mid.swing.data.file.path.1, skip = 0)
      mid.swing.data.2            <- read.csv(mid.swing.data.file.path.2, skip = 0)
      mid.swing.data.1$side <- rep("R", nrow(mid.swing.data.1))
      mid.swing.data.2$side <- rep("L", nrow(mid.swing.data.2))
      
      mid.swing.data <- rbind(mid.swing.data.1, mid.swing.data.2)
      mid.swing.data <- mid.swing.data[order(mid.swing.data$timestamp.ms),]
      
      #step.times <- c(mid.swing.data.1$timestamp.ms, mid.swing.data.2$timestamp.ms)
      step.times <- mid.swing.data$timestamp.ms / 1000
      
      source("./code-snippets/read-kubios-hrv-data.R")
      
      # Data
      heart.times <- c(kubios.hrv.data.time.data$t.s[1] - kubios.hrv.data.time.data$rr.interval.s[1], kubios.hrv.data.time.data$t.s)
      spm <- 60 / diff(step.times) 
      bpm <- 60 / diff(heart.times) 
      y.lim.1 <- c(min(mean(spm) - sd(spm) * 2, mean(bpm) - sd(bpm) * 2), max(mean(spm) + sd(spm) * 2, mean(bpm) + sd(bpm) * 2))
      
      # Plot
      par(mfcol = c(4, 1), mar = c(3.5, 4, 2, 4) + 0.1, mgp = c(2.5, 1, 0))
      y.lim <- c(0, 1)
      
      # SPM vs. BPM 
      plot(step.times[-1], spm, xlab = "", ylab = "Cadence & HR", xaxt = "n", xlim = time.range.s, ylim = y.lim.1, xaxs = "i", pch = 21)
      points(heart.times[-1], bpm, pch = 22)
      abline(v = seq(time.range.s[1], time.range.s[2], time.window.s), lty = "dashed", col = "grey")
      axis(1, at = seq(time.range.s[1], time.range.s[2], time.window.s), labels = seq(time.range.s[1], time.range.s[2], time.window.s), las = 1)
      legend("topright", c("SPM", "BPM"), pch = c(21, 22), bg = "white")
      box()
      
      title(format(session.start, "%Y-%m-%d %H:%M", tz = "CET"))
      
      # Stroboscopic Technique
      instantaneous.phase.data <- ComputeInstantaneousPhases(heart.times, step.times)
      t <- instantaneous.phase.data$ts
      fi <- instantaneous.phase.data$fi # instantaneous phases
      psi <- (fi %% (2 * pi)) / (2 * pi) # relative phases
      
      plot(t, psi, xlab = "", ylab = expression(Psi(t)), xaxt = "n",  yaxt = "n", xlim = time.range.s, xaxs = "i", yaxs = "i", ylim = y.lim, pch = 21)
      abline(v = seq(time.range.s[1], time.range.s[2], time.window.s), lty = "dashed", col = "grey")
      axis(1, at = seq(time.range.s[1], time.range.s[2], time.window.s), labels = seq(time.range.s[1], time.range.s[2], time.window.s), las = 1)
      axis(2, at = seq(y.lim[1], y.lim[2], .2), labels = seq(y.lim[1], y.lim[2], .2))
      box()
      
      # Indexes
      time.points <- seq(min(t) + time.window.s/2, max(t) - time.window.s/2, 1)
      phase.coherence.indexes <- c()
      normalized.shannon.entropy.indexes <- c()
      for (time.point in time.points) {
        phase.coherence.indexes <- c(phase.coherence.indexes, ComputePhaseCoherenceIndex(t, psi, time.point, time.window.s))
        normalized.shannon.entropy.indexes <- c(normalized.shannon.entropy.indexes, ComputeNormalizedShannonEntropyIndex(t, psi, time.point, time.window.s)) 
      }
      rm(time.point)
      plot(time.points, phase.coherence.indexes, type = "l", xlab = "t (s)", ylab = "Indexes", xaxt = "n",  yaxt = "n", xlim = time.range.s, xaxs = "i", yaxs = "i", ylim = y.lim)
      lines(time.points, normalized.shannon.entropy.indexes, lty = 2)
      abline(v = seq(time.range.s[1], time.range.s[2], time.window.s), lty = "dashed", col = "grey")
      axis(1, at = seq(time.range.s[1], time.range.s[2], time.window.s), labels = seq(time.range.s[1], time.range.s[2], time.window.s), las = 1)
      axis(2, at = seq(y.lim[1], y.lim[2], .2), labels = seq(y.lim[1], y.lim[2], .2))
      legend("topright", c("PCoI", "NSEI"), lty = c("solid", "dashed"),  bg = "white")
      box()
      
      # Distribution
      hist(psi, breaks = seq(0, 1, .05), col = "black", border = "white", main = "", xlab = expression(Psi),  xaxs = "i", yaxs = "i", ylab = "")
      
      # Create directory, if needed
      output.directory.path <- paste(preprocessed.data.directory, activity.directory, user.directory, date.directory, sep="")
      if(!file.exists(substr(output.directory.path, 1, nchar(output.directory.path) - 1))) {
        dir.create(output.directory.path, recursive = TRUE)
      }
      
      # Write csv file
      output.file.path <- paste(output.directory.path, "cls-indexes-", i, ".csv", sep = "")
      op <- options(digits.secs=3)
      con <- file(output.file.path, 'w') 
      writeLines(strftime(session.start, format="%Y-%m-%d"), con = con)
      writeLines(strftime(session.start, format="%H:%M:%OS"), con = con)
      write.csv(data.frame(t.s = time.points, pcoi = phase.coherence.indexes, nsei = normalized.shannon.entropy.indexes), file = con, row.names = FALSE)
      close(con)
      options(op) #reset options
      print(paste("Wrote:", output.file.path))
      
      # Write csv file
      output.file.path <- paste(output.directory.path, "cls-phases-", i, ".csv", sep = "")
      op <- options(digits.secs=3)
      con <- file(output.file.path, 'w') 
      writeLines(strftime(session.start, format="%Y-%m-%d"), con = con)
      writeLines(strftime(session.start, format="%H:%M:%OS"), con = con)
      write.csv(data.frame(t.s = t, fi, psi), file = con, row.names = FALSE)
      close(con)
      options(op) #reset options
      print(paste("Wrote:", output.file.path))
      
    } else {
      print("No data")
    }
    readline("Press return to continue > ")
  }
}