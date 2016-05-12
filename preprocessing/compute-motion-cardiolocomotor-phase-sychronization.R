# Remove all variables
rm(list = ls(all = T)) 

# Load libraries
require(flow)

# Set root data directory path
root.data.directory.path          <- "C:/Users/sbogutzky/Desktop/data (lokal)/2013/"

# Set processed data directory path
processed.data.directory.path <- paste(root.data.directory.path, "processed-data/", sep = "")

# Set features directory path
features.directory.path <- paste(root.data.directory.path, "features/", sep = "")

# Read activity directory
activity.directory <- "running/"

# Read user directory
user.directory <- "buse-patrick/"

# Read in body position
body.position <- "leg" # readline("Type in body position and press return to continue (e. g. leg) > ")

# Set time range
time.range.s  <- c(0, 900) #c(as.numeric(readline("Type in start for visualisation in seconds and press return to continue (e. g. 0) > ")), as.numeric(readline("Type in end for visualisation in seconds and press return to continue (e. g. 900) > ")))

# Set time window
time.window.s <- 10 # as.numeric(readline("Type in time window in seconds and press return to continue (e. g. 30) > "))

# Load fss features
fss.features        <- read.csv(paste(features.directory.path, activity.directory, user.directory, "fss-features.csv", sep = ""), stringsAsFactors = F)

for (i in 21:nrow(fss.features)) {
  properties      <- fss.features[i, c(13:20)]
  activity.start  <- properties[, 2]
  activity.end    <- properties[, 3]
  measurement     <- properties[, 5]
  if(measurement == 1) {
    date.directory  <- paste(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y-%m-%d--%H-%M-%S"), "/", sep ="")
  }
  
  # Set file paths
  jerk.cost.data.file.path   <- paste(processed.data.directory.path, activity.directory, user.directory, date.directory, body.position, "-jerk-cost-data-", measurement, ".csv", sep="")
  kubios.hrv.data.file.path  <- paste(processed.data.directory.path, activity.directory, user.directory, date.directory, "ecg-data-", measurement, "_hrv.txt", sep="")
  
  if(file.exists(jerk.cost.data.file.path) & file.exists(kubios.hrv.data.file.path)) {
    
    # Load data
    jerk.cost.data            <- read.csv(jerk.cost.data.file.path, skip = 2)
    kubios.hrv.data.time.data <- read.csv(kubios.hrv.data.file.path, header = F, na.strings = "", fill = T, skip = 117, stringsAsFactors = F, col.names = c("", "t.s", "rr.interval.s", "", "", "", "", "", "", "", ""))[,2:3]
    
    # Data
    step.times <- jerk.cost.data$t.s
    heart.times <- c(kubios.hrv.data.time.data$t.s[1] - kubios.hrv.data.time.data$rr.interval.s[1], kubios.hrv.data.time.data$t.s)
    spm <- 120 / jerk.cost.data$cycle.interval.s
    bpm <- 60 / kubios.hrv.data.time.data$rr.interval.s
    
    print(mean(spm))
    print(mean(bpm))
    
    y.lim.1 <- c(min(mean(spm) - sd(spm) * 2, mean(bpm) - sd(bpm) * 2), max(mean(spm) + sd(spm) * 2, mean(bpm) + sd(bpm) * 2))
    
    # Plot
    par(mfcol = c(3, 1), mar = c(3.5, 4, 2, 4) + 0.1, mgp = c(2.5, 1, 0))
    y.lim <- c(0, 1)
    
    # SPM vs. BPM 
    plot(step.times, spm, xlab = "", ylab = "Cadence & HR", xaxt = "n", xlim = time.range.s, ylim = y.lim.1, xaxs = "i", yaxs = "i", pch = 21)
    points(heart.times[-1], bpm, pch = 22)
    abline(v = seq(time.range.s[1], time.range.s[2], time.window.s), lty = "dashed", col = "grey")
    axis(1, at = seq(time.range.s[1], time.range.s[2], time.window.s), labels = seq(time.range.s[1], time.range.s[2], time.window.s), las = 1)
    legend("topright", c("SPM", "BPM"), pch = c(21, 22), bg = "white")
    box()
    
    title(format(as.POSIXct(activity.start/1000, origin = "1970-01-01", tz = "CET"), "%Y-%m-%d %H:%M", tz = "CET"))
    
    heart.times <- heart.times[heart.times > 60]
    step.times <- step.times[step.times > 60]
    
    # Stroboscopic Technique
    instantaneous.phase.data <- CalculateInstantaneousPhases(heart.times, step.times)
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
      # phase.coherence.indexes <- c(phase.coherence.indexes, CalculatePhaseCoherenceIndex(t, psi, time.point, time.window.s))
      normalized.shannon.entropy.indexes <- c(normalized.shannon.entropy.indexes, CalculateNormalizedShannonEntropyIndex(t, psi, time.point, time.window.s)) 
    }
    rm(time.point)
    plot(time.points, normalized.shannon.entropy.indexes, type = "l", xlab = "t (s)", ylab = "Indexes", xaxt = "n",  yaxt = "n", xlim = time.range.s, xaxs = "i", yaxs = "i", ylim = y.lim)
    # lines(time.points, phase.coherence.indexes, lty = 2)
    abline(v = seq(time.range.s[1], time.range.s[2], time.window.s), lty = "dashed", col = "grey")
    axis(1, at = seq(time.range.s[1], time.range.s[2], time.window.s), labels = seq(time.range.s[1], time.range.s[2], time.window.s), las = 1)
    axis(2, at = seq(y.lim[1], y.lim[2], .2), labels = seq(y.lim[1], y.lim[2], .2))
    # legend("topright", c("PCoI", "NSEI"), lty = c("solid", "dashed"),  bg = "white")
    box()
    
    # Distribution
    # hist(psi, breaks = seq(0, 1, .05), col = "black", border = "white", main = "", xlab = expression(Psi),  xaxs = "i", yaxs = "i", ylab = "")
    
    # # Create directory, if needed
    # output.directory.path <- paste(processed.data.directory.path, activity.directory, user.directory, date.directory, sep="")
    # if(!file.exists(substr(output.directory.path, 1, nchar(output.directory.path) - 1))) {
    #   dir.create(output.directory.path, recursive = TRUE)
    # }
    # 
    # # Write csv file
    # output.file.path <- paste(output.directory.path, body.position, "-cls-indexes-", measurement, ".csv", sep = "")
    # op <- options(digits.secs=3)
    # con <- file(output.file.path, 'w') 
    # writeLines(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y-%m-%d"), con = con)
    # writeLines(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%H:%M:%OS"), con = con)
    # write.csv(data.frame(t.s = time.points, pcoi = phase.coherence.indexes, nsei = normalized.shannon.entropy.indexes), file = con, row.names = FALSE)
    # close(con)
    # options(op) #reset options
    # print(paste("Wrote:", output.file.path))
    # 
    # # Write csv file
    # output.file.path <- paste(output.directory.path,  body.position, "-cls-phases-", measurement, ".csv", sep = "")
    # op <- options(digits.secs=3)
    # con <- file(output.file.path, 'w') 
    # writeLines(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y-%m-%d"), con = con)
    # writeLines(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%H:%M:%OS"), con = con)
    # write.csv(data.frame(t.s = t, fi, psi), file = con, row.names = FALSE)
    # close(con)
    # options(op) #reset options
    # print(paste("Wrote:", output.file.path))
    
  } else {
    print("No data")
  }
  readline("Press return to continue > ")
}