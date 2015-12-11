# Remove all variables
rm(list = ls(all = T)) 

# Load libraries
require(flow)

# Set root data directory path
root.data.directory.path          <- ""
if(file.exists("/Volumes/flow/Documents/archiv/daten/2015/flow-gehen-und-laufen"))
  root.data.directory.path        <- "/Volumes/flow/Documents/archiv/daten/2015/flow-gehen-und-laufen/"
if(file.exists("//gangstore.ddns.net/flow/Documents/archiv/daten/2015/flow-gehen-und-laufen"))
  root.data.directory.path        <- "//gangstore.ddns.net/flow/Documents/archiv/daten/2015/flow-gehen-und-laufen/"
if(file.exists("C:/Users/Simon Bogutzky/Documents/Archiv/flow/data"))
  root.data.directory.path        <- "C:/Users/Simon Bogutzky/Documents/Archiv/flow/data/"

# Set processed data directory path
processed.data.directory.path <- paste(root.data.directory.path, "processed-data/", sep = "")

# Set features directory path
features.directory.path <- paste(root.data.directory.path, "features/", sep = "")

# Read activity directory
activity.directory  <- readline("Type in activity directory and press return to continue (e. g. walking/) > ")

# Read user directory
user.directory      <- readline("Type in user directory and press return to continue (e. g. doe-john/) > ")

# Read in body position
body.position       <- readline("Type in body position and press return to continue (e. g. leg) > ")

# Set time range
time.range  <- c(as.numeric(readline("Type in start for visualisation in seconds and press return to continue (e. g. 0) > ")), as.numeric(readline("Type in end for visualisation in seconds and press return to continue (e. g. 900) > ")))

# Set time window
t.w         <- as.numeric(readline("Type in time window in seconds and press return to continue (e. g. 30) > "))

# Predicted relation
m           <- .5 #as.numeric(readline("Type in predicted relation and press return to continue (e. g. 0.5) > "))

# Load fss features
fss.features        <- read.csv(paste(features.directory.path, activity.directory, user.directory, "fss-features.csv", sep = ""), stringsAsFactors = F)

for (i in 1:nrow(fss.features)) {
  properties      <- fss.features[i, c(6:12)]
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
    t.l       <- jerk.cost.data$t.s
    t.c       <- c(kubios.hrv.data.time.data$t.s[1] - kubios.hrv.data.time.data$rr.interval.s[1], kubios.hrv.data.time.data$t.s)
    spm       <- 120 / jerk.cost.data$cycle.interval.s
    bpm       <- 60 / kubios.hrv.data.time.data$rr.interval.s
    
    # Plot
    par(mfcol = c(4, 1), mar = c(3.5, 4, 2, 4) + 0.1, mgp = c(2.5, 1, 0))
    
    # BPM vs. SPM
    plot(t.l, spm, xlab = "", ylab = "Cadence & HR", pch = 21, xlim = time.range, xaxt = "n")
    points(t.c[-1], bpm, pch = 22)
    abline(v = seq(time.range[1], time.range[2], t.w), lty = "dashed", col = "grey")
    axis(1, at = seq(time.range[1], time.range[2], t.w), labels = seq(time.range[1], time.range[2], t.w), las = 1)
    legend("topright", c("SPM", "BPM"), pch = c(21, 22), bg = "white")
    box()
    
    title(format(as.POSIXct(activity.start/1000, origin = "1970-01-01", tz = "CET"), "%Y-%m-%d %H:%M", tz = "CET"))
    
    # Stroboscopic Technique
    fi  <- CalculateInstantaneousPhases(t.c, t.l)
    t.c <- t.c[complete.cases(fi)]
    fi <- fi[complete.cases(fi)]
    psi <- fi %% (2 * pi * m)
    y.lim <- c(0, 1)
    plot(t.c, psi / (2 * pi * m), xlab = "", ylab = expression(Psi[m](tk) / (2 * pi * m)), xaxt = "n",  yaxt = "n", xlim = time.range, ylim = y.lim, pch = 21)
    abline(v = seq(time.range[1], time.range[2], t.w), lty = "dashed", col = "grey")
    axis(1, at = seq(time.range[1], time.range[2], t.w), labels = seq(time.range[1], time.range[2], t.w), las = 1)
    axis(2, at = seq(y.lim[1], y.lim[2], .2), labels = seq(y.lim[1], y.lim[2], .2))
    box()
    
    # Indexes
    t.s <- seq(min(t.c) + t.w/2, max(t.c) - t.w/2, 1)
    phase.coherence.indexes <- c()
    normalized.shannon.entropy.indexes <- c()
    for (t in t.s) {
      phase.coherence.indexes <- c(phase.coherence.indexes, CalculatePhaseCoherenceIndex(t.c, psi, t, t.w))
      normalized.shannon.entropy.indexes <- c(normalized.shannon.entropy.indexes, CalculateNormalizedShannonEntropyIndex(t.c, psi, t, t.w)) 
    }
    plot(t.s, phase.coherence.indexes, type = "l",  xlab = "t[ s ]", ylab = "Indexes", xaxt = "n",  yaxt = "n", xlim = time.range, ylim = y.lim)
    lines(t.s, normalized.shannon.entropy.indexes, lty = 2)
    abline(v = seq(time.range[1], time.range[2], t.w), lty = "dashed", col = "grey")
    axis(1, at = seq(time.range[1], time.range[2], t.w), labels = seq(time.range[1], time.range[2], t.w), las = 1)
    axis(2, at = seq(y.lim[1], y.lim[2], .2), labels = seq(y.lim[1], y.lim[2], .2))
    legend("topright", c("PCoI", "NSEI"), lty = c("solid", "dashed"),  bg = "white")
    box()
    
    # Distribution
    hist(psi / (2 * pi * m), breaks = seq(0, 1, .05), col = "black", border = "white", main = "", xlab = expression(Psi[m] / (2 * pi * m)), ylab = "")
    
    # Create directory, if needed
    output.directory.path <- paste(processed.data.directory.path, activity.directory, user.directory, date.directory, sep="")
    if(!file.exists(substr(output.directory.path, 1, nchar(output.directory.path) - 1))) {
      dir.create(output.directory.path, recursive = TRUE)
    }
    
    # Write csv file
    output.file.path <- paste(output.directory.path, body.position, "-cls-indexes-", measurement, ".csv", sep = "")
    op <- options(digits.secs=3)
    con <- file(output.file.path, 'w') 
    writeLines(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y-%m-%d"), con = con)
    writeLines(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%H:%M:%OS"), con = con)
    write.csv(data.frame(t.s, pcoi = phase.coherence.indexes, nsei = normalized.shannon.entropy.indexes), file = con, row.names = FALSE)
    close(con)
    options(op) #reset options
    print(paste("Wrote:", output.file.path))
    
    # Write csv file
    output.file.path <- paste(output.directory.path,  body.position, "-cls-relative-phase-", measurement, ".csv", sep = "")
    op <- options(digits.secs=3)
    con <- file(output.file.path, 'w') 
    writeLines(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y-%m-%d"), con = con)
    writeLines(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%H:%M:%OS"), con = con)
    write.csv(data.frame(t.s = t.c, psi.normalized.rad = psi / (2 * pi * m)), file = con, row.names = FALSE)
    close(con)
    options(op) #reset options
    print(paste("Wrote:", output.file.path))
    
  } else {
    print("No data")
  }
  readline("Press return to continue > ")
}