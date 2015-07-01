# Remove all variables
rm(list = ls(all = T)) 

# Load libraries
require(flow)


# Set root data directory path
root.data.directory.path <- ""
if(file.exists("C:/Users/Simon Bogutzky/Documents/flow/data"))
  root.data.directory.path <- "C:/Users/Simon Bogutzky/Documents/flow/data/"
if(file.exists("/Volumes/flow/Documents/simon-bogutzky/data"))
  root.data.directory.path <- "/Volumes/flow/Documents/simon-bogutzky/data/"
if(file.exists("//gangstore.ddns.net/flow/Documents/simon-bogutzky/data"))
  root.data.directory.path <- "//gangstore.ddns.net/flow/Documents/simon-bogutzky/data/"

# Set processed data directory path
processed.data.directory.path <- paste(root.data.directory.path, "processed-data/", sep = "")

# Set features directory path
features.directory.path <- paste(root.data.directory.path, "features/", sep = "")

# Load fss features
fss.features <- read.csv(paste(features.directory.path, "fss-features.csv", sep = ""), stringsAsFactors = F)

# Set body position
body.position       <- "leg"

for (i in 1:nrow(fss.features)) {
  
  properties      <- fss.features[i, c(6:12)]
  activity        <- properties[, 1]
  activity.start  <- properties[, 2]
  measurement     <- properties[, 5]
  last.name       <- properties[, 6]
  first.name      <- properties[, 7]
  if(measurement == 1) {
    date.directory  <- paste(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y-%m-%d--%H-%M-%S"), "/", sep ="")
  }
  
  # Read time data
  motion.time.data.path <- paste(processed.data.directory.path, tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/", date.directory, body.position, "-motion-time-data-", measurement, ".csv", sep="")
  hrv.time.data.path <- paste(processed.data.directory.path, tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/", date.directory, "hrv-time-data-", measurement, ".csv", sep="")
  if(file.exists(motion.time.data.path) & file.exists(hrv.time.data.path)) {
    
    # Load time data
    motion.time.data <- read.csv(motion.time.data.path, skip = 2)
    hrv.time.data <- read.csv(hrv.time.data.path, skip = 2)
    
    
    #TODO: Kubios 
    
    if(max(hrv.time.data) > 800) {
    
    # Plot
    par("mfcol" = c(4, 1), mar = c(2.5, 2.5, .5, 3.5) + 0.1, mgp = c(1.5, .5, 0), las = 1, cex.axis = 0.8, tck = .03, cex.lab = .8, xaxs = "i", yaxs = "i")
    
    # BPM vs. SPM
    t.l       <- motion.time.data$t.s
    t.c       <- hrv.time.data$t.s
    spm       <- 120 / motion.time.data$cycle.interval.s
    bpm       <- 60 / hrv.time.data$rr.interval.s
    x.lim     <- c(min(t.c), max(t.c))
    plot(t.l, spm, xlab = "", ylab = "Cadence & HR", pch = 21, bg = "#3FADCB", cex = .5, xlim = x.lim, xaxt = "n")
    points(t.c, bpm, pch = 22, bg = "#33D100", cex = .5)
    abline(v = seq(x.lim[1], x.lim[2], 20), lty = "dashed", col = "lightgrey")
    axis(1, at = seq(x.lim[1], x.lim[2], 20), labels = seq(x.lim[1], x.lim[2], 20), las = 1)
    legend("bottomright", c("SPM", "BPM"), pch = c(21, 22), pt.bg = c("#3FADCB", "#33D100"), cex = .8, bg = "white")
    box()
    
    # Stroboscopic Technique
    fi  <- CalculateInstantaneousPhases(t.c, t.l)
    m   <- .5
    psi <- fi %% (2 * pi * m)
    y.lim <- c(0, 1)
    plot(t.c, psi / (2 * pi * m), xlab = "", ylab = expression(Psi[m](tk) / (2 * pi * m)), xaxt = "n",  yaxt = "n", xlim = x.lim, ylim = y.lim, pch = 21, bg = "#CB3FAD") # bg = rep(c("#3FADCB", "#CB3FAD"), length(psi)))
    abline(v = seq(x.lim[1], x.lim[2], 20), lty = "dashed", col = "lightgrey")
    axis(1, at = seq(x.lim[1], x.lim[2], 20), labels = seq(x.lim[1], x.lim[2], 20), las = 1)
    axis(2, at = seq(y.lim[1], y.lim[2], .2), labels = seq(y.lim[1], y.lim[2], .2))
    box()
    
    # Indexes
    t.w <- 20
    t.s <- seq(t.w/2, x.lim[2] - t.w/2, 1)
    phase.coherence.indexes <- c()
    phase.normalized.shannon.entropy.indexes <- c()
    for (t in t.s) {
      phase.coherence.indexes <- c(phase.coherence.indexes, CalculatePhaseCoherenceIndex(t.c, psi, t, t.w))
      phase.normalized.shannon.entropy.indexes <- c(phase.normalized.shannon.entropy.indexes, CalculateNormalizedShannonEntropyIndex(t.c, psi, t, t.w)) 
    }
    plot(t.s, phase.coherence.indexes, type = "l",  xlab = "t[ s ]", ylab = "Indexes", xaxt = "n",  yaxt = "n", xlim = x.lim, ylim = y.lim, col = "#3FADCB")
    lines(t.s, phase.normalized.shannon.entropy.indexes, lty = 2)
    abline(v = seq(x.lim[1], x.lim[2], 20), lty = "dashed", col = "lightgrey")
    axis(1, at = seq(x.lim[1], x.lim[2], 20), labels = seq(x.lim[1], x.lim[2], 20), las = 1)
    axis(2, at = seq(y.lim[1], y.lim[2], .2), labels = seq(y.lim[1], y.lim[2], .2))
    legend("bottomright", c("Phase Coherence Index", "Shannon Entropy Index"), lty = c("solid", "dashed"), col = c("#3FADCB", "#000000"), cex = .8, bg = "white")
    box()
    
    hist(psi / (2 * pi * m), breaks = seq(0, 1, .05), col = "black", border = "white", main = "", xlab = expression(Psi[m] / (2 * pi * m)), ylab = "")
    
    # Create directory, if needed
    output.directory.path <- paste(processed.data.directory.path, tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/", date.directory, sep="")
    if(!file.exists(output.directory.path)) {
      dir.create(output.directory.path, recursive = TRUE)
    }
    
    # Write csv file
    output.file.path <- paste(output.directory.path, body.position, "-cps-indexes-", measurement, ".csv", sep = "")
    op <- options(digits.secs=3)
    con <- file(output.file.path, 'w') 
    writeLines(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y-%m-%d"), con = con)
    writeLines(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%H:%M:%OS"), con = con)
    write.csv(data.frame(t.s, phase.coherence.index = phase.coherence.indexes, phase.normalized.shannon.entropy.index = phase.normalized.shannon.entropy.indexes), file = con, row.names = FALSE)
    close(con)
    options(op) #reset options
    print(paste("Wrote:", output.file.path))
    
    # Write csv file
    output.file.path <- paste(output.directory.path,  body.position, "-cps-relative-phase-", measurement, ".csv", sep = "")
    op <- options(digits.secs=3)
    con <- file(output.file.path, 'w') 
    writeLines(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y-%m-%d"), con = con)
    writeLines(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%H:%M:%OS"), con = con)
    write.csv(data.frame(t.s = t.c, psi.normalized.rad = psi / (2 * pi * m)), file = con, row.names = FALSE)
    close(con)
    options(op) #reset options
    print(paste("Wrote:", output.file.path))
    
    }
    
  } else {
    print("No data")
  }
}