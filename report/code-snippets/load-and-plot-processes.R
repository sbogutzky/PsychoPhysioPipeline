# Set directory paths
date.directory <- dir(path = paste(root.data.directory.path, "processed-data/", activity.directory, user.directory, sep = ""), pattern = paste(dates[i], "--[0-9]{2}-[0-9]{2}-[0-9]{2}", sep = ""))
directory.path <- paste(root.data.directory.path, "processed-data/", activity.directory, user.directory, date.directory, "/", sep = "")

# Load heart data
file.name <- paste("ecg-data-", m, "_hrv.txt", sep="")
data.3 <- read.csv(paste(directory.path, file.name, sep = ""), header = F, na.strings = "", fill = T, skip = 117, stringsAsFactors = F, col.names = c("", "t.s", "rr.interval.s", "", "", "", "", "", "", "", ""))[,2:3]

# Create hrv data
require(RHRV)
data.4 <- CreateHRVData()
data.4 <- SetVerbose(data.4, F)
data.4$Beat <- data.frame("Time" = data.3$t.s)

# Build not interpolated heart rates
data.4 <- BuildNIHR(data.4)

# Linear Interpolate the data by 4 Hz (default)
data.4 <- InterpolateNIHR(data.4)

# Create Frequency analysis (CWT) with least asymmetric Daubechies of width 8 for ULF, VLF, LF and HF 
data.4 <- CreateFreqAnalysis(data.4)
data.4 <- CalculatePowerBand(data.4, indexFreqAnalysis=1, type="wavelet", wavelet="la8", bandtolerance=0.005, ULFmin=0, ULFmax=0.0033, VLFmin=0.0033, VLFmax=0.04, LFmin=0.04, LFmax=0.15, HFmin=0.15, HFmax=0.4)

# Load CLS data
file.name <- paste("leg-cls-relative-phase-", m, ".csv", sep = "")
data.5 <- read.csv(paste(directory.path, file.name, sep = ""), skip = 2)

# Load JC data
file.name <- paste("leg-jerk-cost-data-", m, ".csv", sep = "")
data.6 <- read.csv(paste(directory.path, file.name, sep = ""), skip = 2)

par(mfcol = c(5, 1), mar = c(3.5, 4, 2, 4) + 0.1, mgp = c(2.5, 1, 0))

# Plot HR
t.s <- seq(min(data.3$t.s),max(data.3$t.s), length.out = length(data.4$HR))
plot(t.s, data.4$HR, xlab = "", ylab = "HR (BPM)", xlim = time.range, ylim = c(100, 120), pch = 21, xaxt = "n")
axis(1, at = ticks, labels = F)
abline(v = ticks, lty = "dashed", col = "darkgrey")
box()

# Set title
title(paste(dates[i], "30'"))

# Plot Cylce Intervals
plot(data.6$t.s, data.6$cycle.interval.s, xlab = "", ylab = "Cycle Interval (s)", xlim = time.range, ylim = c(1, 1.15), pch = 21, xaxt = "n")
axis(1, at = ticks, labels = F)
abline(v = ticks, lty = "dashed", col = "darkgrey")
box()

# Plot CLS
plot(data.5[, 1], data.5[, 2], xlab = "", ylab = expression(Psi[m](tk) / (2 * pi * m)), xlim = time.range, ylim = c(0, 1), pch = 21, xaxt = "n")
axis(1, at = ticks, labels = F)
abline(v = ticks, lty = "dashed", col = "darkgrey")
box()

# Plot HF
plot(t.s, data.4$FreqAnalysis[[1]]$HF, xlab = "", ylab = expression("HF ("~ms^2~")"), xlim = time.range, ylim = c(0, 50), pch = 21, xaxt = "n")
axis(1, at = ticks, labels = F)
abline(v = ticks, lty = "dashed", col = "darkgrey")
box()

# Plot JC
require(caTools)
plot(data.6$t.s, runmean(data.6$jerk.cost.m2s5/10^4, 10, endrule = "NA"), xlab = "", ylab = expression("JC (x"~10^4~m^2*s^{-5}~")"), xlim = time.range, ylim = c(1, 1.75), pch = 21, xaxt = "n")
axis(1, at = ticks)
abline(v = ticks, lty = "dashed", col = "darkgrey")
box()

rm(data.3, data.4, data.5, data.6, file.name, t.s)