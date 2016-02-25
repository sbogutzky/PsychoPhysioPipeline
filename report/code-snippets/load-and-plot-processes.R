# Set directory paths
directory.path <- dir(path = paste(root.path, "processed-data/", activities.1[i], "/", subjects[i], "/", sep = ""), pattern = paste(dates[i], "--[0-9]{2}-[0-9]{2}-[0-9]{2}", sep = ""), full.names = T)

# Load heart data
file.name <- paste("ecg-data-", as.numeric(measurements[i]), "_hrv.txt", sep="")
process.data.1 <- read.csv(paste(directory.path, "/", file.name, sep = ""), header = F, na.strings = "", fill = T, skip = 117, stringsAsFactors = F, col.names = c("", "t.s", "rr.interval.s", "", "", "", "", "", "", "", ""))[,2:3]

# Create hrv data
require(RHRV)
process.data.2 <- CreateHRVData()
process.data.2 <- SetVerbose(process.data.2, F)
process.data.2$Beat <- data.frame("Time" = process.data.1$t.s)

# Build not interpolated heart rates
process.data.2 <- BuildNIHR(process.data.2)

# Linear Interpolate the data by 4 Hz (default)
process.data.2 <- InterpolateNIHR(process.data.2)

# Create Frequency analysis (CWT) with least asymmetric Daubechies of width 8 for ULF, VLF, LF and HF 
process.data.2 <- CreateFreqAnalysis(process.data.2)
process.data.2 <- CalculatePowerBand(process.data.2, indexFreqAnalysis=1, type="wavelet", wavelet="la8", bandtolerance=0.005, ULFmin=0, ULFmax=0.0033, VLFmin=0.0033, VLFmax=0.04, LFmin=0.04, LFmax=0.15, HFmin=0.15, HFmax=0.4)

# Load CLS data
file.name <- paste("leg-cls-phases-", as.numeric(measurements[i]), ".csv", sep = "")
process.data.3 <- read.csv(paste(directory.path, "/", file.name, sep = ""), skip = 2)
file.name <- paste("leg-cls-indexes-", as.numeric(measurements[i]), ".csv", sep = "")
process.data.4 <- read.csv(paste(directory.path, "/", file.name, sep = ""), skip = 2)

# Load JC data
file.name <- paste("leg-jerk-cost-data-", as.numeric(measurements[i]), ".csv", sep = "")
process.data.5 <- read.csv(paste(directory.path, "/", file.name, sep = ""), skip = 2)

# Compute spm and bpm
spm       <- 60 / (process.data.5$cycle.interval.s / 2)
bpm       <- 60 / process.data.1$rr.interval.s

par("mfcol" = c(4, 1), mar = c(2.5, 2.5, .5, 1.5) + 0.1, mgp = c(1.5, .5, 0), las = 1, cex.axis = 0.8, tck = .03, cex.lab = .8, xaxs = "i", yaxs = "i")

# Plot cadance and hr
if(!exists("beat.plot.range")) {
  beat.plot.range <- c(90, 130)
}
plot(process.data.5$t.s, spm, xlab = "", ylab = "Cadence & Heart rate", xaxt = "n", xlim = time.range, ylim = beat.plot.range, pch = 21, bg = "#3FADCB")
points(process.data.1$t.s, bpm, pch = 22, bg = "#33D100")
axis(1, at = ticks, labels = F)
abline(v = ticks, lty = "dashed", col = "darkgrey")
legend("topleft", c("SPM", "BPM"), pch = c(21, 22), pt.bg = c("#3FADCB", "#33D100"), bty = "n", cex = .8)
box()

# Plot CLS
plot(process.data.3$t.s, process.data.3$psi, xlab = "", ylab = expression(Psi(t)), xaxt = "n", xlim = time.range, ylim = c(0, 1), pch = 21, bg = "#3FADCB")
axis(1, at = ticks, labels = ticks, las = 1)
abline(v = ticks, lty = "dashed", col = "darkgrey")
box()

# Plot a period in detail
if(!exists("detail.plot.range")) {
  detail.plot.range <- c(780, 810)
}
plot(process.data.3$t.s, process.data.3$psi, xlab = "", ylab = expression(Psi(t)), xaxt = "n", xlim = detail.plot.range, ylim = c(0, 1), pch = 21, bg = "black")
axis(1, at = ticks, labels = ticks, las = 1)
abline(v = process.data.3$t.s)
box()

plot(process.data.4$t.s, process.data.4$pcoi, type = "l", xlab = "Time (s)", ylab = "Indexes", xaxt = "n", xlim = time.range, ylim = c(0, 1), lty = 2)
lines(process.data.4$t.s, process.data.4$nsei, col = "#3FADCB")
axis(1, at = ticks, labels = ticks, las = 1)
abline(v = ticks, lty = "dashed", col = "darkgrey")
legend("topleft", c("Phase Coherence Index", "Normalized Shannon Entropy Index"), lty = c("solid", "dashed"), col = c("#3FADCB", "#000000"), bty = "n", cex = .8)
box()

par("mfcol" = c(2, 1), mar = c(2.5, 2.5, .5, 1.5) + 0.1, mgp = c(1.5, .5, 0), las = 1, cex.axis = 0.8, tck = .03, cex.lab = .8, xaxs = "i", yaxs = "i")

# Plot HF
t.s <- seq(min(process.data.1$t.s), max(process.data.1$t.s), length.out = length(process.data.2$HR))
plot(t.s, process.data.2$FreqAnalysis[[1]]$HF, xlab = "", ylab = expression("HF ("~ms^2~")"), type = "l", xlim = time.range, ylim = c(0, 100), pch = 21, xaxt = "n", col = "#3FADCB")
axis(1, at = ticks, labels = F)
abline(v = ticks, lty = "dashed", col = "darkgrey")
box()

# Plot JC
require(caTools)
plot(process.data.5$t.s, runmean(process.data.5$jerk.cost.m2s5/10^4, 10, endrule = "NA"), type = "l", xlab = "", ylab = expression("JC (x"~10^4~m^2*s^{-5}~")"), xlim = time.range, pch = 21, xaxt = "n", col = "#3FADCB")
axis(1, at = ticks, labels = ticks, las = 1)
abline(v = ticks, lty = "dashed", col = "darkgrey")
box()

rm(process.data.1, process.data.2, process.data.3, process.data.4, process.data.5, directory.path, file.name, bpm, spm, t.s)