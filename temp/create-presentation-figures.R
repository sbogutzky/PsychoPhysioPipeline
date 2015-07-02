date.time <- readLines("/Volumes/flow/Documents/simon-bogutzky/data/processed-data/running/buse-patrick/2013-11-07--17-34-06/neu/hrv-spectral-data-3.csv", n = 2)
hrv.spectral.data <- read.csv("/Volumes/flow/Documents/simon-bogutzky/data/processed-data/running/buse-patrick/2013-11-07--17-34-06/neu/hrv-spectral-data-3.csv", skip = 2)

# Plot
par("mfcol" = c(3, 1), mar = c(4, 5, .5, 1) + 0.1, mgp = c(3, .5, 0), las = 1, cex.axis = 1.5, tck = .03, cex.lab = 1.5, xaxs = "i", yaxs = "i")

summary(hrv.spectral.data)

x.lim     <- c(0, 900)

plot(hrv.spectral.data$t.s, hrv.spectral.data$lf.hz, xlab = "", ylab = "LF [ Hz ]", type = "l", xlim = x.lim, xaxt = "n", ylim = c(0,20), col = "#3FADCB")
abline(v = seq(x.lim[1], x.lim[2], 20), lty = "dashed", col = "darkgrey")
box()

plot(hrv.spectral.data$t.s, hrv.spectral.data$hf.hz, xlab = "", ylab = "HF [ Hz ]", type = "l", xlim = x.lim, xaxt = "n", ylim = c(0,20), col = "#3FADCB")
abline(v = seq(x.lim[1], x.lim[2], 20), lty = "dashed", col = "darkgrey")
box()

plot(hrv.spectral.data$t.s, hrv.spectral.data$lfhf, xlab = "t [ s ]", ylab = "LF/HF", type = "l", xlim = x.lim, xaxt = "n",  ylim = c(0,80), col = "#3FADCB")
abline(v = seq(x.lim[1], x.lim[2], 20), lty = "dashed", col = "darkgrey")
box()

axis(1, at = seq(x.lim[1], x.lim[2], 20), labels = seq(x.lim[1], x.lim[2], 20), las = 1)

#-----

date.time.2 <- readLines("/Volumes/flow/Documents/simon-bogutzky/data/processed-data/running/buse-patrick/2013-11-07--17-34-06/leg-motion-jerk-cost-data-3.csv", n = 2)
leg.motion.jerk.cost.data <- read.csv("/Volumes/flow/Documents/simon-bogutzky/data/processed-data/running/buse-patrick/2013-11-07--17-34-06/leg-motion-jerk-cost-data-3.csv", skip = 2)
leg.motion.time.data <- read.csv("/Volumes/flow/Documents/simon-bogutzky/data/processed-data/running/buse-patrick/2013-11-07--17-34-06/leg-motion-time-data-3.csv", skip = 2)

# Plot
par("mfcol" = c(3, 1), mar = c(4, 5, .5, 1) + 0.1, mgp = c(3, .5, 0), las = 1, cex.axis = 1.5, tck = .03, cex.lab = 1.5, xaxs = "i", yaxs = "i")

summary(leg.motion.jerk.cost.data)
summary(leg.motion.time.data)

x.lim     <- c(0, 900)

plot(leg.motion.time.data$t.s, leg.motion.time.data$cycle.interval.s, xlab = "", ylab = "Zyklus-Interval [ s ]", type = "l", xlim = x.lim, xaxt = "n", col = "#3FADCB", ylim = c(.65, .75))
abline(v = seq(x.lim[1], x.lim[2], 20), lty = "dashed", col = "darkgrey")
box()

plot(leg.motion.jerk.cost.data$t.s, leg.motion.jerk.cost.data$jerk.cost.m.2.s..5 / 10^4, xlab = "t [ s ]", ylab = expression("Jerk Cost [" ~ m^2 * s^-5  ~ "] x" ~ 10^4), type = "l", xlim = x.lim, xaxt = "n", col = "#3FADCB", ylim = c(100, 400))
abline(v = seq(x.lim[1], x.lim[2], 20), lty = "dashed", col = "darkgrey")
box()

axis(1, at = seq(x.lim[1], x.lim[2], 20), labels = seq(x.lim[1], x.lim[2], 20), las = 1)

frame()

#----

date.time.3             <- readLines("/Volumes/flow/Documents/simon-bogutzky/data/processed-data/running/buse-patrick/2013-11-07--17-34-06/neu/leg-cps-relative-phase-3.csv", n = 2)
leg.motion.time.data    <- read.csv("/Volumes/flow/Documents/simon-bogutzky/data/processed-data/running/buse-patrick/2013-11-07--17-34-06/leg-motion-time-data-3.csv", skip = 2)
hrv.time.data           <- read.csv("/Volumes/flow/Documents/simon-bogutzky/data/processed-data/running/buse-patrick/2013-11-07--17-34-06/neu/hrv-time-data-3.csv", skip = 2)
leg.cps.indexes         <- read.csv("/Volumes/flow/Documents/simon-bogutzky/data/processed-data/running/buse-patrick/2013-11-07--17-34-06/neu/leg-cps-indexes-3.csv", skip = 2)
leg.cps.relative.phase  <- read.csv("/Volumes/flow/Documents/simon-bogutzky/data/processed-data/running/buse-patrick/2013-11-07--17-34-06/neu/leg-cps-relative-phase-3.csv", skip = 2)


# Plot
par("mfcol" = c(3, 1), mar = c(4, 5, .5, 1) + 0.1, mgp = c(3, .5, 0), las = 1, cex.axis = 1.5, tck = .03, cex.lab = 1.5, xaxs = "i", yaxs = "i")

# BPM vs. SPM
spm       <- 120 / leg.motion.time.data$cycle.interval.s
bpm       <- 60 / hrv.time.data$rr.interval.s
x.lim     <- c(0, 900)
plot(leg.motion.time.data$t.s, spm, xlab = "", ylab = "Cadence & HR", pch = 21, bg = "#3FADCB", cex = 1, xlim = x.lim, xaxt = "n")
points(hrv.time.data$t.s, bpm, pch = 22, bg = "#33D100", cex = 1)
abline(v = seq(x.lim[1], x.lim[2], 20), lty = "dashed", col = "darkgrey")
axis(1, at = seq(x.lim[1], x.lim[2], 20), labels = seq(x.lim[1], x.lim[2], 20), las = 1)
legend("bottomright", c("SPM", "BPM"), pch = c(21, 22), pt.bg = c("#3FADCB", "#33D100"), cex = 1, bg = "white")
box()

# Stroboscopic Technique
y.lim <- c(0, 1)
plot(leg.cps.relative.phase$t.s, leg.cps.relative.phase$psi.normalized.rad, xlab = "", ylab = expression(Psi[m](tk) / (2 * pi * m)), xaxt = "n",  yaxt = "n", xlim = x.lim, ylim = y.lim, pch = 21, bg = "#CB3FAD") # bg = rep(c("#3FADCB", "#CB3FAD"), length(psi)))
abline(v = seq(x.lim[1], x.lim[2], 20), lty = "dashed", col = "darkgrey")
axis(1, at = seq(x.lim[1], x.lim[2], 20), labels = seq(x.lim[1], x.lim[2], 20), las = 1)
axis(2, at = seq(y.lim[1], y.lim[2], .2), labels = seq(y.lim[1], y.lim[2], .2))
box()

# Indexes
plot(leg.cps.indexes$t.s, leg.cps.indexes$phase.coherence.index, type = "l",  xlab = "t[ s ]", ylab = "Indexes", xaxt = "n",  yaxt = "n", xlim = x.lim, ylim = y.lim, col = "#3FADCB", lwd = 2)
lines(leg.cps.indexes$t.s, leg.cps.indexes$normalized.shannon.entropy.index, lty = 2, lwd = 2)
abline(v = seq(x.lim[1], x.lim[2], 20), lty = "dashed", col = "darkgrey")
axis(1, at = seq(x.lim[1], x.lim[2], 20), labels = seq(x.lim[1], x.lim[2], 20), las = 1)
axis(2, at = seq(y.lim[1], y.lim[2], .2), labels = seq(y.lim[1], y.lim[2], .2))
legend("bottomright", c("Phase Coherence Index", "Shannon Entropy Index"), lty = c("solid", "dashed"), col = c("#3FADCB", "#000000"), cex = 1, bg = "white")
box()


ar.peak.2 <- c(1.1055, 1.2617, 1.0781, 1.2422, 1.2227, 1.4531, 1.2148, 1.4531, 1.4492, 1.4297, 1.4297, 1.2305, 1.4453, 1.4648, 1.4883, 1.4609, 1.3672, 1.4570, 1.4688, 1.4805, 1.1055, 1.4492, 1.4453, 1.2930)

