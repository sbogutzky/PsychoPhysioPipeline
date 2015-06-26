t.1 <- read.csv("/Volumes/flow/Documents/simon-bogutzky/data/preprocessed-data/running/buse-patrick/2013-11-07--17-34-06/ecg-data-1_hrv.txt", header = F, na.strings = "", fill = T, skip = 117, stringsAsFactors = F, col.names = c("", "Time", "RR.interval", "FFT.Frequency", "FFT.PSD", "AR.Frequency", "AR.PSD", "VLF.comp.", "LF.comp.", "HF.comp.", ""))[,2]
t.2 <- read.csv("~/Entwicklung/bogutzky/repositories/non-disruptive-flow-measures/data/preprocessed-data/running/buse-patrick/2013-11-07--17-34-06/leg-motion-time-data-1.csv")[,1]

plot(t.1[-1], 60/diff(t.1))
points(t.2[-1], 120/diff(t.2), col = 2)

require(flow)
fi  <- CalculateInstantaneousPhases(t.1, t.2)
m   <- 1
psi <- (fi %% (2*pi*m)) / (2*pi*m)
plot(t.1, psi, col = rep(c(1,2), length(fi)), xaxt = "n")

t <- leg.motion.data.3$t
y <- leg.motion.data.3$motion.rotation.rate.x
t.m <- seq(0, 900000, 10)

require(signal)
y.i <- interp1(t, y, t.m, method = "linear")
t.m <- t.m[!is.na(yi)]
y.i <- yi[!is.na(yi)]
t.c <- round(stride.times, 2)
indexes <- match(t.c, t.m / 1000)

require(hht)
hty <- HilbertTransform(y.i)

fi <- atan(Im(hty[indexes])/Re(hty[indexes]))
m   <- 1
psi <- (fi %% (2*pi*m)) / (2*pi*m)
plot(t.c, psi, col = rep(c(1,2), length(fi)), xaxt = "n")

n   <- 0
m   <- 300
t   <- seq(n, m, .25)
kubios.hrv.data.subset <- kubios.hrv.data[kubios.hrv.data$t >= n & kubios.hrv.data$t <= m,]
leg.motion.time.data.subset <- leg.motion.time.data[leg.motion.time.data$t >= n & leg.motion.time.data$t <= m,]
t.1 <- kubios.hrv.data.subset$t
t.2 <- leg.motion.time.data.subset$t
y.1 <- kubios.hrv.data.subset$rr.interval
y.2 <- leg.motion.time.data.subset$cycle.interval

par(mfcol=c(2, 1))

plot(t.1, y.1, type = "l")
plot(t.2, y.2, type = "l")

# t   <- seq(1, 9, .25)
# t.1 <- seq(1, 9, .5)
# t.2 <- c(seq(1, 3, 1), seq(3.5, 6, .5), seq(7, 9, 1))

par(mfcol=c(3, 1))

# Generalized Phase Difference
fi.1 <- CalculateInstantaneousPhases(t, t.1) 
fi.2 <- CalculateInstantaneousPhases(t, t.2)
n    <- 1
m    <- 2
gpd <- abs(n * fi.1 - m * fi.2) 
plot(t, round(gpd / 2*pi, 3), type= "l")

# Stroboscopic Technique
window.size <- 20 * 4
fi  <- CalculateInstantaneousPhases(t.1, t.2)
m   <- .5
psi <- (fi %% (2*pi*m)) / (2*pi*m)
plot(t.1, psi, col = rep(c(1,2), length(fi)), xaxt = "n")

axis(1, seq(0, 300, window.size/4), seq(0, 300, window.size/4))
abline(v = seq(0, 300, window.size/4), col = "lightgray", lty = "dotted")

# Conditional Probability Index
n    <- 2
m    <- 1
cpi <- rep(NA, window.size)
for(i in 1:(length(t) - window.size)) {
    cpi <- c(cpi, CalculateConditionalProbabilityIndex(t[i:(i+window.size)], t.1, t.2, n, m, 16, F)) 
}
plot(t, cpi, type = "l", ylim = c(0,1), xaxt = "n")

axis(1, seq(0, 300, window.size/4), seq(0, 300, window.size/4))
abline(v = seq(0, 300, window.size/4), col = "lightgray", lty = "dotted")


mean(cpi, na.rm = T)

CalculateConditionalProbabilityIndex(t[i:(i+window.size)], t.1, t.2, n, m, 16, T)
