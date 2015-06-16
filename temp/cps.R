leg.motion.time.data  <- read.csv("~/Entwicklung/bogutzky/repositories/non-disruptive-flow-measures/data/preprocessed-data/running/buse-patrick/2013-10-17--18-07-11/leg-motion-time-data-2.csv")
kubios.hrv.data       <- read.csv("~/Entwicklung/bogutzky/repositories/non-disruptive-flow-measures/data/preprocessed-data/running/buse-patrick/2013-10-17--18-07-11/ecg-data-2_hrv.txt", header = F, na.strings = "", fill = T, skip = 117, stringsAsFactors = FALSE, col.names = c("", "t", "rr.interval", "FFT.Frequency", "FFT.PSD", "AR.Frequency", "AR.PSD", "VLF.comp.", "LF.comp.", "HF.comp.", ""))[,2:3]

leg.motion.time.data.subset <- leg.motion.time.data[leg.motion.time.data$t >= 140 & leg.motion.time.data$t <= 200,]
kubios.hrv.data.subset      <- kubios.hrv.data[kubios.hrv.data$t >= 140 & kubios.hrv.data$t <= 200,]

par(mfcol=c(2, 1))

plot(kubios.hrv.data.subset$t, kubios.hrv.data.subset$rr.interval, type = "l")
plot(leg.motion.time.data.subset$t, leg.motion.time.data.subset$cycle.interval, type = "l")

# Phase difference
t    <- seq(140, 200, .3)
fi.1 <- CalculatePhaseAppearances(t, leg.motion.time.data.subset$t)
fi.2 <- CalculatePhaseAppearances(t, kubios.hrv.data.subset$t)
m <- 2
n <- 1
d <- m * fi.1 - n * fi.2 
plot(d / 2*pi, type= "l")

# Stroboscopic technique
fi  <- CalculatePhaseAppearances(kubios.hrv.data.subset$t, leg.motion.time.data.subset$t)
m   <- .5
psi <- (fi %% (2*pi*m)) / (2 * pi)
plot(psi)

m <- 1
n <- 1

# Conditional probability index
bin.breaks <- seq(0, 2 * pi, 2 * pi / 15)
#l <- 5
for (l in 1:(length(bin.breaks)-1)) {
  t.l <- t[bin.breaks[l] < fi.1 %% (2*pi*m) & fi.1 %% (2*pi*m) < bin.breaks[l+1]] 
  M.l <- length(t.l)
  #print(M.l)
  fi.2 <- CalculatePhaseAppearances(t.l, kubios.hrv.data.subset$t)
  theta <- fi.2 %% (2*pi*n)
  #hist(theta.m, breaks = bin.breaks)
  theta.m <- sqrt(sum(cos(theta))^2 + sum(sin(theta))^2)
  
  #print(theta.m)
  lambda.l <- theta.m * M.l^-1
  print(lambda.l)
}

