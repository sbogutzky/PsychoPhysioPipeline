st1 <- stride.times[stride.times > 1030 & stride.times < 1090]
bt1 <- heart.beat.times[heart.beat.times > 1030 & heart.beat.times < 1090]

st2 <- seq(400, 460, length.out = length(st1))
bt2 <- seq(400, 460, length.out = length(bt1))

print(60 / mean(diff(st1)))
print(60 / mean(diff(bt1)))

print(60 / mean(diff(st2)))
print(60 / mean(diff(bt2)))

par(mfcol = c(2, 1), mar = c(3.5, 4, 2, 4) + 0.1, mgp = c(2.5, 1, 0))

instantaneous.phase.data <- ComputeInstantaneousPhases(bt1, st1)
fi <- instantaneous.phase.data[, 2] # instantaneous phases
psi <- (fi %% (2 * pi)) / (2 * pi) # relative phases
cls.phase.data <- data.frame(timestamp.ms = round(instantaneous.phase.data[, 1] * 1000, 3), fi = round(fi, 3), psi = round(psi, 3))
rm(instantaneous.phase.data, fi, psi)

plot(cls.phase.data[, 1] / 1000, cls.phase.data[, 3], xlab = "", ylab = expression("Rel. Phase " ~ Psi(t)), ylim = y.lim, pch = 24, bg = rgb(96/255, 65/255, 79/255))

instantaneous.phase.data <- ComputeInstantaneousPhases(bt2, st2)
fi <- instantaneous.phase.data[, 2] # instantaneous phases
psi <- (fi %% (2 * pi)) / (2 * pi) # relative phases
cls.phase.data <- data.frame(timestamp.ms = round(instantaneous.phase.data[, 1] * 1000, 3), fi = round(fi, 3), psi = round(psi, 3))
rm(instantaneous.phase.data, fi, psi)

plot(cls.phase.data[, 1] / 1000, cls.phase.data[, 3], xlab = "", ylab = expression("Rel. Phase " ~ Psi(t)), ylim = y.lim, pch = 24, bg = rgb(96/255, 65/255, 79/255))