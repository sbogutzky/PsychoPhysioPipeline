library(tikzDevice)

mean.hr <- c(165, 166, 171, 172, 174, 173, 171, 180, 182, 183)
absorption <- c(4.25, 4.25, 4.25, 4.5, 5.25, 5.25, 4.5, 4.5, 4, 4)
mean.hr.squared <- mean.hr^2

linear.model.1 <- lm(absorption ~ mean.hr)
summary.lm(linear.model.1)

linear.model.2 <- lm(absorption ~ mean.hr + mean.hr.squared)
summary.lm(linear.model.2)

tikz("00-plot.tex", width = 5.22, height = 5.22) #standAlone = TRUE

par(mai = c(.8, .8, .1, .1), mgp = c(2.5, 1, 0))

plot(mean.hr, absorption, xlab = "Mittelere HR (BPM)", ylab = "Absorbiertheit", pch = 21, bg = rgb(52/255, 128/255, 164/255))
abline(linear.model.1)

mean.hr <- seq(160, 190, by = .5)
mean.hr.squared <- mean.hr^2
absorption.predicted <- predict.lm(linear.model.2, data.frame(mean.hr, mean.hr.squared))
lines(mean.hr, absorption.predicted, lty = "dashed")

grid(col = rgb(79/255, 80/255, 84/255))
box()

dev.off()