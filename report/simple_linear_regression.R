
fit.1 = lm(absorption ~ mean.nsei, data = data.2)
par(mfrow=c(1, 1))
plot(x = data.2$mean.nsei, y = data.2$absorption, xlab = "NSEI", ylab = "absorption", main = "Simple Linear Regression")
abline(fit.1)
par(mfrow=c(2, 2))
plot(fit.1)

par(mfrow=c(1, 1))
hist(fit.1$residuals)
shapiro.test(fit.1$residuals)

summary(fit.1)

fit.2 = lm(absorption ~ log10(mean.nsei), data = data.2)
par(mfrow=c(1, 1))
plot(x = log10(data.2$mean.nsei), y = data.2$absorption, xlab = "NSEI", ylab = "absorption", main = "Simple Linear Regression")
abline(fit.2)
par(mfrow=c(2, 2))
plot(fit.2)

par(mfrow=c(1, 1))
hist(fit.2$residuals)
shapiro.test(fit.2$residuals)

summary(fit.2)


#text(log10(data_1$mean.nsei), data_1$absorption, data_1$name, cex=0.6, pos=4, col="black")

