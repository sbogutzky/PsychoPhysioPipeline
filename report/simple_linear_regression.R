library(readr)
data.summary <- read_csv("~/Documents/archiv/studium/promotion/2016/studien/data_summary_1.csv")
data.summary <- data.summary[data.summary$grp != 4, ]

# Recode demand_level
data.summary$demand_level[data.summary$demand_level == 6] <- 4
data.summary$demand_level[data.summary$demand_level == 7] <- 3
data.summary$demand_level[data.summary$demand_level == 8] <- 2
data.summary$demand_level[data.summary$demand_level == 9] <- 1

# Means and standard deviations
round(mean(data.summary$absorption), 2)
round(sd(data.summary$absorption), 2)

round(mean(data.summary$fluency), 2)
round(sd(data.summary$fluency), 2)

round(mean(data.summary$demand_level), 2)
round(sd(data.summary$demand_level), 2)

round(mean(data.summary$pc), 2)
round(sd(data.summary$pc), 2)

round(mean(data.summary$nse), 2)
round(sd(data.summary$nse), 2)

# Intercorrelations
library(Hmisc)
result <- rcorr(as.matrix(data.summary[, c(7:8, 10:12)]))
print(result)

# Observed values pc
par(mfrow=c(1, 1))
plot(x = data.summary$pc, y = data.summary$absorption, xlab = "Phase Coherence", ylab = "", main = "Absorption", xlim = c(0, .15), ylim = c(2, 7), pch = 18, xaxs = "i", yaxs = "i")
fit.1 = lm(data.summary$absorption ~ data.summary$pc)
abline(fit.1)

print(summary(fit.1))

hist(fit.1$residuals)
shapiro.test(fit.1$residuals)

# Observed values nse
par(mfrow=c(1, 1))
plot(x = data.summary$nse, y = data.summary$absorption, xlab = "Normalized Shannon Entropy", ylab = "", main = "Absorption", xlim = c(0, 0.6), ylim = c(2, 7), pch = 18, xaxs = "i", yaxs = "i")
fit.2 = lm(data.summary$absorption ~ data.summary$nse)
abline(fit.2)

print(summary(fit.2))

hist(fit.2$residuals)
shapiro.test(fit.2$residuals)
