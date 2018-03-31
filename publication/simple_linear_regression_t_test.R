library(readr)
data.summary <- read_csv("~/Documents/archiv/studium/promotion/2016/studien/data_summary.csv")
data.summary <- data.summary[data.summary$grp != 4, ]

# Recode flow
data.summary$absorption[data.summary$absorption == 6] <- 4
data.summary$absorption[data.summary$absorption == 7] <- 3
data.summary$absorption[data.summary$absorption == 8] <- 2
data.summary$absorption[data.summary$absorption == 9] <- 1

# Means and standard deviations
round(mean(data.summary$absorption), 2)
round(sd(data.summary$absorption), 2)

round(mean(data.summary$fluency), 2)
round(sd(data.summary$fluency), 2)

round(mean(data.summary$absorption), 2)
round(sd(data.summary$absorption), 2)

round(mean(data.summary$pc), 2)
round(sd(data.summary$pc), 2)

round(mean(data.summary$nse), 2)
round(sd(data.summary$nse), 2)

# Intercorrelations
library(Hmisc)
result <- rcorr(as.matrix(data.summary[, c(7:8, 10:12)]))
print(result)

# Observed values pc
par(mfrow=c(1, 2))

plot(x = data.summary$pc, y = data.summary$absorption, xlab = "Phase Coherence", ylab = "", main = "Absorption", xlim = c(0, .12), ylim = c(2, 7), pch = 18, xaxs = "i", yaxs = "i")
fit.1 = lm(data.summary$absorption ~ data.summary$pc)
abline(fit.1)

mtext("A", side = 2, line = 3.5, padj = -27, las = 1)
#print(summary(fit.1))

#hist(fit.1$residuals)
#shapiro.test(fit.1$residuals)

# Observed values nse
#par(mfrow=c(1, 1))
plot(x = data.summary$nse, y = data.summary$absorption, xlab = "Normalized Shannon Entropy", ylab = "", main = "Absorption", xlim = c(0, 0.6), ylim = c(2, 7), pch = 18, xaxs = "i", yaxs = "i")
fit.2 = lm(data.summary$absorption ~ data.summary$nse)
abline(fit.2)

mtext("B", side = 2, line = 3.5, padj = -27, las = 1)

#print(summary(fit.2))

#hist(fit.2$residuals)
#shapiro.test(fit.2$residuals)

# Grp test
print(data.summary[data.summary$grp == 1, ])
print(data.summary[data.summary$grp == 2, ])
print(data.summary[data.summary$grp == 3, ])

par(mfrow=c(1, 1))
boxplot(data.summary$absorption ~ factor(data.summary$grp, labels = c("step dominated", "synchronized", "heart dominated")), main = "Absorption")

print(t.test(data.summary$absorption[data.summary$grp == 1], data.summary$absorption[data.summary$grp == 2], var.equal = F))
print(t.test(data.summary$absorption[data.summary$grp == 2], data.summary$absorption[data.summary$grp == 3], var.equal = F))
print(t.test(data.summary$absorption[data.summary$grp == 3], data.summary$absorption[data.summary$grp == 1], var.equal = F))
