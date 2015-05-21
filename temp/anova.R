# Repeated Anova Example
# https://statistics.laerd.com/statistical-guides/repeated-measures-anova-statistical-guide.php

# Create Structure
blood.pressure        <- read.csv("~/Entwicklung/bogutzky/repositories/non-disruptive-flow-measures/temp/blood-pressure.csv", sep=";")
blood.pressure.2      <- blood.pressure[1:6, 2:4]
blood.pressure.2      <- stack(blood.pressure.2)
subject               <- rep(blood.pressure$Subjects, 3)  # create the "subject" variable
blood.pressure.2[3]   <- subject                          # add it to the new data frame
rm(subject)                                               # clean up your workspace
colnames(blood.pressure.2)  <- c("blood.pressure", "time", "subject")   # rename the columns
blood.pressure.2$subject    <- as.factor(blood.pressure.2$subject)

with(blood.pressure.2, tapply(blood.pressure, time, sum))

# Check: Underlying Assumptions: Normality

for(i in 1:3) {
  print(names(blood.pressure)[i+1])
  print(shapiro.test(blood.pressure[,i]))
  qqnorm(blood.pressure[,i], ylab = names(blood.pressure)[i+1])
  qqline(blood.pressure[,i])
}

# The above table presents the results from two well-known tests of normality, namely the Kolmogorov-Smirnov Test and the Shapiro-Wilk Test. The Shapiro-Wilk Test is more appropriate for small sample sizes (< 50 samples), but can also handle sample sizes as large as 2000. For this reason, we will use the Shapiro-Wilk test as our numerical means of assessing normality.
# We can see from the above table that for the "Pre.", "X3.Months" and "X3.Months" the dependent variable, "blood pressure", was normally distributed. How do we know this? If the Sig. value of the Shapiro-Wilk Test is greater than 0.05, the data is normal. If it is below 0.05, the data significantly deviate from a normal distribution.

aov.out       <- aov(blood.pressure ~ time + Error(subject/time), data=blood.pressure.2)
summary(aov.out)

blood.pressure.value  <- blood.pressure.2$blood.pressure
time                  <- blood.pressure.2$time
subject               <- blood.pressure.2$subject
data                  <- data.frame(subject, time, blood.pressure.value)

matrix <- with(data, cbind(blood.pressure.value[time=="Pre."], blood.pressure.value[time=="X3.Months"], blood.pressure.value[time=="X6.Months"])) 

model <- lm(matrix ~ 1)

design <- factor(c("Pre.", "X3.Months", "X6.Months"))

library(car)

options(contrasts=c("contr.sum", "contr.poly"))
aov <- Anova(model, idata=data.frame(design), idesign=~design, type="III")
summary(aov, multivariate=F)

