backup.anova  <- anova
anova         <- backup.anova[1:6, 1:4]
anova2        <- anova[1:6, 2:4]
anova2        <- stack(anova2)

subject       <- rep(anova$Subjects,3)  # create the "subject" variable
anova2[3]     <- subject                # add it to the new data frame
rm(subject)                             # clean up your workspace
colnames(anova2)  <- c("v", "t", "s")   # rename the columns
anova2$s <- as.factor(anova2$s)

with(anova2, tapply(v, t, sum))

for(i in 1:3) {
  print(names(anova)[i+1])
  print(shapiro.test(anova[,i]))
  qqnorm(anova[,i], ylab = names(anova)[i+1])
  qqline(anova[,i])
}

aov.out       <- aov(v ~ t + Error(s/t), data=anova2)
summary(aov.out)




