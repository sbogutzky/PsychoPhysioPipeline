#' Calculates the repeteated anova of a feature and execute an pairwise t test.
#' 
#' \code{CalculateRepeatedAnova} calculates the repeteated anova of a feature and execute an pairwise t test.
#' 
#' @param subject. A vector of subjects
#' @param treatment. A factor to group by treatment
#' @param feature. A vector of feature values
#' @param paired. A logical indicating whether you want paired t-tests

CalculateRepeatedAnova <- function(subject, treatment, feature, paired = T) {
  options(contrasts = c("contr.sum", "contr.poly"))
  
#   subject <- subject[complete.cases(feature)]
#   treatment <- treatment[complete.cases(feature)]
#   feature <- feature[complete.cases(feature)]
#   data.1 <- data.frame(subject, treatment, feature)
#   print(with(data.1, tapply(feature, treatment, mean)))
#   aov.out <- aov(feature ~ treatment + Error(subject/treatment), data = data.1)
#   print(summary(aov.out))

  library(car)
  matrix <- c()
  t.levels <- levels(treatment)
  for(t.level in t.levels) {
    matrix <- cbind(matrix, feature[treatment == t.level])
  }
  model <- lm(matrix ~ 1)
  design <- factor(t.levels)
  aov.out <- Anova(model, idata = data.frame(design), idesign = ~design, type = "III")
  print(summary(aov.out, multivariate = F))
  print(with(data.1, pairwise.t.test(feature, treatment, p.adjust.method = "none", paired = paired)))
}