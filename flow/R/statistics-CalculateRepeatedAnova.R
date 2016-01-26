#' Calculates the repeteated anova of a feature and execute an pairwise t test.
#' 
#' \code{CalculateRepeatedAnova} calculates the repeteated anova of a feature and execute an pairwise t test.
#' 
#' @param subject. A vector of subjects
#' @param treatment. A factor to group by treatment
#' @param feature. A vector of feature values

CalculateRepeatedAnova <- function(subject, treatment, feature) {
  options(contrasts = c("contr.sum", "contr.poly"))
  
  subject   <- subject[complete.cases(feature)]
  treatment <- treatment[complete.cases(feature)]
  feature   <- feature[complete.cases(feature)]
  
  df <- data.frame(subject, treatment, feature)
  
  print(with(df, tapply(feature, treatment, mean)))
      
  aov.out <- aov(feature ~ treatment + Error(subject/treatment), data = df)
  print(summary(aov.out))
  
  library(car)
  matrix        <- c()
  treatments    <- levels(df$treatment)
  for(i in 1:length(treatments)) {
    matrix <- cbind(matrix, feature[treatment==treatments[i]])
  }
  model         <- lm(matrix ~ 1)
  design        <- factor(treatments)
  
  aov.out.3     <- Anova(model, idata = data.frame(design), idesign = ~design, type = "III")
  print(summary(aov.out.3, multivariate = F))
  
  print(with(df, pairwise.t.test(feature, treatment, p.adjust.method = "none", paired = T)))
}