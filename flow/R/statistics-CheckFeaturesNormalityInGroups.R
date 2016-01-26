#' Checks normality visually and by the Shapiro.
#' 
#' \code{CheckFeaturesNormalityInGroups} checks normality visually and by the Shapiro.
#' 
#' @param features. A vector of features
#' @param factor. A factor to group the features
#' @param plot. Boolean
#' @return A vector of indexes of normally distributed features

CheckFeaturesNormalityInGroups <- function(features, factor, plot = F) {
  normally.distributed <- c()
  for(i in 1:length(features)) {
    feature.name <- names(features)[i]
    is.normally.distributed <- T
    for(j in levels(factor)) { 
      feature <- features[factor == j, i]
      if(length(feature) > 0) {
        
        test <- shapiro.test(feature)
        if(test$p.value < 0.05) {
          
          is.normally.distributed <- F
          print(paste(feature.name, j))
          print(test)
          
          if(plot) {
            par(mfrow = c(2, 1), xaxs="r", yaxs="r")
            qqnorm(feature, ylab = paste(feature.name, j))
            qqline(feature)
            plot(feature, ylab = paste(feature.name, j))
          }
        }
      }
    }
    if(is.normally.distributed) {
      normally.distributed <- c(normally.distributed, i)
    }
  }
  return(normally.distributed)
}