#' Checks normality visually and by the Shapiro.
#' 
#' \code{CheckFeaturesNormalityInGroups} checks normality visually and by the Shapiro.
#' 
#' @param features. A vector of features
#' @param factor. A factor to group the features
#' @param plot. Boolean

CheckFeaturesNormalityInGroups <- function(features, factor, plot = F) {
  for(i in 1:length(features)) {
    feature.name <- names(features)[i]
    for(j in levels(factor)) { 
      feature <- features[factor == j, i]
      if(length(feature) > 0) {
        print(paste(feature.name, j))
        print(shapiro.test(feature))
        
        if(plot) {
          par(mfrow = c(2, 1), xaxs="r", yaxs="r")
          qqnorm(feature, ylab = paste(feature.name, j))
          qqline(feature)
          plot(feature, ylab = paste(feature.name, j))
        }
      }
    }
  }
}