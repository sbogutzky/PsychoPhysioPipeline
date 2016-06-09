#' Plots groups of features as histogram and boxplat.
#' 
#' \code{PlotFeaturesInGroups} plots groups of features as histogram and boxplat.
#' 
#' @param features. A vector of features
#' @param factor. A factor to group the features

PlotFeaturesInGroups <- function(features, factor) {
  for(i in 1:length(features)) {
    feature       <- features[, i]
    feature.name  <- names(features)[i]
    
    par(mfrow = c(3, 1), xaxs = "r", yaxs = "r")
    plot(feature, pch = as.numeric(factor), main = feature.name, ylab = feature.name)
    
    f        <- factor[complete.cases(feature)]
    feature  <- feature[complete.cases(feature)]
    
    boxplot(feature ~ f, ylab = feature.name)
    
    hist(feature, xlab = feature.name, breaks = 10)
    lines(density(feature))
    rug(jitter(feature))
  } 
}