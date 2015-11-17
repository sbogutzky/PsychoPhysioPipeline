SelectThreshold <- function(yval, pval) {
  #' \code{MultivariateGaussian} finds the best threshold (epsilon) to use for selecting outliers
  #' Finds the best threshold to use for selecting outliers based on the results from a validation set (pval) and the ground truth (yval).

  bestEpsilon = 0
  bestF1 = 0
  F1 = 0

  stepsize = (max(pval) - min(pval)) / 1000;
  epsilons <- seq(min(pval), max(pval), stepsize)
  for (epsilon in epsilons) {
    predictions = (pval < epsilon)
    tp = sum((predictions == 1) & (yval == 1))
    fp = sum((predictions == 1) & (yval == 0))
    fn = sum((predictions == 0) & (yval == 1))
    
    if ((tp + fp) > 0 & (tp + fn) > 0) {
      prec = tp / (tp + fp)
      rec = tp / (tp + fn)
      F1 = 2 * prec * rec / (prec + rec)
    }
    
    if(F1 > bestF1) {
      bestF1 = F1
      bestEpsilon = epsilon
    }
  }
  return(list("epsilon" = bestEpsilon, "f1" = bestF1))
}
