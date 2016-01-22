#' Computes a treshold of the f1 score.
#' 
#' \code{ComputeFScoreTreshold} returns best epsilon and best f1 score.
#' 
#' @param y. A numeric vector with real results
#' @param p. A numeric vector with predictions
#' @return The best epsilon "epsilon" and the best f1 score "f1"

ComputeFScoreTreshold <- function(y, p) {
  
  bestEpsilon = 0
  bestF1 = 0
  F1 = 0

  stepsize = (max(p) - min(p)) / 1000;
  epsilons <- seq(min(p), max(p), stepsize)
  for (epsilon in epsilons) {
    predictions = (p < epsilon)
    tp = sum((predictions == 1) & (y == 1))
    fp = sum((predictions == 1) & (y == 0))
    fn = sum((predictions == 0) & (y == 1))
    
    if (tp > 0 & (tp + fp) > 0 & (tp + fn) > 0) {
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
