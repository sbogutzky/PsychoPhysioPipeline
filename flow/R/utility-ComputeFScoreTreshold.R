# The MIT License (MIT)
# Copyright (c) 2016 Simon Bogutzky
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software
# and associated documentation files (the "Software"), to deal in the Software without restriction,
# including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
# DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
# ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#' Computes a treshold of the f1 score.
#' 
#' \code{ComputeFScoreTreshold} returns best epsilon and best f1 score.
#' 
#' @param y a numeric vector with real results
#' @param p a numeric vector with predictions
#' @return the best epsilon "epsilon" and the best f1 score "f1"

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
