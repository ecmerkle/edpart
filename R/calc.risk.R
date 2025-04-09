"calc.risk" <- function(node,pred,sampsize)
  {
    # Calculates misclassification rate (risk) when the DV
    # is categorical.  Must be calculated separately because
    # splits are chosen from Gini criterion, not misclassification.
    # This is unnecessary for continuous DVs because splits and error
    # are based on SSE.
    imp <- 1-mean(node==pred)

    risk <- imp*(length(node)/sampsize)
  }
