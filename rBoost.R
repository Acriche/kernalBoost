# rBoostConfig <- list(objective = "binary:logistic", evalFunction = "rocAuc", 
#                      nTree = 10, eta = 0.1, colSampling = 0.3, rowSampling = 0.4,
#                      maxSplits = 5, minObsPerLeaf = 30, maxCandidatesPerFeature = 10,
#                      gamma = 0.01, lambda = 0.1,
#                      seed = 1)

rBoost <- function(y, df, rBoostConfig, verbose = T){
  # todo: merge with default rBoostConfig
  n <- length(y)
  p <- ncol(df)
  pred <- rep(0, n)
  treeList <- list()
  set.seed(rBoostConfig$seed)
  colSamplingBins <- lapply(1:rBoostConfig$nTree, function(x) 
    sample(x = 1:p, size = round(rBoostConfig$colSampling * p), replace = F))
  set.seed(rBoostConfig$seed)
  rowSamplingBins <- lapply(1:rBoostConfig$nTree, function(x) 
    sample(x = 1:n, size = round(rBoostConfig$rowSampling * n), replace = F))
  for (i in 1:(rBoostConfig$nTree)){
    tree.i <- growTree(df = df[rowSamplingBins[[i]], colSamplingBins[[i]]], 
                       y = y[rowSamplingBins[[i]]],
                       pred = pred[rowSamplingBins[[i]]],
                       objective = rBoostConfig$objective, 
                       maxSplits = rBoostConfig$maxSplits,
                       minObsPerLeaf = rBoostConfig$minObsPerLeaf, 
                       maxCandidatesPerFeature = rBoostConfig$maxCandidatesPerFeature, 
                       lambda = rBoostConfig$lambda)
    treeList <- append(treeList, list(tree.i))
    pred.i <- predict.rTree(tree = tree.i, df = df)
    pred <- pred + rBoostConfig$eta * pred.i$pred
    auc.i <- eval(parse(text = paste0(rBoostConfig$evalFunction, "(obs = y, pred = pred)")))
    if (verbose) cat(paste0("\nIteration #", i, ": Train ", rBoostConfig$evalFunction, " - ", round(auc.i, 4)))
  }
  return(list(model = treeList, eta = rBoostConfig$eta))
}