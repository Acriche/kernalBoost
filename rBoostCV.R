# rBoostConfig <- list(objective = "binary:logistic", evalFunction = "rocAuc", 
#                      nTree = 10, eta = 0.1, colSampling = 0.3, rowSampling = 0.4,
#                      maxSplits = 5, minObsPerLeaf = 30, maxCandidatesPerFeature = 10,
#                      seed = 1)

rBoostCV <- function(y, df, rBoostConfig, nFolds = 5, nCores = 1, verbose = T){
  # todo: merge with default rBoostConfig
  n <- length(y)
  p <- ncol(df)
  
  registerDoParallel(nCores)
  
  set.seed(rBoostConfig$seed)
  folds <- createFolds(y = 1:n, k = nFolds)
  
  trainDf <- lapply(1:nFolds, function(x) df[!(1:n %in% folds[[x]]), ])
  testDf <- lapply(1:nFolds, function(x) df[folds[[x]], ])
  trainY <- lapply(1:nFolds, function(x) y[!(1:n %in% folds[[x]])])
  testY <- lapply(1:nFolds, function(x) y[folds[[x]]])
  trainPred <- lapply(folds, function(x) rep(0, n - length(x)))
  testPred <- lapply(folds, function(x) rep(0, length(x)))
  evalTrainList <- NULL
  evalTestList <- NULL
  
  for (i in 1:(rBoostConfig$nTree)){
    # build nFold trees and add to the cumulative prediction to the test fold.
    # no need to save the info of the already predicted trees.
    foreachRes <- foreach(fold = 1:nFolds, 
            .combine = rbind)  %dopar%  {
              
              set.seed(fold + i)
              colSampling <- sample(x = 1:p, size = round(rBoostConfig$colSampling * p), 
                                    replace = F)
              set.seed(fold + i)
              rowSampling <- sample(x = 1:nrow(trainDf[[fold]]), 
                                    size = round(rBoostConfig$rowSampling * nrow(trainDf[[fold]])), 
                                    replace = F)
              
              tree.i.fold <- growTree(df = trainDf[[fold]][rowSampling, colSampling], 
                                       y = trainY[[fold]][rowSampling],
                                       pred = trainPred[[fold]][rowSampling],
                                       objective = rBoostConfig$objective, 
                                       maxSplits = rBoostConfig$maxSplits,
                                       minObsPerLeaf = rBoostConfig$minObsPerLeaf, 
                                       maxCandidatesPerFeature = rBoostConfig$maxCandidatesPerFeature,
                                       lambda = rBoostConfig$lambda)   
              
              treeTrainPred <- predict.rTree(tree = tree.i.fold, df = trainDf[[fold]])
              trainPred[[fold]] <- trainPred[[fold]] + rBoostConfig$eta * treeTrainPred$pred
              
              treeTestPred <- predict.rTree(tree = tree.i.fold, df = testDf[[fold]])
              testPred[[fold]] <- testPred[[fold]] + rBoostConfig$eta * treeTestPred$pred
              
              return(list("trainPred" = trainPred[[fold]], "testPred" = testPred[[fold]]))
            }
    
    trainPred <- lapply(seq(1, (length(foreachRes) / 2), by = 1), 
                        function(x) foreachRes[[x]])
    testPred <- lapply(seq(length(foreachRes) / 2 + 1, length(foreachRes), by = 1), 
                       function(x) foreachRes[[x]])
    evalTrain.i <- eval(parse(text = paste0("lapply(1:nFolds, function(x) ",
      rBoostConfig$evalFunction, "(obs = trainY[[x]], pred = trainPred[[x]]))")))
    evalTrain.i.mean <- mean(unlist(evalTrain.i))
    evalTrainList <- c(evalTrainList, evalTrain.i.mean)
    
    evalTest.i <- eval(parse(text = paste0("lapply(1:nFolds, function(x) ",
                                           rBoostConfig$evalFunction, "(obs = testY[[x]], pred = testPred[[x]]))")))
    evalTest.i.mean <- mean(unlist(evalTest.i))
    evalTestList <- c(evalTestList, evalTest.i.mean)
    
    if (verbose) cat(paste0("\nIter #", i, ": Train ", rBoostConfig$evalFunction, " - ", round(evalTrain.i.mean, 4),
                            " | Test ", rBoostConfig$evalFunction, " - ", round(evalTest.i.mean, 4)))
  }
  
  stopImplicitCluster()
  
  return(list(trainRes = evalTrainList, testRes = evalTestList))
}