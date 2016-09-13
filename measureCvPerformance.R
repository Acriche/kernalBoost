measureCvPerformancePerTree <- function(y, df, k = 5, evalFun = "rocAuc"){
  n <- length(y)
  set.seed(1)
  folds <- createFolds(y = 1:n, k = k)
  
  results <- data.frame(folds = 1:5, cvResult = rep(NA, k))
    
  for (i in 1:k){
    ind.i <- !(1:n %in% folds[[i]])
    trainSet.i <- df[ind.i, ]
    testSet.i <- df[folds[[i]], ]
    yTrain.i <- y[ind.i]
    yTest.i <- y[folds[[i]]]
    tree.i <- growTree(df = trainSet.i, y = yTrain.i, objective = "binary:logistic", maxSplits = 4, reportComputeTime = T)
    predictions.i.cv <- predictTree(tree = tree.i, df = testSet.i)
    results$cvResult[i] <- eval(parse(text = paste0(evalFun, "(obs = yTest.i, pred = predictions.i.cv$pred)")))
  }
  return(mean(results$cvResult))
}