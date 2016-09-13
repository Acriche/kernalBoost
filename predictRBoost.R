predict.rBoost <- function(rBoostModel, df){
  nTree <- length(rBoostModel$model)
  eta <- rBoostModel$eta
  pred <- rep(0, nrow(df))
  for (i in 1:nTree){
    pred.i <- predict.rTree(tree = rBoostModel$model[[i]], df = df)
    pred <- pred + eta * pred.i$pred
  }
  normPred <- (pred - min(pred)) / (max(pred) - min(pred))
  return(normPred)
}