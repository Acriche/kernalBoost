

growTree <- function(df, y, objective = "binary:logistic", 
                     maxSplits = 10, minObsPerLeaf = 10, maxCandidatesPerFeature = 50){
  
  # config:
  maxCandidatesPerFeature <<- maxCandidatesPerFeature
  objective <<- objective
  
  # preprocessing:  
  ## clean:
  allNa <- as.vector(sapply(df, function(x) !any(!is.na(x))))
  df <- df[!allNa]
  #zeroVar <- sapply(df, function(x) var(x) == 0)
  #df <- df[!zeroVar]
  
  ## initial categorization:
  numericFeatures <<- as.vector(sapply(df, is.numeric))
  factorFeatures <<- !numericFeatures

  ## find possible splits:
  n <- nrow(df)
  
  # reset pred vector:                                                                    
  pred <- rep(0.5, n)

  # reset tree data.frame - the tree object will save:
  ## nodeId, parentId, parentSplit, acceptMissing, score
  outputTree <- data.frame(nodeId = 1, parentId = NA, parentSplitFeature = NA, 
                           parentSplitValue = NA,
                           acceptMissing = T, score = mean(y))
  
  # leafObj: for now, the leaf object will save the following info about only CURRENT leafs:
  ## nodeId, remaining entities, bestSplitFeature ,bestSplitValue and maxGain. (in case computed)
  ## a leaf will be deleted from the object in case:
  ### 1. it is no longer a leaf.
  ### 2. it passed the stopping criteria.
  ## (todo - add remaining splits in case it was already computed)
  leafObj <- list(list(nodeId = 1, 
                       remainingEntities = 1:n, 
                       bestSplitFeature = NA, 
                       bestSplitValue = NA,
                       missingToLeft = T,
                       maxGain = NA))
  nodeIdCounter <- 1
  nSplits <- 0
  
  # grow tree until stopping criteria is met: run on all (open) leafs in leafObj
  stopGrowing <- F
  while (!stopGrowing){
    for (t in 1:length(leafObj)){
      if (is.na(leafObj[[t]]$maxGain)) {
        # given the leaf's remaining entities indicator, find the best split:
        split.t <- findBestSplit(df[leafObj[[t]]$remainingEntities, ], 
                                 y[leafObj[[t]]$remainingEntities], 
                                 pred[leafObj[[t]]$remainingEntities])
        # update the leafObj with info on the best split:
        leafObj[[t]]$maxGain <- split.t$gain
        leafObj[[t]]$bestSplitFeature <- split.t$feature
        leafObj[[t]]$bestSplitValue <- split.t$split
        leafObj[[t]]$missingToLeft <- split.t$missingToLeft
      }
    }
    # find and lock the best split:
    bestNodeId <- lapply(leafObj, function(x) {
      gains <- unlist(x["maxGain"])
      if (!any(gains != -Inf)) return("FinishTree")
      ind <- which(gains == max(gains))
      return(ind[[1]])
    })[[1]]
    
    if (bestNodeId != "FinishTree"){
      # update leafObj: add 2 new leafs
      parentLeafObj <- unpackLeafObjByNodeId(leafObj, bestNodeId)
      leftNodeRemainingEntities <- computeRemainingEntities(entityIds = parentLeafObj$remainingEntities,
                                                            df = df[parentLeafObj$remainingEntities, ], 
                                                            parentNodeId = parentLeafObj$nodeId, 
                                                            featureSplit = parentLeafObj$bestSplitFeature, 
                                                            valueSplit = parentLeafObj$bestSplitValue,
                                                            missingToLeft = parentLeafObj$missingToLeft,
                                                            getLeftNodeEntities = T)
      rightNodeRemainingEntities <- parentLeafObj$remainingEntities[!(parentLeafObj$remainingEntities %in% 
                                                                       leftNodeRemainingEntities)]
      leftLeaf <- list(nodeId = nodeIdCounter + 1, 
                       remainingEntities = leftNodeRemainingEntities, 
                       bestSplitFeature = NA, 
                       bestSplitValue = NA,
                       maxGain = NA)
      rightLeaf <- list(nodeId = nodeIdCounter + 2, 
                        remainingEntities = rightNodeRemainingEntities, 
                        bestSplitFeature = NA, 
                        bestSplitValue = NA,
                        maxGain = NA)
      leafObj <- append(leafObj, list(leftLeaf, rightLeaf))
      
      # update tree structure: add 2 new leafs - left/right
      leftNodeYMean <- mean(y[leftNodeRemainingEntities])
      rightNodeYMean <- mean(y[rightNodeRemainingEntities])
      newTreeEntries <- data.frame(nodeId = (nodeIdCounter + 1):(nodeIdCounter + 2),
                                   parentId = rep(parentLeafObj$nodeId, 2), 
                                   parentSplitFeature = rep(parentLeafObj$bestSplitFeature, 2), 
                                   parentSplitValue = c(parentLeafObj$bestSplitValue,
                                                        paste0("!", parentLeafObj$bestSplitValue)),
                                   acceptMissing = c(parentLeafObj$missingToLeft, 
                                                     !parentLeafObj$missingToLeft), 
                                   score = c(leftNodeYMean, rightNodeYMean))
      outputTree <- rbind(outputTree, newTreeEntries)
      nodeIdCounter <- nodeIdCounter + 2
      
      # delete parent node from leafObj:
      leafObj[[which(unlist(
        lapply(leafObj, function(x) (x$nodeId))) == parentLeafObj$nodeId)]] <- NULL
      
      # prevent current leafs from splitting according to the stopping criteria:
      leafIndToDelete <- which(unlist(
        lapply(leafObj, function(x) length(x$remainingEntities))) <= minObsPerLeaf)
      if (length(leafIndToDelete) > 0) leafObj[[leafIndToDelete]] <- NULL
      
      # check maxSplits stopping criteria:
      nSplits <- nSplits + 1
      if (nSplits == maxSplits) stopGrowing <- T
      
      if (length(leafObj) == 0) stopGrowing <- T
    } else {
      stopGrowing <- T
    }
  }
  
  return(outputTree)
}


findSplitsForNumeric <- function(numeric.df, maxCandidatesPerFeature){
  quantileStep <- 1/maxCandidatesPerFeature
  quantilesMat <- sapply(numeric.df, function(x) quantile(x, probs = seq(0 + quantileStep, 
                                                                      1 - quantileStep, by = quantileStep), 
                                          na.rm = T))
  quantilesMat <- as.data.frame(quantilesMat)
  quantilesList <- sapply(quantilesMat, function(x) list(unique(x)))
  return(quantilesList)
}

findSplitsForFactor <- function(factor.df, maxCandidatesPerFeature){
  # todo: change to make more optimal
  factorGroups <- sapply(factor.df, function(x) list(unique(x)))
  return(factorGroups)
}

findBestSplit <- function(df, y, pred){
  # input: REMAINING entities in df, y, pred.
  # return: given the remaining entities in the node, best feature/split available
  
  # save the possible splits for this leaf: (todo - save the remaining possible splits per leaf
  # to remove redundant computation)
  numericSplits <- findSplitsForNumeric(df[numericFeatures], maxCandidatesPerFeature)
  factorSplits <- findSplitsForFactor(df[factorFeatures], maxCandidatesPerFeature)
  possibleSplits <- c(numericSplits, factorSplits)
  
  pTag <- length(possibleSplits)
  gainMatrix <- data.frame("feature" = names(possibleSplits), "split" = rep(NA, pTag),
                           "missingToLeft" = rep(T, pTag), "gain" = rep(0, pTag))
  gainMatrix$feature <- as.character(gainMatrix$feature)
  for (i in 1:pTag){
    if (gainMatrix$feature[i] %in% colnames(df)[numericFeatures]){ # numeric feature:
      gainMatrix[i, c("split", "missingToLeft", "gain")] <- 
        findMaxGainForNumericFeature(feature = df[i], 
                                     y = y, 
                                     pred = pred, 
                                     splits = unlist(numericSplits[names(numericSplits) %in% colnames(df)[i]][[1]]))
    } else { # factor feature:
      gainMatrix[i, c("split", "missingToLeft", "gain")] <- findMaxGainForFactorFeature()
    }
  }
  
  if (var(gainMatrix$gain) == 0){
    # do not continue to split the leaf:
    gainMatrix <- gainMatrix[1, ]
    gainMatrix$gain <- -Inf
  }
  
  bestSplit <- gainMatrix[(gainMatrix$gain == max(gainMatrix$gain)), ]
  return(bestSplit)
}

findMaxGainForNumericFeature <- function(feature, y, pred, splits){
  # Desc: for each split in a certain numeric feature, output the best 
  #       split, missing action, and gain.
  
  feature <- feature[[1]]
  splitGainMatrix <- data.frame("split" = splits, 
                                "missingToLeft" = rep(T, length(splits)), 
                                "gain" = rep(NA, length(splits)))
  nTag <- length(feature)
  if (any(is.na(feature))){ # has missing:
    for (j in 1:nrow(splitGainMatrix)){
      leftSplitInd <- which(feature <= splits[j] | is.na(feature)) # try missing to the left
      if (length(leftSplitInd) > 0 && length(leftSplitInd) < nTag){
        missingToLeftGain <- computeGainPerSplit(leftSplitInd, y, pred)
      } else {
        missingToLeftGain <- -Inf
      }
      leftSplitInd <- which(feature <= splits[j]) # try missing to the right
      if (length(leftSplitInd) > 0 && length(leftSplitInd) < nTag){
        missingToRightGain <- computeGainPerSplit(leftSplitInd, y, pred)
      } else {
        missingToRightGain <- -Inf
      }
      if (missingToLeftGain > missingToRightGain) {
        splitGainMatrix$missingToLeft[j] <- T
        splitGainMatrix$gain[j] <- missingToLeftGain
      } else {
        splitGainMatrix$missingToLeft[j] <- F
        splitGainMatrix$gain[j] <- missingToRightGain
      }
    }
  } else { # no missing:
    for (j in 1:length(splits)){
      leftSplitInd <- which(feature <= splits[j])
      if (length(leftSplitInd) > 0 && length(leftSplitInd) < nTag){
        gain <- computeGainPerSplit(leftSplitInd, y, pred)
      } else {
        gain <- -Inf
      }
      splitGainMatrix$gain[j] <- gain
    }
  }
  bestSplit <- which(splitGainMatrix$gain == max(splitGainMatrix$gain))[1]
  return(splitGainMatrix[bestSplit, ])
}

computeGainPerSplit <- function(leftSplitInd, y, pred){
    newLeftLeafPred <- rep(mean(y[leftSplitInd]), length(leftSplitInd))
    newRightLeafPred <- rep(mean(y[!(1:length(y) %in% leftSplitInd)]), (length(y) - length(leftSplitInd)))
    if (objective == "binary:logistic") {
      purityBefore <- purity(grad = binomialGrad(pred = pred, y = y), 
                             hess = binomialHess(pred = pred, y = y))
      purityLeft <- purity(grad = binomialGrad(pred = newLeftLeafPred, y = y[leftSplitInd]), 
                           hess = binomialHess(pred = newLeftLeafPred, y = y[leftSplitInd]))
      purityRight <- purity(grad = binomialGrad(pred = newRightLeafPred, y = y[!(1:length(y) %in% leftSplitInd)]), 
                            hess = binomialHess(pred = newRightLeafPred, y = y[!(1:length(y) %in% leftSplitInd)]))
      splitGain <- gain(purityBefore, purityLeft, purityRight)
    }
    return(splitGain)
}


purity <- function(grad, hess, lambda = 0.1){
  return((sum(grad))^2 / (sum(hess) + lambda))
}

gain <- function(purityBefore, purityLeft, purityRight){
    return(0.5 * (purityLeft + purityRight - purityBefore))
}

binomialGrad <- function(pred, y){
  return(pred - y)
}

binomialHess <- function(pred, y){
  return(pred * (1 - pred))
}

unpackLeafObjByNodeId <- function(leafObj, nodeId){
  ind1 <- lapply(leafObj, function(x){
    return(unlist(x$nodeId))
  })
  ind2 <- which(ind1 == nodeId)
  return(leafObj[[ind2]])
}

computeRemainingEntities <- function(entityIds, df, parentNodeId, featureSplit, valueSplit, missingToLeft, getLeftNodeEntities){
  feature <- df[featureSplit][[1]]
  if (is.numeric(valueSplit)) { # numeric feature:
    if (getLeftNodeEntities){
      res <- which(feature <= valueSplit)
    } else {
      res <- which(feature > valueSplit)
    }
  } else { # factor feature:
    if (getLeftNodeEntities){
      res <- which(feature %in% valueSplit)
    } else {
      res <- which(!(feature %in% valueSplit))
    }
  }
  if ((missingToLeft & getLeftNodeEntities) | (!missingToLeft & !getLeftNodeEntities)){
    res <- c(res, which(is.na(feature)))
  }
  return(entityIds[res])
}