library(XML)

growTree <- function(df, y, pred = NULL, objective = "binary:logistic", 
                     maxSplits = 10, minObsPerLeaf = 10, maxCandidatesPerFeature = 50,
                     lambda = 0.1){
  
  # config:
  maxCandidatesPerFeature <<- maxCandidatesPerFeature
  objective <<- objective
  minObsPerLeaf <<- minObsPerLeaf
  lambda <<- lambda

  # preprocessing:  
  ## clean:
  allNa <- as.vector(sapply(df, function(x) !any(!is.na(x))))
  df <- df[!allNa]
  #zeroVar <- sapply(df, function(x) var(x) == 0)
  #df <- df[!zeroVar]
  
  ## initial categorization:
  ## todo: add support for ordered features
  numericFeatures <<- as.vector(sapply(df, is.numeric))
  factorFeatures <<- !numericFeatures

  ## find possible splits:
  n <- nrow(df)
  
  # reset pred vector if not supplied:                                                                    
  if (is.null(pred)) pred <- rep(mean(y), n)
  
  #compute grad and hess vectors:
  if (objective == "binary:logistic") {
    grad <- binomialGrad(pred = pred, y = y) 
    hess <- binomialHess(pred = pred, y = y)
  }

  # reset tree data.frame - the tree object will save:
  ## nodeId, parentId, parentSplit, acceptMissing, score
  outputTree <- data.frame(nodeId = 1, parentId = NA, 
                           parentSplitFeature = NA, 
                           parentSplitOperator = NA,
                           parentSplitValue = NA,
                           acceptMissing = T, 
                           score = mean(y),
                           scoreVar = var(y),
                           leafN = length(y))
  
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
                                 grad[leafObj[[t]]$remainingEntities],
                                 hess[leafObj[[t]]$remainingEntities])
        
        # update the leafObj with info on the best split:
        leafObj[[t]]$maxGain <- split.t$bestSplit$gain
        leafObj[[t]]$bestSplitFeature <- split.t$bestSplit$feature
        leafObj[[t]]$bestSplitValue <- split.t$bestSplit$split
        leafObj[[t]]$missingToLeft <- split.t$bestSplit$missingToLeft
      }
    }
    
    gains <- unlist(lapply(leafObj, function(x) {
      return(unlist(x["maxGain"]))}))
    if (any(gains > -Inf)) {
      # find and lock the best split:
      bestNodeInd <- which(gains == max(gains))[[1]]
      bestNodeId <- leafObj[[bestNodeInd]]$nodeId
      
      # update leafObj: add 2 new leafs
      parentLeafObj <- unpackLeafObjByNodeId(leafObj, bestNodeId)
      leftNodeRemainingEntities <- computeRemainingEntities(entityIds = parentLeafObj$remainingEntities,
                                                            df = df[parentLeafObj$remainingEntities, ], 
                                                            parentNodeId = parentLeafObj$nodeId, 
                                                            featureSplit = parentLeafObj$bestSplitFeature, 
                                                            valueSplit = parentLeafObj$bestSplitValue,
                                                            missingToLeft = parentLeafObj$missingToLeft,
                                                            getLeftNodeEntities = T, 
                                                            isNumericFeature = parentLeafObj$bestSplitFeature %in% colnames(df)[numericFeatures])
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
      leftNodeYVar <- var(y[leftNodeRemainingEntities])
      rightNodeYVar <- var(y[rightNodeRemainingEntities])
      if (parentLeafObj$bestSplitFeature %in% colnames(df)[numericFeatures]) {
        parentSplitOperator <- c("<=", ">")
      } else 
        parentSplitOperator <- c("%in%", "%!in%")
    
      newTreeEntries <- data.frame(nodeId = (nodeIdCounter + 1):(nodeIdCounter + 2),
                                   parentId = rep(parentLeafObj$nodeId, 2), 
                                   parentSplitFeature = rep(parentLeafObj$bestSplitFeature, 2), 
                                   parentSplitOperator = parentSplitOperator, 
                                   parentSplitValue = rep(parentLeafObj$bestSplitValue, 2),
                                   acceptMissing = c(parentLeafObj$missingToLeft, 
                                                     !parentLeafObj$missingToLeft), 
                                   score = c(leftNodeYMean, rightNodeYMean),
                                   scoreVar = c(leftNodeYVar, rightNodeYVar),
                                   leafN = c(length(leftNodeRemainingEntities), length(rightNodeRemainingEntities)))
      outputTree <- rbind(outputTree, newTreeEntries)
      nodeIdCounter <- nodeIdCounter + 2
      
      # delete parent node from leafObj:
      leafObj[[which(unlist(
        lapply(leafObj, function(x) (x$nodeId))) == parentLeafObj$nodeId)]] <- NULL
      
      # prevent current leafs from splitting according to the stopping criteria:
      leafIndToDelete <- which(unlist(
        lapply(leafObj, function(x) length(x$remainingEntities))) <= minObsPerLeaf)
      if (length(leafIndToDelete) > 0) {
        for (r in 1:length(leafIndToDelete)) {
          leafObj[[leafIndToDelete[r]]] <- NULL
        }
      }
      
      # check maxSplits stopping criteria:
      nSplits <- nSplits + 1
      if (nSplits == maxSplits) stopGrowing <- T
      
      if (length(leafObj) == 0) stopGrowing <- T
    } else {
      stopGrowing <- T
    }
  }
  # tree post processing:
  outputTree$isLeaf <- F
  outputTree$isLeaf[!(outputTree$nodeId %in% outputTree$parentId)] <- T
  outputTree$isNumericFeature <- !grepl(pattern = "in", x = outputTree$parentSplitOperator)

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
  factorGroups <- sapply(factor.df, function(x) list(unique(x[!is.na(x)])))
  return(factorGroups)
}

findBestSplit <- function(df, y, grad, hess){
  # input: REMAINING entities in df, y, pred.
  # return: given the remaining entities in the node, best feature/split available
  

  # save the possible splits for this leaf: (todo - save the remaining possible splits per leaf
  # to remove redundant computation)
  numericSplits <- findSplitsForNumeric(df[numericFeatures], maxCandidatesPerFeature)
  factorSplits <- findSplitsForFactor(df[factorFeatures], maxCandidatesPerFeature)
  possibleSplits <- c(numericSplits, factorSplits)
  
  # compute node G and H:
  G <- sum(grad)
  H <- sum(hess)
  
  pTag <- length(possibleSplits)
  gainMatrix <- data.frame("feature" = names(possibleSplits), "split" = rep("", pTag),
                           "missingToLeft" = rep(T, pTag), "gain" = rep(0, pTag))
  gainMatrix$feature <- as.character(gainMatrix$feature)
  gainMatrix$split <- as.character(gainMatrix$split)
  for (i in 1:pTag){
    splits <- as.character(unlist(possibleSplits[names(possibleSplits) %in% gainMatrix$feature[i]][[1]]))
    if (length(splits) > 0) {
      gainMatrix[i, c("split", "missingToLeft", "gain")] <- 
        findMaxGainForFeature(feature = df[gainMatrix$feature[i]], 
                              y = y, 
                              grad = grad, 
                              hess = hess,
                              G = G,
                              H = H,
                              splits = splits)
    } else {
      gainMatrix[i, "gain"] <- -Inf
    }

  }
  
  bestSplit <- gainMatrix[(gainMatrix$gain == max(gainMatrix$gain)), ][1, ]
  return(list(bestSplit = bestSplit))
}

findMaxGainForFeature <- function(feature, y, grad, hess, G, H, splits){
  # Desc: for each split in a certain numeric feature, output the best 
  #       split, missing action, and gain.
  
  feature <- feature[[1]]
  numericFeature <- is.numeric(feature)
  splitGainMatrix <- data.frame("split" = splits, 
                                "missingToLeft" = rep(T, length(splits)), 
                                "gain" = rep(NA, length(splits)))
  splitGainMatrix$split <- as.character(splitGainMatrix$split)
  
  nTag <- length(feature)
  if (any(is.na(feature))){ # has missing:
    for (j in 1:nrow(splitGainMatrix)){
      if (numericFeature) { # try missing to the left
        leftSplitInd <- which(feature <= splits[j] | is.na(feature)) 
      } else 
        leftSplitInd <- which(feature %in% (splits[j]) | is.na(feature)) 
      leftSplitIndLength <- length(leftSplitInd)
      if (leftSplitIndLength > minObsPerLeaf && leftSplitIndLength <= (nTag - minObsPerLeaf)){
        missingToLeftGain <- computeGainPerSplit(leftSplitInd, grad, hess, G, H)
      } else {
        missingToLeftGain <- -Inf
      }
      if (numericFeature) { # try missing to the right
        leftSplitInd <- which(feature <= splits[j]) 
      } else 
        leftSplitInd <- which(feature %in% splits[j])
      leftSplitIndLength <- length(leftSplitInd)
      if (leftSplitIndLength > minObsPerLeaf && leftSplitIndLength <= (nTag - minObsPerLeaf)){
        missingToRightGain <- computeGainPerSplit(leftSplitInd, grad, hess, G, H)
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
    if (length(splits) > 1){
      for (j in 1:length(splits)){
        if (numericFeature) {
          leftSplitInd <- which(feature <= splits[j])
        } else 
          leftSplitInd <- which(feature %in% splits[j])
        leftSplitIndLength <- length(leftSplitInd)
        if (leftSplitIndLength > minObsPerLeaf && leftSplitIndLength <= (nTag - minObsPerLeaf)){
          gain <- computeGainPerSplit(leftSplitInd, grad, hess, G, H)
        } else {
          gain <- -Inf
        }
        splitGainMatrix$gain[j] <- gain
      }
    } else {
      splitGainMatrix$gain <- -Inf
    }
  }
  bestSplit <- which(splitGainMatrix$gain == max(splitGainMatrix$gain))[1]
  return(splitGainMatrix[bestSplit, ])
}

computeGainPerSplit <- function(leftSplitInd, grad, hess, G, H){
  G.left <- sum(grad[leftSplitInd])
  H.left <- sum(hess[leftSplitInd])
  G.right <- G - G.left
  H.right <- H - H.left
  splitGain <- 0.5 * (purity(G.left, H.left) + purity(G.right, H.right) - purity(G, H))
  return(splitGain)
}


purity <- function(grad, hess){
  return((grad)^2 / (hess + lambda))
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

computeRemainingEntities <- function(entityIds, df, parentNodeId, featureSplit, valueSplit, missingToLeft, getLeftNodeEntities, isNumericFeature){
  feature <- df[featureSplit][[1]]
  if (isNumericFeature) { # numeric feature:
    if (getLeftNodeEntities){
      res <- which(feature <= valueSplit)
    } else {
      res <- which(feature > valueSplit)
    }
  } else { # factor feature:
    if (getLeftNodeEntities){
      res <- which(feature %in% valueSplit)
    } else {
      res <- which(feature %!in% valueSplit)
    }
  }
  if ((missingToLeft & getLeftNodeEntities) | (!missingToLeft & !getLeftNodeEntities)){
    res <- c(res, which(is.na(feature)))
  }
  return(entityIds[res])
}

'%!in%' <- function(x,y){
  !('%in%'(x,y))
}
