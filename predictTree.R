predict.rTree <- function(tree, df){
  # currently this is O(nrow(tree)). This could be changed to O(nLeafs).
  
  modelFeatures <- tree$parentSplitFeature[!is.na(tree$parentSplitFeature)]
  if ("internalId" %in% colnames(df)) stop("\n'internalId' is an invalid column name.")
  if (any(!(modelFeatures %in% colnames(df)))) 
    stop(paste0("\nThe following critical features are missing from the data frame:\n'",
                paste0(modelFeatures[!(modelFeatures %in% colnames(df))], collapse = "', '"), "'"))
  if (nrow(tree) < 2) stop("\nTree has no nodes.")
  
  n <- nrow(df)
  df$internalId <- 1:n
  tree <- tree[order(tree$nodeId), ]
  tree$ind <- NA
  tree$ind[1] <- list(1:n)
  resultsDf <- data.frame(internalId = 1:n, pred = rep(NA, n))
  
  for (i in 2:nrow(tree)){
    tempDf <- df[unlist(tree$ind[tree$nodeId == tree$parentId[i]]), ]
    evalExp <- paste0("tempDf$internalId[tempDf['", 
                      tree$parentSplitFeature[i], "'][[1]] ", 
                   tree$parentSplitOperator[i], " ",
                   ifelse(tree$isNumericFeature[i], tree$parentSplitValue[i], 
                          paste0("'", tree$parentSplitValue[i], "'")),
                   ifelse(tree$acceptMissing[i], 
                          paste0(" | is.na(tempDf['", 
                                 tree$parentSplitFeature[i], "'][[1]])"), ""), "]")
    nodeEntityId <- eval(parse(text = evalExp))
    tree$ind[i] <- list(nodeEntityId)
    if (tree$isLeaf[i]){
     resultsDf$pred[resultsDf$internalId %in% nodeEntityId] <- tree$score[i]
    }
  }
  
  if (any(is.na(resultsDf$pred))) stop("\nBug found in prediction function.")
  
  return(resultsDf)
}