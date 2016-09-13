genPathString <- function(tree){
  tree$parentId[is.na(tree$parentId)] <- 1
  tree$parentSplitFeature[is.na(tree$parentSplitFeature)] <- ""
  tree$parentSplitOperator[is.na(tree$parentSplitOperator)] <- ""
  tree$parentSplitValue[is.na(tree$parentSplitValue)] <- ""
  tree <- tree[order(tree$nodeId), ]
  tree$nodeText <- paste0(tree$parentSplitFeature, " ", tree$parentSplitOperator, " ", tree$parentSplitValue)
  tree$tempPath <- ""
  for(i in 1:nrow(tree)){
    tree$tempPath[i] <- paste(tree$nodeText[tree$parentId[i]], tree$nodeText[i], sep = "/")
    if (tree$parentId[i] %in% tree$nodeId){
      tree$tempPath[i] <- paste(tree$tempPath[tree$nodeId %in% tree$parentId[i]],
                                tree$tempPath[i], sep = "/")
    }
  }
  tree$pathString <- lapply(tree$tempPath, function(x) {
    str <- strsplit(x = x, "/")[[1]]
    str2 <- str[!duplicated(str)]
    str3 <- paste(str2, collapse = "/")
    return(str3)
  })
  tree$pathString <- unlist(tree$pathString)
  return(tree[!(colnames(tree) %in% "tempPath")])
}