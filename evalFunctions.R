rocAuc <- function(obs, pred) {
  n1 <- sum(obs)
  n <- length(obs)
  if (n == n1) {
    return(1)
  }
  return((mean(rank(pred)[obs > 0]) - (n1 + 1)/2)/(n - n1))
}

accuracy <- function(obs, pred){
  if (any(!(unique(y) %in% unique(pred))) | any(!(unique(pred) %in% unique(y)))) 
    stop("\nThe accuracy evaluation function can be used only on discrete classificaiton problems. 
         Please make sure that the values in the pred vector matchs the values in the pred vector.")
  return(sum(y == pred) / length(y))
}

rmse <- function(obs, pred){
  return(mean((obs - pred)^2))
}