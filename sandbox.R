setwd("/Users/yoniacriche/Documents/Personal/TransferLearning/TL_Verison2/R/RBoosting/")
source("RBoostingWrapper.R")


library(rpart)
library(foreign)
arff <- read.arff("/Users/yoniacriche/Downloads/00D300000000RWREA2Lead_USA_train_2016.04.12_16-35-48.arff")
y <- arff$class
y <- as.character(y)
y <- ifelse(y == "POSITIVE", 1, 0)
y <- as.numeric(y)
df <- arff[!(colnames(arff) %in% c("id", "class"))]

dfNumeric <- df[sapply(df, is.numeric)]
trainInd <- sample(x = 1:nrow(dfNumeric), size = (0.75 * nrow(dfNumeric)), replace = F)
testInd <- which(!(1:nrow(dfNumeric) %in% trainInd))
dfNumericTrain <- dfNumeric[trainInd, ]
dfNumericTest <- dfNumeric[testInd, ]


#RTree:
tree <- growTree(df = df, y = y, maxSplits = 6, maxCandidatesPerFeature = 10)
library(data.tree)
tree2 <- genPathString(tree)
plot(as.Node(tree2))
p <- predict.rTree(tree = tree, df = df)
rocAuc(y, p$pred)
cv <- measureCvPerformance(y = y, df = dfNumeric, evalFun = "rocAuc")

#RBoost:
rBoostConfig <- list(objective = "binary:logistic", evalFunction = "rocAuc", 
                     nTree = 50, eta = 0.01, colSampling = 0.3, rowSampling = 1,
                     maxSplits = 3, minObsPerLeaf = 30, maxCandidatesPerFeature = 10,
                     lambda = 10,
                     seed = 1)
rBoostModel <- rBoost(y = y[trainInd], df = dfNumericTrain, rBoostConfig = rBoostConfig)
preds <- predict.rBoost(rBoostModel = rBoostModel, df = dfNumericTest)
rocAuc(y[testInd], preds)

cvRes <- rBoostCV(y = y[trainInd], df = dfNumericTrain, rBoostConfig = rBoostConfig, nFolds = 5, 
                  nCores = 5, verbose = T)
cvRes <- rBoostCV(y = y, df = df, rBoostConfig = rBoostConfig, nFolds = 5, 
                  nCores = 5, verbose = T)


#XGBoost:
setwd("/Users/yoniacriche/salespredict/aurora/src/main/")
source("R/HighSeptonWrapper.R")
dfNumericTrainXgboost <-
  xgb.DMatrix(data.matrix(dfNumericTrain), label = y[trainInd], missing = NA)
dfNumericTrainXgboost <-
  xgb.DMatrix(data.matrix(df), label = y, missing = NA)

xgb.cv(
  data = dfNumericTrainXgboost,
  eta = 0.1,
  nround = 100,
  max.depth = 3,
  colsample_bytree = 0.3,
  min_child_weight = 30,
  nfold = 5,
  nthread = 6,
  objective = "binary:logistic",
  eval_metric = "auc",
  verbose = T
)
HighSeptonLauncher(arff = data.frame("id" = 1:nrow(dfNumeric), dfNumeric, 
                                     class = ifelse(y == 1, "POSITIVE", "NEGATIVE")), verbose = T, 
                   HSSettingsObj = list(nRounds = 20))

#H2O:
library(h2o)
h2o.init()
australia.hex <- h2o.uploadFile(path = ausPath)
h2o <- h2o.gbm(y = y, x = dfNumeric, training_frame = australia.hex,
        ntrees = 100, max_depth = 3)

#gbm:
library(gbm)
gbmModel <- gbm(formula = y~., data = data.frame(y = y[trainInd], dfNumericTrain, check.names = F), n.trees = 20, verbose = T)
rocAuc(y[testInd], predict(object = gbmModel, dfNumericTest, n.trees = 20))


#rpart:
growTree(df = iris[!(colnames(iris) %in% "Species")], y = as.numeric(iris$Species == "versicolor"), maxSplits = 2)

irisSetosa <- iris
irisSetosa$setosa <- as.numeric(irisSetosa$Species == "versicolor")
irisSetosa <- irisSetosa[!(colnames(irisSetosa) %in% "Species")]
rpart(setosa~., irisSetosa)
