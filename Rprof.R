Rprof(tmp <- tempfile(), memory.profiling=T)
rBoostModel <- rBoost(y = y[trainInd], df = dfNumericTrain, 
                      rBoostConfig = list(objective = "binary:logistic", evalFunction = "rocAuc", 
                                          nTree = 1, eta = 0.1, colSampling = 0.6, rowSampling = 1,
                                          maxSplits = 3, minObsPerLeaf = 30, maxCandidatesPerFeature = 10, seed = 1)
                      )
Rprof()
summaryRprof(tmp, memory="both")

require(profr)
require(ggplot2)
library(proftools)
x = profr(rBoost(y = y[trainInd], df = dfNumericTrain, 
                 rBoostConfig = list(objective = "binary:logistic", evalFunction = "rocAuc", 
                                     nTree = 1, eta = 0.1, colSampling = 0.6, rowSampling = 1,
                                     maxSplits = 3, minObsPerLeaf = 30, maxCandidatesPerFeature = 10, seed = 1)
))
ggplot(x)
plotProfileCallGraph(readProfileData(tmp),
                     score = "total")
