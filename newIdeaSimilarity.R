library(mclust)

Similarity <- function(NetAdj, train){
  n <- floor(length(NetAdj) * train)
  trainData <- Reduce("+", NetAdj[1:n])/n
  refAdj <- round(trainData)
  refTri <- refAdj[upper.tri(refAdj)]
  testAdj <- NetAdj
  testTri <- lapply(testAdj, function(x) x[upper.tri(x)])
  sim <- unlist(lapply(testTri, cluster_similarity, labels2 = refTri, similarity = "jaccard"))
  return(sim)
}


Similarity(ex111, 0.25)
