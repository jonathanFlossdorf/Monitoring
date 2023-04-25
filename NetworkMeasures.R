## Calculation of all considered metrics (univariate)

library(igraph)

## Input: dynMat - time series of adjacency matrices of the dynamic network
## Output: Extracts the considered network metrics from the dynamic network

CPMeasures <- function(dynMat){
  
  dynNet <- lapply(dynMat, graph_from_adjacency_matrix, mode = "undirected")  
  ## Adjacency-based
  FrNorm <- unlist(lapply(dynMat, norm, type = "F"))
  twoNorm <- unlist(lapply(dynMat, norm, type = "2"))
  
  ## Graph-based
  CloMean <- unlist(lapply(dynNet, function(x) mean(centr_clo(x, mode = "all")$res)))
  CloCent <- unlist(lapply(dynNet, function(x) mean(centr_clo(x, mode = "all")$centralization)))
  BetwMean <- unlist(lapply(dynNet, function(x) mean(centr_betw(x, directed = FALSE)$res)))
  DegMean <- unlist(lapply(dynNet, function(x) mean(centr_degree(x)$res)))
  BetwCent <- unlist(lapply(dynNet, function(x) centr_betw(x, directed = FALSE)$centralization))
  DegCent <- unlist(lapply(dynNet, function(x) centr_degree(x)$centralization))
  eigenMean <- unlist(lapply(dynNet, function(x) mean(centr_eigen(x)$vector)))
  #ClusterCoef <- unlist(lapply(dynNet, transitivity))
  #AvPathLength <- unlist(lapply(dynNet, average.path.length))
  density <- unlist(lapply(dynNet, edge_density))
  
  return(list(Frobenius = FrNorm, Spektral = twoNorm, 
              CloMean = CloMean, CloCent = CloCent,
              BetwMean = BetwMean, BetwCent = BetwCent, DegreeMean = DegMean, 
              DegreeCent = DegCent, EigenMean = eigenMean,
              Density = density
              #ClusterCoef = ClusterCoef, AvPathLength = AvPathLength
              ))
}
