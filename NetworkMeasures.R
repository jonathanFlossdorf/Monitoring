### In diesem File ist die Berechnung der Masszahlen zu finden. Eine 
### Systematistierung und staendig erweiterte Liste ist im Ueberblick der
### Studie im Word-Dokument zu finden.

CPMeasures <- function(dynMat){
  
  dynNet <- lapply(dynMat, graph_from_adjacency_matrix)
 
   ## Adjacency-based
  FrNorm <- unlist(lapply(dynMat, norm, type = "F"))
  twoNorm <- unlist(lapply(dynMat, norm, type = "2"))
  eigenVal <- unlist(lapply(dynMat, function(x) eigen(x)$value[1]))
  
  ## Graph-based
  BetwMean <- unlist(lapply(dynNet, function(x) mean(centr_betw(x, directed = FALSE)$res)))
  DegMean <- unlist(lapply(dynNet, function(x) mean(centr_degree(x)$res)))
  BetwCent <- unlist(lapply(dynNet, function(x) centr_betw(x, directed = FALSE)$centralization))
  DegCent <- unlist(lapply(dynNet, function(x) centr_degree(x)$centralization))
  eigenCent <- unlist(lapply(dynNet, function(x) centr_eigen(x)$centralization))
  ComponentNo <- unlist(lapply(dynNet, function(x) clusters(x)$no))
  ComponentMax <- unlist(lapply(dynNet, function(x) max(clusters(x)$csize)))
  ClusterCoeff <- unlist(lapply(dynNet, transitivity))
  Diameter <- unlist(lapply(dynNet, diameter))
  AvPathLength <- unlist(lapply(dynNet, average.path.length))
  
  
  ## New Ideas
  ni <- lapply(dynMat, nrow)
  if(length(unique(ni)) == 1){
  jaccard <- Similarity(dynMat, train = 0.25)
  }
  
  else{
    jaccard <- 0
  }
  return(list(Frobenius = FrNorm, Spektral = twoNorm, Eigenvalue = eigenVal,
              BetwMean = BetwMean, BetwCent = BetwCent, DegreeMean = DegMean, 
              DegreeCent = DegCent, EigenCent = eigenCent,
              ComponentNumber = ComponentNo, ComponentMax = ComponentMax,
              ClusterCoeff = ClusterCoeff, Diameter = Diameter, 
              AvPathLength = AvPathLength, Jaccard = jaccard))
}

CPMeasures(examp1)
