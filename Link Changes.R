library(qcc)
library(igraph)

##  Functions for generating dynamic network data for change types GLC and LLC

## Case 1: Link Changes

###-----------------------------------------------------------------------------

### Scenario 1: GLCs, CP due to increased/decreased communication in the whole network

## Input: Ti - number of simulated time points
##        CP - time point at which the change occurs
##        n - number of nodes
##        p0 - link probability in-control (Phase I)
##        p1 - link probability out-of-control (Phase II)
##        inC - if TRUE, the only in-Control networks are simulated

Case1Scenario1 <- function(Ti, CP, n, p0, p1, inC){
  if(inC == TRUE){
    inControl <- replicate(Ti, erdos.renyi.game(n, p.or.m  = p0), simplify = FALSE)
    inAdj <- lapply(inControl, function(x) as.matrix(get.adjacency(x)))
    return(inAdj)
  }
  
  if(inC == FALSE){
  inControl <- replicate(CP, erdos.renyi.game(n, p.or.m  = p0), simplify = FALSE)
  outControl <- replicate(Ti - CP, erdos.renyi.game(n, p.or.m = p1), simplify = FALSE)
  inAdj <- lapply(inControl, function(x) as.matrix(get.adjacency(x)))
  outAdj <- lapply(outControl, function(x) as.matrix(get.adjacency(x)))
  combinedAdj <- c(inAdj, outAdj)
  return(combinedAdj)
  }
}


###-----------------------------------------------------------------------------

### Scenario 2: LLCs
                   
## Input: Ti - number of simulated time points
##        CP - time point at which the change occurs
##        n - number of nodes
##        p0 - link probability in-control for all nodes(Phase I)
##        p1 - link probability out-of-control for some nodes that changed (Phase II)
##        portion - fraction of nodes that changed their link probability after the change (see also description in the paper)
##        inC - if TRUE, the only in-Control networks are simulated                   

Case1Scenario2 <- function(Ti, CP, n, p0, p1, portion, inC){
  if(inC == TRUE){
    inControl <- replicate(Ti, erdos.renyi.game(n, p.or.m  = p0), simplify = FALSE)
    inAdj <- lapply(inControl, function(x) as.matrix(get.adjacency(x)))
    return(inAdj)
  }
  
  if(inC == FALSE){
    inControl <- replicate(CP, erdos.renyi.game(n, p.or.m  = p0), simplify = FALSE)
    inAdj <- lapply(inControl, function(x) as.matrix(get.adjacency(x)))
    sampleOut <- sample(1:n, size = floor(n * portion), replace = FALSE)
    outControlSingle <- function(n, p0, p1, portion){
      outMat <- matrix(p0, nrow = n, ncol = n)
      outMat[sampleOut,] <- p1
      outMat[,sampleOut] <- p1
      upTr <- outMat[upper.tri(outMat)]
      valsOut <- sapply(upTr, function(x) sample(c(0,1), size = 1, prob = c(1 - x, x)))
      AdjOut <- matrix(0, ncol = n, nrow = n)
      AdjOut[upper.tri(AdjOut)] <- valsOut
      diag(AdjOut) <- 0
      AdjOut[lower.tri(AdjOut)]  <- t(AdjOut)[lower.tri(AdjOut)]
      return(AdjOut)
    }
    outAdj <- replicate(Ti - CP, outControlSingle(n = n, p0 = p0, p1 = p1, portion = portion),
                        simplify = FALSE)
    combinedAdj <- c(inAdj, outAdj)
    return(combinedAdj)
  }
}
