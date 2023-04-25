library(qcc)
library(igraph)

## GLCs and LLCs


### In diesem File gilt die Unabghaengigkeitsannahme. Dementsprechend sind die
### Simulationen aufgebaut. Die Untersuchung derselben Fragestellung im Falle
### abhaengiger Netzwerke ist in einem aehnlichen File mittels Markov-Chains zu
### finden.


### Case 1: Netzwerke mit fester Knotenzahl aber ploetzlichem CP ueber die Zeit

###-----------------------------------------------------------------------------

### Scenario 1: CP durch insgesamten Anstieg/Abfall der Beziehungshaeufigkeiten
###             im gesamten Netzwerk 
###             Simulation durch Uebergang zwischen zwei Erdoes-Renyi-Modellen

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

### Scenario 2: CP durch vereinzelten, starken Anstieg/Abfall der Beziehungs-
###             haeufigkeiten 
###             Simulation durch Uebergang von einem Erdoes-Renyi-Modellen
###             zum selben Modell mit veraenderten Wahrscheinlichkeiten
###             bei gewissen Links.


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
