library(qcc)

### In diesem File gilt die Unabghaengigkeitsannahme. Dementsprechend sind die
### Simulationen aufgebaut. Die Untersuchung derselben Fragestellung im Falle
### abhaengiger Netzwerke ist in einem aehnlichen File mittels Markov-Chains zu
### finden.


### Case 1: Netzwerke mit fester Knotenzahl aber ploetzlichem CP ueber die Zeit

###-----------------------------------------------------------------------------

### Scenario 1: CP durch insgesamten Anstieg/Abfall der Beziehungshaeufigkeiten
###             im gesamten Netzwerk 
###             Simulation durch Uebergang zwischen zwei Erdoes-Renyi-Modellen

Case1Scenario1 <- function(Ti, n, p0, p1){
  inControl <- replicate(Ti/2, erdos.renyi.game(n, p.or.m  = p0), simplify = FALSE)
  outControl <- replicate(Ti/2, erdos.renyi.game(n, p.or.m = p1), simplify = FALSE)
  inAdj <- lapply(inControl, function(x) as.matrix(get.adjacency(x)))
  outAdj <- lapply(outControl, function(x) as.matrix(get.adjacency(x)))
  combinedAdj <- c(inAdj, outAdj)
  return(combinedAdj)
}

## Beispiele:

examp1 <- Case1Scenario1(Ti = 200, n = 100, p0 = 0.3, p1 = 0.4)
examp2 <- Case1Scenario1(Ti = 50, n = 20, p0 = 0.2, p1 = 0.25)
vals1 <- unlist(lapply(examp1, norm, type = "2"))
vals2 <- unlist(lapply(examp2, norm, type = "F"))

cusum(vals2[1:10], newdata = vals2[-(1:10)])
ewma(vals2[1:10], newdata = vals2[-(1:10)])


###-----------------------------------------------------------------------------

### Scenario 2: CP durch vereinzelten, starken Anstieg/Abfall der Beziehungs-
###             haeufigkeiten 
###             Simulation durch Uebergang von einem Erdoes-Renyi-Modellen
###             zum selben Modell mit veraenderten Wahrscheinlichkeiten
###             bei gewissen Links.


Case1Scenario2 <- function(Ti, n, p0, p1, portion){
  inControl <- replicate(Ti/2, erdos.renyi.game(n, p.or.m  = p0), simplify = FALSE)
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
  outAdj <- replicate(Ti/2, outControlSingle(n = n, p0 = p0, p1 = p1, portion = portion),
                      simplify = FALSE)
  combinedAdj <- c(inAdj, outAdj)
  return(combinedAdj)
}


## Beispiele:
examp1 <- Case1Scenario2(Ti = 200, n = 100, p0 = 0.3, p1 = 0.5, portion = 0.05)
vals1 <- unlist(lapply(examp1, norm, type = "2"))
vals2 <- unlist(lapply(examp1, norm, type = "F"))

cusum(vals2[1:25], newdata = vals2[-(1:25)])
ewma(vals2[1:25], newdata = vals2[-(1:25)])

###-----------------------------------------------------------------------------

### Scenario 3: CP durch grundlegenden Wechsel in der Beziehungsstruktur der 
###             der Einheiten (z.B. Wechsel von zentralen Einheiten etc.)

Case1Scenario3 <- function(Ti, n, p, pCent, nCent){
  sampleIn <- sample(1:n, size = nCent, replace = FALSE)
  sampleOut <- sample((1:n)[-sampleIn], size = nCent, replace = FALSE)
  createSingleMatrix <- function(n, p, pCent, nCent, whichAdj){
  probsIn <- matrix(p, nrow = n, ncol = n)
  probsOut <- matrix(p, nrow = n, ncol = n)
  probsIn[sampleIn,] <- pCent
  probsIn[,sampleIn] <- pCent
  probsOut[sampleOut,] <- pCent
  probsOut[,sampleOut] <- pCent
  upTrIn <- probsIn[upper.tri(probsIn)]
  upTrOut <- probsOut[upper.tri(probsOut)]
  valsIn <- sapply(upTrIn, function(x) sample(c(0,1), size = 1, prob = c(1 - x, x)))
  valsOut <- sapply(upTrOut, function(x) sample(c(0,1), size = 1, prob = c(1 - x, x)))
  AdjIn <- matrix(0, ncol = n, nrow = n)
  AdjIn[upper.tri(AdjIn)] <- valsIn
  diag(AdjIn) <- 0
  AdjIn[lower.tri(AdjIn)]  <- t(AdjIn)[lower.tri(AdjIn)]
  AdjOut <- matrix(0, ncol = n, nrow = n)
  AdjOut[upper.tri(AdjOut)] <- valsOut
  diag(AdjOut) <- 0
  AdjOut[lower.tri(AdjOut)]  <- t(AdjOut)[lower.tri(AdjOut)]
  if(whichAdj == "In"){
    return(AdjIn)
  }
  if(whichAdj == "Out"){
    return(AdjOut)
  }
  }
  inAdj <- replicate(Ti/2, createSingleMatrix(n = n, p = p, pCent = pCent, nCent = nCent, whichAdj = "In"),
                     simplify = FALSE)
  outAdj <- replicate(Ti/2, createSingleMatrix(n = n, p = p, pCent = pCent, nCent = nCent, whichAdj = "Out"),
                     simplify = FALSE)
  combinedAdj <- c(inAdj, outAdj)
  return(combinedAdj)
}

## Beispiele:
examp1 <- Case1Scenario3(Ti = 200, n = 100, p = 0.3, nCent = 5, pCent = 0.7)
vals1 <- unlist(lapply(examp1, norm, type = "2"))
vals2 <- unlist(lapply(examp1, norm, type = "F"))

cusum(vals1[1:25], newdata = vals1[-(1:25)])
ewma(vals1[1:25], newdata = vals1[-(1:25)])
cusum(vals2[1:25], newdata = vals2[-(1:25)])
ewma(vals2[1:25], newdata = vals2[-(1:25)])


