library(qcc)

### In diesem File gilt die Unabghaengigkeitsannahme. Dementsprechend sind die
### Simulationen aufgebaut. Die Untersuchung derselben Fragestellung im Falle
### abhaengiger Netzwerke ist in einem aehnlichen File mittels Markov-Chains zu
### finden.


### Case 2: Hinzukommen neuer Einheiten verändert Netzwerke ueber die Zeit

###-----------------------------------------------------------------------------

### Scenario 1: CP durch insgesamten grossen Anstieg neuer Einheiten

Case2Scenario1 <- function(Ti, n0, n1, m, Dev){
  inControl <- replicate(Ti/2, 
                         erdos.renyi.game(sample(floor((n0 - n0 * Dev):(n0 + n0 * Dev)), size = 1), 
                                          p.or.m  = sample(floor((m - m * 0.05):(m + m * 0.05)), size = 1),
                                          type = "gnm"), simplify = FALSE)
  outControl <- replicate(Ti/2, 
                          erdos.renyi.game(sample(floor((n1 - n1 * Dev):(n1 + n1 * Dev)), size = 1), 
                                           p.or.m  = sample(floor((m - m * 0.05):(m + m * 0.05)), size = 1),
                                           type = "gnm"), simplify = FALSE)
  inAdj <- lapply(inControl, function(x) as.matrix(get.adjacency(x)))
  outAdj <- lapply(outControl, function(x) as.matrix(get.adjacency(x)))
  combinedAdj <- c(inAdj, outAdj)
  return(combinedAdj)
}


## Beispiele:

examp21 <- Case2Scenario1(Ti = 200, n0 = 100, n1 = 200, m = 1500, Dev = 0.05)
examp21b <- Case2Scenario1(Ti = 200, n0 = 100, n1 = 70, m = 2000, Dev = 0.05)
vals21 <- unlist(lapply(examp21, norm, type = "2"))
vals21b <- unlist(lapply(examp21, norm, type = "F"))

cusum(vals21[1:25], newdata = vals21[-(1:25)])
ewma(vals21[1:25], newdata = vals21[-(1:25)])

cusum(vals21b[1:25], newdata = vals21b[-(1:25)])
ewma(vals21b[1:25], newdata = vals21b[-(1:25)])


###-----------------------------------------------------------------------------

### Scenario 2: Hinzukommen weniger zentraler Einheiten, die viel Linkstruktur 
### beanspruchen, stellt CP dar

Case2Scenario2 <- function(Ti, n0, p, pNew, Dev, pCent, nCent){
  inControl <- replicate(Ti/2, 
                         erdos.renyi.game(sample(floor((n0 - n0 * Dev):(n0 + n0 * Dev)), size = 1), 
                                          p.or.m = p), simplify = FALSE)
  
  outControl <- function(n0, pNew, pCent, nCent){
    n <- sample(floor((n0 - n0 * Dev):(n0 + n0 * Dev)), size = 1)
    outMat <- matrix(pNew, nrow = n + nCent, ncol = n + nCent)
    centralInd <- sample(1:(n + nCent), size = nCent)
    outMat[centralInd,] <- pCent
    outMat[,centralInd] <- pCent
    upTr <- outMat[upper.tri(outMat)]
    vals <- sapply(upTr, function(x) sample(c(0,1), size = 1, prob = c(1 - x, x)))
    AdjMat <- matrix(0, ncol = n + nCent, nrow = n + nCent)
    AdjMat[upper.tri(AdjMat)] <- vals
    diag(AdjMat) <- 0
    AdjMat[lower.tri(AdjMat)]  <- t(AdjMat)[lower.tri(AdjMat)]
    return(AdjMat)
  }

  inAdj <- lapply(inControl, function(x) as.matrix(get.adjacency(x)))
  outAdj <- replicate(Ti/2, outControl(n0 = n0, pNew = pNew, pCent = pCent, nCent = nCent),
                      simplify = FALSE)
  combinedAdj <- c(inAdj, outAdj)
  return(combinedAdj)
}


## Beispiele:

examp22 <- Case2Scenario2(Ti = 200, n0 = 100, p = 0.3, Dev = 0.02, pCent = 0.6, nCent = 4, pNew = 0.25)
examp21b <- Case2Scenario1(Ti = 200, n0 = 100, n1 = 70, m = 2000, Dev = 0.05)
vals22 <- unlist(lapply(examp22, norm, type = "2"))
vals22b <- unlist(lapply(examp22, norm, type = "F"))

cusum(vals21[1:25], newdata = vals21[-(1:25)])
ewma(vals21[1:25], newdata = vals21[-(1:25)])

cusum(vals21b[1:25], newdata = vals21b[-(1:25)])
ewma(vals21b[1:25], newdata = vals21b[-(1:25)])
