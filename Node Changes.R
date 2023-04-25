library(qcc)

### GNCs and LNCs


### In diesem File gilt die Unabghaengigkeitsannahme. Dementsprechend sind die
### Simulationen aufgebaut. Die Untersuchung derselben Fragestellung im Falle
### abhaengiger Netzwerke ist in einem aehnlichen File mittels Markov-Chains zu
### finden.


### Case 2: Hinzukommen neuer Einheiten verändert Netzwerke ueber die Zeit

###-----------------------------------------------------------------------------

### Scenario 1: CP durch insgesamten grossen Anstieg neuer Einheiten

Case2Scenario1 <- function(Ti, CP, n0, n1, m, Dev){
  inControl <- replicate(CP, 
                         erdos.renyi.game(sample(floor((n0 - n0 * Dev):(n0 + n0 * Dev)), size = 1), 
                                          p.or.m  = sample(floor((m - m * 0.05):(m + m * 0.05)), size = 1),
                                          type = "gnm"), simplify = FALSE)
  outControl <- replicate(Ti - CP, 
                          erdos.renyi.game(sample(floor((n1 - n1 * Dev):(n1 + n1 * Dev)), size = 1), 
                                           p.or.m  = sample(floor((m - m * 0.05):(m + m * 0.05)), size = 1),
                                           type = "gnm"), simplify = FALSE)
  inAdj <- lapply(inControl, function(x) as.matrix(get.adjacency(x)))
  outAdj <- lapply(outControl, function(x) as.matrix(get.adjacency(x)))
  combinedAdj <- c(inAdj, outAdj)
  return(combinedAdj)
}



###-----------------------------------------------------------------------------

### Scenario 2: Hinzukommen weniger zentraler Einheiten, die viel Linkstruktur 
### beanspruchen, stellt CP dar

Case2Scenario2 <- function(Ti, CP, n0, p, pNew, Dev, pCent, nCent){
  inControl <- replicate(CP, 
                         erdos.renyi.game(sample(floor((n0 - n0 * Dev):(n0 + n0 * Dev)), size = 1), 
                                          p.or.m = p), simplify = FALSE)
  
  outControl <- function(n0, pCent, nCent){
    n <- sample(floor((n0 - n0 * Dev):(n0 + n0 * Dev)), size = 1)
    l <- choose(n, n-2) # link-amount normal
    m <- choose(n + nCent, (n + nCent - 2))
    mTilde <- sum((n + nCent - 1): n)
    pNew <- (l * p - mTilde * pCent)/(m - mTilde) 
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
  outAdj <- replicate(Ti - CP, outControl(n0 = n0, pCent = pCent, nCent = nCent),
                      simplify = FALSE)
  combinedAdj <- c(inAdj, outAdj)
  return(combinedAdj)
}
