library(qcc)
library(igraph)

##  Functions for generating dynamic network data for change types GNC and LNC

## Case 2: Node Changes

###-----------------------------------------------------------------------------

### Scenario 1: GNCs

## Input: Ti - number of simulated time points
##        CP - time point at which the change occurs
##        n0 - number of nodes in-control (Phase I)
##        n1 - number of nodes out-of-control (Phase II)
##        m - amount of links
##        Dev - variance factor, 0<= Dev <= 1, controls the potential deviations for the mean node amount n1 and n2 (see also Paper)

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

### Scenario 2: LNCs
                   
## Input: Ti - number of simulated time points
##        CP - time point at which the change occurs
##        n0 - number of nodes in-control (Phase I)
##        p - link probability in-control
##        pNew - link probability out-of-control
##        Dev - variance factor, 0<= Dev <= 1, controls the potential deviations for the mean node amount n1 and n2 (see also Paper)
##        pCent, nCent - see formula in the paper for the setup
                   
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
