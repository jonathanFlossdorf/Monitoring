### Case1 mit Abhängigkeitsstruktur durch Markov-Ketten

library(markovchain)

Case1_Markov <- function(n, Ti, p, pTrans0, pTrans01, pTrans10, pTrans1){
  Zustand <- c("0", "1")
  Transit <- rbind(c(pTrans0, pTrans01), c(pTrans10, pTrans1))
  Chain <- new("markovchain", states = Zustand, byrow = TRUE, transitionMatrix = Transit,
               name = "MVC")
  nLinks <- n^2/2 - n/2
  dynAdj1 <- list()
  dynAdj2 <- list()
  samps <- matrix(0, nrow = nLinks, ncol = Ti)
  for(i in 1:nLinks){
    t0 <- sample(c("0","1"), size = 1, prob = c(1-p, p))
    samps[i,] <- c(as.numeric(t0), as.numeric(rmarkovchain(n = Ti - 1, object = Chain, 
                                                           what = "matrix", t0 = t0)))
  }
  for(j in 1:Ti){
    A1 <- matrix(0, nrow = n, ncol = n)
    A.upper1 <- samps[,j]
    A1[upper.tri(A1)] <- A.upper1
    diag(A1) <- 0
    A1[lower.tri(A1)]  <- t(A1)[lower.tri(A1)]
    dynAdj1[[j]] <- A1
  }
  return(dynAdj1)
}


### Beispiel für Szenario 1:
inCont <- Case1_Markov(n = 100, Ti = 100, p = 0.3, pTrans0 = 0.79, pTrans01 = 0.21,
             pTrans10 = 0.5, pTrans1 = 0.5)
outCont <- Case1_Markov(n = 100, Ti = 100, p = 0.4, pTrans0 = 0.66, pTrans01 = 0.34,
                       pTrans10 = 0.5, pTrans1 = 0.5)
dynNet <- c(inCont, outCont)
vals1 <- unlist(lapply(dynNet, norm, type = "2"))
vals2 <- unlist(lapply(dynNet, norm, type = "F"))
qcc(vals1[1:25], type = "xbar.one", newdata = vals1[-(1:25)])
cusum(vals1[1:25], newdata = vals1[-(1:25)])
ewma(vals1[1:25], newdata = vals1[-(1:25)])
