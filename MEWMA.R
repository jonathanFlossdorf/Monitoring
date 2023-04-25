## Multivariate Procedure with the MEWMA chart
## Setup is as described in the paper and analgously to the Bootstrap file

library(qcc)

## requires: simulationStudy.R

my.Mewma <- function(data, train, lambda){
  Matr <- as.matrix(data)
  xMeans <- colMeans(Matr[1:train,])
  newx <- t(apply(Matr, 1, function(x) x - xMeans))
  z <- matrix(0, nrow = nrow(newx), ncol = ncol(newx))
  z[1,] <- lambda * newx[1,]
  for(i in 2:nrow(newx)){
    z[i,] <- lambda * newx[i,] + (1 - lambda) * z[i-1,]
  }
  globalCov <- cov(newx[1:train,])
  T2 <- numeric()
  for(i in 1:nrow(newx)){
    T2[i] <- z[i,] %*% solve((lambda/(2-lambda) * (1-(1-lambda)^(2*i)) * globalCov)) %*% z[i,]
  }
  plot(T2, type = "o")
  abline(h = 10.97, col = "red", lty = 2)
  return(list(statistics = T2, beyond.limits = which(T2 > 10.55))) #10.97
}

mvControlChart_MEWMA <- function(measure1, measure2, measure3, lambda = 0.1){
  df <- data.frame(measure1, measure2, measure3)
  q <- my.Mewma(df, train = 1000, lambda = lambda)
  MEWMALim <- q$beyond.limits
  falseAlarm <- MEWMALim[MEWMALim > 1000 & MEWMALim < 1051]
  falseAlarmRate <- length(falseAlarm)/50
  trueAlarm <- c(MEWMALim[MEWMALim >= 1051], 1400)
  ADL <- min(trueAlarm) - 1050
  return(list(fA = falseAlarmRate, ADL = ADL))
}

mvControlChart_MEWMA4 <- function(measure1, measure2, measure3, measure4, lambda = 0.1){
  df <- data.frame(measure1, measure2, measure3, measure4)
  q <- my.Mewma(df, train = 1000, lambda = lambda)
  MEWMALim <- q$beyond.limits
  falseAlarm <- MEWMALim[MEWMALim > 1000 & MEWMALim < 1051]
  falseAlarmRate <- length(falseAlarm)/50
  trueAlarm <- c(MEWMALim[MEWMALim >= 1051], 1400)
  ADL <- min(trueAlarm) - 1050
  return(list(fA = falseAlarmRate, ADL = ADL))
}


simStudy11_MEWMA <- function(measure1, measure2, measure3){
  return(list(low = lapply(Sim11, function(x) mvControlChart_MEWMA(x$Measure111[measure1],
                                                             x$Measure111[measure2],
                                                             x$Measure111[measure3])),
              medium = lapply(Sim11, function(x) mvControlChart_MEWMA(x$Measure112[measure1],
                                                                x$Measure112[measure2],
                                                                x$Measure112[measure3])),
              heavy = lapply(Sim11, function(x) mvControlChart_MEWMA(x$Measure113[measure1],
                                                               x$Measure113[measure2],
                                                               x$Measure113[measure3]))))
}

mvSBE11_MEWMA <- simStudy11_MEWMA("Spektral", "BetwCent", "EigenMean")
almvSBE11low_MEWMA <- falseAlarm(evaluation(mvSBE11_MEWMA, "low"))
almvSBE11medium_MEWMA <- falseAlarm(evaluation(mvSBE11_MEWMA,"medium"))
almvSBE11heavy_MEWMA <- falseAlarm(evaluation(mvSBE11_MEWMA, "heavy"))
ADLmvSBE11low_MEWMA <- ADL(evaluation(mvSBE11_MEWMA, "low"))
ADLmvSBE11medium_MEWMA <- ADL(evaluation(mvSBE11_MEWMA, "medium"))
ADLmvSBE11heavy_MEWMA <- ADL(evaluation(mvSBE11_MEWMA, "heavy"))

mvSDC11_MEWMA <- simStudy11_MEWMA("Spektral", "DegreeMean", "CloCent")
almvSDC11low_MEWMA <- falseAlarm(evaluation(mvSDC11_MEWMA, "low"))
almvSDC11medium_MEWMA <- falseAlarm(evaluation(mvSDC11_MEWMA,"medium"))
almvSDC11heavy_MEWMA <- falseAlarm(evaluation(mvSDC11_MEWMA, "heavy"))
ADLmvSDC11low_MEWMA <- ADL(evaluation(mvSDC11_MEWMA, "low"))
ADLmvSDC11medium_MEWMA <- ADL(evaluation(mvSDC11_MEWMA, "medium"))
ADLmvSDC11heavy_MEWMA <- ADL(evaluation(mvSDC11_MEWMA, "heavy"))

mvBDS11_MEWMA <- simStudy11_MEWMA("BetwCent", "DegreeCent", "Spektral")
almvBDS11low_MEWMA <- falseAlarm(evaluation(mvBDS11_MEWMA, "low"))
almvBDS11medium_MEWMA <- falseAlarm(evaluation(mvBDS11_MEWMA,"medium"))
almvBDS11heavy_MEWMA <- falseAlarm(evaluation(mvBDS11_MEWMA, "heavy"))
ADLmvBDS11low_MEWMA <- ADL(evaluation(mvBDS11_MEWMA, "low"))
ADLmvBDS11medium_MEWMA <- ADL(evaluation(mvBDS11_MEWMA, "medium"))
ADLmvBDS11heavy_MEWMA <- ADL(evaluation(mvBDS11_MEWMA, "heavy"))

mvCDS11_MEWMA <- simStudy11_MEWMA("CloMean", "DegreeMean", "Spektral")
almvCDS11low_MEWMA <- falseAlarm(evaluation(mvCDS11_MEWMA, "low"))
almvCDS11medium_MEWMA <- falseAlarm(evaluation(mvCDS11_MEWMA,"medium"))
almvCDS11heavy_MEWMA <- falseAlarm(evaluation(mvCDS11_MEWMA, "heavy"))
ADLmvCDS11low_MEWMA <- ADL(evaluation(mvCDS11_MEWMA, "low"))
ADLmvCDS11medium_MEWMA <- ADL(evaluation(mvCDS11_MEWMA, "medium"))
ADLmvCDS11heavy_MEWMA <- ADL(evaluation(mvCDS11_MEWMA, "heavy"))

mvBCD11_MEWMA <- simStudy11_MEWMA("BetwCent", "CloCent", "DegreeCent")
almvBCD11low_MEWMA <- falseAlarm(evaluation(mvBCD11_MEWMA, "low"))
almvBCD11medium_MEWMA <- falseAlarm(evaluation(mvBCD11_MEWMA,"medium"))
almvBCD11heavy_MEWMA <- falseAlarm(evaluation(mvBCD11_MEWMA, "heavy"))
ADLmvBCD11low_MEWMA <- ADL(evaluation(mvBCD11_MEWMA, "low"))
ADLmvBCD11medium_MEWMA <- ADL(evaluation(mvBCD11_MEWMA, "medium"))
ADLmvBCD11heavy_MEWMA <- ADL(evaluation(mvBCD11_MEWMA, "heavy"))


mvFSB11_MEWMA <- simStudy11_MEWMA("Frobenius", "Spektral", "BetwCent")
almvFSB11low_MEWMA <- falseAlarm(evaluation(mvFSB11_MEWMA, "low"))
almvFSB11medium_MEWMA <- falseAlarm(evaluation(mvFSB11_MEWMA,"medium"))
almvFSB11heavy_MEWMA <- falseAlarm(evaluation(mvFSB11_MEWMA, "heavy"))
ADLmvFSB11low_MEWMA <- ADL(evaluation(mvFSB11_MEWMA, "low"))
ADLmvFSB11medium_MEWMA <- ADL(evaluation(mvFSB11_MEWMA, "medium"))
ADLmvFSB11heavy_MEWMA <- ADL(evaluation(mvFSB11_MEWMA, "heavy"))



falseAlarm11_MEWMA <- data.frame(SBE = c(almvSBE11low_MEWMA, almvSBE11medium_MEWMA, almvSBE11heavy_MEWMA),
                             SDC = c(almvSDC11low_MEWMA, almvSDC11medium_MEWMA, almvSDC11heavy_MEWMA),
                             BDS = c(almvBDS11low_MEWMA, almvBDS11medium_MEWMA, almvBDS11heavy_MEWMA),
                             CDS = c(almvCDS11low_MEWMA, almvCDS11medium_MEWMA, almvCDS11heavy_MEWMA),
                             BCD = c(almvBCD11low_MEWMA, almvBCD11medium_MEWMA, almvBCD11heavy_MEWMA),
                             FSB = c(almvFSB11low_MEWMA, almvFSB11medium_MEWMA, almvFSB11heavy_MEWMA))

ADL11_MEWMA <- data.frame(SBE = c(ADLmvSBE11low_MEWMA, ADLmvSBE11medium_MEWMA, ADLmvSBE11heavy_MEWMA),
                      SDC = c(ADLmvSDC11low_MEWMA, ADLmvSDC11medium_MEWMA, ADLmvSDC11heavy_MEWMA),
                      BDS = c(ADLmvBDS11low_MEWMA, ADLmvBDS11medium_MEWMA, ADLmvBDS11heavy_MEWMA),
                      CDS = c(ADLmvCDS11low_MEWMA, ADLmvCDS11medium_MEWMA, ADLmvCDS11heavy_MEWMA),
                      BCD = c(ADLmvBCD11low_MEWMA, ADLmvBCD11medium_MEWMA, ADLmvBCD11heavy_MEWMA),
                      FSB = c(ADLmvFSB11low_MEWMA, ADLmvFSB11medium_MEWMA, ADLmvFSB11heavy_MEWMA))

simStudy12_MEWMA <- function(measure1, measure2, measure3){
  return(list(low = lapply(Sim12, function(x) mvControlChart_MEWMA(x$Measure121[measure1],
                                                             x$Measure121[measure2],
                                                             x$Measure121[measure3])),
              medium = lapply(Sim12, function(x) mvControlChart_MEWMA(x$Measure122[measure1],
                                                                x$Measure122[measure2],
                                                                x$Measure122[measure3])),
              heavy = lapply(Sim12, function(x) mvControlChart_MEWMA(x$Measure123[measure1],
                                                               x$Measure123[measure2],
                                                               x$Measure123[measure3]))))
}

mvSBE12_MEWMA <- simStudy12_MEWMA("Spektral", "BetwCent", "EigenMean")
almvSBE12low_MEWMA <- falseAlarm(evaluation(mvSBE12_MEWMA, "low"))
almvSBE12medium_MEWMA <- falseAlarm(evaluation(mvSBE12_MEWMA,"medium"))
almvSBE12heavy_MEWMA <- falseAlarm(evaluation(mvSBE12_MEWMA, "heavy"))
ADLmvSBE12low_MEWMA <- ADL(evaluation(mvSBE12_MEWMA, "low"))
ADLmvSBE12medium_MEWMA <- ADL(evaluation(mvSBE12_MEWMA, "medium"))
ADLmvSBE12heavy_MEWMA <- ADL(evaluation(mvSBE12_MEWMA, "heavy"))

mvSDC12_MEWMA <- simStudy12_MEWMA("Spektral", "DegreeMean", "CloCent")
almvSDC12low_MEWMA <- falseAlarm(evaluation(mvSDC12_MEWMA, "low"))
almvSDC12medium_MEWMA <- falseAlarm(evaluation(mvSDC12_MEWMA,"medium"))
almvSDC12heavy_MEWMA <- falseAlarm(evaluation(mvSDC12_MEWMA, "heavy"))
ADLmvSDC12low_MEWMA <- ADL(evaluation(mvSDC12_MEWMA, "low"))
ADLmvSDC12medium_MEWMA <- ADL(evaluation(mvSDC12_MEWMA, "medium"))
ADLmvSDC12heavy_MEWMA <- ADL(evaluation(mvSDC12_MEWMA, "heavy"))

mvBDS12_MEWMA <- simStudy12_MEWMA("BetwCent", "DegreeCent", "Spektral")
almvBDS12low_MEWMA <- falseAlarm(evaluation(mvBDS12_MEWMA, "low"))
almvBDS12medium_MEWMA <- falseAlarm(evaluation(mvBDS12_MEWMA,"medium"))
almvBDS12heavy_MEWMA <- falseAlarm(evaluation(mvBDS12_MEWMA, "heavy"))
ADLmvBDS12low_MEWMA <- ADL(evaluation(mvBDS12_MEWMA, "low"))
ADLmvBDS12medium_MEWMA <- ADL(evaluation(mvBDS12_MEWMA, "medium"))
ADLmvBDS12heavy_MEWMA <- ADL(evaluation(mvBDS12_MEWMA, "heavy"))

mvCDS12_MEWMA <- simStudy12_MEWMA("CloMean", "DegreeMean", "Spektral")
almvCDS12low_MEWMA <- falseAlarm(evaluation(mvCDS12_MEWMA, "low"))
almvCDS12medium_MEWMA <- falseAlarm(evaluation(mvCDS12_MEWMA,"medium"))
almvCDS12heavy_MEWMA <- falseAlarm(evaluation(mvCDS12_MEWMA, "heavy"))
ADLmvCDS12low_MEWMA <- ADL(evaluation(mvCDS12_MEWMA, "low"))
ADLmvCDS12medium_MEWMA <- ADL(evaluation(mvCDS12_MEWMA, "medium"))
ADLmvCDS12heavy_MEWMA <- ADL(evaluation(mvCDS12_MEWMA, "heavy"))

mvBCD12_MEWMA <- simStudy12_MEWMA("BetwCent", "CloCent", "DegreeCent")
almvBCD12low_MEWMA <- falseAlarm(evaluation(mvBCD12_MEWMA, "low"))
almvBCD12medium_MEWMA <- falseAlarm(evaluation(mvBCD12_MEWMA,"medium"))
almvBCD12heavy_MEWMA <- falseAlarm(evaluation(mvBCD12_MEWMA, "heavy"))
ADLmvBCD12low_MEWMA <- ADL(evaluation(mvBCD12_MEWMA, "low"))
ADLmvBCD12medium_MEWMA <- ADL(evaluation(mvBCD12_MEWMA, "medium"))
ADLmvBCD12heavy_MEWMA <- ADL(evaluation(mvBCD12_MEWMA, "heavy"))


mvFSB12_MEWMA <- simStudy12_MEWMA("Frobenius", "Spektral", "BetwCent")
almvFSB12low_MEWMA <- falseAlarm(evaluation(mvFSB12_MEWMA, "low"))
almvFSB12medium_MEWMA <- falseAlarm(evaluation(mvFSB12_MEWMA,"medium"))
almvFSB12heavy_MEWMA <- falseAlarm(evaluation(mvFSB12_MEWMA, "heavy"))
ADLmvFSB12low_MEWMA <- ADL(evaluation(mvFSB12_MEWMA, "low"))
ADLmvFSB12medium_MEWMA <- ADL(evaluation(mvFSB12_MEWMA, "medium"))
ADLmvFSB12heavy_MEWMA <- ADL(evaluation(mvFSB12_MEWMA, "heavy"))



falseAlarm12_MEWMA <- data.frame(SBE = c(almvSBE12low_MEWMA, almvSBE12medium_MEWMA, almvSBE12heavy_MEWMA),
                             SDC = c(almvSDC12low_MEWMA, almvSDC12medium_MEWMA, almvSDC12heavy_MEWMA),
                             BDS = c(almvBDS12low_MEWMA, almvBDS12medium_MEWMA, almvBDS12heavy_MEWMA),
                             CDS = c(almvCDS12low_MEWMA, almvCDS12medium_MEWMA, almvCDS12heavy_MEWMA),
                             BCD = c(almvBCD12low_MEWMA, almvBCD12medium_MEWMA, almvBCD12heavy_MEWMA),
                             FSB = c(almvFSB12low_MEWMA, almvFSB12medium_MEWMA, almvFSB12heavy_MEWMA))

ADL12_MEWMA <- data.frame(SBE = c(ADLmvSBE12low_MEWMA, ADLmvSBE12medium_MEWMA, ADLmvSBE12heavy_MEWMA),
                      SDC = c(ADLmvSDC12low_MEWMA, ADLmvSDC12medium_MEWMA, ADLmvSDC12heavy_MEWMA),
                      BDS = c(ADLmvBDS12low_MEWMA, ADLmvBDS12medium_MEWMA, ADLmvBDS12heavy_MEWMA),
                      CDS = c(ADLmvCDS12low_MEWMA, ADLmvCDS12medium_MEWMA, ADLmvCDS12heavy_MEWMA),
                      BCD = c(ADLmvBCD12low_MEWMA, ADLmvBCD12medium_MEWMA, ADLmvBCD12heavy_MEWMA),
                      FSB = c(ADLmvFSB12low_MEWMA, ADLmvFSB12medium_MEWMA, ADLmvFSB12heavy_MEWMA))



simStudy21_MEWMA <- function(measure1, measure2, measure3){
  return(list(low = lapply(Sim21, function(x) mvControlChart_MEWMA(x$Measure211[measure1],
                                                             x$Measure211[measure2],
                                                             x$Measure211[measure3])),
              medium = lapply(Sim21, function(x) mvControlChart_MEWMA(x$Measure212[measure1],
                                                                x$Measure212[measure2],
                                                                x$Measure212[measure3])),
              heavy = lapply(Sim21, function(x) mvControlChart_MEWMA(x$Measure213[measure1],
                                                               x$Measure213[measure2],
                                                               x$Measure213[measure3]))))
}

mvSBE21_MEWMA <- simStudy21_MEWMA("Spektral", "BetwCent", "EigenMean")
almvSBE21low_MEWMA <- falseAlarm(evaluation(mvSBE21_MEWMA, "low"))
almvSBE21medium_MEWMA <- falseAlarm(evaluation(mvSBE21_MEWMA,"medium"))
almvSBE21heavy_MEWMA <- falseAlarm(evaluation(mvSBE21_MEWMA, "heavy"))
ADLmvSBE21low_MEWMA <- ADL(evaluation(mvSBE21_MEWMA, "low"))
ADLmvSBE21medium_MEWMA <- ADL(evaluation(mvSBE21_MEWMA, "medium"))
ADLmvSBE21heavy_MEWMA <- ADL(evaluation(mvSBE21_MEWMA, "heavy"))

mvSDC21_MEWMA <- simStudy21_MEWMA("Spektral", "DegreeMean", "CloCent")
almvSDC21low_MEWMA <- falseAlarm(evaluation(mvSDC21_MEWMA, "low"))
almvSDC21medium_MEWMA <- falseAlarm(evaluation(mvSDC21_MEWMA,"medium"))
almvSDC21heavy_MEWMA <- falseAlarm(evaluation(mvSDC21_MEWMA, "heavy"))
ADLmvSDC21low_MEWMA <- ADL(evaluation(mvSDC21_MEWMA, "low"))
ADLmvSDC21medium_MEWMA <- ADL(evaluation(mvSDC21_MEWMA, "medium"))
ADLmvSDC21heavy_MEWMA <- ADL(evaluation(mvSDC21_MEWMA, "heavy"))

mvBDS21_MEWMA <- simStudy21_MEWMA("BetwCent", "DegreeCent", "Spektral")
almvBDS21low_MEWMA <- falseAlarm(evaluation(mvBDS21_MEWMA, "low"))
almvBDS21medium_MEWMA <- falseAlarm(evaluation(mvBDS21_MEWMA,"medium"))
almvBDS21heavy_MEWMA <- falseAlarm(evaluation(mvBDS21_MEWMA, "heavy"))
ADLmvBDS21low_MEWMA <- ADL(evaluation(mvBDS21_MEWMA, "low"))
ADLmvBDS21medium_MEWMA <- ADL(evaluation(mvBDS21_MEWMA, "medium"))
ADLmvBDS21heavy_MEWMA <- ADL(evaluation(mvBDS21_MEWMA, "heavy"))

mvCDS21_MEWMA <- simStudy21_MEWMA("CloMean", "DegreeMean", "Spektral")
almvCDS21low_MEWMA <- falseAlarm(evaluation(mvCDS21_MEWMA, "low"))
almvCDS21medium_MEWMA <- falseAlarm(evaluation(mvCDS21_MEWMA,"medium"))
almvCDS21heavy_MEWMA <- falseAlarm(evaluation(mvCDS21_MEWMA, "heavy"))
ADLmvCDS21low_MEWMA <- ADL(evaluation(mvCDS21_MEWMA, "low"))
ADLmvCDS21medium_MEWMA <- ADL(evaluation(mvCDS21_MEWMA, "medium"))
ADLmvCDS21heavy_MEWMA <- ADL(evaluation(mvCDS21_MEWMA, "heavy"))

mvBCD21_MEWMA <- simStudy21_MEWMA("BetwCent", "CloCent", "DegreeCent")
almvBCD21low_MEWMA <- falseAlarm(evaluation(mvBCD21_MEWMA, "low"))
almvBCD21medium_MEWMA <- falseAlarm(evaluation(mvBCD21_MEWMA,"medium"))
almvBCD21heavy_MEWMA <- falseAlarm(evaluation(mvBCD21_MEWMA, "heavy"))
ADLmvBCD21low_MEWMA <- ADL(evaluation(mvBCD21_MEWMA, "low"))
ADLmvBCD21medium_MEWMA <- ADL(evaluation(mvBCD21_MEWMA, "medium"))
ADLmvBCD21heavy_MEWMA <- ADL(evaluation(mvBCD21_MEWMA, "heavy"))


mvFSB21_MEWMA <- simStudy21_MEWMA("Frobenius", "Spektral", "BetwCent")
almvFSB21low_MEWMA <- falseAlarm(evaluation(mvFSB21_MEWMA, "low"))
almvFSB21medium_MEWMA <- falseAlarm(evaluation(mvFSB21_MEWMA,"medium"))
almvFSB21heavy_MEWMA <- falseAlarm(evaluation(mvFSB21_MEWMA, "heavy"))
ADLmvFSB21low_MEWMA <- ADL(evaluation(mvFSB21_MEWMA, "low"))
ADLmvFSB21medium_MEWMA <- ADL(evaluation(mvFSB21_MEWMA, "medium"))
ADLmvFSB21heavy_MEWMA <- ADL(evaluation(mvFSB21_MEWMA, "heavy"))



falseAlarm21_MEWMA <- data.frame(SBE = c(almvSBE21low_MEWMA, almvSBE21medium_MEWMA, almvSBE21heavy_MEWMA),
                             SDC = c(almvSDC21low_MEWMA, almvSDC21medium_MEWMA, almvSDC21heavy_MEWMA),
                             BDS = c(almvBDS21low_MEWMA, almvBDS21medium_MEWMA, almvBDS21heavy_MEWMA),
                             CDS = c(almvCDS21low_MEWMA, almvCDS21medium_MEWMA, almvCDS21heavy_MEWMA),
                             BCD = c(almvBCD21low_MEWMA, almvBCD21medium_MEWMA, almvBCD21heavy_MEWMA),
                             FSB = c(almvFSB21low_MEWMA, almvFSB21medium_MEWMA, almvFSB21heavy_MEWMA))

ADL21_MEWMA <- data.frame(SBE = c(ADLmvSBE21low_MEWMA, ADLmvSBE21medium_MEWMA, ADLmvSBE21heavy_MEWMA),
                      SDC = c(ADLmvSDC21low_MEWMA, ADLmvSDC21medium_MEWMA, ADLmvSDC21heavy_MEWMA),
                      BDS = c(ADLmvBDS21low_MEWMA, ADLmvBDS21medium_MEWMA, ADLmvBDS21heavy_MEWMA),
                      CDS = c(ADLmvCDS21low_MEWMA, ADLmvCDS21medium_MEWMA, ADLmvCDS21heavy_MEWMA),
                      BCD = c(ADLmvBCD21low_MEWMA, ADLmvBCD21medium_MEWMA, ADLmvBCD21heavy_MEWMA),
                      FSB = c(ADLmvFSB21low_MEWMA, ADLmvFSB21medium_MEWMA, ADLmvFSB21heavy_MEWMA))



simStudy22_MEWMA <- function(measure1, measure2, measure3){
  return(list(low = lapply(Sim22, function(x) mvControlChart_MEWMA(x$Measure221[measure1],
                                                             x$Measure221[measure2],
                                                             x$Measure221[measure3])),
              medium = lapply(Sim22, function(x) mvControlChart_MEWMA(x$Measure222[measure1],
                                                                x$Measure222[measure2],
                                                                x$Measure222[measure3])),
              heavy = lapply(Sim22, function(x) mvControlChart_MEWMA(x$Measure223[measure1],
                                                               x$Measure223[measure2],
                                                               x$Measure223[measure3]))))
}

mvSBE22_MEWMA <- simStudy22_MEWMA("Spektral", "BetwCent", "EigenMean")
almvSBE22low_MEWMA <- falseAlarm(evaluation(mvSBE22_MEWMA, "low"))
almvSBE22medium_MEWMA <- falseAlarm(evaluation(mvSBE22_MEWMA,"medium"))
almvSBE22heavy_MEWMA <- falseAlarm(evaluation(mvSBE22_MEWMA, "heavy"))
ADLmvSBE22low_MEWMA <- ADL(evaluation(mvSBE22_MEWMA, "low"))
ADLmvSBE22medium_MEWMA <- ADL(evaluation(mvSBE22_MEWMA, "medium"))
ADLmvSBE22heavy_MEWMA <- ADL(evaluation(mvSBE22_MEWMA, "heavy"))

mvSDC22_MEWMA <- simStudy22_MEWMA("Spektral", "DegreeMean", "CloCent")
almvSDC22low_MEWMA <- falseAlarm(evaluation(mvSDC22_MEWMA, "low"))
almvSDC22medium_MEWMA <- falseAlarm(evaluation(mvSDC22_MEWMA,"medium"))
almvSDC22heavy_MEWMA <- falseAlarm(evaluation(mvSDC22_MEWMA, "heavy"))
ADLmvSDC22low_MEWMA <- ADL(evaluation(mvSDC22_MEWMA, "low"))
ADLmvSDC22medium_MEWMA <- ADL(evaluation(mvSDC22_MEWMA, "medium"))
ADLmvSDC22heavy_MEWMA <- ADL(evaluation(mvSDC22_MEWMA, "heavy"))

mvBDS22_MEWMA <- simStudy22_MEWMA("BetwCent", "DegreeCent", "Spektral")
almvBDS22low_MEWMA <- falseAlarm(evaluation(mvBDS22_MEWMA, "low"))
almvBDS22medium_MEWMA <- falseAlarm(evaluation(mvBDS22_MEWMA,"medium"))
almvBDS22heavy_MEWMA <- falseAlarm(evaluation(mvBDS22_MEWMA, "heavy"))
ADLmvBDS22low_MEWMA <- ADL(evaluation(mvBDS22_MEWMA, "low"))
ADLmvBDS22medium_MEWMA <- ADL(evaluation(mvBDS22_MEWMA, "medium"))
ADLmvBDS22heavy_MEWMA <- ADL(evaluation(mvBDS22_MEWMA, "heavy"))

mvCDS22_MEWMA <- simStudy22_MEWMA("CloMean", "DegreeMean", "Spektral")
almvCDS22low_MEWMA <- falseAlarm(evaluation(mvCDS22_MEWMA, "low"))
almvCDS22medium_MEWMA <- falseAlarm(evaluation(mvCDS22_MEWMA,"medium"))
almvCDS22heavy_MEWMA <- falseAlarm(evaluation(mvCDS22_MEWMA, "heavy"))
ADLmvCDS22low_MEWMA <- ADL(evaluation(mvCDS22_MEWMA, "low"))
ADLmvCDS22medium_MEWMA <- ADL(evaluation(mvCDS22_MEWMA, "medium"))
ADLmvCDS22heavy_MEWMA <- ADL(evaluation(mvCDS22_MEWMA, "heavy"))

mvBCD22_MEWMA <- simStudy22_MEWMA("BetwCent", "CloCent", "DegreeCent")
almvBCD22low_MEWMA <- falseAlarm(evaluation(mvBCD22_MEWMA, "low"))
almvBCD22medium_MEWMA <- falseAlarm(evaluation(mvBCD22_MEWMA,"medium"))
almvBCD22heavy_MEWMA <- falseAlarm(evaluation(mvBCD22_MEWMA, "heavy"))
ADLmvBCD22low_MEWMA <- ADL(evaluation(mvBCD22_MEWMA, "low"))
ADLmvBCD22medium_MEWMA <- ADL(evaluation(mvBCD22_MEWMA, "medium"))
ADLmvBCD22heavy_MEWMA <- ADL(evaluation(mvBCD22_MEWMA, "heavy"))


mvFSB22_MEWMA <- simStudy22_MEWMA("Frobenius", "Spektral", "BetwCent")
almvFSB22low_MEWMA <- falseAlarm(evaluation(mvFSB22_MEWMA, "low"))
almvFSB22medium_MEWMA <- falseAlarm(evaluation(mvFSB22_MEWMA,"medium"))
almvFSB22heavy_MEWMA <- falseAlarm(evaluation(mvFSB22_MEWMA, "heavy"))
ADLmvFSB22low_MEWMA <- ADL(evaluation(mvFSB22_MEWMA, "low"))
ADLmvFSB22medium_MEWMA <- ADL(evaluation(mvFSB22_MEWMA, "medium"))
ADLmvFSB22heavy_MEWMA <- ADL(evaluation(mvFSB22_MEWMA, "heavy"))



falseAlarm22_MEWMA <- data.frame(SBE = c(almvSBE22low_MEWMA, almvSBE22medium_MEWMA, almvSBE22heavy_MEWMA),
                             SDC = c(almvSDC22low_MEWMA, almvSDC22medium_MEWMA, almvSDC22heavy_MEWMA),
                             BDS = c(almvBDS22low_MEWMA, almvBDS22medium_MEWMA, almvBDS22heavy_MEWMA),
                             CDS = c(almvCDS22low_MEWMA, almvCDS22medium_MEWMA, almvCDS22heavy_MEWMA),
                             BCD = c(almvBCD22low_MEWMA, almvBCD22medium_MEWMA, almvBCD22heavy_MEWMA),
                             FSB = c(almvFSB22low_MEWMA, almvFSB22medium_MEWMA, almvFSB22heavy_MEWMA))

ADL22_MEWMA <- data.frame(SBE = c(ADLmvSBE22low_MEWMA, ADLmvSBE22medium_MEWMA, ADLmvSBE22heavy_MEWMA),
                      SDC = c(ADLmvSDC22low_MEWMA, ADLmvSDC22medium_MEWMA, ADLmvSDC22heavy_MEWMA),
                      BDS = c(ADLmvBDS22low_MEWMA, ADLmvBDS22medium_MEWMA, ADLmvBDS22heavy_MEWMA),
                      CDS = c(ADLmvCDS22low_MEWMA, ADLmvCDS22medium_MEWMA, ADLmvCDS22heavy_MEWMA),
                      BCD = c(ADLmvBCD22low_MEWMA, ADLmvBCD22medium_MEWMA, ADLmvBCD22heavy_MEWMA),
                      FSB = c(ADLmvFSB22low_MEWMA, ADLmvFSB22medium_MEWMA, ADLmvFSB22heavy_MEWMA))


