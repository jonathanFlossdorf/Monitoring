## Multivariate Procedure with the Bootstrap Control Chart

## In this file: The simulations for the considered metric sets of the paper. 
## All other metric sets can be calculated as well by simply changing the input of the corresponding functions.

my.bootstrap <- function(q, m, B){
  bootSample <- replicate(B, sample(q, m, replace = TRUE))
  bootQuantiles <- apply(bootSample, 2, quantile, probs = 0.99, type = 1, na.rm = TRUE)
  return(mean(bootQuantiles))
}

## input of the three considered metrics
mvControlChart_BS <- function(measure1, measure2, measure3){
  n <- length(measure1)
  train <- 1000
  df <- data.frame(measure1, measure2, measure3)
  q <- mqcc(df[1:train,], type = "T2.single", 
            plot = FALSE)$statistics
  bootQuant <- my.bootstrap(q, 1000, 1000)
  Hotelling <- mqcc(df[1:train,], newdata = df[-(1:train),], type = "T2.single",
                    limits = c(0, bootQuant))
  HotellingLim <- Hotelling$violations$beyond.limits
  falseAlarm <- HotellingLim[HotellingLim < 1001]
  falseAlarmRate <- length(falseAlarm)/1000
  trueAlarm <- c(HotellingLim[HotellingLim >= 1051], 1400)
  ADL <- min(trueAlarm) - 1050
  return(list(fA = falseAlarmRate, ADL = ADL))
}

## alternatively with four involved metrics (e.g. for the set of Salmasnia et al (2020))
mvControlChart_BS4 <- function(measure1, measure2, measure3, measure4){
  n <- length(measure1)
  train <- 1000
  df <- data.frame(measure1, measure2, measure3, measure4)
  q <- mqcc(df[1:train,], type = "T2.single", 
            plot = FALSE)$statistics
  bootQuant <- my.bootstrap(q, 1000, 1000)
  Hotelling <- mqcc(df[1:train,], newdata = df[-(1:train),], type = "T2.single",
                    limits = c(0, bootQuant))
  HotellingLim <- Hotelling$violations$beyond.limits
  falseAlarm <- HotellingLim[HotellingLim < 1001]
  falseAlarmRate <- length(falseAlarm)/1000
  trueAlarm <- c(HotellingLim[HotellingLim >= 1051], 1400)
  ADL <- min(trueAlarm) - 1050
  return(list(fA = falseAlarmRate, ADL = ADL))
}


simStudy11_BS <- function(measure1, measure2, measure3){
  return(list(low = lapply(Sim11, function(x) mvControlChart_BS(x$Measure111[measure1],
                                                               x$Measure111[measure2],
                                                               x$Measure111[measure3])),
              medium = lapply(Sim11, function(x) mvControlChart_BS(x$Measure112[measure1],
                                                                  x$Measure112[measure2],
                                                                  x$Measure112[measure3])),
              heavy = lapply(Sim11, function(x) mvControlChart_BS(x$Measure113[measure1],
                                                                 x$Measure113[measure2],
                                                                 x$Measure113[measure3]))))
}

## SBE set
mvSBE11_BS <- simStudy11_BS("Spektral", "BetwCent", "EigenMean")
almvSBE11low_BS <- falseAlarm(evaluation(mvSBE11_BS, "low"))
almvSBE11medium_BS <- falseAlarm(evaluation(mvSBE11_BS,"medium"))
almvSBE11heavy_BS <- falseAlarm(evaluation(mvSBE11_BS, "heavy"))
ADLmvSBE11low_BS <- ADL(evaluation(mvSBE11_BS, "low"))
ADLmvSBE11medium_BS <- ADL(evaluation(mvSBE11_BS, "medium"))
ADLmvSBE11heavy_BS <- ADL(evaluation(mvSBE11_BS, "heavy"))

## SDC set
mvSDC11_BS <- simStudy11_BS("Spektral", "DegreeMean", "CloCent")
almvSDC11low_BS <- falseAlarm(evaluation(mvSDC11_BS, "low"))
almvSDC11medium_BS <- falseAlarm(evaluation(mvSDC11_BS,"medium"))
almvSDC11heavy_BS <- falseAlarm(evaluation(mvSDC11_BS, "heavy"))
ADLmvSDC11low_BS <- ADL(evaluation(mvSDC11_BS, "low"))
ADLmvSDC11medium_BS <- ADL(evaluation(mvSDC11_BS, "medium"))
ADLmvSDC11heavy_BS <- ADL(evaluation(mvSDC11_BS, "heavy"))

## BDS set                             
mvBDS11_BS <- simStudy11_BS("BetwCent", "DegreeCent", "Spektral")
almvBDS11low_BS <- falseAlarm(evaluation(mvBDS11_BS, "low"))
almvBDS11medium_BS <- falseAlarm(evaluation(mvBDS11_BS,"medium"))
almvBDS11heavy_BS <- falseAlarm(evaluation(mvBDS11_BS, "heavy"))
ADLmvBDS11low_BS <- ADL(evaluation(mvBDS11_BS, "low"))
ADLmvBDS11medium_BS <- ADL(evaluation(mvBDS11_BS, "medium"))
ADLmvBDS11heavy_BS <- ADL(evaluation(mvBDS11_BS, "heavy"))

## CDS set                             
mvCDS11_BS <- simStudy11_BS("CloMean", "DegreeMean", "Spektral")
almvCDS11low_BS <- falseAlarm(evaluation(mvCDS11_BS, "low"))
almvCDS11medium_BS <- falseAlarm(evaluation(mvCDS11_BS,"medium"))
almvCDS11heavy_BS <- falseAlarm(evaluation(mvCDS11_BS, "heavy"))
ADLmvCDS11low_BS <- ADL(evaluation(mvCDS11_BS, "low"))
ADLmvCDS11medium_BS <- ADL(evaluation(mvCDS11_BS, "medium"))
ADLmvCDS11heavy_BS <- ADL(evaluation(mvCDS11_BS, "heavy"))

## BCD set                             
mvBCD11_BS <- simStudy11_BS("BetwCent", "CloCent", "DegreeCent")
almvBCD11low_BS <- falseAlarm(evaluation(mvBCD11_BS, "low"))
almvBCD11medium_BS <- falseAlarm(evaluation(mvBCD11_BS,"medium"))
almvBCD11heavy_BS <- falseAlarm(evaluation(mvBCD11_BS, "heavy"))
ADLmvBCD11low_BS <- ADL(evaluation(mvBCD11_BS, "low"))
ADLmvBCD11medium_BS <- ADL(evaluation(mvBCD11_BS, "medium"))
ADLmvBCD11heavy_BS <- ADL(evaluation(mvBCD11_BS, "heavy"))

## FSB set
mvFSB11_BS <- simStudy11_BS("Frobenius", "Spektral", "BetwCent")
almvFSB11low_BS <- falseAlarm(evaluation(mvFSB11_BS, "low"))
almvFSB11medium_BS <- falseAlarm(evaluation(mvFSB11_BS,"medium"))
almvFSB11heavy_BS <- falseAlarm(evaluation(mvFSB11_BS, "heavy"))
ADLmvFSB11low_BS <- ADL(evaluation(mvFSB11_BS, "low"))
ADLmvFSB11medium_BS <- ADL(evaluation(mvFSB11_BS, "medium"))
ADLmvFSB11heavy_BS <- ADL(evaluation(mvFSB11_BS, "heavy"))



falseAlarm11_BS <- data.frame(SBE = c(almvSBE11low_BS, almvSBE11medium_BS, almvSBE11heavy_BS),
                             SDC = c(almvSDC11low_BS, almvSDC11medium_BS, almvSDC11heavy_BS),
                             BDS = c(almvBDS11low_BS, almvBDS11medium_BS, almvBDS11heavy_BS),
                             CDS = c(almvCDS11low_BS, almvCDS11medium_BS, almvCDS11heavy_BS),
                             BCD = c(almvBCD11low_BS, almvBCD11medium_BS, almvBCD11heavy_BS),
                             FSB = c(almvFSB11low_BS, almvFSB11medium_BS, almvFSB11heavy_BS))

ADL11_BS <- data.frame(SBE = c(ADLmvSBE11low_BS, ADLmvSBE11medium_BS, ADLmvSBE11heavy_BS),
                      SDC = c(ADLmvSDC11low_BS, ADLmvSDC11medium_BS, ADLmvSDC11heavy_BS),
                      BDS = c(ADLmvBDS11low_BS, ADLmvBDS11medium_BS, ADLmvBDS11heavy_BS),
                      CDS = c(ADLmvCDS11low_BS, ADLmvCDS11medium_BS, ADLmvCDS11heavy_BS),
                      BCD = c(ADLmvBCD11low_BS, ADLmvBCD11medium_BS, ADLmvBCD11heavy_BS),
                      FSB = c(ADLmvFSB11low_BS, ADLmvFSB11medium_BS, ADLmvFSB11heavy_BS))


## now for LLCs                             
simStudy12_BS <- function(measure1, measure2, measure3){
  return(list(low = lapply(Sim12, function(x) mvControlChart_BS(x$Measure121[measure1],
                                                             x$Measure121[measure2],
                                                             x$Measure121[measure3])),
              medium = lapply(Sim12, function(x) mvControlChart_BS(x$Measure122[measure1],
                                                                x$Measure122[measure2],
                                                                x$Measure122[measure3])),
              heavy = lapply(Sim12, function(x) mvControlChart_BS(x$Measure123[measure1],
                                                               x$Measure123[measure2],
                                                               x$Measure123[measure3]))))
}

mvSBE12_BS <- simStudy12_BS("Spektral", "BetwCent", "EigenMean")
almvSBE12low_BS <- falseAlarm(evaluation(mvSBE12_BS, "low"))
almvSBE12medium_BS <- falseAlarm(evaluation(mvSBE12_BS,"medium"))
almvSBE12heavy_BS <- falseAlarm(evaluation(mvSBE12_BS, "heavy"))
ADLmvSBE12low_BS <- ADL(evaluation(mvSBE12_BS, "low"))
ADLmvSBE12medium_BS <- ADL(evaluation(mvSBE12_BS, "medium"))
ADLmvSBE12heavy_BS <- ADL(evaluation(mvSBE12_BS, "heavy"))

mvSDC12_BS <- simStudy12_BS("Spektral", "DegreeMean", "CloCent")
almvSDC12low_BS <- falseAlarm(evaluation(mvSDC12_BS, "low"))
almvSDC12medium_BS <- falseAlarm(evaluation(mvSDC12_BS,"medium"))
almvSDC12heavy_BS <- falseAlarm(evaluation(mvSDC12_BS, "heavy"))
ADLmvSDC12low_BS <- ADL(evaluation(mvSDC12_BS, "low"))
ADLmvSDC12medium_BS <- ADL(evaluation(mvSDC12_BS, "medium"))
ADLmvSDC12heavy_BS <- ADL(evaluation(mvSDC12_BS, "heavy"))

mvBDS12_BS <- simStudy12_BS("BetwCent", "DegreeCent", "Spektral")
almvBDS12low_BS <- falseAlarm(evaluation(mvBDS12_BS, "low"))
almvBDS12medium_BS <- falseAlarm(evaluation(mvBDS12_BS,"medium"))
almvBDS12heavy_BS <- falseAlarm(evaluation(mvBDS12_BS, "heavy"))
ADLmvBDS12low_BS <- ADL(evaluation(mvBDS12_BS, "low"))
ADLmvBDS12medium_BS <- ADL(evaluation(mvBDS12_BS, "medium"))
ADLmvBDS12heavy_BS <- ADL(evaluation(mvBDS12_BS, "heavy"))

mvCDS12_BS <- simStudy12_BS("CloMean", "DegreeMean", "Spektral")
almvCDS12low_BS <- falseAlarm(evaluation(mvCDS12_BS, "low"))
almvCDS12medium_BS <- falseAlarm(evaluation(mvCDS12_BS,"medium"))
almvCDS12heavy_BS <- falseAlarm(evaluation(mvCDS12_BS, "heavy"))
ADLmvCDS12low_BS <- ADL(evaluation(mvCDS12_BS, "low"))
ADLmvCDS12medium_BS <- ADL(evaluation(mvCDS12_BS, "medium"))
ADLmvCDS12heavy_BS <- ADL(evaluation(mvCDS12_BS, "heavy"))

mvBCD12_BS <- simStudy12_BS("BetwCent", "CloCent", "DegreeCent")
almvBCD12low_BS <- falseAlarm(evaluation(mvBCD12_BS, "low"))
almvBCD12medium_BS <- falseAlarm(evaluation(mvBCD12_BS,"medium"))
almvBCD12heavy_BS <- falseAlarm(evaluation(mvBCD12_BS, "heavy"))
ADLmvBCD12low_BS <- ADL(evaluation(mvBCD12_BS, "low"))
ADLmvBCD12medium_BS <- ADL(evaluation(mvBCD12_BS, "medium"))
ADLmvBCD12heavy_BS <- ADL(evaluation(mvBCD12_BS, "heavy"))


mvFSB12_BS <- simStudy12_BS("Frobenius", "Spektral", "BetwCent")
almvFSB12low_BS <- falseAlarm(evaluation(mvFSB12_BS, "low"))
almvFSB12medium_BS <- falseAlarm(evaluation(mvFSB12_BS,"medium"))
almvFSB12heavy_BS <- falseAlarm(evaluation(mvFSB12_BS, "heavy"))
ADLmvFSB12low_BS <- ADL(evaluation(mvFSB12_BS, "low"))
ADLmvFSB12medium_BS <- ADL(evaluation(mvFSB12_BS, "medium"))
ADLmvFSB12heavy_BS <- ADL(evaluation(mvFSB12_BS, "heavy"))



falseAlarm12_BS <- data.frame(SBE = c(almvSBE12low_BS, almvSBE12medium_BS, almvSBE12heavy_BS),
                             SDC = c(almvSDC12low_BS, almvSDC12medium_BS, almvSDC12heavy_BS),
                             BDS = c(almvBDS12low_BS, almvBDS12medium_BS, almvBDS12heavy_BS),
                             CDS = c(almvCDS12low_BS, almvCDS12medium_BS, almvCDS12heavy_BS),
                             BCD = c(almvBCD12low_BS, almvBCD12medium_BS, almvBCD12heavy_BS),
                             FSB = c(almvFSB12low_BS, almvFSB12medium_BS, almvFSB12heavy_BS))

ADL12_BS <- data.frame(SBE = c(ADLmvSBE12low_BS, ADLmvSBE12medium_BS, ADLmvSBE12heavy_BS),
                      SDC = c(ADLmvSDC12low_BS, ADLmvSDC12medium_BS, ADLmvSDC12heavy_BS),
                      BDS = c(ADLmvBDS12low_BS, ADLmvBDS12medium_BS, ADLmvBDS12heavy_BS),
                      CDS = c(ADLmvCDS12low_BS, ADLmvCDS12medium_BS, ADLmvCDS12heavy_BS),
                      BCD = c(ADLmvBCD12low_BS, ADLmvBCD12medium_BS, ADLmvBCD12heavy_BS),
                      FSB = c(ADLmvFSB12low_BS, ADLmvFSB12medium_BS, ADLmvFSB12heavy_BS))


## now for GNCs
simStudy21_BS <- function(measure1, measure2, measure3){
  return(list(low = lapply(Sim21, function(x) mvControlChart_BS(x$Measure211[measure1],
                                                               x$Measure211[measure2],
                                                               x$Measure211[measure3])),
              medium = lapply(Sim21, function(x) mvControlChart_BS(x$Measure212[measure1],
                                                                  x$Measure212[measure2],
                                                                  x$Measure212[measure3])),
              heavy = lapply(Sim21, function(x) mvControlChart_BS(x$Measure213[measure1],
                                                                 x$Measure213[measure2],
                                                                 x$Measure213[measure3]))))
}

mvSBE21_BS <- simStudy21_BS("Spektral", "BetwCent", "EigenMean")
almvSBE21low_BS <- falseAlarm(evaluation(mvSBE21_BS, "low"))
almvSBE21medium_BS <- falseAlarm(evaluation(mvSBE21_BS,"medium"))
almvSBE21heavy_BS <- falseAlarm(evaluation(mvSBE21_BS, "heavy"))
ADLmvSBE21low_BS <- ADL(evaluation(mvSBE21_BS, "low"))
ADLmvSBE21medium_BS <- ADL(evaluation(mvSBE21_BS, "medium"))
ADLmvSBE21heavy_BS <- ADL(evaluation(mvSBE21_BS, "heavy"))

mvSDC21_BS <- simStudy21_BS("Spektral", "DegreeMean", "CloCent")
almvSDC21low_BS <- falseAlarm(evaluation(mvSDC21_BS, "low"))
almvSDC21medium_BS <- falseAlarm(evaluation(mvSDC21_BS,"medium"))
almvSDC21heavy_BS <- falseAlarm(evaluation(mvSDC21_BS, "heavy"))
ADLmvSDC21low_BS <- ADL(evaluation(mvSDC21_BS, "low"))
ADLmvSDC21medium_BS <- ADL(evaluation(mvSDC21_BS, "medium"))
ADLmvSDC21heavy_BS <- ADL(evaluation(mvSDC21_BS, "heavy"))

mvBDS21_BS <- simStudy21_BS("BetwCent", "DegreeCent", "Spektral")
almvBDS21low_BS <- falseAlarm(evaluation(mvBDS21_BS, "low"))
almvBDS21medium_BS <- falseAlarm(evaluation(mvBDS21_BS,"medium"))
almvBDS21heavy_BS <- falseAlarm(evaluation(mvBDS21_BS, "heavy"))
ADLmvBDS21low_BS <- ADL(evaluation(mvBDS21_BS, "low"))
ADLmvBDS21medium_BS <- ADL(evaluation(mvBDS21_BS, "medium"))
ADLmvBDS21heavy_BS <- ADL(evaluation(mvBDS21_BS, "heavy"))

mvCDS21_BS <- simStudy21_BS("CloMean", "DegreeMean", "Spektral")
almvCDS21low_BS <- falseAlarm(evaluation(mvCDS21_BS, "low"))
almvCDS21medium_BS <- falseAlarm(evaluation(mvCDS21_BS,"medium"))
almvCDS21heavy_BS <- falseAlarm(evaluation(mvCDS21_BS, "heavy"))
ADLmvCDS21low_BS <- ADL(evaluation(mvCDS21_BS, "low"))
ADLmvCDS21medium_BS <- ADL(evaluation(mvCDS21_BS, "medium"))
ADLmvCDS21heavy_BS <- ADL(evaluation(mvCDS21_BS, "heavy"))

mvBCD21_BS <- simStudy21_BS("BetwCent", "CloCent", "DegreeCent")
almvBCD21low_BS <- falseAlarm(evaluation(mvBCD21_BS, "low"))
almvBCD21medium_BS <- falseAlarm(evaluation(mvBCD21_BS,"medium"))
almvBCD21heavy_BS <- falseAlarm(evaluation(mvBCD21_BS, "heavy"))
ADLmvBCD21low_BS <- ADL(evaluation(mvBCD21_BS, "low"))
ADLmvBCD21medium_BS <- ADL(evaluation(mvBCD21_BS, "medium"))
ADLmvBCD21heavy_BS <- ADL(evaluation(mvBCD21_BS, "heavy"))


mvFSB21_BS <- simStudy21_BS("Frobenius", "Spektral", "BetwCent")
almvFSB21low_BS <- falseAlarm(evaluation(mvFSB21_BS, "low"))
almvFSB21medium_BS <- falseAlarm(evaluation(mvFSB21_BS,"medium"))
almvFSB21heavy_BS <- falseAlarm(evaluation(mvFSB21_BS, "heavy"))
ADLmvFSB21low_BS <- ADL(evaluation(mvFSB21_BS, "low"))
ADLmvFSB21medium_BS <- ADL(evaluation(mvFSB21_BS, "medium"))
ADLmvFSB21heavy_BS <- ADL(evaluation(mvFSB21_BS, "heavy"))



falseAlarm21_BS <- data.frame(SBE = c(almvSBE21low_BS, almvSBE21medium_BS, almvSBE21heavy_BS),
                             SDC = c(almvSDC21low_BS, almvSDC21medium_BS, almvSDC21heavy_BS),
                             BDS = c(almvBDS21low_BS, almvBDS21medium_BS, almvBDS21heavy_BS),
                             CDS = c(almvCDS21low_BS, almvCDS21medium_BS, almvCDS21heavy_BS),
                             BCD = c(almvBCD21low_BS, almvBCD21medium_BS, almvBCD21heavy_BS),
                             FSB = c(almvFSB21low_BS, almvFSB21medium_BS, almvFSB21heavy_BS))

ADL21_BS <- data.frame(SBE = c(ADLmvSBE21low_BS, ADLmvSBE21medium_BS, ADLmvSBE21heavy_BS),
                      SDC = c(ADLmvSDC21low_BS, ADLmvSDC21medium_BS, ADLmvSDC21heavy_BS),
                      BDS = c(ADLmvBDS21low_BS, ADLmvBDS21medium_BS, ADLmvBDS21heavy_BS),
                      CDS = c(ADLmvCDS21low_BS, ADLmvCDS21medium_BS, ADLmvCDS21heavy_BS),
                      BCD = c(ADLmvBCD21low_BS, ADLmvBCD21medium_BS, ADLmvBCD21heavy_BS),
                      FSB = c(ADLmvFSB21low_BS, ADLmvFSB21medium_BS, ADLmvFSB21heavy_BS))


## now for LNCs
simStudy22_BS <- function(measure1, measure2, measure3){
  return(list(low = lapply(Sim22, function(x) mvControlChart_BS(x$Measure221[measure1],
                                                               x$Measure221[measure2],
                                                               x$Measure221[measure3])),
              medium = lapply(Sim22, function(x) mvControlChart_BS(x$Measure222[measure1],
                                                                  x$Measure222[measure2],
                                                                  x$Measure222[measure3])),
              heavy = lapply(Sim22, function(x) mvControlChart_BS(x$Measure223[measure1],
                                                                 x$Measure223[measure2],
                                                                 x$Measure223[measure3]))))
}

mvSBE22_BS <- simStudy22_BS("Spektral", "BetwCent", "EigenMean")
almvSBE22low_BS <- falseAlarm(evaluation(mvSBE22_BS, "low"))
almvSBE22medium_BS <- falseAlarm(evaluation(mvSBE22_BS,"medium"))
almvSBE22heavy_BS <- falseAlarm(evaluation(mvSBE22_BS, "heavy"))
ADLmvSBE22low_BS <- ADL(evaluation(mvSBE22_BS, "low"))
ADLmvSBE22medium_BS <- ADL(evaluation(mvSBE22_BS, "medium"))
ADLmvSBE22heavy_BS <- ADL(evaluation(mvSBE22_BS, "heavy"))

mvSDC22_BS <- simStudy22_BS("Spektral", "DegreeMean", "CloCent")
almvSDC22low_BS <- falseAlarm(evaluation(mvSDC22_BS, "low"))
almvSDC22medium_BS <- falseAlarm(evaluation(mvSDC22_BS,"medium"))
almvSDC22heavy_BS <- falseAlarm(evaluation(mvSDC22_BS, "heavy"))
ADLmvSDC22low_BS <- ADL(evaluation(mvSDC22_BS, "low"))
ADLmvSDC22medium_BS <- ADL(evaluation(mvSDC22_BS, "medium"))
ADLmvSDC22heavy_BS <- ADL(evaluation(mvSDC22_BS, "heavy"))

mvBDS22_BS <- simStudy22_BS("BetwCent", "DegreeCent", "Spektral")
almvBDS22low_BS <- falseAlarm(evaluation(mvBDS22_BS, "low"))
almvBDS22medium_BS <- falseAlarm(evaluation(mvBDS22_BS,"medium"))
almvBDS22heavy_BS <- falseAlarm(evaluation(mvBDS22_BS, "heavy"))
ADLmvBDS22low_BS <- ADL(evaluation(mvBDS22_BS, "low"))
ADLmvBDS22medium_BS <- ADL(evaluation(mvBDS22_BS, "medium"))
ADLmvBDS22heavy_BS <- ADL(evaluation(mvBDS22_BS, "heavy"))

mvCDS22_BS <- simStudy22_BS("CloMean", "DegreeMean", "Spektral")
almvCDS22low_BS <- falseAlarm(evaluation(mvCDS22_BS, "low"))
almvCDS22medium_BS <- falseAlarm(evaluation(mvCDS22_BS,"medium"))
almvCDS22heavy_BS <- falseAlarm(evaluation(mvCDS22_BS, "heavy"))
ADLmvCDS22low_BS <- ADL(evaluation(mvCDS22_BS, "low"))
ADLmvCDS22medium_BS <- ADL(evaluation(mvCDS22_BS, "medium"))
ADLmvCDS22heavy_BS <- ADL(evaluation(mvCDS22_BS, "heavy"))

mvBCD22_BS <- simStudy22_BS("BetwCent", "CloCent", "DegreeCent")
almvBCD22low_BS <- falseAlarm(evaluation(mvBCD22_BS, "low"))
almvBCD22medium_BS <- falseAlarm(evaluation(mvBCD22_BS,"medium"))
almvBCD22heavy_BS <- falseAlarm(evaluation(mvBCD22_BS, "heavy"))
ADLmvBCD22low_BS <- ADL(evaluation(mvBCD22_BS, "low"))
ADLmvBCD22medium_BS <- ADL(evaluation(mvBCD22_BS, "medium"))
ADLmvBCD22heavy_BS <- ADL(evaluation(mvBCD22_BS, "heavy"))


mvFSB22_BS <- simStudy22_BS("Frobenius", "Spektral", "BetwCent")
almvFSB22low_BS <- falseAlarm(evaluation(mvFSB22_BS, "low"))
almvFSB22medium_BS <- falseAlarm(evaluation(mvFSB22_BS,"medium"))
almvFSB22heavy_BS <- falseAlarm(evaluation(mvFSB22_BS, "heavy"))
ADLmvFSB22low_BS <- ADL(evaluation(mvFSB22_BS, "low"))
ADLmvFSB22medium_BS <- ADL(evaluation(mvFSB22_BS, "medium"))
ADLmvFSB22heavy_BS <- ADL(evaluation(mvFSB22_BS, "heavy"))



falseAlarm22_BS <- data.frame(SBE = c(almvSBE22low_BS, almvSBE22medium_BS, almvSBE22heavy_BS),
                             SDC = c(almvSDC22low_BS, almvSDC22medium_BS, almvSDC22heavy_BS),
                             BDS = c(almvBDS22low_BS, almvBDS22medium_BS, almvBDS22heavy_BS),
                             CDS = c(almvCDS22low_BS, almvCDS22medium_BS, almvCDS22heavy_BS),
                             BCD = c(almvBCD22low_BS, almvBCD22medium_BS, almvBCD22heavy_BS),
                             FSB = c(almvFSB22low_BS, almvFSB22medium_BS, almvFSB22heavy_BS))

ADL22_BS <- data.frame(SBE = c(ADLmvSBE22low_BS, ADLmvSBE22medium_BS, ADLmvSBE22heavy_BS),
                      SDC = c(ADLmvSDC22low_BS, ADLmvSDC22medium_BS, ADLmvSDC22heavy_BS),
                      BDS = c(ADLmvBDS22low_BS, ADLmvBDS22medium_BS, ADLmvBDS22heavy_BS),
                      CDS = c(ADLmvCDS22low_BS, ADLmvCDS22medium_BS, ADLmvCDS22heavy_BS),
                      BCD = c(ADLmvBCD22low_BS, ADLmvBCD22medium_BS, ADLmvBCD22heavy_BS),
                      FSB = c(ADLmvFSB22low_BS, ADLmvFSB22medium_BS, ADLmvFSB22heavy_BS))



