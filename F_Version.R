## Multivariate Procedure with the traditional parametric Hotellings T^2 chart
## Setup is as described in the paper and analgously to the Bootstrap file

library(qcc)

## requires: simulationStudy.R

mvControlChart_F <- function(measure1, measure2, measure3, train = 1000){
  n <- length(measure1)
  df <- data.frame(measure1, measure2, measure3)
  Hotelling <- mqcc(df[1:train,], newdata = df[-(1:train),], type = "T2.single",
                    confidence.level = 0.99)
  HotellingLim <- Hotelling$violations$beyond.limits
  falseAlarm <- HotellingLim[HotellingLim < 1001]
  falseAlarmRate <- length(falseAlarm)/1000
  trueAlarm <- c(HotellingLim[HotellingLim >= 1051], 1400)
  ADL <- min(trueAlarm) - 1050
  return(list(fA = falseAlarmRate, ADL = ADL))
}

mvControlChart_F4 <- function(measure1, measure2, measure3, measure4, train = 1000){
  df <- data.frame(measure1, measure2, measure3, measure4)
  Hotelling <- mqcc(df[1:train,], newdata = df[-(1:train),], type = "T2.single",
                    confidence.level = 0.99)
  HotellingLim <- Hotelling$violations$beyond.limits
  falseAlarm <- HotellingLim[HotellingLim > 1001 & HotellingLim < 1050]
  falseAlarmRate <- length(falseAlarm)/50
  trueAlarm <- c(HotellingLim[HotellingLim >= 1051], 1400)
  ADL <- min(trueAlarm) - 1050
  return(list(fA = falseAlarmRate, ADL = ADL))
}


simStudy11_F <- function(measure1, measure2, measure3){
  return(list(low = lapply(Sim11, function(x) mvControlChart_F(x$Measure111[measure1],
                                                             x$Measure111[measure2],
                                                             x$Measure111[measure3])),
              medium = lapply(Sim11, function(x) mvControlChart_F(x$Measure112[measure1],
                                                                x$Measure112[measure2],
                                                                x$Measure112[measure3])),
              heavy = lapply(Sim11, function(x) mvControlChart_F(x$Measure113[measure1],
                                                               x$Measure113[measure2],
                                                               x$Measure113[measure3]))))
}

mvSBE11_F <- simStudy11_F("Spektral", "BetwCent", "EigenMean")
almvSBE11low_F <- falseAlarm(evaluation(mvSBE11_F, "low"))
almvSBE11medium_F <- falseAlarm(evaluation(mvSBE11_F,"medium"))
almvSBE11heavy_F <- falseAlarm(evaluation(mvSBE11_F, "heavy"))
ADLmvSBE11low_F <- ADL(evaluation(mvSBE11_F, "low"))
ADLmvSBE11medium_F <- ADL(evaluation(mvSBE11_F, "medium"))
ADLmvSBE11heavy_F <- ADL(evaluation(mvSBE11_F, "heavy"))

mvSDC11_F <- simStudy11_F("Spektral", "DegreeMean", "CloCent")
almvSDC11low_F <- falseAlarm(evaluation(mvSDC11_F, "low"))
almvSDC11medium_F <- falseAlarm(evaluation(mvSDC11_F,"medium"))
almvSDC11heavy_F <- falseAlarm(evaluation(mvSDC11_F, "heavy"))
ADLmvSDC11low_F <- ADL(evaluation(mvSDC11_F, "low"))
ADLmvSDC11medium_F <- ADL(evaluation(mvSDC11_F, "medium"))
ADLmvSDC11heavy_F <- ADL(evaluation(mvSDC11_F, "heavy"))

mvBDS11_F <- simStudy11_F("BetwCent", "DegreeCent", "Spektral")
almvBDS11low_F <- falseAlarm(evaluation(mvBDS11_F, "low"))
almvBDS11medium_F <- falseAlarm(evaluation(mvBDS11_F,"medium"))
almvBDS11heavy_F <- falseAlarm(evaluation(mvBDS11_F, "heavy"))
ADLmvBDS11low_F <- ADL(evaluation(mvBDS11_F, "low"))
ADLmvBDS11medium_F <- ADL(evaluation(mvBDS11_F, "medium"))
ADLmvBDS11heavy_F <- ADL(evaluation(mvBDS11_F, "heavy"))

mvCDS11_F <- simStudy11_F("CloMean", "DegreeMean", "Spektral")
almvCDS11low_F <- falseAlarm(evaluation(mvCDS11_F, "low"))
almvCDS11medium_F <- falseAlarm(evaluation(mvCDS11_F,"medium"))
almvCDS11heavy_F <- falseAlarm(evaluation(mvCDS11_F, "heavy"))
ADLmvCDS11low_F <- ADL(evaluation(mvCDS11_F, "low"))
ADLmvCDS11medium_F <- ADL(evaluation(mvCDS11_F, "medium"))
ADLmvCDS11heavy_F <- ADL(evaluation(mvCDS11_F, "heavy"))

mvBCD11_F <- simStudy11_F("BetwCent", "CloCent", "DegreeCent")
almvBCD11low_F <- falseAlarm(evaluation(mvBCD11_F, "low"))
almvBCD11medium_F <- falseAlarm(evaluation(mvBCD11_F,"medium"))
almvBCD11heavy_F <- falseAlarm(evaluation(mvBCD11_F, "heavy"))
ADLmvBCD11low_F <- ADL(evaluation(mvBCD11_F, "low"))
ADLmvBCD11medium_F <- ADL(evaluation(mvBCD11_F, "medium"))
ADLmvBCD11heavy_F <- ADL(evaluation(mvBCD11_F, "heavy"))


mvFSB11_F <- simStudy11_F("Frobenius", "Spektral", "BetwCent")
almvFSB11low_F <- falseAlarm(evaluation(mvFSB11_F, "low"))
almvFSB11medium_F <- falseAlarm(evaluation(mvFSB11_F,"medium"))
almvFSB11heavy_F <- falseAlarm(evaluation(mvFSB11_F, "heavy"))
ADLmvFSB11low_F <- ADL(evaluation(mvFSB11_F, "low"))
ADLmvFSB11medium_F <- ADL(evaluation(mvFSB11_F, "medium"))
ADLmvFSB11heavy_F <- ADL(evaluation(mvFSB11_F, "heavy"))



falseAlarm11_F <- data.frame(SBE = c(almvSBE11low_F, almvSBE11medium_F, almvSBE11heavy_F),
                           SDC = c(almvSDC11low_F, almvSDC11medium_F, almvSDC11heavy_F),
                           BDS = c(almvBDS11low_F, almvBDS11medium_F, almvBDS11heavy_F),
                           CDS = c(almvCDS11low_F, almvCDS11medium_F, almvCDS11heavy_F),
                           BCD = c(almvBCD11low_F, almvBCD11medium_F, almvBCD11heavy_F),
                           FSB = c(almvFSB11low_F, almvFSB11medium_F, almvFSB11heavy_F))

ADL11_F <- data.frame(SBE = c(ADLmvSBE11low_F, ADLmvSBE11medium_F, ADLmvSBE11heavy_F),
                    SDC = c(ADLmvSDC11low_F, ADLmvSDC11medium_F, ADLmvSDC11heavy_F),
                    BDS = c(ADLmvBDS11low_F, ADLmvBDS11medium_F, ADLmvBDS11heavy_F),
                    CDS = c(ADLmvCDS11low_F, ADLmvCDS11medium_F, ADLmvCDS11heavy_F),
                    BCD = c(ADLmvBCD11low_F, ADLmvBCD11medium_F, ADLmvBCD11heavy_F),
                    FSB = c(ADLmvFSB11low_F, ADLmvFSB11medium_F, ADLmvFSB11heavy_F))


simStudy12_F <- function(measure1, measure2, measure3){
  return(list(low = lapply(Sim12, function(x) mvControlChart_F(x$Measure121[measure1],
                                                             x$Measure121[measure2],
                                                             x$Measure121[measure3])),
              medium = lapply(Sim12, function(x) mvControlChart_F(x$Measure122[measure1],
                                                                x$Measure122[measure2],
                                                                x$Measure122[measure3])),
              heavy = lapply(Sim12, function(x) mvControlChart_F(x$Measure123[measure1],
                                                               x$Measure123[measure2],
                                                               x$Measure123[measure3]))))
}

mvSBE12_F <- simStudy12_F("Spektral", "BetwCent", "EigenMean")
almvSBE12low_F <- falseAlarm(evaluation(mvSBE12_F, "low"))
almvSBE12medium_F <- falseAlarm(evaluation(mvSBE12_F,"medium"))
almvSBE12heavy_F <- falseAlarm(evaluation(mvSBE12_F, "heavy"))
ADLmvSBE12low_F <- ADL(evaluation(mvSBE12_F, "low"))
ADLmvSBE12medium_F <- ADL(evaluation(mvSBE12_F, "medium"))
ADLmvSBE12heavy_F <- ADL(evaluation(mvSBE12_F, "heavy"))

mvSDC12_F <- simStudy12_F("Spektral", "DegreeMean", "CloCent")
almvSDC12low_F <- falseAlarm(evaluation(mvSDC12_F, "low"))
almvSDC12medium_F <- falseAlarm(evaluation(mvSDC12_F,"medium"))
almvSDC12heavy_F <- falseAlarm(evaluation(mvSDC12_F, "heavy"))
ADLmvSDC12low_F <- ADL(evaluation(mvSDC12_F, "low"))
ADLmvSDC12medium_F <- ADL(evaluation(mvSDC12_F, "medium"))
ADLmvSDC12heavy_F <- ADL(evaluation(mvSDC12_F, "heavy"))

mvBDS12_F <- simStudy12_F("BetwCent", "DegreeCent", "Spektral")
almvBDS12low_F <- falseAlarm(evaluation(mvBDS12_F, "low"))
almvBDS12medium_F <- falseAlarm(evaluation(mvBDS12_F,"medium"))
almvBDS12heavy_F <- falseAlarm(evaluation(mvBDS12_F, "heavy"))
ADLmvBDS12low_F <- ADL(evaluation(mvBDS12_F, "low"))
ADLmvBDS12medium_F <- ADL(evaluation(mvBDS12_F, "medium"))
ADLmvBDS12heavy_F <- ADL(evaluation(mvBDS12_F, "heavy"))

mvCDS12_F <- simStudy12_F("CloMean", "DegreeMean", "Spektral")
almvCDS12low_F <- falseAlarm(evaluation(mvCDS12_F, "low"))
almvCDS12medium_F <- falseAlarm(evaluation(mvCDS12_F,"medium"))
almvCDS12heavy_F <- falseAlarm(evaluation(mvCDS12_F, "heavy"))
ADLmvCDS12low_F <- ADL(evaluation(mvCDS12_F, "low"))
ADLmvCDS12medium_F <- ADL(evaluation(mvCDS12_F, "medium"))
ADLmvCDS12heavy_F <- ADL(evaluation(mvCDS12_F, "heavy"))

mvBCD12_F <- simStudy12_F("BetwCent", "CloCent", "DegreeCent")
almvBCD12low_F <- falseAlarm(evaluation(mvBCD12_F, "low"))
almvBCD12medium_F <- falseAlarm(evaluation(mvBCD12_F,"medium"))
almvBCD12heavy_F <- falseAlarm(evaluation(mvBCD12_F, "heavy"))
ADLmvBCD12low_F <- ADL(evaluation(mvBCD12_F, "low"))
ADLmvBCD12medium_F <- ADL(evaluation(mvBCD12_F, "medium"))
ADLmvBCD12heavy_F <- ADL(evaluation(mvBCD12_F, "heavy"))


mvFSB12_F <- simStudy12_F("Frobenius", "Spektral", "BetwCent")
almvFSB12low_F <- falseAlarm(evaluation(mvFSB12_F, "low"))
almvFSB12medium_F <- falseAlarm(evaluation(mvFSB12_F,"medium"))
almvFSB12heavy_F <- falseAlarm(evaluation(mvFSB12_F, "heavy"))
ADLmvFSB12low_F <- ADL(evaluation(mvFSB12_F, "low"))
ADLmvFSB12medium_F <- ADL(evaluation(mvFSB12_F, "medium"))
ADLmvFSB12heavy_F <- ADL(evaluation(mvFSB12_F, "heavy"))



falseAlarm12_F <- data.frame(SBE = c(almvSBE12low_F, almvSBE12medium_F, almvSBE12heavy_F),
                           SDC = c(almvSDC12low_F, almvSDC12medium_F, almvSDC12heavy_F),
                           BDS = c(almvBDS12low_F, almvBDS12medium_F, almvBDS12heavy_F),
                           CDS = c(almvCDS12low_F, almvCDS12medium_F, almvCDS12heavy_F),
                           BCD = c(almvBCD12low_F, almvBCD12medium_F, almvBCD12heavy_F),
                           FSB = c(almvFSB12low_F, almvFSB12medium_F, almvFSB12heavy_F),
                           SAL = c(existing12_F$fA[1], existing12_F$fA[2], existing12_F$fA[3]))

ADL12_F <- data.frame(SBE = c(ADLmvSBE12low_F, ADLmvSBE12medium_F, ADLmvSBE12heavy_F),
                    SDC = c(ADLmvSDC12low_F, ADLmvSDC12medium_F, ADLmvSDC12heavy_F),
                    BDS = c(ADLmvBDS12low_F, ADLmvBDS12medium_F, ADLmvBDS12heavy_F),
                    CDS = c(ADLmvCDS12low_F, ADLmvCDS12medium_F, ADLmvCDS12heavy_F),
                    BCD = c(ADLmvBCD12low_F, ADLmvBCD12medium_F, ADLmvBCD12heavy_F),
                    FSB = c(ADLmvFSB12low_F, ADLmvFSB12medium_F, ADLmvFSB12heavy_F))



simStudy21_F <- function(measure1, measure2, measure3){
  return(list(low = lapply(Sim21, function(x) mvControlChart_F(x$Measure211[measure1],
                                                             x$Measure211[measure2],
                                                             x$Measure211[measure3])),
              medium = lapply(Sim21, function(x) mvControlChart_F(x$Measure212[measure1],
                                                                x$Measure212[measure2],
                                                                x$Measure212[measure3])),
              heavy = lapply(Sim21, function(x) mvControlChart_F(x$Measure213[measure1],
                                                               x$Measure213[measure2],
                                                               x$Measure213[measure3]))))
}

mvSBE21_F <- simStudy21_F("Spektral", "BetwCent", "EigenMean")
almvSBE21low_F <- falseAlarm(evaluation(mvSBE21_F, "low"))
almvSBE21medium_F <- falseAlarm(evaluation(mvSBE21_F,"medium"))
almvSBE21heavy_F <- falseAlarm(evaluation(mvSBE21_F, "heavy"))
ADLmvSBE21low_F <- ADL(evaluation(mvSBE21_F, "low"))
ADLmvSBE21medium_F <- ADL(evaluation(mvSBE21_F, "medium"))
ADLmvSBE21heavy_F <- ADL(evaluation(mvSBE21_F, "heavy"))

mvSDC21_F <- simStudy21_F("Spektral", "DegreeMean", "CloCent")
almvSDC21low_F <- falseAlarm(evaluation(mvSDC21_F, "low"))
almvSDC21medium_F <- falseAlarm(evaluation(mvSDC21_F,"medium"))
almvSDC21heavy_F <- falseAlarm(evaluation(mvSDC21_F, "heavy"))
ADLmvSDC21low_F <- ADL(evaluation(mvSDC21_F, "low"))
ADLmvSDC21medium_F <- ADL(evaluation(mvSDC21_F, "medium"))
ADLmvSDC21heavy_F <- ADL(evaluation(mvSDC21_F, "heavy"))

mvBDS21_F <- simStudy21_F("BetwCent", "DegreeCent", "Spektral")
almvBDS21low_F <- falseAlarm(evaluation(mvBDS21_F, "low"))
almvBDS21medium_F <- falseAlarm(evaluation(mvBDS21_F,"medium"))
almvBDS21heavy_F <- falseAlarm(evaluation(mvBDS21_F, "heavy"))
ADLmvBDS21low_F <- ADL(evaluation(mvBDS21_F, "low"))
ADLmvBDS21medium_F <- ADL(evaluation(mvBDS21_F, "medium"))
ADLmvBDS21heavy_F <- ADL(evaluation(mvBDS21_F, "heavy"))

mvCDS21_F <- simStudy21_F("CloMean", "DegreeMean", "Spektral")
almvCDS21low_F <- falseAlarm(evaluation(mvCDS21_F, "low"))
almvCDS21medium_F <- falseAlarm(evaluation(mvCDS21_F,"medium"))
almvCDS21heavy_F <- falseAlarm(evaluation(mvCDS21_F, "heavy"))
ADLmvCDS21low_F <- ADL(evaluation(mvCDS21_F, "low"))
ADLmvCDS21medium_F <- ADL(evaluation(mvCDS21_F, "medium"))
ADLmvCDS21heavy_F <- ADL(evaluation(mvCDS21_F, "heavy"))

mvBCD21_F <- simStudy21_F("BetwCent", "CloCent", "DegreeCent")
almvBCD21low_F <- falseAlarm(evaluation(mvBCD21_F, "low"))
almvBCD21medium_F <- falseAlarm(evaluation(mvBCD21_F,"medium"))
almvBCD21heavy_F <- falseAlarm(evaluation(mvBCD21_F, "heavy"))
ADLmvBCD21low_F <- ADL(evaluation(mvBCD21_F, "low"))
ADLmvBCD21medium_F <- ADL(evaluation(mvBCD21_F, "medium"))
ADLmvBCD21heavy_F <- ADL(evaluation(mvBCD21_F, "heavy"))


mvFSB21_F <- simStudy21_F("Frobenius", "Spektral", "BetwCent")
almvFSB21low_F <- falseAlarm(evaluation(mvFSB21_F, "low"))
almvFSB21medium_F <- falseAlarm(evaluation(mvFSB21_F,"medium"))
almvFSB21heavy_F <- falseAlarm(evaluation(mvFSB21_F, "heavy"))
ADLmvFSB21low_F <- ADL(evaluation(mvFSB21_F, "low"))
ADLmvFSB21medium_F <- ADL(evaluation(mvFSB21_F, "medium"))
ADLmvFSB21heavy_F <- ADL(evaluation(mvFSB21_F, "heavy"))



falseAlarm21_F <- data.frame(SBE = c(almvSBE21low_F, almvSBE21medium_F, almvSBE21heavy_F),
                           SDC = c(almvSDC21low_F, almvSDC21medium_F, almvSDC21heavy_F),
                           BDS = c(almvBDS21low_F, almvBDS21medium_F, almvBDS21heavy_F),
                           CDS = c(almvCDS21low_F, almvCDS21medium_F, almvCDS21heavy_F),
                           BCD = c(almvBCD21low_F, almvBCD21medium_F, almvBCD21heavy_F),
                           FSB = c(almvFSB21low_F, almvFSB21medium_F, almvFSB21heavy_F))

ADL21_F <- data.frame(SBE = c(ADLmvSBE21low_F, ADLmvSBE21medium_F, ADLmvSBE21heavy_F),
                    SDC = c(ADLmvSDC21low_F, ADLmvSDC21medium_F, ADLmvSDC21heavy_F),
                    BDS = c(ADLmvBDS21low_F, ADLmvBDS21medium_F, ADLmvBDS21heavy_F),
                    CDS = c(ADLmvCDS21low_F, ADLmvCDS21medium_F, ADLmvCDS21heavy_F),
                    BCD = c(ADLmvBCD21low_F, ADLmvBCD21medium_F, ADLmvBCD21heavy_F),
                    FSB = c(ADLmvFSB21low_F, ADLmvFSB21medium_F, ADLmvFSB21heavy_F))



simStudy22_F <- function(measure1, measure2, measure3){
  return(list(low = lapply(Sim22, function(x) mvControlChart_F(x$Measure221[measure1],
                                                             x$Measure221[measure2],
                                                             x$Measure221[measure3])),
              medium = lapply(Sim22, function(x) mvControlChart_F(x$Measure222[measure1],
                                                                x$Measure222[measure2],
                                                                x$Measure222[measure3])),
              heavy = lapply(Sim22, function(x) mvControlChart_F(x$Measure223[measure1],
                                                               x$Measure223[measure2],
                                                               x$Measure223[measure3]))))
}

mvSBE22_F <- simStudy22_F("Spektral", "BetwCent", "EigenMean")
almvSBE22low_F <- falseAlarm(evaluation(mvSBE22_F, "low"))
almvSBE22medium_F <- falseAlarm(evaluation(mvSBE22_F,"medium"))
almvSBE22heavy_F <- falseAlarm(evaluation(mvSBE22_F, "heavy"))
ADLmvSBE22low_F <- ADL(evaluation(mvSBE22_F, "low"))
ADLmvSBE22medium_F <- ADL(evaluation(mvSBE22_F, "medium"))
ADLmvSBE22heavy_F <- ADL(evaluation(mvSBE22_F, "heavy"))

mvSDC22_F <- simStudy22_F("Spektral", "DegreeMean", "CloCent")
almvSDC22low_F <- falseAlarm(evaluation(mvSDC22_F, "low"))
almvSDC22medium_F <- falseAlarm(evaluation(mvSDC22_F,"medium"))
almvSDC22heavy_F <- falseAlarm(evaluation(mvSDC22_F, "heavy"))
ADLmvSDC22low_F <- ADL(evaluation(mvSDC22_F, "low"))
ADLmvSDC22medium_F <- ADL(evaluation(mvSDC22_F, "medium"))
ADLmvSDC22heavy_F <- ADL(evaluation(mvSDC22_F, "heavy"))

mvBDS22_F <- simStudy22_F("BetwCent", "DegreeCent", "Spektral")
almvBDS22low_F <- falseAlarm(evaluation(mvBDS22_F, "low"))
almvBDS22medium_F <- falseAlarm(evaluation(mvBDS22_F,"medium"))
almvBDS22heavy_F <- falseAlarm(evaluation(mvBDS22_F, "heavy"))
ADLmvBDS22low_F <- ADL(evaluation(mvBDS22_F, "low"))
ADLmvBDS22medium_F <- ADL(evaluation(mvBDS22_F, "medium"))
ADLmvBDS22heavy_F <- ADL(evaluation(mvBDS22_F, "heavy"))

mvCDS22_F <- simStudy22_F("CloMean", "DegreeMean", "Spektral")
almvCDS22low_F <- falseAlarm(evaluation(mvCDS22_F, "low"))
almvCDS22medium_F <- falseAlarm(evaluation(mvCDS22_F,"medium"))
almvCDS22heavy_F <- falseAlarm(evaluation(mvCDS22_F, "heavy"))
ADLmvCDS22low_F <- ADL(evaluation(mvCDS22_F, "low"))
ADLmvCDS22medium_F <- ADL(evaluation(mvCDS22_F, "medium"))
ADLmvCDS22heavy_F <- ADL(evaluation(mvCDS22_F, "heavy"))

mvBCD22_F <- simStudy22_F("BetwCent", "CloCent", "DegreeCent")
almvBCD22low_F <- falseAlarm(evaluation(mvBCD22_F, "low"))
almvBCD22medium_F <- falseAlarm(evaluation(mvBCD22_F,"medium"))
almvBCD22heavy_F <- falseAlarm(evaluation(mvBCD22_F, "heavy"))
ADLmvBCD22low_F <- ADL(evaluation(mvBCD22_F, "low"))
ADLmvBCD22medium_F <- ADL(evaluation(mvBCD22_F, "medium"))
ADLmvBCD22heavy_F <- ADL(evaluation(mvBCD22_F, "heavy"))


mvFSB22_F <- simStudy22_F("Frobenius", "Spektral", "BetwCent")
almvFSB22low_F <- falseAlarm(evaluation(mvFSB22_F, "low"))
almvFSB22medium_F <- falseAlarm(evaluation(mvFSB22_F,"medium"))
almvFSB22heavy_F <- falseAlarm(evaluation(mvFSB22_F, "heavy"))
ADLmvFSB22low_F <- ADL(evaluation(mvFSB22_F, "low"))
ADLmvFSB22medium_F <- ADL(evaluation(mvFSB22_F, "medium"))
ADLmvFSB22heavy_F <- ADL(evaluation(mvFSB22_F, "heavy"))



falseAlarm22_F <- data.frame(SBE = c(almvSBE22low_F, almvSBE22medium_F, almvSBE22heavy_F),
                           SDC = c(almvSDC22low_F, almvSDC22medium_F, almvSDC22heavy_F),
                           BDS = c(almvBDS22low_F, almvBDS22medium_F, almvBDS22heavy_F),
                           CDS = c(almvCDS22low_F, almvCDS22medium_F, almvCDS22heavy_F),
                           BCD = c(almvBCD22low_F, almvBCD22medium_F, almvBCD22heavy_F),
                           FSB = c(almvFSB22low_F, almvFSB22medium_F, almvFSB22heavy_F))

ADL22_F <- data.frame(SBE = c(ADLmvSBE22low_F, ADLmvSBE22medium_F, ADLmvSBE22heavy_F),
                    SDC = c(ADLmvSDC22low_F, ADLmvSDC22medium_F, ADLmvSDC22heavy_F),
                    BDS = c(ADLmvBDS22low_F, ADLmvBDS22medium_F, ADLmvBDS22heavy_F),
                    CDS = c(ADLmvCDS22low_F, ADLmvCDS22medium_F, ADLmvCDS22heavy_F),
                    BCD = c(ADLmvBCD22low_F, ADLmvBCD22medium_F, ADLmvBCD22heavy_F),
                    FSB = c(ADLmvFSB22low_F, ADLmvFSB22medium_F, ADLmvFSB22heavy_F))


