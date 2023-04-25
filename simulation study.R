### Durchfuehrung der Simulationsstudie
### Zun?chst einfache Besipiele, um ein Gefuehl fuer Masszahlen zu bekommen,
### spaeter umfangreicher.

###-----------------------------------------------------------------------------

### Case 1: Changes triggered by links - GLCs + LLCs

###-----------------------------------------------------------------------------

### Scenario 1: CP durch Sprung in der Linkw'keit im gesamten Netzwerk
### Bsp: IT-Sicherheit (Cyber-Attacken), Terroristen-Netzwerke

## Leichte, Mittlere und starke Abweichungen:

simData11 <- function(){
  ex111 <- Case1Scenario1(Ti = 1400, CP = 1050, n = 50, p0 = 0.4, p1 = 0.43,
                          inC = FALSE)
  ex112 <- Case1Scenario1(Ti = 1400, CP = 1050, n = 50, p0 = 0.4, p1 = 0.45, 
                          inC = FALSE)
  ex113 <- Case1Scenario1(Ti = 1400, CP = 1050, n = 50, p0 = 0.4, p1 = 0.5,
                          inC = FALSE)

  #refAdj111 <- Case1Scenario1(Ti = 200, CP = 1100, n = 100, p0 = 0.3, p1 = 0.32, inC = TRUE)
  #refAdj112 <- Case1Scenario1(Ti = 200, CP = 1100, n = 100, p0 = 0.3, p1 = 0.35, inC = TRUE)
  #refAdj113 <- Case1Scenario1(Ti = 200, CP = 1100, n = 100, p0 = 0.3, p1 = 0.5, inC = TRUE)
    
  Measure111 <- CPMeasures(ex111, method = "all")
  Measure112 <- CPMeasures(ex112, method = "all")
  Measure113 <- CPMeasures(ex113, method = "all")
  # return(list(Adj111 = ex111, Adj112 = ex112, Adj113 = ex113, Measure111 = Measure111, 
  #             Measure112 = Measure112, Measure113 = Measure113))
  return(list(Measure111 = Measure111, Measure112 = Measure112, Measure113 = Measure113))
}

Sim11 <- replicate(100, simData11(), simplify = FALSE)
save(Sim11, file = "sim11.RData")

simStudy11 <- function(Measure){
  
  ## Frobeniusnorm
  if(Measure == "Frobenius"){
    return(list(low = lapply(Sim11, function(x) controlCharts(x$Measure111$Frobenius)),
                medium = lapply(Sim11, function(x) controlCharts(x$Measure112$Frobenius)),
                heavy = lapply(Sim11, function(x) controlCharts(x$Measure113$Frobenius))))
  }
  
  
  ## 2-Norm
  if(Measure == "twoNorm"){
    return(list(low = lapply(Sim11, function(x) controlCharts(x$Measure111$Spektral)),
                medium = lapply(Sim11, function(x) controlCharts(x$Measure112$Spektral)),
                heavy = lapply(Sim11, function(x) controlCharts(x$Measure113$Spektral))))
  }
  
  
  ## Closeness-Mean
  if(Measure == "CloMean"){
    return(list(low = lapply(Sim11, function(x) controlCharts(x$Measure111$CloMean)),
                medium = lapply(Sim11, function(x) controlCharts(x$Measure112$CloMean)),
                heavy = lapply(Sim11, function(x) controlCharts(x$Measure113$CloMean))))
  }
  
  
  ## Closeness-Centrality
  if(Measure == "CloCent"){
    return(list(low = lapply(Sim11, function(x) controlCharts(x$Measure111$CloCent)),
                medium = lapply(Sim11, function(x) controlCharts(x$Measure112$CloCent)),
                heavy = lapply(Sim11, function(x) controlCharts(x$Measure113$CloCent))))
  }
  
  ## Beetweeness-Mean
  if(Measure == "BetwMean"){
    return(list(low = lapply(Sim11, function(x) controlCharts(x$Measure111$BetwMean)),
                medium = lapply(Sim11, function(x) controlCharts(x$Measure112$BetwMean)),
                heavy = lapply(Sim11, function(x) controlCharts(x$Measure113$BetwMean))))
  }
  
  
  ## Beetweeness-Centrality
  if(Measure == "BetwCent"){
    return(list(low = lapply(Sim11, function(x) controlCharts(x$Measure111$BetwCent)),
                medium = lapply(Sim11, function(x) controlCharts(x$Measure112$BetwCent)),
                heavy = lapply(Sim11, function(x) controlCharts(x$Measure113$BetwCent))))
  }
  
  
  ## Degree-Mean
  if(Measure == "DegreeMean"){
    return(list(low = lapply(Sim11, function(x) controlCharts(x$Measure111$DegreeMean)),
                medium = lapply(Sim11, function(x) controlCharts(x$Measure112$DegreeMean)),
                heavy = lapply(Sim11, function(x) controlCharts(x$Measure113$DegreeMean))))
  }
  
  
  ## Degree-Centrality
  if(Measure == "DegreeCent"){
    return(list(low = lapply(Sim11, function(x) controlCharts(x$Measure111$DegreeCent)),
                medium = lapply(Sim11, function(x) controlCharts(x$Measure112$DegreeCent)),
                heavy = lapply(Sim11, function(x) controlCharts(x$Measure113$DegreeCent))))
  }
  
  ## Eigen-Mean
  if(Measure == "EigenMean"){
    return(list(low = lapply(Sim11, function(x) controlCharts(x$Measure111$EigenMean)),
                medium = lapply(Sim11, function(x) controlCharts(x$Measure112$EigenMean)),
                heavy = lapply(Sim11, function(x) controlCharts(x$Measure113$EigenMean))))
  }
  
  
  ## Cluster Coefficient
  if(Measure == "ClusterCoef"){
    return(list(low = lapply(Sim11, function(x) controlCharts(x$Measure111$ClusterCoef)),
                medium = lapply(Sim11, function(x) controlCharts(x$Measure112$ClusterCoef)),
                heavy = lapply(Sim11, function(x) controlCharts(x$Measure113$ClusterCoef))))
  }
  
  
  ## Average Path Length
  if(Measure == "AvPathLength"){
    return(list(low = lapply(Sim11, function(x) controlCharts(x$Measure111$AvPathLength)),
                medium = lapply(Sim11, function(x) controlCharts(x$Measure112$AvPathLength)),
                heavy = lapply(Sim11, function(x) controlCharts(x$Measure113$AvPathLength))))
  }
  
  
  if(Measure == "density"){
    return(list(low = lapply(Sim11, function(x) controlCharts(x$Measure111$Density)),
                medium = lapply(Sim11, function(x) controlCharts(x$Measure112$Density)),
                heavy = lapply(Sim11, function(x) controlCharts(x$Measure113$Density))))
  }
}


Frobenius11 <- simStudy11("Frobenius")
alFrobenius11low <- falseAlarm(evaluation(Frobenius11, "low"))
alFrobenius11medium <- falseAlarm(evaluation(Frobenius11,"medium"))
alFrobenius11heavy <- falseAlarm(evaluation(Frobenius11, "heavy"))
ADLFrobenius11low <- ADL(evaluation(Frobenius11, "low"))
ADLFrobenius11medium <- ADL(evaluation(Frobenius11, "medium"))
ADLFrobenius11heavy <- ADL(evaluation(Frobenius11, "heavy"))



twoNorm11 <- simStudy11("twoNorm")
altwoNorm11low <- falseAlarm(evaluation(twoNorm11, "low"))
altwoNorm11medium <- falseAlarm(evaluation(twoNorm11,"medium"))
altwoNorm11heavy <- falseAlarm(evaluation(twoNorm11, "heavy"))
ADLtwoNorm11low <- ADL(evaluation(twoNorm11, "low"))
ADLtwoNorm11medium <- ADL(evaluation(twoNorm11, "medium"))
ADLtwoNorm11heavy <- ADL(evaluation(twoNorm11, "heavy"))


CloMean11 <- simStudy11("CloMean")
alCloMean11low <- falseAlarm(evaluation(CloMean11, "low"))
alCloMean11medium <- falseAlarm(evaluation(CloMean11,"medium"))
alCloMean11heavy <- falseAlarm(evaluation(CloMean11, "heavy"))
ADLCloMean11low <- ADL(evaluation(CloMean11, "low"))
ADLCloMean11medium <- ADL(evaluation(CloMean11, "medium"))
ADLCloMean11heavy <- ADL(evaluation(CloMean11, "heavy"))



CloCent11 <- simStudy11("CloCent")
alCloCent11low <- falseAlarm(evaluation(CloCent11, "low"))
alCloCent11medium <- falseAlarm(evaluation(CloCent11,"medium"))
alCloCent11heavy <- falseAlarm(evaluation(CloCent11, "heavy"))
ADLCloCent11low <- ADL(evaluation(CloCent11, "low"))
ADLCloCent11medium <- ADL(evaluation(CloCent11, "medium"))
ADLCloCent11heavy <- ADL(evaluation(CloCent11, "heavy"))



BetwMean11 <- simStudy11("BetwMean")
alBetwMean11low <- falseAlarm(evaluation(BetwMean11, "low"))
alBetwMean11medium <- falseAlarm(evaluation(BetwMean11,"medium"))
alBetwMean11heavy <- falseAlarm(evaluation(BetwMean11, "heavy"))
ADLBetwMean11low <- ADL(evaluation(BetwMean11, "low"))
ADLBetwMean11medium <- ADL(evaluation(BetwMean11, "medium"))
ADLBetwMean11heavy <- ADL(evaluation(BetwMean11, "heavy"))


BetwCent11 <- simStudy11("BetwCent")
alBetwCent11low <- falseAlarm(evaluation(BetwCent11, "low"))
alBetwCent11medium <- falseAlarm(evaluation(BetwCent11,"medium"))
alBetwCent11heavy <- falseAlarm(evaluation(BetwCent11, "heavy"))
ADLBetwCent11low <- ADL(evaluation(BetwCent11, "low"))
ADLBetwCent11medium <- ADL(evaluation(BetwCent11, "medium"))
ADLBetwCent11heavy <- ADL(evaluation(BetwCent11, "heavy"))

DegreeMean11 <- simStudy11("DegreeMean")
alDegreeMean11low <- falseAlarm(evaluation(DegreeMean11, "low"))
alDegreeMean11medium <- falseAlarm(evaluation(DegreeMean11,"medium"))
alDegreeMean11heavy <- falseAlarm(evaluation(DegreeMean11, "heavy"))
ADLDegreeMean11low <- ADL(evaluation(DegreeMean11, "low"))
ADLDegreeMean11medium <- ADL(evaluation(DegreeMean11, "medium"))
ADLDegreeMean11heavy <- ADL(evaluation(DegreeMean11, "heavy"))


DegreeCent11 <- simStudy11("DegreeCent")
alDegreeCent11low <- falseAlarm(evaluation(DegreeCent11, "low"))
alDegreeCent11medium <- falseAlarm(evaluation(DegreeCent11,"medium"))
alDegreeCent11heavy <- falseAlarm(evaluation(DegreeCent11, "heavy"))
ADLDegreeCent11low <- ADL(evaluation(DegreeCent11, "low"))
ADLDegreeCent11medium <- ADL(evaluation(DegreeCent11, "medium"))
ADLDegreeCent11heavy <- ADL(evaluation(DegreeCent11, "heavy"))


EigenMean11 <- simStudy11("EigenMean")
alEigenMean11low <- falseAlarm(evaluation(EigenMean11, "low"))
alEigenMean11medium <- falseAlarm(evaluation(EigenMean11,"medium"))
alEigenMean11heavy <- falseAlarm(evaluation(EigenMean11, "heavy"))
ADLEigenMean11low <- ADL(evaluation(EigenMean11, "low"))
ADLEigenMean11medium <- ADL(evaluation(EigenMean11, "medium"))
ADLEigenMean11heavy <- ADL(evaluation(EigenMean11, "heavy"))



# ClusterCoef11 <- simStudy11("ClusterCoef")
# alClusterCoef11low <- falseAlarm(evaluation(ClusterCoef11, "low"))
# alClusterCoef11medium <- falseAlarm(evaluation(ClusterCoef11,"medium"))
# alClusterCoef11heavy <- falseAlarm(evaluation(ClusterCoef11, "heavy"))
# ADLClusterCoef11low <- ADL(evaluation(ClusterCoef11, "low"))
# ADLClusterCoef11medium <- ADL(evaluation(ClusterCoef11, "medium"))
# ADLClusterCoef11heavy <- ADL(evaluation(ClusterCoef11, "heavy"))
# 
# 
# AvPathLength11 <- simStudy11("AvPathLength")
# alAvPathLength11low <- falseAlarm(evaluation(AvPathLength11, "low"))
# alAvPathLength11medium <- falseAlarm(evaluation(AvPathLength11,"medium"))
# alAvPathLength11heavy <- falseAlarm(evaluation(AvPathLength11, "heavy"))
# ADLAvPathLength11low <- ADL(evaluation(AvPathLength11, "low"))
# ADLAvPathLength11medium <- ADL(evaluation(AvPathLength11, "medium"))
# ADLAvPathLength11heavy <- ADL(evaluation(AvPathLength11, "heavy"))


# Dens11 <- simStudy11("density")
# alDens11low <- falseAlarm(evaluation(Dens11, "low"))
# alDens11medium <- falseAlarm(evaluation(Dens11,"medium"))
# alDens11heavy <- falseAlarm(evaluation(Dens11, "heavy"))
# ADLDens11low <- ADL(evaluation(Dens11, "low"))
# ADLDens11medium <- ADL(evaluation(Dens11, "medium"))
# ADLDens11heavy <- ADL(evaluation(Dens11, "heavy"))

falseAlarm11 <- data.frame(Frobenius = c(alFrobenius11low, alFrobenius11medium, alFrobenius11heavy),
                             twoNorm = c(altwoNorm11low, altwoNorm11medium, altwoNorm11heavy),
                             CloMean = c(alCloMean11low, alCloMean11medium, alCloMean11heavy),
                             CloCent = c(alCloCent11low, alCloCent11medium, alCloCent11heavy),
                             BetwMean = c(alBetwMean11low, alBetwMean11medium, alBetwMean11heavy),
                             BetwCent = c(alBetwCent11low, alBetwCent11medium, alBetwCent11heavy),
                             DegreeMean = c(alDegreeMean11low, alDegreeMean11medium, alDegreeMean11heavy),
                             DegreeCent = c(alDegreeCent11low, alDegreeCent11medium, alDegreeCent11heavy),
                             EigenMean = c(alEigenMean11low, alEigenMean11medium, alEigenMean11heavy))
                             #ClusterCoef = c(alClusterCoef11low, alClusterCoef11medium, alClusterCoef11heavy),
                             #AvPathLength = c(alAvPathLength11low, alAvPathLength11medium, alAvPathLength11heavy))

ADL11 <- data.frame(Frobenius = c(ADLFrobenius11low, ADLFrobenius11medium, ADLFrobenius11heavy),
                    twoNorm = c(ADLtwoNorm11low, ADLtwoNorm11medium, ADLtwoNorm11heavy),
                    CloMean = c(ADLCloMean11low, ADLCloMean11medium, ADLCloMean11heavy),
                    CloCent = c(ADLCloCent11low, ADLCloCent11medium, ADLCloCent11heavy),
                    BetwMean = c(ADLBetwMean11low, ADLBetwMean11medium, ADLBetwMean11heavy),
                    BetwCent = c(ADLBetwCent11low, ADLBetwCent11medium, ADLBetwCent11heavy),
                    DegreeMean = c(ADLDegreeMean11low, ADLDegreeMean11medium, ADLDegreeMean11heavy),
                    DegreeCent = c(ADLDegreeCent11low, ADLDegreeCent11medium, ADLDegreeCent11heavy),
                    EigenMean = c(ADLEigenMean11low, ADLEigenMean11medium, ADLEigenMean11heavy))
                  #  ClusterCoef = c(ADLClusterCoef11low, ADLClusterCoef11medium, ADLClusterCoef11heavy),
                   # AvPathLength = c(ADLAvPathLength11low, ADLAvPathLength11medium, ADLAvPathLength11heavy))



###-----------------------------------------------------------------------------

### Scenario 2: CP durch Sprung in der Linkw'keit von zentralen Knoten
### Bsp: Entwicklung von Hierarchien in sozialen Netzwerken

## Leichte, Mittlere und starke Abweichungen (abhg. von Anteil verandernder 
### Instanzen):

simData12 <- function(){
  ex121 <- Case1Scenario2(Ti = 1400, CP = 1050, n = 50, p0 = 0.35, p1 = 0.45, portion = 0.1, inC = FALSE)
  ex122 <- Case1Scenario2(Ti = 1400, CP = 1050, n = 50, p0 = 0.4, p1 = 0.6, portion = 0.04, inC = FALSE)
  ex123 <- Case1Scenario2(Ti = 1400, CP = 1050, n = 50, p0 = 0.35, p1 = 0.65, portion = 0.02, inC = FALSE)
  
  Measure121 <- CPMeasures(ex121, method = "all")
  Measure122 <- CPMeasures(ex122, method = "all")
  Measure123 <- CPMeasures(ex123, method = "all")
  # return(list(Adj121 = ex121, Adj122 = ex122, Adj123 = ex123, Measure121 = Measure121, 
  #             Measure122 = Measure122, Measure123 = Measure123))
  return(list(Measure121 = Measure121, Measure122 = Measure122, Measure123 = Measure123))
}

Sim12 <- replicate(100, simData12(), simplify = FALSE)
save(Sim12, file = "sim12.RData")


simStudy12 <- function(Measure){
  
  ## Frobeniusnorm
  if(Measure == "Frobenius"){
    return(list(low = lapply(Sim12, function(x) controlCharts(x$Measure121$Frobenius)),
                medium = lapply(Sim12, function(x) controlCharts(x$Measure122$Frobenius)),
                heavy = lapply(Sim12, function(x) controlCharts(x$Measure123$Frobenius))))
  }
  
  
  ## 2-Norm
  if(Measure == "twoNorm"){
    return(list(low = lapply(Sim12, function(x) controlCharts(x$Measure121$Spektral)),
                medium = lapply(Sim12, function(x) controlCharts(x$Measure122$Spektral)),
                heavy = lapply(Sim12, function(x) controlCharts(x$Measure123$Spektral))))
  }
  
  
  
  ## Closeness-Mean
  if(Measure == "CloMean"){
    return(list(low = lapply(Sim12, function(x) controlCharts(x$Measure121$CloMean)),
                medium = lapply(Sim12, function(x) controlCharts(x$Measure122$CloMean)),
                heavy = lapply(Sim12, function(x) controlCharts(x$Measure123$CloMean))))
  }
  
  
  ## Closeness-Centrality
  if(Measure == "CloCent"){
    return(list(low = lapply(Sim12, function(x) controlCharts(x$Measure121$CloCent)),
                medium = lapply(Sim12, function(x) controlCharts(x$Measure122$CloCent)),
                heavy = lapply(Sim12, function(x) controlCharts(x$Measure123$CloCent))))
  }
  
  ## Beetweeness-Mean
  if(Measure == "BetwMean"){
    return(list(low = lapply(Sim12, function(x) controlCharts(x$Measure121$BetwMean)),
                medium = lapply(Sim12, function(x) controlCharts(x$Measure122$BetwMean)),
                heavy = lapply(Sim12, function(x) controlCharts(x$Measure123$BetwMean))))
  }
  
  
  ## Beetweeness-Centrality
  if(Measure == "BetwCent"){
    return(list(low = lapply(Sim12, function(x) controlCharts(x$Measure121$BetwCent)),
                medium = lapply(Sim12, function(x) controlCharts(x$Measure122$BetwCent)),
                heavy = lapply(Sim12, function(x) controlCharts(x$Measure123$BetwCent))))
  }
  
  
  ## Degree-Mean
  if(Measure == "DegreeMean"){
    return(list(low = lapply(Sim12, function(x) controlCharts(x$Measure121$DegreeMean)),
                medium = lapply(Sim12, function(x) controlCharts(x$Measure122$DegreeMean)),
                heavy = lapply(Sim12, function(x) controlCharts(x$Measure123$DegreeMean))))
  }
  
  
  ## Degree-Centrality
  if(Measure == "DegreeCent"){
    return(list(low = lapply(Sim12, function(x) controlCharts(x$Measure121$DegreeCent)),
                medium = lapply(Sim12, function(x) controlCharts(x$Measure122$DegreeCent)),
                heavy = lapply(Sim12, function(x) controlCharts(x$Measure123$DegreeCent))))
  }
  
  ## Eigen-Mean
  if(Measure == "EigenMean"){
    return(list(low = lapply(Sim12, function(x) controlCharts(x$Measure121$EigenMean)),
                medium = lapply(Sim12, function(x) controlCharts(x$Measure122$EigenMean)),
                heavy = lapply(Sim12, function(x) controlCharts(x$Measure123$EigenMean))))
  }
  
  
  ## Cluster Coefficient
  if(Measure == "ClusterCoef"){
    return(list(low = lapply(Sim12, function(x) controlCharts(x$Measure121$ClusterCoef)),
                medium = lapply(Sim12, function(x) controlCharts(x$Measure122$ClusterCoef)),
                heavy = lapply(Sim12, function(x) controlCharts(x$Measure123$ClusterCoef))))
  }
  
  
  ## Average Path Length
  if(Measure == "AvPathLength"){
    return(list(low = lapply(Sim12, function(x) controlCharts(x$Measure121$AvPathLength)),
                medium = lapply(Sim12, function(x) controlCharts(x$Measure122$AvPathLength)),
                heavy = lapply(Sim12, function(x) controlCharts(x$Measure123$AvPathLength))))
  }
}

Frobenius12 <- simStudy12("Frobenius")
alFrobenius12low <- falseAlarm(evaluation(Frobenius12, "low"))
alFrobenius12medium <- falseAlarm(evaluation(Frobenius12,"medium"))
alFrobenius12heavy <- falseAlarm(evaluation(Frobenius12, "heavy"))
ADLFrobenius12low <- ADL(evaluation(Frobenius12, "low"))
ADLFrobenius12medium <- ADL(evaluation(Frobenius12, "medium"))
ADLFrobenius12heavy <- ADL(evaluation(Frobenius12, "heavy"))


twoNorm12 <- simStudy12("twoNorm")
altwoNorm12low <- falseAlarm(evaluation(twoNorm12, "low"))
altwoNorm12medium <- falseAlarm(evaluation(twoNorm12,"medium"))
altwoNorm12heavy <- falseAlarm(evaluation(twoNorm12, "heavy"))
ADLtwoNorm12low <- ADL(evaluation(twoNorm12, "low"))
ADLtwoNorm12medium <- ADL(evaluation(twoNorm12, "medium"))
ADLtwoNorm12heavy <- ADL(evaluation(twoNorm12, "heavy"))


CloMean12 <- simStudy12("CloMean")
alCloMean12low <- falseAlarm(evaluation(CloMean12, "low"))
alCloMean12medium <- falseAlarm(evaluation(CloMean12,"medium"))
alCloMean12heavy <- falseAlarm(evaluation(CloMean12, "heavy"))
ADLCloMean12low <- ADL(evaluation(CloMean12, "low"))
ADLCloMean12medium <- ADL(evaluation(CloMean12, "medium"))
ADLCloMean12heavy <- ADL(evaluation(CloMean12, "heavy"))


CloCent12 <- simStudy12("CloCent")
alCloCent12low <- falseAlarm(evaluation(CloCent12, "low"))
alCloCent12medium <- falseAlarm(evaluation(CloCent12,"medium"))
alCloCent12heavy <- falseAlarm(evaluation(CloCent12, "heavy"))
ADLCloCent12low <- ADL(evaluation(CloCent12, "low"))
ADLCloCent12medium <- ADL(evaluation(CloCent12, "medium"))
ADLCloCent12heavy <- ADL(evaluation(CloCent12, "heavy"))


BetwMean12 <- simStudy12("BetwMean")
alBetwMean12low <- falseAlarm(evaluation(BetwMean12, "low"))
alBetwMean12medium <- falseAlarm(evaluation(BetwMean12,"medium"))
alBetwMean12heavy <- falseAlarm(evaluation(BetwMean12, "heavy"))
ADLBetwMean12low <- ADL(evaluation(BetwMean12, "low"))
ADLBetwMean12medium <- ADL(evaluation(BetwMean12, "medium"))
ADLBetwMean12heavy <- ADL(evaluation(BetwMean12, "heavy"))


BetwCent12 <- simStudy12("BetwCent")
alBetwCent12low <- falseAlarm(evaluation(BetwCent12, "low"))
alBetwCent12medium <- falseAlarm(evaluation(BetwCent12,"medium"))
alBetwCent12heavy <- falseAlarm(evaluation(BetwCent12, "heavy"))
ADLBetwCent12low <- ADL(evaluation(BetwCent12, "low"))
ADLBetwCent12medium <- ADL(evaluation(BetwCent12, "medium"))
ADLBetwCent12heavy <- ADL(evaluation(BetwCent12, "heavy"))

DegreeMean12 <- simStudy12("DegreeMean")
alDegreeMean12low <- falseAlarm(evaluation(DegreeMean12, "low"))
alDegreeMean12medium <- falseAlarm(evaluation(DegreeMean12,"medium"))
alDegreeMean12heavy <- falseAlarm(evaluation(DegreeMean12, "heavy"))
ADLDegreeMean12low <- ADL(evaluation(DegreeMean12, "low"))
ADLDegreeMean12medium <- ADL(evaluation(DegreeMean12, "medium"))
ADLDegreeMean12heavy <- ADL(evaluation(DegreeMean12, "heavy"))


DegreeCent12 <- simStudy12("DegreeCent")
alDegreeCent12low <- falseAlarm(evaluation(DegreeCent12, "low"))
alDegreeCent12medium <- falseAlarm(evaluation(DegreeCent12,"medium"))
alDegreeCent12heavy <- falseAlarm(evaluation(DegreeCent12, "heavy"))
ADLDegreeCent12low <- ADL(evaluation(DegreeCent12, "low"))
ADLDegreeCent12medium <- ADL(evaluation(DegreeCent12, "medium"))
ADLDegreeCent12heavy <- ADL(evaluation(DegreeCent12, "heavy"))


EigenMean12 <- simStudy12("EigenMean")
alEigenMean12low <- falseAlarm(evaluation(EigenMean12, "low"))
alEigenMean12medium <- falseAlarm(evaluation(EigenMean12,"medium"))
alEigenMean12heavy <- falseAlarm(evaluation(EigenMean12, "heavy"))
ADLEigenMean12low <- ADL(evaluation(EigenMean12, "low"))
ADLEigenMean12medium <- ADL(evaluation(EigenMean12, "medium"))
ADLEigenMean12heavy <- ADL(evaluation(EigenMean12, "heavy"))


# ClusterCoef12 <- simStudy12("ClusterCoef")
# alClusterCoef12low <- falseAlarm(evaluation(ClusterCoef12, "low"))
# alClusterCoef12medium <- falseAlarm(evaluation(ClusterCoef12,"medium"))
# alClusterCoef12heavy <- falseAlarm(evaluation(ClusterCoef12, "heavy"))
# ADLClusterCoef12low <- ADL(evaluation(ClusterCoef12, "low"))
# ADLClusterCoef12medium <- ADL(evaluation(ClusterCoef12, "medium"))
# ADLClusterCoef12heavy <- ADL(evaluation(ClusterCoef12, "heavy"))
# 
# 
# AvPathLength12 <- simStudy12("AvPathLength")
# alAvPathLength12low <- falseAlarm(evaluation(AvPathLength12, "low"))
# alAvPathLength12medium <- falseAlarm(evaluation(AvPathLength12,"medium"))
# alAvPathLength12heavy <- falseAlarm(evaluation(AvPathLength12, "heavy"))
# ADLAvPathLength12low <- ADL(evaluation(AvPathLength12, "low"))
# ADLAvPathLength12medium <- ADL(evaluation(AvPathLength12, "medium"))
# ADLAvPathLength12heavy <- ADL(evaluation(AvPathLength12, "heavy"))


falseAlarm12 <- data.frame(Frobenius = c(alFrobenius12low, alFrobenius12medium, alFrobenius12heavy),
                             twoNorm = c(altwoNorm12low, altwoNorm12medium, altwoNorm12heavy),
                             CloMean = c(alCloMean12low, alCloMean12medium, alCloMean12heavy),
                             CloCent = c(alCloCent12low, alCloCent12medium, alCloCent12heavy),
                             BetwMean = c(alBetwMean12low, alBetwMean12medium, alBetwMean12heavy),
                             BetwCent = c(alBetwCent12low, alBetwCent12medium, alBetwMean12heavy),
                             DegreeMean = c(alDegreeMean12low, alDegreeMean12medium, alDegreeMean12heavy),
                             DegreeCent = c(alDegreeCent12low, alDegreeCent12medium, alDegreeCent12heavy),
                             EigenMean = c(alEigenMean12low, alEigenMean12medium, alEigenMean12heavy))
                            # ClusterCoef = c(alClusterCoef12low, alClusterCoef12medium, alClusterCoef12heavy),
                            # AvPathLength = c(alAvPathLength12low, alAvPathLength12medium, alAvPathLength12heavy))

ADL12 <- data.frame(Frobenius = c(ADLFrobenius12low, ADLFrobenius12medium, ADLFrobenius12heavy),
                    twoNorm = c(ADLtwoNorm12low, ADLtwoNorm12medium, ADLtwoNorm12heavy),
                    CloMean = c(ADLCloMean12low, ADLCloMean12medium, ADLCloMean12heavy),
                    CloCent = c(ADLCloCent12low, ADLCloCent12medium, ADLCloCent12heavy),
                    BetwMean = c(ADLBetwMean12low, ADLBetwMean12medium, ADLBetwMean12heavy),
                    BetwCent = c(ADLBetwCent12low, ADLBetwCent12medium, ADLBetwCent12heavy),
                    DegreeMean = c(ADLDegreeMean12low, ADLDegreeMean12medium, ADLDegreeMean12heavy),
                    DegreeCent = c(ADLDegreeCent12low, ADLDegreeCent12medium, ADLDegreeCent12heavy),
                    EigenMean = c(ADLEigenMean12low, ADLEigenMean12medium, ADLEigenMean12heavy))
                   # ClusterCoef = c(ADLClusterCoef12low, ADLClusterCoef12medium, ADLClusterCoef12heavy),
                  #  AvPathLength = c(ADLAvPathLength12low, ADLAvPathLength12medium, ADLAvPathLength12heavy))

###-----------------------------------------------------------------------------



###-----------------------------------------------------------------------------

### Case 2: Changes triggered by nodes / GNCs + LNCs

###-----------------------------------------------------------------------------

### Scenario 1: CP durch ploetzliches Hinzukommen neuer Einheiten bei gleich 
### bleibender Link-Anzahl
### Bsp: Ausnutzung neuer Maschinen in der Produktion, Transportnetzwerke

simData21 <- function(){
  ex211 <- Case2Scenario1(Ti = 1400, CP = 1050, n0 = 50, n1 = 55, m = 800, Dev = 0.05)
  ex212 <- Case2Scenario1(Ti = 1400, CP = 1050, n0 = 50, n1 = 60, m = 800, Dev = 0.05)
  ex213 <- Case2Scenario1(Ti = 1400, CP = 1050, n0 = 50, n1 = 75, m = 800, Dev = 0.01)
  
  Measure211 <- CPMeasures(ex211, method = "all")
  Measure212 <- CPMeasures(ex212, method = "all")
  Measure213 <- CPMeasures(ex213, method = "all")
  # return(list(Adj211 = ex211, Adj212 = ex212, Adj213 = ex213, Measure211 = Measure211,
  #             Measure212 = Measure212, Measure213 = Measure213))
  return(list(Measure211 = Measure211, Measure212 = Measure212, Measure213 = Measure213))
}

Sim21 <- replicate(100, simData21(), simplify = FALSE)
save(Sim21, file = "sim21.RData")

simStudy21 <- function(Measure){
  
  ## Frobeniusnorm
  if(Measure == "Frobenius"){
    return(list(low = lapply(Sim21, function(x) controlCharts(x$Measure211$Frobenius)),
                medium = lapply(Sim21, function(x) controlCharts(x$Measure212$Frobenius)),
                heavy = lapply(Sim21, function(x) controlCharts(x$Measure213$Frobenius))))
  }
  
  
  ## 2-Norm
  if(Measure == "twoNorm"){
    return(list(low = lapply(Sim21, function(x) controlCharts(x$Measure211$Spektral)),
                medium = lapply(Sim21, function(x) controlCharts(x$Measure212$Spektral)),
                heavy = lapply(Sim21, function(x) controlCharts(x$Measure213$Spektral))))
  }
  
  
  ## Eigenvalue
  if(Measure == "Eigenvalue"){
    return(list(low = lapply(Sim21, function(x) controlCharts(x$Measure211$Eigenvalue)),
                medium = lapply(Sim21, function(x) controlCharts(x$Measure212$Eigenvalue)),
                heavy = lapply(Sim21, function(x) controlCharts(x$Measure213$Eigenvalue))))
  }
  
  ## Closeness-Mean
  if(Measure == "CloMean"){
    return(list(low = lapply(Sim21, function(x) controlCharts(x$Measure211$CloMean)),
                medium = lapply(Sim21, function(x) controlCharts(x$Measure212$CloMean)),
                heavy = lapply(Sim21, function(x) controlCharts(x$Measure213$CloMean))))
  }
  
  
  ## Closeness-Centrality
  if(Measure == "CloCent"){
    return(list(low = lapply(Sim21, function(x) controlCharts(x$Measure211$CloCent)),
                medium = lapply(Sim21, function(x) controlCharts(x$Measure212$CloCent)),
                heavy = lapply(Sim21, function(x) controlCharts(x$Measure213$CloCent))))
  }
  
  ## Beetweeness-Mean
  if(Measure == "BetwMean"){
    return(list(low = lapply(Sim21, function(x) controlCharts(x$Measure211$BetwMean)),
                medium = lapply(Sim21, function(x) controlCharts(x$Measure212$BetwMean)),
                heavy = lapply(Sim21, function(x) controlCharts(x$Measure213$BetwMean))))
  }
  
  
  ## Beetweeness-Centrality
  if(Measure == "BetwCent"){
    return(list(low = lapply(Sim21, function(x) controlCharts(x$Measure211$BetwCent)),
                medium = lapply(Sim21, function(x) controlCharts(x$Measure212$BetwCent)),
                heavy = lapply(Sim21, function(x) controlCharts(x$Measure213$BetwCent))))
  }
  
  
  ## Degree-Mean
  if(Measure == "DegreeMean"){
    return(list(low = lapply(Sim21, function(x) controlCharts(x$Measure211$DegreeMean)),
                medium = lapply(Sim21, function(x) controlCharts(x$Measure212$DegreeMean)),
                heavy = lapply(Sim21, function(x) controlCharts(x$Measure213$DegreeMean))))
  }
  
  
  ## Degree-Centrality
  if(Measure == "DegreeCent"){
    return(list(low = lapply(Sim21, function(x) controlCharts(x$Measure211$DegreeCent)),
                medium = lapply(Sim21, function(x) controlCharts(x$Measure212$DegreeCent)),
                heavy = lapply(Sim21, function(x) controlCharts(x$Measure213$DegreeCent))))
  }
  
  ## Eigen-Mean
  if(Measure == "EigenMean"){
    return(list(low = lapply(Sim21, function(x) controlCharts(x$Measure211$EigenMean)),
                medium = lapply(Sim21, function(x) controlCharts(x$Measure212$EigenMean)),
                heavy = lapply(Sim21, function(x) controlCharts(x$Measure213$EigenMean))))
  }
  
  ## Cluster Coefficient
  if(Measure == "ClusterCoef"){
    return(list(low = lapply(Sim21, function(x) controlCharts(x$Measure211$ClusterCoef)),
                medium = lapply(Sim21, function(x) controlCharts(x$Measure212$ClusterCoef)),
                heavy = lapply(Sim21, function(x) controlCharts(x$Measure213$ClusterCoef))))
  }
  
  
  ## Average Path Length
  if(Measure == "AvPathLength"){
    return(list(low = lapply(Sim21, function(x) controlCharts(x$Measure211$AvPathLength)),
                medium = lapply(Sim21, function(x) controlCharts(x$Measure212$AvPathLength)),
                heavy = lapply(Sim21, function(x) controlCharts(x$Measure213$AvPathLength))))
  }
}


Frobenius21 <- simStudy21("Frobenius")
alFrobenius21low <- falseAlarm(evaluation(Frobenius21, "low"))
alFrobenius21medium <- falseAlarm(evaluation(Frobenius21,"medium"))
alFrobenius21heavy <- falseAlarm(evaluation(Frobenius21, "heavy"))
ADLFrobenius21low <- ADL(evaluation(Frobenius21, "low"))
ADLFrobenius21medium <- ADL(evaluation(Frobenius21, "medium"))
ADLFrobenius21heavy <- ADL(evaluation(Frobenius21, "heavy"))



twoNorm21 <- simStudy21("twoNorm")
altwoNorm21low <- falseAlarm(evaluation(twoNorm21, "low"))
altwoNorm21medium <- falseAlarm(evaluation(twoNorm21,"medium"))
altwoNorm21heavy <- falseAlarm(evaluation(twoNorm21, "heavy"))
ADLtwoNorm21low <- ADL(evaluation(twoNorm21, "low"))
ADLtwoNorm21medium <- ADL(evaluation(twoNorm21, "medium"))
ADLtwoNorm21heavy <- ADL(evaluation(twoNorm21, "heavy"))



CloMean21 <- simStudy21("CloMean")
alCloMean21low <- falseAlarm(evaluation(CloMean21, "low"))
alCloMean21medium <- falseAlarm(evaluation(CloMean21,"medium"))
alCloMean21heavy <- falseAlarm(evaluation(CloMean21, "heavy"))
ADLCloMean21low <- ADL(evaluation(CloMean21, "low"))
ADLCloMean21medium <- ADL(evaluation(CloMean21, "medium"))
ADLCloMean21heavy <- ADL(evaluation(CloMean21, "heavy"))



CloCent21 <- simStudy21("CloCent")
alCloCent21low <- falseAlarm(evaluation(CloCent21, "low"))
alCloCent21medium <- falseAlarm(evaluation(CloCent21,"medium"))
alCloCent21heavy <- falseAlarm(evaluation(CloCent21, "heavy"))
ADLCloCent21low <- ADL(evaluation(CloCent21, "low"))
ADLCloCent21medium <- ADL(evaluation(CloCent21, "medium"))
ADLCloCent21heavy <- ADL(evaluation(CloCent21, "heavy"))



BetwMean21 <- simStudy21("BetwMean")
alBetwMean21low <- falseAlarm(evaluation(BetwMean21, "low"))
alBetwMean21medium <- falseAlarm(evaluation(BetwMean21,"medium"))
alBetwMean21heavy <- falseAlarm(evaluation(BetwMean21, "heavy"))
ADLBetwMean21low <- ADL(evaluation(BetwMean21, "low"))
ADLBetwMean21medium <- ADL(evaluation(BetwMean21, "medium"))
ADLBetwMean21heavy <- ADL(evaluation(BetwMean21, "heavy"))


BetwCent21 <- simStudy21("BetwCent")
alBetwCent21low <- falseAlarm(evaluation(BetwCent21, "low"))
alBetwCent21medium <- falseAlarm(evaluation(BetwCent21,"medium"))
alBetwCent21heavy <- falseAlarm(evaluation(BetwCent21, "heavy"))
ADLBetwCent21low <- ADL(evaluation(BetwCent21, "low"))
ADLBetwCent21medium <- ADL(evaluation(BetwCent21, "medium"))
ADLBetwCent21heavy <- ADL(evaluation(BetwCent21, "heavy"))

DegreeMean21 <- simStudy21("DegreeMean")
alDegreeMean21low <- falseAlarm(evaluation(DegreeMean21, "low"))
alDegreeMean21medium <- falseAlarm(evaluation(DegreeMean21,"medium"))
alDegreeMean21heavy <- falseAlarm(evaluation(DegreeMean21, "heavy"))
ADLDegreeMean21low <- ADL(evaluation(DegreeMean21, "low"))
ADLDegreeMean21medium <- ADL(evaluation(DegreeMean21, "medium"))
ADLDegreeMean21heavy <- ADL(evaluation(DegreeMean21, "heavy"))


DegreeCent21 <- simStudy21("DegreeCent")
alDegreeCent21low <- falseAlarm(evaluation(DegreeCent21, "low"))
alDegreeCent21medium <- falseAlarm(evaluation(DegreeCent21,"medium"))
alDegreeCent21heavy <- falseAlarm(evaluation(DegreeCent21, "heavy"))
ADLDegreeCent21low <- ADL(evaluation(DegreeCent21, "low"))
ADLDegreeCent21medium <- ADL(evaluation(DegreeCent21, "medium"))
ADLDegreeCent21heavy <- ADL(evaluation(DegreeCent21, "heavy"))


EigenMean21 <- simStudy21("EigenMean")
alEigenMean21low <- falseAlarm(evaluation(EigenMean21, "low"))
alEigenMean21medium <- falseAlarm(evaluation(EigenMean21,"medium"))
alEigenMean21heavy <- falseAlarm(evaluation(EigenMean21, "heavy"))
ADLEigenMean21low <- ADL(evaluation(EigenMean21, "low"))
ADLEigenMean21medium <- ADL(evaluation(EigenMean21, "medium"))
ADLEigenMean21heavy <- ADL(evaluation(EigenMean21, "heavy"))


# ClusterCoef21 <- simStudy21("ClusterCoef")
# alClusterCoef21low <- falseAlarm(evaluation(ClusterCoef21, "low"))
# alClusterCoef21medium <- falseAlarm(evaluation(ClusterCoef21,"medium"))
# alClusterCoef21heavy <- falseAlarm(evaluation(ClusterCoef21, "heavy"))
# ADLClusterCoef21low <- ADL(evaluation(ClusterCoef21, "low"))
# ADLClusterCoef21medium <- ADL(evaluation(ClusterCoef21, "medium"))
# ADLClusterCoef21heavy <- ADL(evaluation(ClusterCoef21, "heavy"))
# 
# 
# AvPathLength21 <- simStudy21("AvPathLength")
# alAvPathLength21low <- falseAlarm(evaluation(AvPathLength21, "low"))
# alAvPathLength21medium <- falseAlarm(evaluation(AvPathLength21,"medium"))
# alAvPathLength21heavy <- falseAlarm(evaluation(AvPathLength21, "heavy"))
# ADLAvPathLength21low <- ADL(evaluation(AvPathLength21, "low"))
# ADLAvPathLength21medium <- ADL(evaluation(AvPathLength21, "medium"))
# ADLAvPathLength21heavy <- ADL(evaluation(AvPathLength21, "heavy"))



falseAlarm21 <- data.frame(Frobenius = c(alFrobenius21low, alFrobenius21medium, alFrobenius21heavy),
                             twoNorm = c(altwoNorm21low, altwoNorm21medium, altwoNorm21heavy),
                             CloMean = c(alCloMean21low, alCloMean21medium, alCloMean21heavy),
                             CloCent = c(alCloCent21low, alCloCent21medium, alCloCent21heavy),
                             BetwMean = c(alBetwMean21low, alBetwMean21medium, alBetwMean21heavy),
                             BetwCent = c(alBetwCent21low, alBetwCent21medium, alBetwMean21heavy),
                             DegreeMean = c(alDegreeMean21low, alDegreeMean21medium, alDegreeMean21heavy),
                             DegreeCent = c(alDegreeCent21low, alDegreeCent21medium, alDegreeCent21heavy),
                             EigenMean = c(alEigenMean21low, alEigenMean21medium, alEigenMean21heavy))
                            # ClusterCoef = c(alClusterCoef21low, alClusterCoef21medium, alClusterCoef21heavy),
                            # AvPathLength = c(alAvPathLength21low, alAvPathLength21medium, alAvPathLength21heavy))

ADL21 <- data.frame(Frobenius = c(ADLFrobenius21low, ADLFrobenius21medium, ADLFrobenius21heavy),
                    twoNorm = c(ADLtwoNorm21low, ADLtwoNorm21medium, ADLtwoNorm21heavy),
                    CloMean = c(ADLCloMean21low, ADLCloMean21medium, ADLCloMean21heavy),
                    CloCent = c(ADLCloCent21low, ADLCloCent21medium, ADLCloCent21heavy),
                    BetwMean = c(ADLBetwMean21low, ADLBetwMean21medium, ADLBetwMean21heavy),
                    BetwCent = c(ADLBetwCent21low, ADLBetwCent21medium, ADLBetwCent21heavy),
                    DegreeMean = c(ADLDegreeMean21low, ADLDegreeMean21medium, ADLDegreeMean21heavy),
                    DegreeCent = c(ADLDegreeCent21low, ADLDegreeCent21medium, ADLDegreeCent21heavy),
                    EigenMean = c(ADLEigenMean21low, ADLEigenMean21medium, ADLEigenMean21heavy))
                   # ClusterCoef = c(ADLClusterCoef21low, ADLClusterCoef21medium, ADLClusterCoef21heavy),
                  #  AvPathLength = c(ADLAvPathLength21low, ADLAvPathLength21medium, ADLAvPathLength21heavy))





###-----------------------------------------------------------------------------#

### Scenario 2: CP durch ploetzliches Hinzukommen weniger, aber zentraler neuer
### Einheiten, die viel Linkstruktur beanspruchen.
### Bsp: neue Standortzentren  in Supply Chains, 
###      neue Hotspots in Virus-Netzwerken

## Leichte, Mittlere und starke Anstiege (in Zusammenspiel mit Anzahl neuer 
## Einheiten):

simData22 <- function(){
  ex221 <- Case2Scenario2(Ti = 1400, CP = 1050, n0 = 50, p = 0.4, Dev = 0.15, pCent = 0.6, nCent = 4)
  ex222 <- Case2Scenario2(Ti = 1400, CP = 1050, n0 = 50, p = 0.4, Dev = 0.15, pCent = 0.7, nCent = 2)
  ex223 <- Case2Scenario2(Ti = 1400, CP = 1050, n0 = 50, p = 0.4, Dev = 0.15, pCent = 0.8, nCent = 1)
  
  Measure221 <- CPMeasures(ex221, method = "all")
  Measure222 <- CPMeasures(ex222, method = "all")
  Measure223 <- CPMeasures(ex223, method = "all")
  # return(list(Adj221 = ex221, Adj222 = ex222, Adj223 = ex223, Measure221 = Measure221,
  #             Measure222 = Measure222, Measure223 = Measure223))
  return(list(Measure221 = Measure221, Measure222 = Measure222, Measure223 = Measure223))
}

Sim22 <- replicate(100, simData22(), simplify = FALSE)
save(Sim22, file = "sim22.RData")

simStudy22 <- function(Measure){
  
  ## Frobeniusnorm
  if(Measure == "Frobenius"){
    return(list(low = lapply(Sim22, function(x) controlCharts(x$Measure221$Frobenius)),
                medium = lapply(Sim22, function(x) controlCharts(x$Measure222$Frobenius)),
                heavy = lapply(Sim22, function(x) controlCharts(x$Measure223$Frobenius))))
  }
  
  
  ## 2-Norm
  if(Measure == "twoNorm"){
    return(list(low = lapply(Sim22, function(x) controlCharts(x$Measure221$Spektral)),
                medium = lapply(Sim22, function(x) controlCharts(x$Measure222$Spektral)),
                heavy = lapply(Sim22, function(x) controlCharts(x$Measure223$Spektral))))
  }
  
 
  
  ## Closeness-Mean
  if(Measure == "CloMean"){
    return(list(low = lapply(Sim22, function(x) controlCharts(x$Measure221$CloMean)),
                medium = lapply(Sim22, function(x) controlCharts(x$Measure222$CloMean)),
                heavy = lapply(Sim22, function(x) controlCharts(x$Measure223$CloMean))))
  }
  
  
  ## Closeness-Centrality
  if(Measure == "CloCent"){
    return(list(low = lapply(Sim22, function(x) controlCharts(x$Measure221$CloCent)),
                medium = lapply(Sim22, function(x) controlCharts(x$Measure222$CloCent)),
                heavy = lapply(Sim22, function(x) controlCharts(x$Measure223$CloCent))))
  }
  
  ## Beetweeness-Mean
  if(Measure == "BetwMean"){
    return(list(low = lapply(Sim22, function(x) controlCharts(x$Measure221$BetwMean)),
                medium = lapply(Sim22, function(x) controlCharts(x$Measure222$BetwMean)),
                heavy = lapply(Sim22, function(x) controlCharts(x$Measure223$BetwMean))))
  }
  
  
  ## Beetweeness-Centrality
  if(Measure == "BetwCent"){
    return(list(low = lapply(Sim22, function(x) controlCharts(x$Measure221$BetwCent)),
                medium = lapply(Sim22, function(x) controlCharts(x$Measure222$BetwCent)),
                heavy = lapply(Sim22, function(x) controlCharts(x$Measure223$BetwCent))))
  }
  
  
  ## Degree-Mean
  if(Measure == "DegreeMean"){
    return(list(low = lapply(Sim22, function(x) controlCharts(x$Measure221$DegreeMean)),
                medium = lapply(Sim22, function(x) controlCharts(x$Measure222$DegreeMean)),
                heavy = lapply(Sim22, function(x) controlCharts(x$Measure223$DegreeMean))))
  }
  
  
  ## Degree-Centrality
  if(Measure == "DegreeCent"){
    return(list(low = lapply(Sim22, function(x) controlCharts(x$Measure221$DegreeCent)),
                medium = lapply(Sim22, function(x) controlCharts(x$Measure222$DegreeCent)),
                heavy = lapply(Sim22, function(x) controlCharts(x$Measure223$DegreeCent))))
  }
  
  ## Eigen-Mean
  if(Measure == "EigenMean"){
    return(list(low = lapply(Sim22, function(x) controlCharts(x$Measure221$EigenMean)),
                medium = lapply(Sim22, function(x) controlCharts(x$Measure222$EigenMean)),
                heavy = lapply(Sim22, function(x) controlCharts(x$Measure223$EigenMean))))
  }

  
  ## Cluster Coefficient
  if(Measure == "ClusterCoef"){
    return(list(low = lapply(Sim22, function(x) controlCharts(x$Measure221$ClusterCoef)),
                medium = lapply(Sim22, function(x) controlCharts(x$Measure222$ClusterCoef)),
                heavy = lapply(Sim22, function(x) controlCharts(x$Measure223$ClusterCoef))))
  }
  
  
  ## Average Path Length
  if(Measure == "AvPathLength"){
    return(list(low = lapply(Sim22, function(x) controlCharts(x$Measure221$AvPathLength)),
                medium = lapply(Sim22, function(x) controlCharts(x$Measure222$AvPathLength)),
                heavy = lapply(Sim22, function(x) controlCharts(x$Measure223$AvPathLength))))
  }
  
}


Frobenius22 <- simStudy22("Frobenius")
alFrobenius22low <- falseAlarm(evaluation(Frobenius22, "low"))
alFrobenius22medium <- falseAlarm(evaluation(Frobenius22,"medium"))
alFrobenius22heavy <- falseAlarm(evaluation(Frobenius22, "heavy"))
ADLFrobenius22low <- ADL(evaluation(Frobenius22, "low"))
ADLFrobenius22medium <- ADL(evaluation(Frobenius22, "medium"))
ADLFrobenius22heavy <- ADL(evaluation(Frobenius22, "heavy"))



twoNorm22 <- simStudy22("twoNorm")
altwoNorm22low <- falseAlarm(evaluation(twoNorm22, "low"))
altwoNorm22medium <- falseAlarm(evaluation(twoNorm22,"medium"))
altwoNorm22heavy <- falseAlarm(evaluation(twoNorm22, "heavy"))
ADLtwoNorm22low <- ADL(evaluation(twoNorm22, "low"))
ADLtwoNorm22medium <- ADL(evaluation(twoNorm22, "medium"))
ADLtwoNorm22heavy <- ADL(evaluation(twoNorm22, "heavy"))



CloMean22 <- simStudy22("CloMean")
alCloMean22low <- falseAlarm(evaluation(CloMean22, "low"))
alCloMean22medium <- falseAlarm(evaluation(CloMean22,"medium"))
alCloMean22heavy <- falseAlarm(evaluation(CloMean22, "heavy"))
ADLCloMean22low <- ADL(evaluation(CloMean22, "low"))
ADLCloMean22medium <- ADL(evaluation(CloMean22, "medium"))
ADLCloMean22heavy <- ADL(evaluation(CloMean22, "heavy"))


CloCent22 <- simStudy22("CloCent")
alCloCent22low <- falseAlarm(evaluation(CloCent22, "low"))
alCloCent22medium <- falseAlarm(evaluation(CloCent22,"medium"))
alCloCent22heavy <- falseAlarm(evaluation(CloCent22, "heavy"))
ADLCloCent22low <- ADL(evaluation(CloCent22, "low"))
ADLCloCent22medium <- ADL(evaluation(CloCent22, "medium"))
ADLCloCent22heavy <- ADL(evaluation(CloCent22, "heavy"))



BetwMean22 <- simStudy22("BetwMean")
alBetwMean22low <- falseAlarm(evaluation(BetwMean22, "low"))
alBetwMean22medium <- falseAlarm(evaluation(BetwMean22,"medium"))
alBetwMean22heavy <- falseAlarm(evaluation(BetwMean22, "heavy"))
ADLBetwMean22low <- ADL(evaluation(BetwMean22, "low"))
ADLBetwMean22medium <- ADL(evaluation(BetwMean22, "medium"))
ADLBetwMean22heavy <- ADL(evaluation(BetwMean22, "heavy"))


BetwCent22 <- simStudy22("BetwCent")
alBetwCent22low <- falseAlarm(evaluation(BetwCent22, "low"))
alBetwCent22medium <- falseAlarm(evaluation(BetwCent22,"medium"))
alBetwCent22heavy <- falseAlarm(evaluation(BetwCent22, "heavy"))
ADLBetwCent22low <- ADL(evaluation(BetwCent22, "low"))
ADLBetwCent22medium <- ADL(evaluation(BetwCent22, "medium"))
ADLBetwCent22heavy <- ADL(evaluation(BetwCent22, "heavy"))


DegreeMean22 <- simStudy22("DegreeMean")
alDegreeMean22low <- falseAlarm(evaluation(DegreeMean22, "low"))
alDegreeMean22medium <- falseAlarm(evaluation(DegreeMean22,"medium"))
alDegreeMean22heavy <- falseAlarm(evaluation(DegreeMean22, "heavy"))
ADLDegreeMean22low <- ADL(evaluation(DegreeMean22, "low"))
ADLDegreeMean22medium <- ADL(evaluation(DegreeMean22, "medium"))
ADLDegreeMean22heavy <- ADL(evaluation(DegreeMean22, "heavy"))


DegreeCent22 <- simStudy22("DegreeCent")
alDegreeCent22low <- falseAlarm(evaluation(DegreeCent22, "low"))
alDegreeCent22medium <- falseAlarm(evaluation(DegreeCent22,"medium"))
alDegreeCent22heavy <- falseAlarm(evaluation(DegreeCent22, "heavy"))
ADLDegreeCent22low <- ADL(evaluation(DegreeCent22, "low"))
ADLDegreeCent22medium <- ADL(evaluation(DegreeCent22, "medium"))
ADLDegreeCent22heavy <- ADL(evaluation(DegreeCent22, "heavy"))


EigenMean22 <- simStudy22("EigenMean")
alEigenMean22low <- falseAlarm(evaluation(EigenMean22, "low"))
alEigenMean22medium <- falseAlarm(evaluation(EigenMean22,"medium"))
alEigenMean22heavy <- falseAlarm(evaluation(EigenMean22, "heavy"))
ADLEigenMean22low <- ADL(evaluation(EigenMean22, "low"))
ADLEigenMean22medium <- ADL(evaluation(EigenMean22, "medium"))
ADLEigenMean22heavy <- ADL(evaluation(EigenMean22, "heavy"))


# ClusterCoef22 <- simStudy22("ClusterCoef")
# alClusterCoef22low <- falseAlarm(evaluation(ClusterCoef22, "low"))
# alClusterCoef22medium <- falseAlarm(evaluation(ClusterCoef22,"medium"))
# alClusterCoef22heavy <- falseAlarm(evaluation(ClusterCoef22, "heavy"))
# ADLClusterCoef22low <- ADL(evaluation(ClusterCoef22, "low"))
# ADLClusterCoef22medium <- ADL(evaluation(ClusterCoef22, "medium"))
# ADLClusterCoef22heavy <- ADL(evaluation(ClusterCoef22, "heavy"))
# 
# 
# AvPathLength22 <- simStudy22("AvPathLength")
# alAvPathLength22low <- falseAlarm(evaluation(AvPathLength22, "low"))
# alAvPathLength22medium <- falseAlarm(evaluation(AvPathLength22,"medium"))
# alAvPathLength22heavy <- falseAlarm(evaluation(AvPathLength22, "heavy"))
# ADLAvPathLength22low <- ADL(evaluation(AvPathLength22, "low"))
# ADLAvPathLength22medium <- ADL(evaluation(AvPathLength22, "medium"))
# ADLAvPathLength22heavy <- ADL(evaluation(AvPathLength22, "heavy"))



falseAlarm22 <- data.frame(Frobenius = c(alFrobenius22low, alFrobenius22medium, alFrobenius22heavy),
                             twoNorm = c(altwoNorm22low, altwoNorm22medium, altwoNorm22heavy),
                             CloMean = c(alCloMean22low, alCloMean22medium, alCloMean22heavy),
                             CloCent = c(alCloCent22low, alCloCent22medium, alCloCent22heavy),
                             BetwMean = c(alBetwMean22low, alBetwMean22medium, alBetwMean22heavy),
                             BetwCent = c(alBetwCent22low, alBetwCent22medium, alBetwMean22heavy),
                             DegreeMean = c(alDegreeMean22low, alDegreeMean22medium, alDegreeMean22heavy),
                             DegreeCent = c(alDegreeCent22low, alDegreeCent22medium, alDegreeCent22heavy),
                             EigenMean = c(alEigenMean22low, alEigenMean22medium, alEigenMean22heavy))
                             #ClusterCoef = c(alClusterCoef22low, alClusterCoef22medium, alClusterCoef22heavy),
                             #AvPathLength = c(alAvPathLength22low, alAvPathLength22medium, alAvPathLength22heavy))

ADL22 <- data.frame(Frobenius = c(ADLFrobenius22low, ADLFrobenius22medium, ADLFrobenius22heavy),
                    twoNorm = c(ADLtwoNorm22low, ADLtwoNorm22medium, ADLtwoNorm22heavy),
                    CloMean = c(ADLCloMean22low, ADLCloMean22medium, ADLCloMean22heavy),
                    CloCent = c(ADLCloCent22low, ADLCloCent22medium, ADLCloCent22heavy),
                    BetwMean = c(ADLBetwMean22low, ADLBetwMean22medium, ADLBetwMean22heavy),
                    BetwCent = c(ADLBetwCent22low, ADLBetwCent22medium, ADLBetwCent22heavy),
                    DegreeMean = c(ADLDegreeMean22low, ADLDegreeMean22medium, ADLDegreeMean22heavy),
                    DegreeCent = c(ADLDegreeCent22low, ADLDegreeCent22medium, ADLDegreeCent22heavy),
                    EigenMean = c(ADLEigenMean22low, ADLEigenMean22medium, ADLEigenMean22heavy))
                 #   ClusterCoef = c(ADLClusterCoef22low, ADLClusterCoef22medium, ADLClusterCoef22heavy),
                  #  AvPathLength = c(ADLAvPathLength22low, ADLAvPathLength22medium, ADLAvPathLength22heavy))



# ##------------------------------------------------------------------------------
# ### Scenario 3: Special Cases (Community Changes with multiple Changes)
# 
# simData3 <- function(){
#   ex31 <- Case3Scenario1(Ti = 1400, CP = 1050)
#   ex32 <- Case3Scenario2(Ti = 1400, CP = 1050)
#   ex33 <- Case3Scenario3(Ti = 1400, CP = 1050)
#   ex34 <- Case3Scenario4(Ti = 1400, CP = 1050)
#   Measure31 <- CPMeasures(ex31, method = "all")
#   Measure32 <- CPMeasures(ex32, method = "all")
#   Measure33 <- CPMeasures(ex33, method = "all")
#   Measure34 <- CPMeasures(ex34, method = "all")
#   return(list(Measure31 = Measure31, Measure32 = Measure32, 
#               Measure33 = Measure33, Mesure34 = Measure34))
# }
# 
# Sim3 <- replicate(100, simData3(), simplify = FALSE)
# 
# 
# simStudy3 <- function(Measure){
#   
#   ## Frobeniusnorm
#   if(Measure == "Frobenius"){
#     return(list(one = lapply(Sim3, function(x) controlCharts(x$Measure31$Frobenius)),
#                 two = lapply(Sim3, function(x) controlCharts(x$Measure32$Frobenius)),
#                 three = lapply(Sim3, function(x) controlCharts(x$Measure33$Frobenius)),
#                 four = lapply(Sim3, function(x) controlCharts(x$Mesure34$Frobenius))))
#   }
#   
#   
#   ## 2-Norm
#   if(Measure == "twoNorm"){
#     return(list(one = lapply(Sim3, function(x) controlCharts(x$Measure31$Spektral)),
#                 two = lapply(Sim3, function(x) controlCharts(x$Measure32$Spektral)),
#                 three = lapply(Sim3, function(x) controlCharts(x$Measure33$Spektral)),
#                 four = lapply(Sim3, function(x) controlCharts(x$Mesure34$Spektral))))
#   }
#   
#   
#   
#   ## Closeness-Mean
#   if(Measure == "CloMean"){
#     return(list(one = lapply(Sim3, function(x) controlCharts(x$Measure31$CloMean)),
#                 two = lapply(Sim3, function(x) controlCharts(x$Measure32$CloMean)),
#                 three = lapply(Sim3, function(x) controlCharts(x$Measure33$CloMean)),
#                 four = lapply(Sim3, function(x) controlCharts(x$Mesure34$CloMean))))
#   }
#   
#   
#   ## Closeness-Centrality
#   if(Measure == "CloCent"){
#     return(list(one = lapply(Sim3, function(x) controlCharts(x$Measure31$CloCent)),
#                 two = lapply(Sim3, function(x) controlCharts(x$Measure32$CloCent)),
#                 three = lapply(Sim3, function(x) controlCharts(x$Measure33$CloCent)),
#                 four = lapply(Sim3, function(x) controlCharts(x$Mesure34$CloCent))))
#   }
#   
#   ## Beetweeness-Mean
#   if(Measure == "BetwMean"){
#     return(list(one = lapply(Sim3, function(x) controlCharts(x$Measure31$BetwMean)),
#                 two = lapply(Sim3, function(x) controlCharts(x$Measure32$BetwMean)),
#                 three = lapply(Sim3, function(x) controlCharts(x$Measure33$BetwMean)),
#                 four = lapply(Sim3, function(x) controlCharts(x$Mesure34$BetwMean))))
#   }
#   
#   
#   ## Beetweeness-Centrality
#   if(Measure == "BetwCent"){
#     return(list(one = lapply(Sim3, function(x) controlCharts(x$Measure31$BetwCent)),
#                 two = lapply(Sim3, function(x) controlCharts(x$Measure32$BetwCent)),
#                 three = lapply(Sim3, function(x) controlCharts(x$Measure33$BetwCent)),
#                 four = lapply(Sim3, function(x) controlCharts(x$Mesure34$BetwCent))))
#   }
#   
#   
#   ## Degree-Mean
#   if(Measure == "DegreeMean"){
#     return(list(one = lapply(Sim3, function(x) controlCharts(x$Measure31$DegreeMean)),
#                 two = lapply(Sim3, function(x) controlCharts(x$Measure32$DegreeMean)),
#                 three = lapply(Sim3, function(x) controlCharts(x$Measure33$DegreeMean)),
#                 four = lapply(Sim3, function(x) controlCharts(x$Mesure34$DegreeMean))))
#   }
#   
#   
#   ## Degree-Centrality
#   if(Measure == "DegreeCent"){
#     return(list(one = lapply(Sim3, function(x) controlCharts(x$Measure31$DegreeCent)),
#                 two = lapply(Sim3, function(x) controlCharts(x$Measure32$DegreeCent)),
#                 three = lapply(Sim3, function(x) controlCharts(x$Measure33$DegreeCent)),
#                 four = lapply(Sim3, function(x) controlCharts(x$Mesure34$DegreeCent))))
#   }
#   
#   ## Eigen-Mean
#   if(Measure == "EigenMean"){
#     return(list(one = lapply(Sim3, function(x) controlCharts(x$Measure31$EigenMean)),
#                 two = lapply(Sim3, function(x) controlCharts(x$Measure32$EigenMean)),
#                 three = lapply(Sim3, function(x) controlCharts(x$Measure33$EigenMean)),
#                 four = lapply(Sim3, function(x) controlCharts(x$Mesure34$EigenMean))))
#   }
#   
#   
#   ## Cluster Coefficient
#   if(Measure == "ClusterCoef"){
#     return(list(one = lapply(Sim3, function(x) controlCharts(x$Measure31$ClusterCoef)),
#                 two = lapply(Sim3, function(x) controlCharts(x$Measure32$ClusterCoef)),
#                 three = lapply(Sim3, function(x) controlCharts(x$Measure33$ClusterCoef)),
#                 four = lapply(Sim3, function(x) controlCharts(x$Mesure34$ClusterCoef))))
#   }
#   
#   
#   ## Average Path Length
#   if(Measure == "AvPathLength"){
#     return(list(one = lapply(Sim3, function(x) controlCharts(x$Measure31$AvPathLength)),
#                 two = lapply(Sim3, function(x) controlCharts(x$Measure32$AvPathLength)),
#                 three = lapply(Sim3, function(x) controlCharts(x$Measure33$AvPathLength)),
#                 four = lapply(Sim3, function(x) controlCharts(x$Mesure34$AvPathLength))))
#   }
# 
# }
# 
# 
# Frobenius3 <- simStudy3("Frobenius")
# alFrobenius31 <- falseAlarm(evaluation(Frobenius3, "one"))
# alFrobenius32 <- falseAlarm(evaluation(Frobenius3,"two"))
# alFrobenius33 <- falseAlarm(evaluation(Frobenius3, "three"))
# alFrobenius34 <- falseAlarm(evaluation(Frobenius3, "four"))
# ADLFrobenius31 <- ADL(evaluation(Frobenius3, "one"))
# ADLFrobenius32 <- ADL(evaluation(Frobenius3, "two"))
# ADLFrobenius33 <- ADL(evaluation(Frobenius3, "three"))
# ADLFrobenius34 <- ADL(evaluation(Frobenius3, "four"))
# 
# 
# 
# twoNorm3 <- simStudy3("twoNorm")
# altwoNorm31 <- falseAlarm(evaluation(twoNorm3, "one"))
# altwoNorm32 <- falseAlarm(evaluation(twoNorm3,"two"))
# altwoNorm33 <- falseAlarm(evaluation(twoNorm3, "three"))
# altwoNorm34 <- falseAlarm(evaluation(twoNorm3, "four"))
# ADLtwoNorm31 <- ADL(evaluation(twoNorm3, "one"))
# ADLtwoNorm32 <- ADL(evaluation(twoNorm3, "two"))
# ADLtwoNorm33 <- ADL(evaluation(twoNorm3, "three"))
# ADLtwoNorm34 <- ADL(evaluation(twoNorm3, "four"))
# 
# 
# 
# CloMean3 <- simStudy3("CloMean")
# alCloMean31 <- falseAlarm(evaluation(CloMean3, "one"))
# alCloMean32 <- falseAlarm(evaluation(CloMean3,"two"))
# alCloMean33 <- falseAlarm(evaluation(CloMean3, "three"))
# alCloMean34 <- falseAlarm(evaluation(CloMean3, "four"))
# ADLCloMean31 <- ADL(evaluation(CloMean3, "one"))
# ADLCloMean32 <- ADL(evaluation(CloMean3, "two"))
# ADLCloMean33 <- ADL(evaluation(CloMean3, "three"))
# ADLCloMean34 <- ADL(evaluation(CloMean3, "four"))
# 
# 
# CloCent3 <- simStudy3("CloCent")
# alCloCent31 <- falseAlarm(evaluation(CloCent3, "one"))
# alCloCent32 <- falseAlarm(evaluation(CloCent3,"two"))
# alCloCent33 <- falseAlarm(evaluation(CloCent3, "three"))
# alCloCent34 <- falseAlarm(evaluation(CloCent3, "four"))
# ADLCloCent31 <- ADL(evaluation(CloCent3, "one"))
# ADLCloCent32 <- ADL(evaluation(CloCent3, "two"))
# ADLCloCent33 <- ADL(evaluation(CloCent3, "three"))
# ADLCloCent34 <- ADL(evaluation(CloCent3, "four"))
# 
# 
# 
# BetwMean3 <- simStudy3("BetwMean")
# alBetwMean31 <- falseAlarm(evaluation(BetwMean3, "one"))
# alBetwMean32 <- falseAlarm(evaluation(BetwMean3,"two"))
# alBetwMean33 <- falseAlarm(evaluation(BetwMean3, "three"))
# alBetwMean34 <- falseAlarm(evaluation(BetwMean3, "four"))
# ADLBetwMean31 <- ADL(evaluation(BetwMean3, "one"))
# ADLBetwMean32 <- ADL(evaluation(BetwMean3, "two"))
# ADLBetwMean33 <- ADL(evaluation(BetwMean3, "three"))
# ADLBetwMean34 <- ADL(evaluation(BetwMean3, "four"))
# 
# 
# BetwCent3 <- simStudy3("BetwCent")
# alBetwCent31 <- falseAlarm(evaluation(BetwCent3, "one"))
# alBetwCent32 <- falseAlarm(evaluation(BetwCent3,"two"))
# alBetwCent33 <- falseAlarm(evaluation(BetwCent3, "three"))
# alBetwCent34 <- falseAlarm(evaluation(BetwCent3, "four"))
# ADLBetwCent31 <- ADL(evaluation(BetwCent3, "one"))
# ADLBetwCent32 <- ADL(evaluation(BetwCent3, "two"))
# ADLBetwCent33 <- ADL(evaluation(BetwCent3, "three"))
# ADLBetwCent34 <- ADL(evaluation(BetwCent3, "four"))
# 
# 
# DegreeMean3 <- simStudy3("DegreeMean")
# alDegreeMean31 <- falseAlarm(evaluation(DegreeMean3, "one"))
# alDegreeMean32 <- falseAlarm(evaluation(DegreeMean3,"two"))
# alDegreeMean33 <- falseAlarm(evaluation(DegreeMean3, "three"))
# alDegreeMean34 <- falseAlarm(evaluation(DegreeMean3, "four"))
# ADLDegreeMean31 <- ADL(evaluation(DegreeMean3, "one"))
# ADLDegreeMean32 <- ADL(evaluation(DegreeMean3, "two"))
# ADLDegreeMean33 <- ADL(evaluation(DegreeMean3, "three"))
# ADLDegreeMean34 <- ADL(evaluation(DegreeMean3, "four"))
# 
# 
# DegreeCent3 <- simStudy3("DegreeCent")
# alDegreeCent31 <- falseAlarm(evaluation(DegreeCent3, "one"))
# alDegreeCent32 <- falseAlarm(evaluation(DegreeCent3,"two"))
# alDegreeCent33 <- falseAlarm(evaluation(DegreeCent3, "three"))
# alDegreeCent34 <- falseAlarm(evaluation(DegreeCent3, "four"))
# ADLDegreeCent31 <- ADL(evaluation(DegreeCent3, "one"))
# ADLDegreeCent32 <- ADL(evaluation(DegreeCent3, "two"))
# ADLDegreeCent33 <- ADL(evaluation(DegreeCent3, "three"))
# ADLDegreeCent34 <- ADL(evaluation(DegreeCent3, "four"))
# 
# 
# EigenMean3 <- simStudy3("EigenMean")
# alEigenMean31 <- falseAlarm(evaluation(EigenMean3, "one"))
# alEigenMean32 <- falseAlarm(evaluation(EigenMean3,"two"))
# alEigenMean33 <- falseAlarm(evaluation(EigenMean3, "three"))
# alEigenMean34 <- falseAlarm(evaluation(EigenMean3, "four"))
# ADLEigenMean31 <- ADL(evaluation(EigenMean3, "one"))
# ADLEigenMean32 <- ADL(evaluation(EigenMean3, "two"))
# ADLEigenMean33 <- ADL(evaluation(EigenMean3, "three"))
# ADLEigenMean34 <- ADL(evaluation(EigenMean3, "four"))
# 
# 
# ClusterCoef3 <- simStudy3("ClusterCoef")
# alClusterCoef31 <- falseAlarm(evaluation(ClusterCoef3, "one"))
# alClusterCoef32 <- falseAlarm(evaluation(ClusterCoef3,"two"))
# alClusterCoef33 <- falseAlarm(evaluation(ClusterCoef3, "three"))
# alClusterCoef34 <- falseAlarm(evaluation(ClusterCoef3, "four"))
# ADLClusterCoef31 <- ADL(evaluation(ClusterCoef3, "one"))
# ADLClusterCoef32 <- ADL(evaluation(ClusterCoef3, "two"))
# ADLClusterCoef33 <- ADL(evaluation(ClusterCoef3, "three"))
# ADLClusterCoef34 <- ADL(evaluation(ClusterCoef3, "four"))
# 
# 
# AvPathLength3 <- simStudy3("AvPathLength")
# alAvPathLength31 <- falseAlarm(evaluation(AvPathLength3, "one"))
# alAvPathLength32 <- falseAlarm(evaluation(AvPathLength3,"two"))
# alAvPathLength33 <- falseAlarm(evaluation(AvPathLength3, "three"))
# alAvPathLength34 <- falseAlarm(evaluation(AvPathLength3, "four"))
# ADLAvPathLength31 <- ADL(evaluation(AvPathLength3, "one"))
# ADLAvPathLength32 <- ADL(evaluation(AvPathLength3, "two"))
# ADLAvPathLength33 <- ADL(evaluation(AvPathLength3, "three"))
# ADLAvPathLegth34 <- ADL(evaluation(AvPathLength3, "four"))
# 
# 
# 
# falseAlarm3 <- data.frame(Frobenius = c(alFrobenius31, alFrobenius32, alFrobenius33, alFrobenius34),
#                            twoNorm = c(altwoNorm31, altwoNorm32, altwoNorm33, altwoNorm34),
#                            CloMean = c(alCloMean31, alCloMean32, alCloMean33, alCloMean34),
#                            CloCent = c(alCloCent31, alCloCent32, alCloCent33, alCloCent34),
#                            BetwMean = c(alBetwMean31, alBetwMean32, alBetwMean33, alBetwMean34),
#                            BetwCent = c(alBetwCent31, alBetwCent32, alBetwCent33, alBetwCent34),
#                            DegreeMean = c(alDegreeMean31, alDegreeMean32, alDegreeMean33, alDegreeMean34),
#                            DegreeCent = c(alDegreeCent31, alDegreeCent32, alDegreeCent33, alDegreeCent34),
#                            EigenMean = c(alEigenMean31, alEigenMean32, alEigenMean33, alEigenMean34),
#                            ClusterCoef = c(alClusterCoef31, alClusterCoef32, alClusterCoef33, alClusterCoef34),
#                            AvPathLength = c(alAvPathLength31, alAvPathLength32, alAvPathLength33, alAvPathLength34))
# 
# ADL3 <- data.frame(Frobenius = c(ADLFrobenius31, ADLFrobenius32, ADLFrobenius33, ADLFrobenius34),
#                     twoNorm = c(ADLtwoNorm31, ADLtwoNorm32, ADLtwoNorm33, ADLtwoNorm34),
#                     CloMean = c(ADLCloMean31, ADLCloMean32, ADLCloMean33, ADLCloMean34),
#                     CloCent = c(ADLCloCent31, ADLCloCent32, ADLCloCent33, ADLCloCent34),
#                     BetwMean = c(ADLBetwMean31, ADLBetwMean32, ADLBetwMean33, ADLBetwMean34),
#                     BetwCent = c(ADLBetwCent31, ADLBetwCent32, ADLBetwCent33, ADLBetwCent34),
#                     DegreeMean = c(ADLDegreeMean31, ADLDegreeMean32, ADLDegreeMean33, ADLDegreeMean34),
#                     DegreeCent = c(ADLDegreeCent31, ADLDegreeCent32, ADLDegreeCent33, ADLDegreeCent34),
#                     EigenMean = c(ADLEigenMean31, ADLEigenMean32, ADLEigenMean33, ADLEigenMean34),
#                     ClusterCoef = c(ADLClusterCoef31, ADLClusterCoef32, ADLClusterCoef33, ADLClusterCoef34),
#                     AvPathLength = c(ADLAvPathLength31, ADLAvPathLength32, ADLAvPathLength33, ADLAvPathLegth34))
