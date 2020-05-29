### Durchführung der Simulationsstudie
### Zunächst einfache Besipiele, um ein Gefuehl fuer Masszahlen zu bekommen,
### spaeter umfangreicher.

###-----------------------------------------------------------------------------

### Case 1: Feste Knotenzahl

###-----------------------------------------------------------------------------

### Scenario 1: CP durch Sprung in der Linkw'keit im gesamten Netzwerk
### Bsp: IT-Sicherheit (Cyber-Attacken), Terroristen-Netzwerke

## Leichte, Mittlere und starke Abweichungen:
ex111 <- Case1Scenario1(Ti = 200, n = 100, p0 = 0.2, p1 = 0.22)
ex112 <- Case1Scenario1(Ti = 200, n = 100, p0 = 0.4, p1 = 0.45)
ex113 <- Case1Scenario1(Ti = 200, n = 100, p0 = 0.3, p1 = 0.5)

Measure111 <- CPMeasures(ex111)
Measure112 <- CPMeasures(ex112)
Measure113 <- CPMeasures(ex113)


## Frobeniusnorm
controlCharts(NetMeasure = Measure111$Frobenius, method = "all")
controlCharts(NetMeasure = Measure112$Frobenius, method = "all")
controlCharts(NetMeasure = Measure113$Frobenius, method = "all")

## 2-Norm
controlCharts(NetMeasure = Measure111$Spektral, method = "all")
controlCharts(NetMeasure = Measure112$Spektral, method = "all")
controlCharts(NetMeasure = Measure113$Spektral, method = "all")

## Eigenvalue
controlCharts(NetMeasure = Measure111$Eigenvalue, method = "all")
controlCharts(NetMeasure = Measure112$Eigenvalue, method = "all")
controlCharts(NetMeasure = Measure113$Eigenvalue, method = "all")

## Beetweeness-Mean
controlCharts(NetMeasure = Measure111$BetwMean, method = "all")
controlCharts(NetMeasure = Measure112$BetwMean, method = "all")
controlCharts(NetMeasure = Measure113$BetwMean, method = "all")

## Beetweeness-Centrality
controlCharts(NetMeasure = Measure111$BetwCent, method = "all")
controlCharts(NetMeasure = Measure112$BetwCent, method = "all")
controlCharts(NetMeasure = Measure113$BetwCent, method = "all")

## Degree-Mean
controlCharts(NetMeasure = Measure111$DegreeMean, method = "all")
controlCharts(NetMeasure = Measure112$DegreeMean, method = "all")
controlCharts(NetMeasure = Measure113$DegreeMean, method = "all")

## Degree-Centrality
controlCharts(NetMeasure = Measure111$DegreeCent, method = "all")
controlCharts(NetMeasure = Measure112$DegreeCent, method = "all")
controlCharts(NetMeasure = Measure113$DegreeCent, method = "all")

## Eigen-Centrality
controlCharts(NetMeasure = Measure111$EigenCent, method = "all")
controlCharts(NetMeasure = Measure112$EigenCent, method = "all")
controlCharts(NetMeasure = Measure113$EigenCent, method = "all")

## Cluster Coefficient
controlCharts(NetMeasure = Measure111$ClusterCoef, method = "all")
controlCharts(NetMeasure = Measure112$ClusterCoef, method = "all")
controlCharts(NetMeasure = Measure113$ClusterCoef, method = "all")

## Average Path Length
controlCharts(NetMeasure = Measure111$AvPathLength, method = "all")
controlCharts(NetMeasure = Measure112$AvPathLength, method = "all")
controlCharts(NetMeasure = Measure113$AvPathLength, method = "all")

## Jaccard-Koeffizient
controlCharts(NetMeasure = Measure111$Jaccard, method = "all")
controlCharts(NetMeasure = Measure112$Jaccard, method = "all")
controlCharts(NetMeasure = Measure113$Jaccard, method = "all")


###-----------------------------------------------------------------------------

### Scenario 2: CP durch Sprung in der Linkw'keit von zentralen Knoten
### Bsp: Entwicklung von Hierarchien in sozialen Netzwerken

## Leichte, Mittlere und starke Abweichungen (abhg. von Anteil verandernder 
### Instanzen):
ex121 <- Case1Scenario2(Ti = 200, n = 100, p0 = 0.2, p1 = 0.3, portion = 0.05)
ex122 <- Case1Scenario2(Ti = 200, n = 100, p0 = 0.4, p1 = 0.55, portion = 0.035)
ex123 <- Case1Scenario2(Ti = 200, n = 100, p0 = 0.3, p1 = 0.7, portion = 0.02)

Measure121 <- CPMeasures(ex121)
Measure122 <- CPMeasures(ex122)
Measure123 <- CPMeasures(ex123)


## Frobeniusnorm
controlCharts(NetMeasure = Measure121$Frobenius, method = "all")
controlCharts(NetMeasure = Measure122$Frobenius, method = "all")
controlCharts(NetMeasure = Measure123$Frobenius, method = "all")

## 2-Norm
controlCharts(NetMeasure = Measure121$Spektral, method = "all")
controlCharts(NetMeasure = Measure122$Spektral, method = "all")
controlCharts(NetMeasure = Measure123$Spektral, method = "all")

## Eigenvalue
controlCharts(NetMeasure = Measure121$Eigenvalue, method = "all")
controlCharts(NetMeasure = Measure122$Eigenvalue, method = "all")
controlCharts(NetMeasure = Measure123$Eigenvalue, method = "all")

## Beetweeness-Mean
controlCharts(NetMeasure = Measure121$BetwMean, method = "all")
controlCharts(NetMeasure = Measure122$BetwMean, method = "all")
controlCharts(NetMeasure = Measure123$BetwMean, method = "all")

## Beetweeness-Centrality
controlCharts(NetMeasure = Measure121$BetwCent, method = "all")
controlCharts(NetMeasure = Measure122$BetwCent, method = "all")
controlCharts(NetMeasure = Measure123$BetwCent, method = "all")

## Degree-Mean
controlCharts(NetMeasure = Measure121$DegreeMean, method = "all")
controlCharts(NetMeasure = Measure122$DegreeMean, method = "all")
controlCharts(NetMeasure = Measure123$DegreeMean, method = "all")

## Degree-Centrality
controlCharts(NetMeasure = Measure121$DegreeCent, method = "all")
controlCharts(NetMeasure = Measure122$DegreeCent, method = "all")
controlCharts(NetMeasure = Measure123$DegreeCent, method = "all")

## Eigen-Centrality
controlCharts(NetMeasure = Measure121$EigenCent, method = "all")
controlCharts(NetMeasure = Measure122$EigenCent, method = "all")
controlCharts(NetMeasure = Measure123$EigenCent, method = "all")

## Cluster Coefficient
controlCharts(NetMeasure = Measure121$ClusterCoef, method = "all")
controlCharts(NetMeasure = Measure122$ClusterCoef, method = "all")
controlCharts(NetMeasure = Measure123$ClusterCoef, method = "all")

## Average Path Length
controlCharts(NetMeasure = Measure121$AvPathLength, method = "all")
controlCharts(NetMeasure = Measure122$AvPathLength, method = "all")
controlCharts(NetMeasure = Measure123$AvPathLength, method = "all")

## Jaccard-Koeffizient
controlCharts(NetMeasure = Measure121$Jaccard, method = "all")
controlCharts(NetMeasure = Measure122$Jaccard, method = "all")
controlCharts(NetMeasure = Measure123$Jaccard, method = "all")


###-----------------------------------------------------------------------------

### Scenario 3: CP durch Wechsel von zentralen Knoten
### Bsp: Veraenderung von Hierarchien in sozialen Netzwerken mit 
### gleichbleibender Hierarchiestruktur (Arbeitsnetzwerke, Politiknetzwerke etc.) 

## Leichte, Mittlere und starke Abweichungen (strenge bis flache Hierarchien):
ex131 <- Case1Scenario3(Ti = 200, n = 100, p = 0.2, pCent = 0.6, nCent = 5)
ex132 <- Case1Scenario3(Ti = 200, n = 100, p = 0.4, pCent = 0.6, nCent = 15)
ex133 <- Case1Scenario3(Ti = 200, n = 100, p = 0.3, pCent = 0.4, nCent = 30)

Measure131 <- CPMeasures(ex131)
Measure132 <- CPMeasures(ex132)
Measure133 <- CPMeasures(ex133)


## Frobeniusnorm
controlCharts(NetMeasure = Measure131$Frobenius, method = "all")
controlCharts(NetMeasure = Measure132$Frobenius, method = "all")
controlCharts(NetMeasure = Measure133$Frobenius, method = "all")

## 2-Norm
controlCharts(NetMeasure = Measure131$Spektral, method = "all")
controlCharts(NetMeasure = Measure132$Spektral, method = "all")
controlCharts(NetMeasure = Measure133$Spektral, method = "all")

## Eigenvalue
controlCharts(NetMeasure = Measure131$Eigenvalue, method = "all")
controlCharts(NetMeasure = Measure132$Eigenvalue, method = "all")
controlCharts(NetMeasure = Measure133$Eigenvalue, method = "all")

## Beetweeness-Mean
controlCharts(NetMeasure = Measure131$BetwMean, method = "all")
controlCharts(NetMeasure = Measure132$BetwMean, method = "all")
controlCharts(NetMeasure = Measure133$BetwMean, method = "all")

## Beetweeness-Centrality
controlCharts(NetMeasure = Measure131$BetwCent, method = "all")
controlCharts(NetMeasure = Measure132$BetwCent, method = "all")
controlCharts(NetMeasure = Measure133$BetwCent, method = "all")

## Degree-Mean
controlCharts(NetMeasure = Measure131$DegreeMean, method = "all")
controlCharts(NetMeasure = Measure132$DegreeMean, method = "all")
controlCharts(NetMeasure = Measure133$DegreeMean, method = "all")

## Degree-Centrality
controlCharts(NetMeasure = Measure131$DegreeCent, method = "all")
controlCharts(NetMeasure = Measure132$DegreeCent, method = "all")
controlCharts(NetMeasure = Measure133$DegreeCent, method = "all")

## Eigen-Centrality
controlCharts(NetMeasure = Measure131$EigenCent, method = "all")
controlCharts(NetMeasure = Measure132$EigenCent, method = "all")
controlCharts(NetMeasure = Measure133$EigenCent, method = "all")

## Cluster Coefficient
controlCharts(NetMeasure = Measure131$ClusterCoef, method = "all")
controlCharts(NetMeasure = Measure132$ClusterCoef, method = "all")
controlCharts(NetMeasure = Measure133$ClusterCoef, method = "all")

## Average Path Length
controlCharts(NetMeasure = Measure131$AvPathLength, method = "all")
controlCharts(NetMeasure = Measure132$AvPathLength, method = "all")
controlCharts(NetMeasure = Measure133$AvPathLength, method = "all")

## Jaccard-Koeffizient
controlCharts(NetMeasure = Measure131$Jaccard, method = "all")
controlCharts(NetMeasure = Measure132$Jaccard, method = "all")
controlCharts(NetMeasure = Measure133$Jaccard, method = "all")


###-----------------------------------------------------------------------------

### Case 2: Variable Knotenzahl - steigende Knotenzahl

###-----------------------------------------------------------------------------

### Scenario 1: CP durch ploetzliches Hinzukommen neuer Einheiten bei gleich 
### bleibender Link-Anzahl
### Bsp: Ausnutzung neuer Maschinen in der Produktion, Transportnetzwerke

## Leichte, Mittlere und starke Anstiege:
ex211 <- Case2Scenario1(Ti = 200, n0 = 100, n1 = 130, m = 1500, Dev = 0.05)
ex212 <- Case2Scenario1(Ti = 200, n0 = 100, n1 = 150, m = 1500, Dev = 0.05)
ex213 <- Case2Scenario1(Ti = 200, n0 = 100, n1 = 200, m = 1500, Dev = 0.05)

Measure211 <- CPMeasures(ex211)
Measure212 <- CPMeasures(ex212)
Measure213 <- CPMeasures(ex213)


## Frobeniusnorm
controlCharts(NetMeasure = Measure211$Frobenius, method = "all")
controlCharts(NetMeasure = Measure212$Frobenius, method = "all")
controlCharts(NetMeasure = Measure213$Frobenius, method = "all")

## 2-Norm
controlCharts(NetMeasure = Measure211$Spektral, method = "all")
controlCharts(NetMeasure = Measure212$Spektral, method = "all")
controlCharts(NetMeasure = Measure213$Spektral, method = "all")

## Eigenvalue
controlCharts(NetMeasure = Measure211$Eigenvalue, method = "all")
controlCharts(NetMeasure = Measure212$Eigenvalue, method = "all")
controlCharts(NetMeasure = Measure213$Eigenvalue, method = "all")

## Beetweeness-Mean
controlCharts(NetMeasure = Measure211$BetwMean, method = "all")
controlCharts(NetMeasure = Measure212$BetwMean, method = "all")
controlCharts(NetMeasure = Measure213$BetwMean, method = "all")

## Beetweeness-Centrality
controlCharts(NetMeasure = Measure211$BetwCent, method = "all")
controlCharts(NetMeasure = Measure212$BetwCent, method = "all")
controlCharts(NetMeasure = Measure213$BetwCent, method = "all")

## Degree-Mean
controlCharts(NetMeasure = Measure211$DegreeMean, method = "all")
controlCharts(NetMeasure = Measure212$DegreeMean, method = "all")
controlCharts(NetMeasure = Measure213$DegreeMean, method = "all")

## Degree-Centrality
controlCharts(NetMeasure = Measure211$DegreeCent, method = "all")
controlCharts(NetMeasure = Measure212$DegreeCent, method = "all")
controlCharts(NetMeasure = Measure213$DegreeCent, method = "all")

## Eigen-Centrality
controlCharts(NetMeasure = Measure211$EigenCent, method = "all")
controlCharts(NetMeasure = Measure212$EigenCent, method = "all")
controlCharts(NetMeasure = Measure213$EigenCent, method = "all")

## Cluster Coefficient
controlCharts(NetMeasure = Measure211$ClusterCoef, method = "all")
controlCharts(NetMeasure = Measure212$ClusterCoef, method = "all")
controlCharts(NetMeasure = Measure213$ClusterCoef, method = "all")

## Average Path Length
controlCharts(NetMeasure = Measure211$AvPathLength, method = "all")
controlCharts(NetMeasure = Measure212$AvPathLength, method = "all")
controlCharts(NetMeasure = Measure213$AvPathLength, method = "all")

###-----------------------------------------------------------------------------#

### Scenario 2: CP durch ploetzliches Hinzukommen weniger, aber zentraler neuer
### Einheiten, die viel Linkstruktur beanspruchen.
### Bsp: neue Standortzentren  in Supply Chains, 
###      neue Hotspots in Virus-Netzwerken

## Leichte, Mittlere und starke Anstiege (in Zusammenspiel mit Anzahl neuer 
## Einheiten):
ex221 <- Case2Scenario2(Ti = 200, n0 = 100, p = 0.3, Dev = 0.02, pCent = 0.6, nCent = 4, pNew = 0.25)
ex222 <- Case2Scenario2(Ti = 200, n0 = 100, p = 0.5, Dev = 0.02, pCent = 0.9, nCent = 2, pNew = 0.45)
ex223 <- Case2Scenario2(Ti = 200, n0 = 100, p = 0.2, Dev = 0.02, pCent = 0.3, nCent = 10, pNew = 0.15)

Measure221 <- CPMeasures(ex221)
Measure222 <- CPMeasures(ex222)
Measure223 <- CPMeasures(ex223)


## Frobeniusnorm
controlCharts(NetMeasure = Measure221$Frobenius, method = "all")
controlCharts(NetMeasure = Measure222$Frobenius, method = "all")
controlCharts(NetMeasure = Measure223$Frobenius, method = "all")

## 2-Norm
controlCharts(NetMeasure = Measure221$Spektral, method = "all")
controlCharts(NetMeasure = Measure222$Spektral, method = "all")
controlCharts(NetMeasure = Measure223$Spektral, method = "all")

## Eigenvalue
controlCharts(NetMeasure = Measure221$Eigenvalue, method = "all")
controlCharts(NetMeasure = Measure222$Eigenvalue, method = "all")
controlCharts(NetMeasure = Measure223$Eigenvalue, method = "all")

## Beetweeness-Mean
controlCharts(NetMeasure = Measure221$BetwMean, method = "all")
controlCharts(NetMeasure = Measure222$BetwMean, method = "all")
controlCharts(NetMeasure = Measure223$BetwMean, method = "all")

## Beetweeness-Centrality
controlCharts(NetMeasure = Measure221$BetwCent, method = "all")
controlCharts(NetMeasure = Measure222$BetwCent, method = "all")
controlCharts(NetMeasure = Measure223$BetwCent, method = "all")

## Degree-Mean
controlCharts(NetMeasure = Measure221$DegreeMean, method = "all")
controlCharts(NetMeasure = Measure222$DegreeMean, method = "all")
controlCharts(NetMeasure = Measure223$DegreeMean, method = "all")

## Degree-Centrality
controlCharts(NetMeasure = Measure221$DegreeCent, method = "all")
controlCharts(NetMeasure = Measure222$DegreeCent, method = "all")
controlCharts(NetMeasure = Measure223$DegreeCent, method = "all")

## Eigen-Centrality
controlCharts(NetMeasure = Measure221$EigenCent, method = "all")
controlCharts(NetMeasure = Measure222$EigenCent, method = "all")
controlCharts(NetMeasure = Measure223$EigenCent, method = "all")

## Cluster Coefficient
controlCharts(NetMeasure = Measure221$ClusterCoef, method = "all")
controlCharts(NetMeasure = Measure222$ClusterCoef, method = "all")
controlCharts(NetMeasure = Measure223$ClusterCoef, method = "all")

## Average Path Length
controlCharts(NetMeasure = Measure221$AvPathLength, method = "all")
controlCharts(NetMeasure = Measure222$AvPathLength, method = "all")
controlCharts(NetMeasure = Measure223$AvPathLength, method = "all")


