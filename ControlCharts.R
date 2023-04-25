### Implementation of the univariate control charts (EWMA and Shewart charts) with the settings of the simulation study

## Output: false alarm rate and ARL_1.

library(qcc)

## EWMA control chart

## Input: NetMeasure - desired metric extracted from the time series of Networks (see CPMeasures function)
##        lambda - memory parameter of the EWMA chart
##        train - length of in-Control state
controlCharts_EWMA <- function(NetMeasure, lambda, train){
  EWMA <- ewma(NetMeasure[1:train], newdata = NetMeasure[-(1:train)],
               nsigmas = 2.635376, lambda = lambda)
  EWMALim <- EWMA$violations
  falseAlarm <- EWMALim[EWMALim < 1051 & EWMALim > 1000]
  falseAlarmRate <- length(falseAlarm)/50
  trueAlarm <- c(EWMALim[EWMALim >= 1051], 1400)
  ADL <- min(trueAlarm) - 1050
  return(list(fA = falseAlarmRate, ADL = ADL))
}

##Shewart Control Chart
controlCharts <- function(NetMeasure, train = 1000){
  ShewHelp <- qcc(NetMeasure[1:train], type = "xbar.one", plot = FALSE)
  ShewQu <- c(quantile(ShewHelp$statistics, 0.005), quantile(ShewHelp$statistics, 0.995))
  Shew <- qcc(NetMeasure[1:train], newdata = NetMeasure[-(1:train)], type = "xbar.one",
              limits = ShewQu)
  ShewLim <- sort(Shew$violations$beyond.limits)
  falseAlarm <- ShewLim[ShewLim < 1051 & ShewLim > 1000]
  falseAlarmRate <- length(falseAlarm)/50
  trueAlarm <- c(ShewLim[ShewLim >= 1051], 1400)
  ADL <- min(trueAlarm) - 1050
  return(list(fA = falseAlarmRate, ADL = ADL))
}

