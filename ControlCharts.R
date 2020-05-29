### In diesem File werden typische Control Charts implementiert. Die Funktion 
### kann verwendet werden, um Masszahlen in den erstellten Netzwerkstrukturen
### zur Ueberwachung zu verwenden.

library(qcc)

controlCharts <- function(NetMeasure, method){
  n <- length(NetMeasure)
  train <- floor(n * 0.25)
  if(method == "CUSUM"){
    cusum(NetMeasure[1:train], newdata = NetMeasure[-(1:train)])
  }
  if(method == "EWMA"){
    ewma(NetMeasure[1:train], newdata = NetMeasure[-(1:train)])
  }
  if(method == "Shewart"){
    qcc(NetMeasure[1:train], newdata = NetMeasure[-(1:train)], type = "xbar.one")
  } 
  if(method == "all"){
    qcc(NetMeasure[1:train], newdata = NetMeasure[-(1:train)], type = "xbar.one")
    cusum(NetMeasure[1:train], newdata = NetMeasure[-(1:train)])
    ewma(NetMeasure[1:train], newdata = NetMeasure[-(1:train)])
  }
}





