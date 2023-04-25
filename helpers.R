### This file contains help functions for the evaluation of the simulation
### study

falseAlarm <- function(ev){
  ev <- stack(unlist(ev))
  mean(ev[which(ev$ind == "fA"), 1])
}

ADL <- function(ev){
  ev <- stack(unlist(ev))
  mean(ev[which(ev$ind == "ADL"), 1])
}

evaluation <- function(data, situation){
  if(situation == "low"){
    low <- data$low
    return(low)
  }
  if(situation == "medium"){
    medium <- data$medium
    return(medium)
  }
  if(situation == "heavy"){
    heavy <- data$heavy
    return(heavy)
  }
}

evaluation <- function(data, situation){
  if(situation == "one"){
    one <- data$one
    return(one)
  }
  if(situation == "two"){
    two <- data$two
    return(two)
  }
  if(situation == "three"){
    three <- data$three
    return(three)
  }
  if(situation == "four"){
    four <- data$four
    return(four)
  }
}

