loadData <- function() {
  library(regtools)
  data(day1)
}

removeVars_plotter <- function () {
  attach(mtcars)
  par(mfrow=c(2,2))
  
  plot(day1$mnth, day1$temp, main="Temperature by Month (2011-2012)", xlab = "Month", ylab = "Temperature (C)")
  plot(day1$season, day1$temp, main="Temperature by Season (2011-2012)", xlab = "Season", ylab = "Temperature (C)")
  
  plot(day1$temp, day1$tot, main="Bike Users vs Temperature (2011-2012)", xlab = "Temperature (C)", ylab = "Number of Users (per day)")
}

predictorToTargetRelation_plotter <- function() {
  attach(mtcars)
  par(mfrow=c(2,2))
  
  plot(day1$temp, day1$atemp, main="Temperature by Atemp (2011-2012)", xlab = "Temperature (C)", ylab = "Atemp (C)")
  plot(day1$temp, day1$mnth, main="Temperature by Month Squared (2011-2012)", xlab = "Temperature (C)", ylab = "Month Squared")
  plot(day1$temp, day1$tot, main="Temperature From Bike Users (2011-2012)", xlab = "Temperature (C)", ylab = "Total # of Users (per day)")
  
  plot(day1$hum, day1$atemp, main="Humidity's Effect on Temperature Feel", xlab = "Humidity", ylab = "Atemp (C)")
  plot(day1$windspeed, day1$atemp, main="Windspeed's Effect on Temperature Feel", xlab = "Windspeed", ylab = "Atemp (C)")
  
  plot(day1$weathersit, day1$hum, main="Weather Situation's Effect on Humidity", xlab = "weathersit", ylab = "Humidity")
  plot(day1$windspeed, day1$hum, main="Windspeed's Effect on Humidity", xlab = "windspeed", ylab = "Humidity")
}

makeLinearModelMatrix <- function() {
  day1relevant <- day1[,c(5,9:13,16)]
  
  weathersitMatrix <- matrix(nrow = nrow(day1relevant), ncol = 4)
  for (i in 1:nrow(day1relevant)) {
    if (day1relevant$weathersit[i] == 1) {
      weathersitMatrix[i,1] <- 1
      weathersitMatrix[i,2:4] <- 0
    } else if (day1relevant$weathersit[i] == 2) {
      weathersitMatrix[i,2] <- 1
      weathersitMatrix[i,c(1,3:4)] <- 0
    } else if (day1relevant$weathersit[i] == 3) {
      weathersitMatrix[i,3] <- 1
      weathersitMatrix[i,c(1:2,4)] <- 0
    } else {
      weathersitMatrix[i,4] <- 1
      weathersitMatrix[i,c(1:3)] <- 0
    }
  }

  day1LMMatrix <- data.frame(mnth=day1relevant[,1], mnth_sq=(day1relevant[,1]^2), isGood=weathersitMatrix[,1], isModerate=weathersitMatrix[,2], isBad=weathersitMatrix[,3], isTerrible=weathersitMatrix[,4], temp=day1relevant[,3], atemp=day1relevant[,4], hum=day1relevant[,5], windspeed=day1relevant[,6], tot=day1relevant[,7])
  return(day1LMMatrix)
}

linearModelTrain <- function(target) {
  if (target == "temp") {
    summary(lm(day1LMMatrix$temp ~ day1LMMatrix$atemp + day1LMMatrix$tot + day1LMMatrix$mnth_sq))
  } else if (target == "atemp") {
    summary(lm(day1LMMatrix$atemp ~ day1LMMatrix$temp + day1LMMatrix$tot + day1LMMatrix$mnth_sq))
  } else if (target == "hum") {
    summary(lm(day1LMMatrix$hum ~ day1LMMatrix$windspeed + day1LMMatrix$isGood + day1LMMatrix$isModerate + day1LMMatrix$isBad))
  } else if (target == "windspeed") {
    summary(lm(day1LMMatrix$windspeed ~ day1LMMatrix$hum))
  } else if (target == "isGood") {
    summary(lm(day1LMMatrix$isGood ~ day1LMMatrix$hum))
  } else if (target == "isModerate") {
    summary(lm(day1LMMatrix$isModerate ~ day1LMMatrix$hum))
  } else if (target == "isBad") {
    summary(lm(day1LMMatrix$isBad ~ day1LMMatrix$hum))
  } else {
    print("Invalid target variable")
  }
}

linearModelTrainV2 <- function(target) {
  if (target == "temp") {
    summary(lm(day1LMMatrix$temp ~ day1LMMatrix$atemp))
  } else if (target == "atemp") {
    summary(lm(day1LMMatrix$atemp ~ day1LMMatrix$temp + day1LMMatrix$tot))
  } else if (target == "hum") {
    summary(lm(day1LMMatrix$hum ~ day1LMMatrix$windspeed + day1LMMatrix$isGood + day1LMMatrix$isModerate + day1LMMatrix$isBad))
  } else if (target == "windspeed") {
    summary(lm(day1LMMatrix$windspeed ~ day1LMMatrix$hum))
  } else if (target == "isGood") {
    summary(lm(day1LMMatrix$isGood ~ day1LMMatrix$hum))
  } else if (target == "isModerate") {
    summary(lm(day1LMMatrix$isModerate ~ day1LMMatrix$hum))
  } else if (target == "isBad") {
    summary(lm(day1LMMatrix$isBad ~ day1LMMatrix$hum))
  } else {
    print("Invalid target variable")
  }
}



basicKNNTrain <- function(target) {
  day1KNNTrain <- day1LMMatrix[1:631,]
  day1KNNValid <- day1LMMatrix[632:731,]
  if (target == "temp") {
    day1x <- day1KNNTrain[,c(2,8,11)]
    temp <- day1KNNTrain$temp
    knnout <- basicKNN(day1x,temp,c(day1KNNValid[1,2], day1KNNValid[1,8],day1KNNValid[1,11]),2)
    knnout
  } else if (target == "atemp") {
    day1x <- day1KNNTrain[,c(2,7,11)]
    atemp <- day1KNNTrain$atemp
    knnout <- basicKNN(day1x,atemp,c(day1KNNValid[1,2], day1KNNValid[1,7],day1KNNValid[1,11]),2)
    knnout  
  } else if (target == "hum") {
    day1x <- day1KNNTrain[,c(3:5,10)]
    hum <- day1KNNTrain$hum
    knnout <- basicKNN(day1x,hum,c(day1KNNValid[1,3], day1KNNValid[1,4], day1KNNValid[1,5], day1KNNValid[1,10]),2)
    knnout
  } else if (target == "windspeed") {
    day1x <- day1KNNTrain$hum
    windspeed <- day1KNNTrain$windspeed
    knnout <- basicKNN(day1x,windspeed,c(day1KNNValid[1,9]),2)
    knnout 
  } else if (target == "isGood") {
    day1x <- day1KNNTrain$hum
    isG <- day1KNNTrain$isGood
    knnout <- basicKNN(day1x,isG,c(day1KNNValid[1,9]),2)
    knnout 
  } else if (target == "isModerate") {
    day1x <- day1KNNTrain$hum
    isM <- day1KNNTrain$isModerate
    knnout <- basicKNN(day1x,isM,c(day1KNNValid[1,9]),2)
    knnout 
  } else if (target == "isBad") {
    day1x <- day1KNNTrain$hum
    isB <- day1KNNTrain$isBad
    knnout <- basicKNN(day1x,isB,c(day1KNNValid[1,9]),2)
    knnout 
  } else {
    print("Invalid target variable")
  }
}