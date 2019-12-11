removeVars_plotter <- function () {
  attach(mtcars)
  par(mfrow=c(2,2))
  
  plot(day1$mnth, day1$temp, main="Temperature by Month (2011-2012)", xlab = "Month", ylab = "Temperature (C)")
  plot(day1$season, day1$temp, main="Temperature by Season (2011-2012)", xlab = "Season", ylab = "Temperature (C)")
  
  plot(day1$temp, day1$tot, main="Bike Users vs Temperature (2011-2012)", xlab = "Temperature (C)", ylab = "Number of Users (per day)")
}

linearModel <- function() {
  summary(lm())
}