removeVars_plotter <- function () {
  attach(mtcars)
  par(mfrow=c(2,1))
  
  plot(day1$mnth, day1$temp, main="Temperature by Month (2011-2012)", xlab = "Month", ylab = "Temperature (C)")
  plot(day1$season, day1$temp, main="Temperature by Season (2011-2012)", xlab = "Season", ylab = "Temperature (C)")
}