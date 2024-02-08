# for when i really need it
source("libraries.R")
setwd("C:/Users/a.bateman/Documents/GITHUB/Trans_anal")
Hoppers <- read.table("Monitoring_sheet.txt", header = TRUE)


install.packages("ggplot2")
library(ggplot2)

# CHECKING CORRELATIONS

View(Hoppers)
cor.test(Hoppers$OrthCaeS,Hoppers$Date)
cor.test(Hoppers$Total,Hoppers$Temperature)
cor.test(Hoppers$Total,Hoppers$DayNum)
cor.test(Hoppers$Temperature, Hoppers$DayNum)

plot(Hoppers$Total,Hoppers$Temperature)

# General linear regressions
## significant trend in how Total number of prey items are related to temperature
gentrend <- lm(Total~Temperature, data = Hoppers)
summary(gentrend)
summary(gentrend)$r.squared

## significant trend in how temperature is related to day 
gentrend2 <- lm(Temperature~DayNum, data = Hoppers)
summary(gentrend2)


## regression line does not look supper steep
attach(Hoppers)
plot(Temperature, log(Total), xlab = "T (ºC)", ylab = "Total prey", abline(lm(log(Total)~Temperature)))

plot(DayNum, Temperature, xlab = "Number of day", ylab = "Temperature", abline(lm(Temperature~DayNum)))


