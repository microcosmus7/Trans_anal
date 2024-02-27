# for when i really need it
source("libraries.R")
setwd("C:/Users/a.bateman/Documents/GITHUB/Trans_anal")
setwd("~/PhD/PhD/projects/transect/Trans_anal/Trans_anal")
Hoppers <- read.table("Monitoring_sheet.txt", header = TRUE)


install.packages("ggplot2")
install.packages("lme4")

library(ggplot2)
library(lme4)
# CHECKING CORRELATIONS

View(Hoppers)
cor.test(Hoppers$OrthCaeS,Hoppers$Date)
cor.test(Hoppers$Total,Hoppers$Temperature) # significant p-value in the correlation of total grasshoppers and temperature
cor.test(Hoppers$Total,Hoppers$DayNum) # different towers, insects, habitats -> including this in the analisis may show rel.
cor.test(Hoppers$Temperature, Hoppers$DayNum) # significant p-value in the correlation of temperature and the date

plot(Hoppers$Temperature, Hoppers$Total)

# General linear regressions
## significant trend in how Total number of prey items are related to temperature
gentrend <- lm(Total~Temperature, data = Hoppers)
summary(gentrend)
summary(gentrend)$r.squared #rsquared is 0.03... too low

## significant trend in how temperature is related to day 
gentrend2 <- lm(Temperature~DayNum, data = Hoppers)
summary(gentrend2)
summary(gentrend2)$r.squared #rsquared is 0.52


## regression line does not look supper steep
attach(Hoppers)
plot(Temperature, log(Total),xlab = "T (ºC)", ylab = "Total prey", abline(lm(log(Total)~Temperature)))

plot(DayNum, Temperature, xlab = "Number of day", ylab = "Temperature", abline(lm(Temperature~DayNum)))

mean<-aggregate(Hoppers$Total,list(Hoppers$Temperature), mean)
ggplot(data=mean, aes(Group.1,x))+geom_point(colour="black")+
  xlab("Temperature")+ylab("Total prey")+  
  stat_smooth(method = "lm", colour="black", formula = y ~ x + I(x^2))+
  theme_bw(base_size = 19)

