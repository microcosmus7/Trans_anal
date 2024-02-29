# for when i really need it
source("libraries.R")
# from work
setwd("C:/Users/a.bateman/Documents/GITHUB/Trans_anal")
# from home
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

ggplot(aes(x = years)) + 
  geom_histogram(binwidth = 1)

Hoppers$category[Hoppers$Hour < 9:59] <- "9"
Hoppers$category[Hoppers$Hour > 10:00 & Hoppers$Hour < 10:59] <- "10"
Hoppers$category[Hoppers$Hour > 11:00 & Hoppers$Hour < 11:59] <- "11"
Hoppers$category[Hoppers$Hour > 12:00 & Hoppers$Hour < 12:59] <- "12"
Hoppers$category[Hoppers$Hour > 13:00 & Hoppers$Hour < 13:59] <- "13"
Hoppers$category[Hoppers$Hour > 14:00 & Hoppers$Hour < 14:59] <- "14"
df$category[df$a > 0.6] <- "high"

ggplot(data=Hoppers, aes(x=Hour)) +
  geom_histogram( binwidth=3) +
  theme(    plot.title = element_text(size=15)
  )
