# for when i really need it
source("libraries.R")
# from work
setwd("C:/Users/a.bateman/Documents/GITHUB/Trans_anal")
# from home
setwd("~/PhD/PhD/projects/transect/Trans_anal/Trans_anal")


Hoppers <- read.table("Monitoring_sheet.txt", header = TRUE)


install.packages("fitdistrplus")
library(fitdistrplus)


## Step (1) Plot 
descdist( data = Hoppers$AntS , discrete = T) # if you have continuous data --> discrete = F. The closest the observation is to a distributions, the more likely it's to show that distribution.
descdist(data = Hoppers$AntS, discrete = T, boot=1000) # to show bootstraped values

## Step (2) Fit
fitdist(Hoppers$AntS,"nbinom") # distribution in question is negative binomial or exponential
pois__ = fitdist(Hoppers$AntS,"nbinom") # fitting the distribution of data to a certain type
plot(pois__)

## Step (3) Estimate parameters
print(pois__)
summary(pois__) # comparing AIC with other fittings might help in choosing a distribution when unclear



