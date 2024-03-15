# for when i really need it
source("libraries.R")
# from work
setwd("C:/Users/a.bateman/Documents/GITHUB/Trans_anal")
# from home
setwd("~/PhD/PhD/projects/transect/Trans_anal/Trans_anal")


Hoppers <- read.table("Hoppers_d.txt", header = TRUE)
View(Hoppers)

install.packages("fitdistrplus")
library(fitdistrplus)


## Step (1) Plot 
descdist( data = Hopper$Orth , discrete = T) # if you have continuous data --> discrete = F. The closest the observation is to a distributions, the more likely it's to show that distribution.
descdist(data = Hopper$Orth, discrete = T, boot=1000) # to show bootstraped values

## Step (2) Fit
fitdist(Hopper$Orth,"pois") # distribution in question is negative binomial or exponential
pois__ = fitdist(Hopper$Orth,"pois") # fitting the distribution of data to a certain type
plot(pois__)

## Step (3) Estimate parameters
print(pois__)
summary(pois__) # comparing AIC with other fittings might help in choosing a distribution when unclear



