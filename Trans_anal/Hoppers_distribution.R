# from work
setwd("C:/Users/a.bateman/Documents/GITHUB/Trans_anal")
# from home
setwd("~/PhD/PhD/projects/transect/Trans_anal")

# packages for jacks and hoppers
source("libraries.R")

Hoppers <- read.table("Hoppers_d.txt", header = TRUE)
View(Hoppers)

install.packages("fitdistrplus")
library(fitdistrplus)


## Step (1) Plot 
descdist( data = mean_pes$mean_pes , discrete = T) # if you have continuous data --> discrete = F. The closest the observation is to a distributions, the more likely it's to show that distribution.
descdist(data = mean_pes$mean_pes, discrete = T, boot=1000) # to show bootstraped values

## Step (2) Fit
fitdist(mean_pes$mean_pes,"hyper") # distribution in question is negative binomial or exponential
pois__ = fitdist(mean_pes$mean_pes,"nbinom") # fitting the distribution of data to a certain type
plot(pois__)

## Step (3) Estimate parameters
print(pois__)
summary(pois__) # comparing AIC with other fittings might help in choosing a distribution when unclear
?fitdist


