# for when i really need it
source("libraries.R")
# from work
setwd("C:/Users/a.bateman/Documents/GITHUB/Trans_anal")
# from home
setwd("~/PhD/PhD/projects/transect/Trans_anal/Trans_anal")

Hoppers <- read.csv("Hoppers_a.csv", sep=";", stringsAsFactors=TRUE)  
Jack <- read.csv("Jackdawta.csv", sep=";", stringsAsFactors=TRUE)  

str(Hoppers)
str(Jack)
View(Hoppers)
View(Jack)

install.packages("ggplot2")
install.packages("lme4")
install.packages("dplyr")
install.packages("DHARMa")

library(ggplot2)
library(lme4)
library(dplyr)
library(DHARMa)

## summ groups disregarding family and size.

Hoppers <- Hoppers %>% 
  mutate(Orth = OrthCaeS+OrthCaeM+OrthCaeL+OrthEnsS+OrthEnsM+OrthEnsL+OrthUnkS+OrthUnkM+OrthUnkL) %>%
  mutate(Lep = LepS+LepM+LepL) %>%
  mutate(Col = ColS+ColM+ColL) %>% 
  mutate(Dip = DipS+DipM+DipL) %>% 
  mutate(Ant = AntS+AntM+AntL) %>% 
  mutate(Oth = OthS+OthM+OthL)

## PERIODS PER TRANSECT

plot(Hoppers$Orth~Hoppers$julian) # we see there are differentiated dates with a slight trend to peak btw 150 and 160 days
plot(Hopper$Orth~Hopper$Temperature) # 
plot(Hopper$Ant~Hopper$julian)

# view sheet from now...
View(Hopper)

# take out outlier of day 189
Hopper <- subset(Hoppers, julian <= 180)

# divide in periods, 7 clearly diff periods

Hopper$period <- Hopper$julian
Hopper$period <- ifelse((Hopper$julian>=120&Hopper$julian<=125), 'A', Hopper$period)
Hopper$period <- ifelse((Hopper$julian>=125&Hopper$julian<=135), 'B', Hopper$period)
Hopper$period <- ifelse((Hopper$julian>=135&Hopper$julian<=145), 'C', Hopper$period)
Hopper$period <- ifelse((Hopper$julian>=146&Hopper$julian<=155), 'D', Hopper$period)
Hopper$period <- ifelse((Hopper$julian>=155&Hopper$julian<=165), 'E', Hopper$period)
Hopper$period <- ifelse((Hopper$julian>=165&Hopper$julian<=175), 'F', Hopper$period)

Hopper$time <- as.numeric(gsub("\\:.*$", "", Hopper$Hour))
Hopper$dayper <- Hopper$time
Hopper$dayper <- ifelse(Hopper$time >= 9 & Hopper$time<=11, 'morning', Hopper$dayper)
Hopper$dayper <- ifelse(Hopper$time > 11 & Hopper$time<=13, 'midday', Hopper$dayper)
Hopper$dayper <- ifelse(Hopper$time > 13 & Hopper$time<=15, 'afternoon', Hopper$dayper)

Hopper <-subset(Hopper, Weather!= "CloudyRainyWindy")

# CHECK THE MODEL ASSUMPTIONS OF THE LMM: normality and homoscedasticity
max.simResid<-simulateResiduals (abunP, 1000)
plot (max.simResid)

# Phenology is important, when both, flowers and seeds, we see generally more orthopters.
abunP <- lme4::glmer.nb(Orth ~ period + (1|Tower), data = Hopper)
summary(abunP)

### Is this related to time of year (blooming of shrubs)?

abunPH <- lme4::glmer.nb(Orth ~ Phen + period + (1|Tower), data = Hopper)
summary(abunPH)

## lower AIC value in this model
### What if interaction?

abunPH <- lme4::glmer.nb(Orth ~ Phen * period + (1|Tower), data = Hopper)
summary(abunPH)
## lower AIC value, but what does this mean? Flowers and period B has more orthopters? generally? or compared to each period/phenology?


# Orthopters, Ant, Dip are more abundant in the wild type of habitat.
abun <- lme4::glmer.nb(Orth ~ HabitatType + (1|Tower), data = Hopper)
summary(abun)

# No harvest is important to presence of Ants. 
abun <- lme4::glmer.nb(Ant ~ Harvest + (1|Tower), data = Hopper)
summary(abun)

# Abundance depends on day of year?

fit <- glmer(Orth ~ julian + (1|Tower), family = "poisson", data = Hopper)
summary(fit)

######################### JOINING DATA FRAMES #####################
str(Jack)

Jackou <- subset(Jack,edat == "ou")
Jackpoll <- subset(Jack, edat != "adult"&edat!="ou")


Jack2<-subset(Jack1,)

plot(Jack$edat_Euring~Jack$julian)

summary(abun)


?merge
