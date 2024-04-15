# for when i really need it
source("libraries.R")
# from work
setwd("C:/Users/a.bateman/Documents/GITHUB/Trans_anal")
# from home
setwd("~/PhD/PhD/projects/transect/Trans_anal")

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


## julian date
Hoppers$dat<- as.Date(Hoppers$Date, "%d/%m/%Y")
Hoppers$julian <- as.numeric(format(Hoppers$dat, "%j"))
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
Hopper$dayper <- ifelse(Hopper$time >= 9 & Hopper$time<=11, 'early_morning', Hopper$dayper)
Hopper$dayper <- ifelse(Hopper$time > 11 & Hopper$time<=13, 'morning', Hopper$dayper)
Hopper$dayper <- ifelse(Hopper$time > 13 & Hopper$time<=15, 'midday', Hopper$dayper)

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

Jackhop <- merge(Jackpoll.na, Hoppers, by = "julian", all.x = TRUE, all.y = TRUE)

### NExt steps

## I want to see whether the abundance of orthopters matches growth rate of chicks
## I could instead see the number of chicks at each age stage and the number of orthopters, as i know growth rate is greatest between 

sum_stats <- Jackhop %>%
  group_by(julian, AgeAtWeighing) %>%
  summarise(ChickCount = n_distinct(metalica_esq))

sum_stat <- Jackhop %>%
  group_by(julian) %>%
  summarise(mean_orth = mean(Orth), SD_orth = sd(Orth))

summ <- merge(sum_stat, sum_stats, by = "julian", all.x = TRUE, all.y = TRUE)

ggplot() +
  geom_point(data = summ, aes(x = julian, y = ChickCount, color = as.factor(AgeAtWeighing)), stat = "identity", fill = "blue", alpha = 0.5) +
  geom_point(data = summ, aes(x = julian, y = mean_orth, group = 1), color = "red") +
  geom_abline(data = summ)
  labs(x = "Age at Weighing (days) / Julian Date", y = "Count", title = "Chick Count and Total Orthopter Count") +
  theme_minimal()

Jack2<-subset(Jack1,)

plot(Jack$edat_Euring~Jack$julian)

summary(abun)


?merge
