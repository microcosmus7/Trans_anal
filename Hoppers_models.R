# for when i really need it
source("libraries.R")
# from work
setwd("C:/Users/a.bateman/Documents/GITHUB/Trans_anal")
# from home
setwd("~/PhD/PhD/projects/transect/Trans_anal/Trans_anal")


Hoppers <- read.table("Hoppers_d.txt", header = TRUE)
View(Hoppers)

install.packages("ggplot2")
install.packages("lme4")
install.packages("dplyr")

library(ggplot2)
library(lme4)
library(dplyr)

## summ groups disregarding family and size.
Hoppers <- Hoppers %>%
  mutate(Orth = select(., OrthCaeS:OrthUnkL) %>% rowSums(na.rm = TRUE))

Hoppers <- Hoppers %>%
  mutate(Lep = select(., LepS:LepL) %>% rowSums(na.rm = TRUE))

Hoppers <- Hoppers %>%
  mutate(Col = select(., ColS:ColL) %>% rowSums(na.rm = TRUE))

Hoppers <- Hoppers %>%
  mutate(Dip = select(., DipS:DipL) %>% rowSums(na.rm = TRUE))

Hoppers <- Hoppers %>%
  mutate(Ant = select(., AntS:AntL) %>% rowSums(na.rm = TRUE))

Hoppers <- Hoppers %>%
  mutate(Oth = select(., OthS:OthL) %>% rowSums(na.rm = TRUE))

# Habitat type wild and Harvest NO is significant when it comes to abundance (Total). I can't put both in the same model
abun <- lme4::glmer.nb(Total ~ Harvest + (1|Tower), data = Hoppers)
summary(abun)

# Harvest is not important to specific group types, but wild habitat type is important to abundance of Orthopters, Dipters and Ants
abun <- lme4::glmer.nb(Orth ~ Harvest + (1|Tower), data = Hoppers)
summary(abun)

# Diversity does not depend on anything but the person collecting data... albert saw more stuff than anyone else.

abun <- lme4::glmer.nb(Total ~ Harvest + (1|Tower), data = Hoppers)
summary(abun)


abun <- lme4::glmer.nb(Total ~ DayNum + (1|Tower), data = Hoppers)
abun <- lme4::glmer.nb(Total ~ Harvest + (1|Tower), data = Hoppers)


summary(abun)


