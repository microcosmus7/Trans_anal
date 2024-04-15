# for when i really need it
source("libraries.R")
# from work
setwd("C:/Users/a.bateman/Documents/GITHUB/Trans_anal")
# from home
setwd("~/PhD/PhD/projects/transect/Trans_anal/Trans_anal")

Hoppers <- read.csv("Hoppers_a.csv", sep=";", stringsAsFactors=TRUE)
View(Hoppers)

install.packages("ggplot2")
install.packages("lme4")
install.packages("dplyr")

library(ggplot2)
library(lme4)
library(dplyr)


############ add julian date ##############
Hoppers$dat<- as.Date(Hoppers$Date, "%d/%m/%Y")
Hoppers$julian <- as.numeric(format(Hoppers$dat, "%j"))




##### first arrange data for total abundance ######
Hoppers <- Hoppers %>%
  arrange(Tower, julian)

## summ groups disregarding family and size.
Hoppers <- Hoppers %>%
  mutate(Orth = rowSums(.[, grepl("^Orth", names(.))], na.rm = TRUE))

Hoppers <- Hoppers %>%
  mutate(Orth = rowSums(.[, grepl("^Lep", names(.))], na.rm = TRUE))

Hoppers <- Hoppers %>%
  mutate(Orth = rowSums(.[, grepl("^Col", names(.))], na.rm = TRUE))

Hoppers <- Hoppers %>%
  mutate(Orth = rowSums(.[, grepl("^Dip", names(.))], na.rm = TRUE))

Hoppers <- Hoppers %>%
  mutate(Orth = rowSums(.[, grepl("^Ant", names(.))], na.rm = TRUE))

Hoppers <- Hoppers %>%
  mutate(Orth = rowSums(.[, grepl("^Oth", names(.))], na.rm = TRUE))


############# add time categories ####################
##first convert time to character
Hoppers <- Hoppers %>%
  mutate(hora_c = as.character(Hour),  # Convert time to character
         hora_c = sub("^([0-9]):", "0\\1:", hora_c))  # Add leading zero if hour has one digit
print(Hoppers$hora_c)

# Function to convert time string to minutes past midnight
time_to_minutes <- function(time_str) {
  hours <- as.numeric(substr(time_str, 1, 2))  # Extract hours
  minutes <- as.numeric(substr(time_str, 4, 5))  # Extract minutes
  total_minutes <- hours * 60 + minutes
  return(total_minutes)
}

# Apply the function to the 'time' column
Hoppers$hora_cont <- unlist(lapply(Hoppers$hora_c, time_to_minutes))

View(Hoppers)

assign_time_label <- function(time_str) {
  ifelse(time_str < "09:00", "before_early_morning",
         ifelse(time_str < "11:00", "early_morning",
                ifelse(time_str < "13:00", "morning",
                       ifelse(time_str < "15:00", "midday"))))
}
# Apply the custom function to assign labels to time intervals
Hoppers <- Hoppers %>%
  mutate(categoria_hora = assign_time_label(hora_c))

View(Hoppers)

Hoppers$period <- Hoppers$julian
Hoppers$period <- ifelse((Hoppers$julian<=125), 'A', Hoppers$period)
Hoppers$period <- ifelse((Hoppers$julian>125&Hoppers$julian<=135), 'B', Hoppers$period)
Hoppers$period <- ifelse((Hoppers$julian>135&Hoppers$julian<=145), 'C', Hoppers$period)
Hoppers$period <- ifelse((Hoppers$julian>145&Hoppers$julian<=155), 'D', Hoppers$period)
Hoppers$period <- ifelse((Hoppers$julian>155&Hoppers$julian<=165), 'E', Hoppers$period)
Hoppers$period <- ifelse((Hoppers$julian>165&Hoppers$julian<=175), 'F', Hoppers$period)

# Aggregate the data by date to get the total abundance per day
total <- aggregate(Orth ~ period, data = Hoppers, FUN = sum)

# Create barplot
barplot(total$Orth,names.arg = total$period, 
        main = "Orthopter abundance per period",
        xlab = "Stage period",
        ylab = "Total Orthopter",
        col = "skyblue",
        border = "black")

########### Analyses ################

a <- lme4::glmer.nb(Orth ~ categoria_hora + (1|Tower), data = Hoppers)
b <- lme4::glmer.nb(Orth ~ categoria_hora + period + (1|Tower), data = Hoppers)
c <- lme4::glmer.nb(Orth ~ categoria_hora * period + (1|Tower), data = Hoppers)

summary(c)
anova(a)
AIC(a,b,c)

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


