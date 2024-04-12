
# from work
setwd("C:/Users/a.bateman/Documents/GITHUB/Trans_anal")
# from home
setwd("~/PhD/PhD/projects/transect/Trans_anal")


# libraries for hoppers and jacks
source("libraries.R")

# jack doc
Jack <- read.csv("Jackdawta.csv", sep=";", stringsAsFactors=TRUE)

# see structure of data
str(Jack)

# add julian date
Jack$dat<- as.Date(Jack$data, "%d/%m/%Y")
Jack$julian <- as.numeric(format(Jack$dat, "%j"))

Jackpoll <- subset(Jack, edat != "adult" & edat!="ou" & edat!="poll" & metalica_esq != "NA")

# First order chicks based on 1st day of measure within each nest.
Jackpoll <- Jackpoll %>% 
  arrange(idCaixaNiu, julian)
str(Jackpoll)
# order of birth based on date they were first weighed
Jackpoll <- Jackpoll %>%
    group_by(idCaixaNiu) %>%
    mutate(order_birth = ifelse(edat == "1_dia", row_number(), NA)) 


# Order chicks based on 1st day of measure within each nest and same id (different measures of same individual)
Jackpoll <- Jackpoll %>% 
  arrange(idCaixaNiu, metalica_esq)

Jackpoll <- Jackpoll %>%
  arrange(idCaixaNiu, metalica_esq, julian) %>%
  group_by(idCaixaNiu, metalica_esq) %>%
  mutate(order_birth = na.locf(order_birth, na.rm = FALSE))

# Assign age to chicks
Jackpoll.na <- subset(Jackpoll, order_birth != "NA" & edat != "3_dies" & order_birth <=6)

Jackpoll.na <- Jackpoll.na %>%
  group_by(metalica_esq) %>%
  mutate(AgeAtWeighing = as.numeric(julian - min(julian)))

Jackpoll.na <- subset(Jackpoll.na, metalica_esq != "L007463")
## mean and sd to plot

sum_stats <- Jackpoll.na %>%
  group_by(order_birth, AgeAtWeighing) %>%
  summarise(mean_pes = mean(pes), SD_pes = sd(pes))

sum_stats <- subset(sum_stats, SD_pes != "NA")
# Plot the growth curve with mean and standard deviation
ggplot() +
  geom_line(data = sum_stats, aes(x = AgeAtWeighing, y = mean_pes,  color = as.factor(order_birth)), size = 1, linetype = "solid") +
  geom_ribbon(data = sum_stats, aes(x = AgeAtWeighing, ymin = mean_pes - SD_pes, ymax = mean_pes + SD_pes), alpha = 0.3, fill="grey") +
  labs(x = "Age at Weighing (days)", y = "Weight (grams)", color = "Order of Birth") +
  ggtitle("Chick Growth Curve by Order of Birth")

## analyse: the order of birth does matter in jackdaws weight

p<-lmer(pes ~ order_birth + (1|metalica_esq) + (1|AgeAtWeighing), data = Jackpoll.na)
summary(p)
anova(p)

# growth rate
Jackpoll.na <- Jackpoll.na %>%
  group_by(metalica_esq) %>%
  mutate(WeightDiff = c(0, diff(pes)),   # Calculate weight difference between consecutive weighings
         DaysDiff = c(0, diff(AgeAtWeighing)),  # Calculate time difference between consecutive weighings
         GrowthRate = ifelse(DaysDiff > 0, WeightDiff / DaysDiff, 0))  # Calculate growth rate (grams per day)

sum_stats <- Jackpoll.na %>%
  group_by(AgeAtWeighing) %>%
  summarise(mean_pes = mean(pes), SD_pes = sd(pes))
J <- subset(Jackpoll.na, idEstructura == "ValldelsPous")

# plot growth rate

  ggplot() +
    geom_line()+
    geom_smooth(data=Jackpoll.na, aes(x = AgeAtWeighing, y = GrowthRate), method = "loess", se = FALSE, color = "red", size = 1) +  # Adding growth rate trend line
    labs(x = "Age at Weighing (days)", y = "Increase in weight (grams/day)") +
    ggtitle("Chick Growth Rate Curve") +
    theme_minimal()


st2 <- subset(Jackpoll.na, ChickPeriod == "Stage 2")

ggplot() +
  geom_point(data=Jackpoll.na, aes(x = julian, y = AgeAtWeighing))+  
  labs(x = "Day", y = "AgeAtWeighing") +
  ggtitle("") +
  theme_minimal()


### growth periods
  assign_period <- function(age) {
    if (age >= 0 & age <= 7) {
      return("Stage 1")
    } else if (age >= 8 & age <= 14) {
      return("Stage 2")
    } else if (age >= 15 & age <= 21) {
      return("Stage 3")
    } else {
      return(NA)
    }
  }
  
  # Add new column for period intervals
  Jackpoll.na <- Jackpoll.na %>%
    mutate(ChickPeriod = sapply(AgeAtWeighing, assign_period))
p <- lmer(GrowthRate ~ idEstructura + (1|metalica_esq) + (1|ChickPeriod), data=Jackpoll.na)
summary(p) 
anova(p)

## plot age chicks of diff ages at weighing day. SEE THAT MOST STAGE 2 (AGE 7-14) FALL INTO DAYS 150-155, PEAK ABUNDANCE IN ORTHOPTERS.
sum_orth <- Hopper %>%
  group_by(julian) %>%
  summarise(mean_orth = mean(Orth), SD_Orth = sd(Orth))


ggplot() +
  geom_point(data=Jackpoll.na, aes(x = julian, y = AgeAtWeighing))+  
  geom_line(data=sum_orth, aes(x=julian,y=mean_orth))
  labs(x = "Day", y = "AgeAtWeighing") +
  ggtitle("") +
  theme_minimal()
  

ggplot() +
    geom_point(data=Jackpoll.na, aes(x = julian, y = AgeAtWeighing), color = "red")+  
    geom_point(data=Hopper, aes(x=julian,y=Orth), color = "blue")
  labs(x = "Day", y = "AgeAtWeighing") +
    ggtitle("") +
    theme_minimal()

  
  
  p <- ggplot(Jackpoll.na, aes(x = as.factor(julian))) +
    geom_bar(aes(fill = factor(AgeAtWeighing)), position = "stack") +
    labs(title = "Distribution of Chicks at different ages",
         x = "Julian Days",
         y = "Number of Chicks") +
    theme_minimal() +
    theme(legend.position = "top") +
    scale_fill_discrete(name = "Age")
  p
  # Add secondary y-axis for 'other_variable'
  p + geom_line(data = sum_orth, aes(x = julian, y = mean_orth), color = "red") +
    scale_y_continuous(sec.axis = sec_axis(, name = "Orthopter abundance"))
  