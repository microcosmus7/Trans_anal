
# from work
setwd("C:/Users/a.bateman/Documents/GITHUB/Trans_anal")
# from home
setwd("~/PhD/PhD/projects/transect/Trans_anal")


# libraries for hoppers and jacks
source("libraries.R")

#################### let's first arrange the data so we can use all added variables if needed #######################

#### upload needed docs - first jackdaw data
# jack doc
Jack <- read.csv("Jackdawta.csv", sep=";", stringsAsFactors=TRUE)  

# see structure of data
str(Jack)

# add julian date
Jack$dat<- as.Date(Jack$data, "%d/%m/%Y")
Jack$julian <- as.numeric(format(Jack$dat, "%j"))

# Separate the eggs of jackdaws
Jackou <- subset(Jack,edat == "ou" & pes>0 & volum_ou>0)
plot(Jackou$pes ~ Jackou$volum_ou)

str(Jackou)

# DISTRIBUTION OF WEIGHT SEEMS TO BE NORMAL
descdist( data = Jackou$pes , discrete = T) # if you have continuous data --> discrete = F. The closest the observation is to a distributions, the more likely it's to show that distribution.
descdist(data = Jackou$pes, discrete = T, boot=1000)

#### Add a column for the order of the eggs within each nest
## first arrange data
Jackou <- Jackou %>%
  arrange(idCaixaNiu, julian)

## then add order of eggs based on clutch
Jackou <- Jackou %>% 
  group_by(idCaixaNiu) %>% 
  mutate(egg_order = row_number())

## we add a column that describes the certainty of this order. We add uncertain to those eggs that 
### belong to the same nest and were measured on the same day.

Jackou <- Jackou %>%
  group_by(idCaixaNiu, julian) %>%
  mutate(certainty = ifelse(n() > 1, "uncertain", "certain"))


## Get a CLUTCH SIZE for each nest
Jackou <- Jackou %>%
  group_by(idCaixaNiu) %>%
  mutate(clutch_size = n_distinct(pes))

## get the NUMBER OF NESTS per tower
Jackou <- Jackou %>%
  group_by(idEstructura) %>%
  mutate(colony_size = n_distinct(idCaixaNiu))

## 
# Sort the data by measurement date
Jackou <- Jackou %>% arrange(idCaixaNiu, julian)

# Group the data by clutch ID
Jackou <- Jackou %>% group_by(idCaixaNiu)

# Calculate laying date for each clutch
Jackou <- Jackou %>% 
  mutate(laying_date = min(julian))

# Calculate mean laying date for each clutch
Jackou <- Jackou %>%
  group_by(idEstructura) %>%
  mutate(mean_laying_date = mean(laying_date))

# Calculate synchrony values for each clutch within tower, we see the difference between laying date of the nest (first layed egg) and mean laying date of the whole nesting tower.
Jackou <- Jackou %>%
  mutate(synchrony_tower = laying_date - mean_laying_date)

# Calculate synchrony values for each egg within clutch, we see the difference between the laying date of each egg and the mean laying date of the nesting tower.
## maybe I should consider the difference beteween the laying date of each egg and the laying date of the first egg. But it wouldn't make sense if these are layed one per day.
Jackou <- Jackou %>%
  mutate(synchrony_nest = julian - mean_laying_date)

View(Jackou)

#### upload needed docs - time data

# Read the TIME sheet into a data frame
Time <- read.csv("Time_arrival.csv", sep=";", stringsAsFactors=TRUE)

# add julian date
Time$dat<- as.Date(Time$data, "%d/%m/%Y")
Time$julian <- as.numeric(format(Time$dat, "%j"))

## filter to get only 2023
Time <- subset(Time, Time$any == 2023)

# Merge the two data frames based on towerID and date
Jackou_t <- merge(Jackou, Time, by = c("julian", "idEstructura"))

## delete columns that are not needed
Jackou_t <- subset(Jackou_t, select = -c(id_Social,any.y,data.y,X,X.1,X.2,X.3,X.4,X.5,X.6,X.7,dat.y))

##first convert time to character
Jackou_t <- Jackou_t %>%
  mutate(hora_c = as.character(hora),  # Convert time to character
         hora_c = sub("^([0-9]):", "0\\1:", hora_c))  # Add leading zero if hour has one digit
print(Jackou_t$hora_c)

# Function to convert time string to minutes past midnight
time_to_minutes <- function(time_str) {
  hours <- as.numeric(substr(time_str, 1, 2))  # Extract hours
  minutes <- as.numeric(substr(time_str, 4, 5))  # Extract minutes
  total_minutes <- hours * 60 + minutes
  return(total_minutes)
}

# Apply the function to the 'time' column
Jackou_t$hora_cont <- unlist(lapply(Jackou_t$hora_c, time_to_minutes))

View(Jackou_t)

assign_time_label <- function(time_str) {
  ifelse(time_str < "07:30", "before_early_morning",
         ifelse(time_str < "11:00", "early_morning",
                ifelse(time_str < "13:00", "morning",
                       ifelse(time_str < "15:00", "midday",
                              ifelse(time_str < "17:00", "afternoon",
                                     ifelse(time_str < "20:00", "late_afternoon", "evening"))))))
}
# Apply the custom function to assign labels to time intervals
Jackou_t <- Jackou_t %>%
  mutate(categoria_hora = assign_time_label(hora_c))

View(Jackou_t)

########### SIMPLE WEIGHT - VOLUME RELATIONSHIP ##############
#### weight is related to volume, little space for location or nest effect, but more nest effect than location.

uu <-lmer(pes ~ volum_ou + (1|idCaixaNiu) + (1|idEstructura), data=Jackou)

summary(uu)
rsq(uu)

#### does weight depend on laying date?

uu <- lmer(pes ~ julian + (1|idCaixaNiu) + (1|idEstructura), data=Jackou) # nesting idCaixaNiu within idEstructura is NOT correct
summary(uu)
rsq(uu)


############# THERE ARE DIFFERENCES BETWEEN TOWERS ###################

### we see that cadolles has heavier eggs than the other towers, but valldelsPous. 
fi <- lm(pes ~ idEstructura, data=Jackou)
summary(fi)

f <- lmer(pes ~ idEstructura + (1|hora_cont), data=Jackou_t)
summary(f)
## although there is some variation due to time (as a time category or as number of minutes), when categorized it has no variation explained by it.
#### creating summary for mean and sd of egg WEIGHT
egg_summary <- Jackou %>%
  group_by(idEstructura) %>%
  summarise(mean_pes = mean(pes),
            sd_pes = sd(pes) / sqrt(n()))

# Create the boxplot with standard deviation bars - WEIGHT
ggplot(egg_summary, aes(x = idEstructura, y = mean_pes)) +
  geom_boxplot() +
  geom_errorbar(aes(ymin = mean_pes - sd_pes, 
                    ymax = mean_pes + sd_pes),
                width = 0.2, 
                position = position_dodge(width = 0.75)) +
  labs(x = "Nesting Tower", y = "Mean Egg Weight") +
  ggtitle("Mean Egg Weight per Nesting Tower with Standard Error Bars")

#### creating summary for mean and sd of egg VOLUME
egg_summary <- Jackou %>%
  group_by(idEstructura) %>%
  summarise(mean_vol = mean(volum_ou),
            sd_vol = sd(volum_ou) / sqrt(n()))

# Create the boxplot with standard deviation bars - VOLUME
ggplot(egg_summary, aes(x = idEstructura, y = mean_vol)) +
  geom_boxplot() +
  geom_errorbar(aes(ymin = mean_vol - sd_vol, 
                    ymax = mean_vol + sd_vol),
                width = 0.2, 
                position = position_dodge(width = 0.75)) +
  labs(x = "Nesting Tower", y = "Mean Egg Volume") +
  ggtitle("Mean Egg Volume per Nesting Tower with Standard Error Bars")


#### creating summary for mean and sd of egg DENSITY (JUST PLAYING)
egg_summary <- Jackou %>%
  group_by(idEstructura) %>%
  summarise(mean_dens = mean(pes/volum_ou),
            sd_dens = sd(pes/volum_ou) / sqrt(n()))

# Create the boxplot with standard deviation bars - DENSITY
ggplot(egg_summary, aes(x = idEstructura, y = mean_dens)) +
  geom_boxplot() +
  geom_errorbar(aes(ymin = mean_dens - sd_dens, 
                    ymax = mean_dens + sd_dens),
                width = 0.2, 
                position = position_dodge(width = 0.75)) +
  labs(x = "Nesting Tower", y = "Mean Egg Density") +
  ggtitle("Mean Egg Density per Nesting Tower with Standard Error Bars")

######################### WHY? IS IT TIME OF MEASUREMENT (WEIGHT)? ########################

# see whether time affects egg weight. What should be random? tower and nest. What should be fixed? time
fit <- lmer(pes ~ categoria_hora + (1|idEstructura), data = Jackou_t)
summary(fit)

Ja <- subset(Jackou_t, Jackou_t$egg_order==5)

fit <- lmer(pes ~ hora_cont + (1|idEstructura), data = Ja)
summary(fit)


############################## EGG ORDER ###########################

###### egg order, is this smth to consider?

## all eggs (first, seconds, thirds, etc.) seem to have the same relationship (visually speaking)
j<-subset(Jackou_certain, egg_order == 5)
plot(j$pes ~ j$volum_ou)


## we create a dataframe without uncertainty.
Jackou_certain <- subset(Jackou, Jackou$certainty != "uncertain")

## model to see whether order affects weight (is there some variance explained by laying date?)
fit1 <- lmer(pes ~ egg_order + (1|idCaixaNiu) + (1|idEstructura), data = Jackou_certain)
fit2 <- lmer(pes ~ egg_order + julian + (1|idCaixaNiu) + (1|idEstructura), data = Jackou_certain)
fit3 <- lmer(pes ~ egg_order + (1|julian) + (1|idCaixaNiu) + (1|idEstructura), data = Jackou_certain)
summary(fit)
AIC(fit1,fit2,fit3)

## model to see order effect on volume
fit1 <- lmer(volum_ou ~ egg_order + (1|idCaixaNiu) + (1|idEstructura), data = Jackou_certain)
fit2 <- lmer(volum_ou ~ egg_order + julian + (1|idCaixaNiu) + (1|idEstructura), data = Jackou_certain)
fit3 <- lmer(volum_ou ~ egg_order + (1|julian) + (1|idCaixaNiu) + (1|idEstructura), data = Jackou_certain)
summary(fit)
AIC(fit1,fit2,fit3)

## make two categories of egg laying order
Jackou_certain$egg_order2 <- ifelse(Jackou_certain$egg_order==1, "First",
                            ifelse(Jackou_certain$egg_order==5,"Second",NA))


## does order explain volume?

fit2 <- lmer(volum_ou ~ egg_order2 + julian + (1|idCaixaNiu) + (1|idEstructura), data = Jackou)
summary(fit2)
anova(fit2)

## does volume relate to weight based on tower id when only considering the last egg? 
Jackou_FirstEgg <- subset(Jackou_certain, Jackou_certain$egg_order==1)
fit1 <- lmer(log10(pes) ~ log10(volum_ou)*idEstructura + julian +I(julian^2) + (1|idEstructura), data = Jackou_FirstEgg)
fit2 <- lmer(log10(pes) ~ log10(volum_ou)*idEstructura + julian + (1|idEstructura), data = Jackou_FirstEgg)
fit3 <- lmer(log10(pes) ~ log10(volum_ou)*idEstructura + julian +I(julian^2) + (1|idEstructura), data = Jackou_FirstEgg)
fit4 <- lmer(log10(pes) ~ log10(volum_ou) + idEstructura + julian + I(julian^2) + (1|idEstructura), data = Jackou_FirstEgg)
summary(fit2)
anova(fit2)

AIC(fit2, fit3, fit4) ##fit2 is the best based on AIC values


#### Are there differences in volume, weight and density between the 1st and 5th egg?
## make two categories of egg laying order
Jackou_certain$egg_order2 <- ifelse(Jackou_certain$egg_order==1, "First",
                                    ifelse(Jackou_certain$egg_order==5,"Second",NA))

## does order explain volume?
fit1 <- lmer(volum_ou ~ egg_order2 + (1|idCaixaNiu) + (1|idEstructura), data = Jackou_certain)
fit2 <- lmer(volum_ou ~ egg_order2 + julian + (1|idCaixaNiu) + (1|idEstructura), data = Jackou_certain)
fit3 <- lmer(volum_ou ~ egg_order2 + julian + I(julian^2) + (1|idCaixaNiu) + (1|idEstructura), data = Jackou_certain)
summary(fit3)
anova(fit2)

AIC(fit1, fit2, fit3)

## does order explain weight?
fit1 <- lmer(pes ~ egg_order2 + (1|idCaixaNiu) + (1|idEstructura), data = Jackou_certain)
fit2 <- lmer(pes ~ egg_order2 + julian + (1|idCaixaNiu) + (1|idEstructura), data = Jackou_certain)
fit3 <- lmer(pes ~ egg_order2 + julian + I(julian^2) + (1|idCaixaNiu) + (1|idEstructura), data = Jackou_certain)
summary(fit1)
summary(fit2)
summary(fit3)
anova(fit2)

AIC(fit1, fit2, fit3)

## does order explain density? maybe i'm not doing this right...
fit1 <- lmer(pes/volum_ou ~ egg_order2 + (1|idCaixaNiu) + (1|idEstructura), data = Jackou_certain)
fit2 <- lmer(pes/volum_ou ~ egg_order2 + julian + (1|idCaixaNiu) + (1|idEstructura), data = Jackou_certain)
fit3 <- lmer(pes/volum_ou ~ egg_order2 + julian + I(julian^2) + (1|idCaixaNiu) + (1|idEstructura), data = Jackou_certain)
summary(fit3)
anova(fit2)

AIC(fit1, fit2, fit3)

################################## CLUTCH SIZE ########################################

## I want to know whether larger clutch sizes affect egg weight and volume. 
##AND if so, whether it affects differently depending on the order of the egg; do larger clutches have more even weight among eggs or do 1st and last egg differ more in size?

fit1 <- lmer(pes ~ clutch_size + (1|idCaixaNiu) + (1|idEstructura), data = Jackou)
fit2 <- lmer(pes ~ clutch_size + julian + (1|idCaixaNiu) + (1|idEstructura), data = Jackou)
fit3 <- lmer(pes ~ clutch_size + julian + I(julian^2) + (1|idCaixaNiu) + (1|idEstructura), data = Jackou)

summary(fit1)
summary(fit2)
summary(fit3)
AIC(fit1, fit2, fit3) ## fit2 is the better fit
## egg weight increases with clutch size, and decreases with date

plot(Jackou$pes ~ Jackou$clutch_size)

fit1 <- lmer(volum_ou ~ clutch_size + (1|idCaixaNiu) + (1|idEstructura), data = Jackou)
fit2 <- lmer(volum_ou ~ clutch_size + julian + (1|idCaixaNiu) + (1|idEstructura), data = Jackou)
fit3 <- lmer(volum_ou ~ clutch_size + julian + I(julian^2) + (1|idCaixaNiu) + (1|idEstructura), data = Jackou)

summary(fit1)
summary(fit2)
summary(fit3)
AIC(fit1, fit2, fit3) ## fit2 is the better fit
## egg volume increases with clutch size, and decreases with date

## does this vary with only eggs we are certain of?
Jackou_certain <- subset(Jackou, Jackou$certainty != "uncertain")

fit1 <- lmer(pes ~ clutch_size + (1|idCaixaNiu) + (1|idEstructura), data = Jackou_certain)
fit2 <- lmer(pes ~ clutch_size + julian + (1|idCaixaNiu) + (1|idEstructura), data = Jackou_certain)
fit3 <- lmer(pes ~ clutch_size + julian + I(julian^2) + (1|idCaixaNiu) + (1|idEstructura), data = Jackou_certain)

summary(fit1)
summary(fit2)
summary(fit3)
AIC(fit1, fit2, fit3) ## fit2 is the better fit
## same results...

## does this depend on the egg_order? Whether the clutch size affects weight differently based on the order the egg was layed -- dont know how to test this. 

## make two categories of egg laying order
Jackou_certain$egg_order2 <- ifelse(Jackou_certain$egg_order==1, "First",
                                    ifelse(Jackou_certain$egg_order==5,"Second",NA))

fit1 <- lmer(pes ~ clutch_size*egg_order2 + (1|idCaixaNiu) + (1|idEstructura), data = Jackou_certain)
fit2 <- lmer(pes ~ clutch_size + egg_order2 + (1|idCaixaNiu) + (1|idEstructura), data = Jackou_certain)
AIC(fit1,fit2)
summary(fit2)
################################## COLONY SIZE #########################################
## Does the number of pairs in a same tower affect the egg size and weight within each tower?


## so, does colony size affect egg weight?
fit1 <- lmer(volum_ou ~ colony_size + (1|idEstructura), data = Jackou)

summary(fit1)
AIC(fit1,fit2)

### no, but does it affect synchrony?
fit1 <- lmer(synchrony_nest ~ colony_size + (1|idEstructura), data = Jackou)
summary(fit1)

################################## SYNCHRONY ###################################
## figuring this shit out, how can i see whether synchrony is a factor to consider?

### FIRST, I need a value that describes this synchrony: meaning how far away from the mean (in days) is each 1st egg layed. Values can be negative (X days before the mean of the tower) or positive (X days after the mean of the tower)
### Thus, I need to create a mean per tower, and then calculate the difference of LD per nest to their own tower mean.

## now we see whether synchrony affects weight of eggs
# scaling synchrony variable does not help

## synchrony does not affect weight, volume or density of eggs

fit <- lmer(pes ~  synchrony_nest + (1|idCaixaNiu) + (1|idEstructura), data = Jackou)
summary(fit)

### however, lets see if synchrony affects W, V adn D when only considering the first egg

Jackou_1 <- subset(Jackou, Jackou$egg_order == 1)

fit <- lmer(pes ~ synchrony_nest + (1|idEstructura), data = Jackou_1)
summary(fit)

## no effect... so...
## what other questions can synchrony of laying date answer?
#### for example, colony size may lead to greater synchrony in laying... we could test whether this is also true in hatching dates...









##########################  DONT REMEMBER WHAT THIS IS... ############################
ggplot(synchrony_summary, aes(x = julian, y = mean_pes, fill=idEstructura)) +
  geom_boxplot() +
  geom_errorbar(aes(ymin = mean_pes - sd_pes, 
                    ymax = mean_pes + sd_pes),
                width = 0.2, 
                position = position_dodge(width = 0.75)) +
  labs(x = "Laying Date", y = "Mean Egg weight") +
  ggtitle("Mean Egg weight per Nesting Tower with Standard Error Bars")


ggplot(synchrony_summary, aes(x = julian, y = mean_pes, fill = idEstructura)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Eggs Measured per Tower per Day",
       x = "Date",
       y = "Number of Eggs",
       fill = "Tower ID") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_discrete(name = "Tower ID") +
  guides(fill = guide_legend(title.position = "top"))


J <- subset(Jackou,idEstructura =="TossalRodo")
# Plot scatterplot
ggplot(J, aes(x = egg_order, y = volum_ou, color = idCaixaNiu)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Egg Order", y = "Volume") +
  ggtitle("Weight based on Order of Eggs within Nests") +
  theme_minimal()

clutch_size_data <- Jackou %>%
  group_by(idCaixaNiu) %>%
  summarise(clutch_size = n())

# Merge the clutch size data back to the original dataset
Jackou <- left_join(Jackou, clutch_size_data, by = "idCaixaNiu")

fit <- lm(volum_ou ~ clutch_size,data = Jackou)
summary(fit)

plot(Jackou$volum_ou~Jackou$clutch_size)

############# DISTRIBUTION OF LAYING DATES ###################

J <- subset(Jackou, Jackou$certainty != "uncertain")
Ja <- subset(J, J$egg_order==1)



total <- aggregate(egg_order ~ julian, data = Ja, FUN = sum)
View(total)
barplot(total$egg_order,names.arg = total$julian, 
     main = "Total Eggs Measured Per Day",
     xlab = "Total Eggs",
     ylab = "Frequency",
     col = "skyblue",
     border = "black")
hist(J$egg_order,J$julian)