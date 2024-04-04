
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

# Separate the eggs of jackdaws
Jackou <- subset(Jack,edat == "ou" & pes>0 & volum_ou>0)
plot(Jackou$pes ~ Jackou$volum_ou)

str(Jackou)

# DISTRIBUTION OF WEIGHT SEEMS TO BE NORMAL
descdist( data = Jackou$pes , discrete = T) # if you have continuous data --> discrete = F. The closest the observation is to a distributions, the more likely it's to show that distribution.
descdist(data = Jackou$pes, discrete = T, boot=1000)

#### weight is related to volume, little space for location or nest effect, but more nest effect than location.

uu <-lmer(pes ~ volum_ou + (1|idCaixaNiu) + (1|idEstructura), data=Jackou)

summary(uu)
rsq(uu)

#### does weight depend on laying date?

uu <- lmer(pes ~ julian + (1|idCaixaNiu) + (1|idEstructura), data=Jackou) # i don't know if nesting idCaixaNiu is correct
summary(uu)
rsq(uu)


############# THERE ARE DIFFERENCES BETWEEN TOWERS ###################

### we see that cadolles has heavier eggs than the other towers, but valldels pous. 
fi <- lm(pes/volum_ou ~ idEstructura, data=Jackou)
summary(fi)


#### creating summary for mean and sd of egg weight
egg_summary <- Jackou %>%
  group_by(idEstructura) %>%
  summarise(mean_pes = mean(pes),
            sd_pes = sd(pes) / sqrt(n()))

# Create the boxplot with standard deviation bars
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

# Create the boxplot with standard deviation bars VOLUME
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

# Create the boxplot with standard deviation bars DENSITY
ggplot(egg_summary, aes(x = idEstructura, y = mean_dens)) +
  geom_boxplot() +
  geom_errorbar(aes(ymin = mean_dens - sd_dens, 
                    ymax = mean_dens + sd_dens),
                width = 0.2, 
                position = position_dodge(width = 0.75)) +
  labs(x = "Nesting Tower", y = "Mean Egg Density") +
  ggtitle("Mean Egg Density per Nesting Tower with Standard Error Bars")


######################### WHY? IS IT TIME OF MEASUREMENT (WEIGHT)? ########################
## we don't have time, we have to add it from some other sheet

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
Jackou_t <- subset(Jackou_t, select = -c(id_Social,any.y,data.y,X,X.1))
Jackou_t <- subset(Jackou_t, select = -c(X.2,X.3,X.4,X.5,X.6))
Jackou_t <- subset(Jackou_t, select = -c(X.7,dat.y))

##first convert time to character
Jackou_t$hora_c <- as.character(Jackou_t$hora)

# Function to convert time string to minutes past midnight
time_to_minutes <- function(time_str) {
  time_parts <- strsplit(time_str, ":")[[1]]
  hours <- as.numeric(time_parts[1])
  minutes <- as.numeric(time_parts[2])
  seconds <- as.numeric(time_parts[3])
  total_minutes <- hours * 60 + minutes
  return(total_minutes)
}

# Function to convert time string to minutes past midnight
time_to_minutes <- function(time_str) {
  hours <- as.numeric(substr(time_str, 1, 2))  # Extract hours
  minutes <- as.numeric(substr(time_str, 4, 5))  # Extract minutes
  total_minutes <- hours * 60 + minutes
  return(total_minutes)
}
# Apply the function to the 'time' column
Jackou_t$hora_cont <- unlist(lapply(Jackou_t$hora, time_to_minutes))
# see whether time affects egg weight. What should be random? tower and nest. What should be fixed? time
fit <- lmer(pes ~ hora_cont + (1|idCaixaNiu) + (1|idEstructura), data = Jackou_t)
summary(fit)

Ja <- subset(Jackou_t, Jackou_t$egg_order==1)

fit <- lmer(pes ~ hora_cont + (1|idEstructura), data = Ja)
summary(fit)









## does egg quality vary based on synchrony of the tower?

###### egg order, is this smth to consider?

# Add a column for the order of the eggs within each nest

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

# View the updated dataframe
View(Jackou)


## we create a dataframe without uncertainty.
Jackou_certain <- subset(Jackou, Jackou$certainty != "uncertain")

## model to see whether order affects the volume or weight
fit <- lmer(volum_ou ~ egg_order + julian + (1|idCaixaNiu) + (1|idEstructura), data = Jackou)
summary(fit)

## model to see whether only the certain order has same effect

fi <- lmer(volum_ou ~ egg_order*idEstructura + (1|idCaixaNiu) + (1|idEstructura), data = Jackou_certain)
summary(fi)

## figuring this shit out, how can i see whether synchrony is a factor to consider?

### FIRST, I need a value that describes this synchrony: meaning how far away from the mean (in days) is each 1st egg layed. Values can be negative (X days before the mean of the tower) or positive (X days after the mean of the tower)
### Thus, I need to create a mean per tower, and then calculate the difference of LD per nest to their own tower mean.

# Sort the data by measurement date
Jackou_syn <- Jackou %>% arrange(idCaixaNiu, julian)

# Group the data by clutch ID
Jackou_syn <- Jackou_syn %>% group_by(idCaixaNiu)

# Calculate laying date for each clutch
Jackou_syn <- Jackou_syn %>% 
  mutate(laying_date = min(julian))

# Calculate mean laying date for each clutch
Jackou_syn <- Jackou_syn %>%
  group_by(idEstructura) %>%
  mutate(mean_laying_date = mean(laying_date))

# Calculate synchrony values for each egg within clutch
Jackou_syn <- Jackou_syn %>%
  mutate(synchrony = laying_date - mean_laying_date)

View(Jackou_syn)

## success, finally
## now we see whether synchrony affects weight of eggs
# scaling synchrony variable does not help

## synchrony does not affect weight, volume or density of eggs

fit <- lmer(pes ~ volum_ou + synchrony + (1|idCaixaNiu) + (1|idEstructura), data = Jackou_syn)
summary(fit)

### however, lets see if synchrony affects W, V adn D when only considering the first egg

Jackou_syn_1 <- subset(Jackou_syn, Jackou_syn$egg_order == 1)

fit <- lmer(pes ~ synchrony + (1|idEstructura), data = Jackou_syn_1)
summary(fit)

## no effect... so...
## what other questions can synchrony of laying date answer?








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


