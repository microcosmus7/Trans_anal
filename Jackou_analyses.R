# for when i really need it
source("libraries.R")
# from work
setwd("C:/Users/a.bateman/Documents/GITHUB/Trans_anal")
# from home
setwd("~/PhD/PhD/projects/transect/Trans_anal/Trans_anal")
 
Jack <- read.csv("Jackdawta.csv", sep=";", stringsAsFactors=TRUE)  

# see structure of data
str(Jack)

# add julian date
Jack$dat<- as.Date(Jack$data, "%d/%m/%Y")
Jack$julian <- yday(Jack$dat)
Jack$julian <- as.numeric(format(Jack$dat, "%j"))

# Separate the eggs of jackdaws
Jackou <- subset(Jack,edat == "ou" & pes>0 & volum_ou>0)
plot(Jackou$pes ~ Jackou$volum_ou)

str(Jackou)

# DISTRIBUTION OF WEIGHT SEEMS TO BE NORMAL
descdist( data = Jackou$pes , discrete = T) # if you have continuous data --> discrete = F. The closest the observation is to a distributions, the more likely it's to show that distribution.
descdist(data = Jackou$pes, discrete = T, boot=1000)

#### weight is related to volume, little space for location or nest effect, but more nest effect than location.

uu <-lmer(pes ~ volum_ou + (1|idCaixaNiu), data=Jackou)
summary(uu)
rsq(uu)

#### 

### we see that cadolles has heavier eggs than the other towers, but valldels pous. 
fi <- lm(pes ~ idEstructura, data=Jackou)
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

###### egg order, is this smth to consider?

# First order eggs based on laying date within each nest.
Jackou <- Jackou %>% 
  arrange(idCaixaNiu, julian)

# Add a column for the order of the eggs within each nest
Jackou <- Jackou %>% 
  group_by(idCaixaNiu) %>% 
  mutate(egg_order = row_number())

# View the updated dataframe
View(Jackou)

## model to see order
fit <- lmer(volum_ou ~ egg_order + (1|idCaixaNiu) + (1|idCaixaNiu/idEstructura),data = Jackou)
summary(fit)


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


Jackpoll <- subset(Jack, edat != "adult" & edat!="ou" & edat!="poll")

plot(Jackpoll$pes~Jackpoll$julian)

p<-lmer(pes ~ julian + (1|id_individu) + (1|id_individu/idCaixaNiu) , data = Jackpoll)
summary(p)
rsq(p)
