# for when i really need it
source("libraries.R")
# from work
setwd("C:/Users/a.bateman/Documents/GITHUB/Trans_anal")
# from home
setwd("~/PhD/PhD/projects/transect/Trans_anal/Trans_anal")


Hoppers <- read.table("Hoppers_d.txt", header = TRUE)
Jack <- read.table("Jackdawta.txt", header = TRUE)

install.packages("lubridate")

library(lubridate) #work with dates

View(Hoppers)
View(Jack)


# first set date for R to read it
Hoppers$dat<- as.Date(Hoppers$Date, "%d/%m/%Y")
Jack$dat<- as.Date(Jack$data, "%d/%m/%Y")


# same output, not real julian dates, but I think its the one AINA uses

Hoppers$julian <- yday(Hoppers$dat)
Jack$julian <- yday(Jack$dat)

Hoppers$julian <- as.numeric(format(Hoppers$dat, "%j"))

