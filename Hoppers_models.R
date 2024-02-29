# for when i really need it
source("libraries.R")
# from work
setwd("C:/Users/a.bateman/Documents/GITHUB/Trans_anal")
# from home
setwd("~/PhD/PhD/projects/transect/Trans_anal/Trans_anal")


Hoppers <- read.table("Monitoring_sheet.txt", header = TRUE)


install.packages("ggplot2")
install.packages("lme4")

library(ggplot2)
library(lme4)

