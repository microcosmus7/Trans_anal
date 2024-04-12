install.packages("installr")
library(installr)
update()

check.for.updates.R()

updateR(
  fast = FALSE,
  browse_news = TRUE,
  install_R = TRUE,
  copy_package = FALSE,
  copy_Rprofile.site = TRUE,
  keep_old_packages = FALSE,
  update_packages = TRUE
)

install.packages("fitdistrplus")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("lme4")
install.packages("dplyr")
install.packages("DHARMa")
install.packages("mosaic")
install.packages("rsq")
install.packages("zoo")
install.packages("pbkrtest")
install.packages("lmerTest")

library(mosaic)
library(ggplot2)
library(lme4)
library(dplyr)
library(DHARMa)
library(lubridate) #work with dates
library(fitdistrplus)
library(rsq)
library(zoo)
library(pbkrtest)
library(lmerTest)

install.packages("lme4", type = "source")
install.packages("Matrix")

library(Matrix)
library(lme4)


