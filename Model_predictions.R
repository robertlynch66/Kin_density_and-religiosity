## Predictions
library(tidybayes)
library(tidyverse)
library(brms)
library(readr)
# get data
setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh")
newdata <- read_csv("newdata.csv")
options(scipen=999)

# get model (Model 1 - total NW size)
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/NW_total_lognormal.rds")

get_variables(M1)


# ## posterior predictive checks
# pp <- predict(fit)
# head(pp)

## predict response for new data
pr <- c( 0.25, 0.5, 0.75)

quantile(newdata$familyBariReligiousAfter,  probs = pr)


# new df
attach(newdata)
data <- data.frame(kids_in_hh = mean(kids_in_hh,na.rm=T),
                   R_NUM_SIBS = mean(R_NUM_SIBS,na.rm=T),
                   religion = mean(religion,na.rm=T),
                   familyBariReligiousAfter=c(-1,1),
                   religious_knowledge_scale=mean(religious_knowledge_scale,na.rm=T),
                   MI_geo_proximity=mean(MI_geo_proximity,na.rm=T),
                   MI_economic_capital=mean(MI_economic_capital,na.rm=T),
                   MI_human_capital=mean(MI_human_capital,na.rm=T))

detach(newdata)



# predictions


fitted(M1, newdata = data, probs = c(0.05, 0.95))
