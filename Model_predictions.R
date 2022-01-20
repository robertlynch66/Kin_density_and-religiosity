## Predictions
library(tidybayes)
library(tidyverse)
library(brms)
library(readr)
# get data
setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh")
newdata <- read_csv("newdata.csv")
options(scipen=999)

# get model (Model 1 - geo distance rels)
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Geo_distance_relatives_ord_cum.rds")

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

fitted(M1, newdata = data, allow_new_levels=TRUE,probs = c(0.05, 0.95))
# Estimate    Est.Error           Q5        Q95
# [1,] 0.0008212521 0.0005373739 0.0002197001 0.00187677
# [2,] 0.0006250710 0.0004083865 0.0001654345 0.00142693
# 
# , , 2
# 
# Estimate  Est.Error        Q5       Q95
# [1,] 0.8240910 0.04554648 0.7646440 0.8874615
# [2,] 0.7823097 0.05288433 0.7195565 0.8560690
# 
# , , 3
# 
# Estimate  Est.Error         Q5       Q95
# [1,] 0.08233509 0.01954671 0.05461037 0.1083903
# [2,] 0.09911002 0.02121681 0.06868468 0.1259394
# 
# , , 4
# 
# Estimate  Est.Error         Q5       Q95
# [1,] 0.09275262 0.02653899 0.05632592 0.1284119
# [2,] 0.11795524 0.03223492 0.07370512 0.1576271

# get model (Model 1a - geo distance non-rels)
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Geo_distance_non_relatives_ord_cum.rds")

attach(newdata)
data <- data.frame(kids_in_hh = mean(kids_in_hh,na.rm=T),
                   R_NUM_SIBS = mean(R_NUM_SIBS,na.rm=T),
                   familyBariReligiousAfter=c(-1,1),
                   religious_knowledge_scale=mean(religious_knowledge_scale,na.rm=T),
                   MI_geo_proximity=mean(MI_geo_proximity,na.rm=T),
                   MI_economic_capital=mean(MI_economic_capital,na.rm=T),
                   MI_human_capital=mean(MI_human_capital,na.rm=T))

detach(newdata)

fitted(M1, newdata = data, allow_new_levels=TRUE,probs = c(0.05, 0.95))

# Estimate   Est.Error           Q5         Q95
# [1,] 0.007664241 0.006388801 0.0016233080 0.020233672
# [2,] 0.002710597 0.002233679 0.0005599923 0.007027411
# 
# , , 2
# 
# Estimate  Est.Error        Q5       Q95
# [1,] 0.9172739 0.02728843 0.8719638 0.9587967
# [2,] 0.8151791 0.05870173 0.7354705 0.9139174
# 
# , , 3
# 
# Estimate  Est.Error         Q5        Q95
# [1,] 0.05530512 0.02207900 0.02087065 0.09091708
# [2,] 0.12929763 0.04128885 0.05927999 0.18696676
# 
# , , 4
# 
# Estimate  Est.Error          Q5        Q95
# [1,] 0.01975671 0.00889296 0.006705954 0.03504956
# [2,] 0.05281266 0.02071614 0.020468211 0.08508131
# get model (Model 2 - total NW size)
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/NW_total_lognormal.rds")
get_variables(M1)

# new df
attach(newdata)
data <- data.frame(#kids_in_hh = mean(kids_in_hh,na.rm=T),
                   re_formula = NULL,
                   #R_NUM_SIBS = mean(R_NUM_SIBS,na.rm=T),
                   #religion = mean(religion,na.rm=T),
                   familyBariReligiousAfter=c(-1,1),
                   religious_knowledge_scale=mean(religious_knowledge_scale,na.rm=T),
                   MI_geo_proximity=mean(MI_geo_proximity,na.rm=T),
                   MI_economic_capital=mean(MI_economic_capital,na.rm=T),
                   MI_human_capital=mean(MI_human_capital,na.rm=T))

detach(newdata)
# predictions

fitted(M1, newdata = data, probs = c(0.05, 0.95))

# get model (Model 3 - Non-rels in NW)
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/non_relatives_in_NW_neg_bin.rds")

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
# Estimate  Est.Error        Q5      Q95
# [1,] 1.036635 0.11157601 0.8636392 1.229415
# [2,] 1.034894 0.07691263 0.9132790 1.167101

# get model (Model 4 - Rels in NW)
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/relatives_in_NW_lognormal.rds")

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
# Estimate Est.Error       Q5       Q95
# [1,]  7.857436 0.2758194 7.417709  8.321587
# [2,] 10.287373 0.2523866 9.876293 10.702438

# get model (Model 5 - Percentage Rels in NW)
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/percent_relatives_in_NW_lognormal.rds")

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
# Estimate  Est.Error        Q5       Q95
# [1,] 0.8234063 0.01563555 0.7979556 0.8491596
# [2,] 0.8697950 0.01161132 0.8507601 0.8892937

# get model (Model 6 - Emotional support)
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/rels_emot_support_poisson.rds")

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

# Estimate Est.Error       Q5      Q95
# [1,] 4.655604 0.1678519 4.382736 4.932412
# [2,] 6.779583 0.1532954 6.526651 7.034983

# get model (Model 7 - Economic support)
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/rels_econ_help_poisson.rds")

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

fitted(M1, newdata = data, probs = c(0.05, 0.95),allow_new_levels=TRUE) # allow_new_levels=TRUE averages across random effects
# Estimate Est.Error       Q5      Q95
# [1,] 3.089169 0.1467246 2.852014 3.336625
# [2,] 3.033113 0.1016908 2.867596 3.202516


# get model (Model 9 - childcare support)
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/childcare_help_rels_neg_binom.rds")

# new df
attach(newdata)
data <- data.frame(religion = mean(religion,na.rm=T),
                   familyBariReligiousAfter=c(-1,1),
                   religious_knowledge_scale=mean(religious_knowledge_scale,na.rm=T),
                   MI_geo_proximity=mean(MI_geo_proximity,na.rm=T),
                   MI_economic_capital=mean(MI_economic_capital,na.rm=T),
                   MI_human_capital=mean(MI_human_capital,na.rm=T))

detach(newdata)

# predictions
fitted(M1, newdata = data, probs = c(0.05, 0.95),allow_new_levels=TRUE)

# get model (Model 10 - childcare support non relatives)
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/childcare_help_non_rels_neg_binom.rds")
# new df
attach(newdata)
data <- data.frame(religion = mean(religion,na.rm=T),
                   kids_in_hh=mean(kids_in_hh,na.rm=T),
                   R_NUM_SIBS=mean(R_NUM_SIBS,na.rm=T),
                   familyBariReligiousAfter=c(-1,1),
                   religious_knowledge_scale=mean(religious_knowledge_scale,na.rm=T),
                   MI_geo_proximity=mean(MI_geo_proximity,na.rm=T),
                   MI_economic_capital=mean(MI_economic_capital,na.rm=T),
                   MI_human_capital=mean(MI_human_capital,na.rm=T))

detach(newdata)

# predictions
fitted(M1, newdata = data, probs = c(0.05, 0.95),allow_new_levels=TRUE)

aggregate(newdata$childcare_help_non_rels~newdata$familyBariReligiousAfter,FUN=mean)
