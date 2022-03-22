## Models with interactions between MI and religiosity and model comparisons between models with interactionsa and those without 

########################################################################################
########################################################################################
#DO THIS FIRST READ IN DATA
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
library(tidyverse)
library(brms)
library(readr)
#####READ in and filter newdata
setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh")
data <- read_csv("newdata.csv")
options(scipen=999)
# remove duplicated women
# data <- data %>% distinct(idwife, .keep_all = TRUE)
# # 1) center and scale variables for easier interpretability fo parameter estimates
# data$religious_knowledge_scale <-  data$religious_knowledge_scale-mean(data$religious_knowledge_scale, na.rm=T)
# data$hh_total  <- data$hh_total-mean(data$hh_total, na.rm=T)  
# data$kids_in_hh  <- data$kids_in_hh-mean(data$kids_in_hh, na.rm=T)

# data$kids_in_hh[is.na(data$kids_in_hh)]<- 0
# data$religious_knowledge_scale[is.na(data$religious_knowledge_scale)]<-0
# data$religion[is.na(data$religion)]<-0
# data$sex <- NULL
d <- data[c(10,7:9,44,47,49,51,45)] 
d <- d[complete.cases(d), ] 


hist(d$NW_total)

### Model 1 (Total NW size) - Model with interaction a little better
d$NW_total <- as.numeric(d$NW_total)
### try model
model1 <- brm(NW_total ~ kids_in_hh+
                religion+familyBariReligiousAfter+religious_knowledge_scale+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital, data=d, 
              family = lognormal(),
              prior = c(set_prior("normal(0,2)", class = "b"),
                        set_prior("normal(0,10)", class="b",coef="age_wife")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model1, prob=0.95,priors=TRUE), digits = 6)
fit1 <- add_criterion(model1, c("loo","waic"))


#familyBariReligiousAfter   0.120942  0.021494  0.078437  0.163359 1.000212    16250    11572

## with interaction
model1a <- brm(NW_total ~ kids_in_hh+
                religion+familyBariReligiousAfter+religious_knowledge_scale+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital+familyBariReligiousAfter*MI_economic_capital, data=d, 
              family = lognormal(),
              prior = c(set_prior("normal(0,2)", class = "b"),
                        set_prior("normal(0,10)", class="b",coef="age_wife")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model1a, prob=0.95,priors=TRUE), digits = 6)

fit2 <- add_criterion(model1a, c("loo","waic"))

#familyBariReligiousAfter                      0.112321  0.021457  0.070031  0.154275 1.000698    17499    11273
#familyBariReligiousAfter:MI_economic_capital -0.053829  0.021743 -0.096824 -0.011795 1.000213    13603    12166
## model comparison
loo_compare(fit1, fit2,criterion = "waic")
 # elpd_diff se_diff
 # fit2  0.0       0.0   
 # fit1 -2.1       2.5 


# get the weights
model_weights(fit1, fit2,
              weights = "waic") %>% 
  round(digits = 3)
# fit1  fit2 
# 0.109 0.891 


### Model 2 (percent rels in NW)  - model without the interaction is quite a bit better!!!
d <- data[c(21,7:9,44,47,49,51,45)] 
d <- d[complete.cases(d), ] 

d$percent_rels_in_NW<- d$percent_rels_in_NW+0.01
## run as log normal  
model2 <- brm(percent_rels_in_NW ~ kids_in_hh+
                religion+familyBariReligiousAfter+religious_knowledge_scale+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital, data=d, 
              family = lognormal(),
              prior = c(set_prior("normal(0,2)", class = "b"),
                        set_prior("normal(0,10)", class="b",coef="age_wife")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model2, prob=0.95,priors=TRUE), digits = 6)
#familyBariReligiousAfter   0.030196  0.013646  0.003443  0.056911 1.000099    15972    11656
fit1 <- add_criterion(model2, c("loo","waic"))

model2a <- brm(percent_rels_in_NW ~ kids_in_hh+
                 religion+familyBariReligiousAfter+religious_knowledge_scale+
                 MI_geo_proximity+
                 MI_economic_capital+
                 MI_human_capital+familyBariReligiousAfter*MI_economic_capital, data=d, 
               family = lognormal(),
               prior = c(set_prior("normal(0,2)", class = "b"),
                         set_prior("normal(0,10)", class="b",coef="age_wife")),
               warmup = 1000, iter = 5000, chains = 4,
               control = list(adapt_delta = 0.95))

print(summary(model2a, prob=0.95,priors=TRUE), digits = 6)

# familyBariReligiousAfter                      0.030354  0.013709  0.003760  0.057325 1.000376
# familyBariReligiousAfter:MI_economic_capital  0.001634  0.013966 -0.025864  0.028900 1.000012

fit2 <- add_criterion(model2a, c("loo","waic"))

## model comparison
loo_compare(fit1, fit2,criterion = "waic")

# elpd_diff se_diff
# fit1  0.0       0.0   
# fit2 -7.4       6.5  
# get the weights
model_weights(fit1, fit2,
              weights = "waic") %>% 
  round(digits = 3)

# fit1  fit2 
# 0.999 0.001 
### Rels in NW model 3 (similar weights interaction slightly better, negative interaction between MI and religiosity)
d <- data[c(19,7:9,44,47,49,51,45)] 
d <- d[complete.cases(d), ] 

d$rels_in_NW<- d$rels_in_NW+0.01
 
model3 <- brm(rels_in_NW ~ kids_in_hh+
                  religion+familyBariReligiousAfter+religious_knowledge_scale+
                  MI_geo_proximity+
                  MI_economic_capital+
                  MI_human_capital, data=d, 
                family = lognormal(),
                prior = c(set_prior("normal(0,2)", class = "b"),
                          set_prior("normal(0,10)", class="b",coef="age_wife")),
                warmup = 1000, iter = 5000, chains = 4,
                control = list(adapt_delta = 0.95))

print(summary(model3, prob=0.95,priors=TRUE), digits = 6)
#familyBariReligiousAfter   0.140206  0.025081  0.091564  0.189330 1.000093    17442    12478

fit1 <- add_criterion(model3, c("loo","waic"))

model3a <- brm(rels_in_NW ~ kids_in_hh+
                religion+familyBariReligiousAfter+religious_knowledge_scale+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital+familyBariReligiousAfter*MI_economic_capital, data=d, 
              family = lognormal(),
              prior = c(set_prior("normal(0,2)", class = "b"),
                        set_prior("normal(0,10)", class="b",coef="age_wife")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model3a, prob=0.95,priors=TRUE), digits = 6)
#familyBariReligiousAfter                      0.133770  0.024840  0.084530  0.182353 0.999995
#familyBariReligiousAfter:MI_economic_capital -0.042685  0.025297 -0.091346  0.007172 1.000595
fit2 <- add_criterion(model3a, c("loo","waic"))

## model comparison
loo_compare(fit1, fit2,criterion = "waic")
# elpd_diff se_diff
# fit2  0.0       0.0   
# fit1 -0.3       1.8 

# get the weights
model_weights(fit1, fit2,
              weights = "waic") %>% 
  round(digits = 3)

# fit1  fit2 
# 0.416 0.584

# Model 4 (Non rels in NW) - model with interaction a little bit better (negative interaction between MI and religiosity)
d <- data[c(11,7:9,44,47,49,51,45)] 
d <- d[complete.cases(d), ] 
d$non_rels<- d$non_rels+0.01

model4 <- brm(non_rels ~ kids_in_hh+
                religion+familyBariReligiousAfter+religious_knowledge_scale+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital, data=d, 
              family = lognormal(),
              prior = c(set_prior("normal(0,2)", class = "b"),
                        set_prior("normal(0,10)", class="b",coef="age_wife")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model4, prob=0.95,priors=TRUE), digits = 6)
#familyBariReligiousAfter   0.032535  0.155589 -0.273798  0.337934 1.000370    22716    12189

fit1 <- add_criterion(model4, c("loo","waic"))

model4a <- brm(non_rels ~ kids_in_hh+
                 religion+familyBariReligiousAfter+religious_knowledge_scale+
                 MI_geo_proximity+
                 MI_economic_capital+
                 MI_human_capital+familyBariReligiousAfter*MI_economic_capital, data=d, 
               family = lognormal(),
               prior = c(set_prior("normal(0,2)", class = "b"),
                         set_prior("normal(0,10)", class="b",coef="age_wife")),
               warmup = 1000, iter = 5000, chains = 4,
               control = list(adapt_delta = 0.95))

print(summary(model4a, prob=0.95,priors=TRUE), digits = 6)
#familyBariReligiousAfter                     -0.010012  0.156891 -0.318378  0.296723 1.000206
#familyBariReligiousAfter:MI_economic_capital -0.293949  0.158610 -0.604123  0.016175 1.000238


fit2 <- add_criterion(model4a, c("loo","waic"))

## model comparison
loo_compare(fit1, fit2,criterion = "waic")
# elpd_diff se_diff
# fit2  0.0       0.0   
# fit1 -1.0       1.8

loo_compare(fit1, fit2,criterion = "loo")
# elpd_diff se_diff
# fit2  0.0       0.0   
# fit1 -1.0       1.8 
# get the weights
model_weights(fit1, fit2,
              weights = "waic") %>% 
  round(digits = 3)

# fit1  fit2 
# 0.278 0.722



#### Consider running interactions with the other MI indices (e.g. human capital and geo proximity)

## But first run the interactions with geo proximity and emotional closeness and childcare
# (i.e. a positive relationship between the interaction of religiosity with market integration and density of kin networks)


# all the P4's should be positive if any p4a-p4c are true
# P4a) Those high in religiosity and MI will live closer to relatives
### try model

library(tidyverse)
library(brms)
library(readr)
library(tidybayes)
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Geo_distance_relatives_lognormal.rds")
print(summary(M1, prob=0.95,priors=TRUE), digits = 6)
fit1 <- add_criterion(M1, c("loo","waic"))

setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh")
newdata <- read_csv("newdata.csv")
options(scipen=999)

d <- data[c(7:9,44,47,49,51,45)]  
d <- d[complete.cases(d), ] 

# read in wife NW
WifeNW <- read_csv("HHQPeopleinNW.csv")

# key variables are location and relationship
WifeNW$relationship <- as.numeric(WifeNW$relationship)
plyr::count(WifeNW$relationship)
WifeNW$relationship[is.na(WifeNW$relationship)]<- 99
WifeNW$location[WifeNW$location == 0] <- NA
WifeNW$location[WifeNW$location >5 ] <- NA
plyr::count(WifeNW$location)


z <- WifeNW %>% dplyr::select (2,3,6,8)

# get location of non relatives
nr <- z %>% filter (relationship==0)
r <- z %>% filter (relationship>0 & relationship<8)


## relatives geo_distance
rels <- r %>% left_join (d, by=c("id_Questionaire"="idwife"))
rels <- rels[complete.cases(rels),]

# Make models in brms  

M1 <- 
  brm(data = rels, 
      family = cumulative("logit"),
      location ~ 1 +  MI_geo_proximity + MI_economic_capital + MI_human_capital +R_NUM_SIBS+
        religion+ religious_knowledge_scale + kids_in_hh + familyBariReligiousAfter,
      prior = c(prior(normal(0, 1.5), class = Intercept),
                prior(normal(0, 1.5), class = b)),
      iter = 5000, warmup = 1000, cores = 4, chains = 4)


print(summary(M1, prob=0.95,priors=TRUE), digits = 6)

# model with intx
M1a <- 
  brm(data = rels, 
      family = cumulative("logit"),
      location ~ 1 +  MI_geo_proximity + MI_economic_capital + MI_human_capital +R_NUM_SIBS+
        religion+ religious_knowledge_scale + kids_in_hh + familyBariReligiousAfter+
        familyBariReligiousAfter*MI_economic_capital,
      prior = c(prior(normal(0, 1.5), class = Intercept),
                prior(normal(0, 1.5), class = b)),
      iter = 5000, warmup = 1000, cores = 4, chains = 4)


print(summary(M1a, prob=0.95,priors=TRUE), digits = 6)

# familyBariReligiousAfter                0.027428  0.012959  0.002170  0.052725 1.000176    17867    11693

# 
# familyBariReligiousAfter                         0.12      0.04     0.04     0.21 1.00    21334    13044
# MI_economic_capital:familyBariReligiousAfter    -0.04      0.04    -0.13     0.05 1.00    18117    13563
fit1 <- add_criterion(M1, c("loo","waic"))
fit2 <- add_criterion(M1a, c("loo","waic"))
## model comparison
loo_compare(fit1, fit2,criterion = "waic")
# elpd_diff se_diff
# fit1  0.0       0.0   
# fit2 -0.6       1.0


# get the weights
model_weights(fit1, fit2,
              weights = "waic") %>% 
  round(digits = 3)
# fit1  fit2 
# 0.655 0.345 


# P4b) Those high in religiosity and MI will receive more emotional support from relatives

d <- data[c(29,7:9,44,47,49,51,45)] 

d <- d[complete.cases(d), ]

d$emot_support_rels<-as.integer(d$emot_support_rels)

M1<-brm(emot_support_rels ~ kids_in_hh+R_NUM_SIBS+
                religion+familyBariReligiousAfter+religious_knowledge_scale+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital, data=d, family = "poisson",
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(M1, prob=0.95,priors=TRUE), digits = 6)

M1a<-brm(emot_support_rels ~ kids_in_hh+R_NUM_SIBS+
                religion+familyBariReligiousAfter+religious_knowledge_scale+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital+MI_economic_capital*familyBariReligiousAfter, data=d, family = "poisson",
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(M1a, prob=0.95,priors=TRUE), digits = 6)

#familyBariReligiousAfter   0.188108  0.025015  0.139562  0.237375 1.000351    14054    11866

# familyBariReligiousAfter                      0.182709  0.025339  0.133403  0.232143 1.000282    14120    11977
# familyBariReligiousAfter:MI_economic_capital -0.046121  0.026477 -0.097830  0.005517 1.000311    12676    12039

fit1 <- add_criterion(M1, c("loo","waic"))
fit2 <- add_criterion(M1a, c("loo","waic"))
## model comparison
loo_compare(fit1, fit2,criterion = "waic")
# elpd_diff se_diff
# fit2  0.0       0.0   
# fit1 -0.4       2.0 


# get the weights
model_weights(fit1, fit2,
              weights = "waic") %>% 
  round(digits = 3)
# #fit1  fit2 
# 0.407 0.593 

# P4c) Those high in religiosity and MI receive more alloparenting support from relatives
library(brms)
library(readr)

d <- data[c(35,7:9,44,47,49,51,45)] 

d <- d[complete.cases(d), ] 
d$childcare_help_rels<-as.integer(d$childcare_help_rels)

M1<-brm(childcare_help_rels ~ kids_in_hh+R_NUM_SIBS+
                religion+familyBariReligiousAfter+religious_knowledge_scale+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital, data=d, family = negbinomial(link = "log", link_shape = "identity"),
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(M1, prob=0.95,priors=TRUE), digits = 6)
#familyBariReligiousAfter  -0.012527  0.035177 -0.081814  0.056733 1.000444    16050    11888

#familyBariReligiousAfter                     -0.023213  0.035542 -0.092807  0.046915 1.000047    16961    12439
#familyBariReligiousAfter:MI_economic_capital -0.088932  0.037067 -0.160947 -0.016943 1.000115    15586    12168

M1a<-brm(childcare_help_rels ~ kids_in_hh+R_NUM_SIBS+
          religion+familyBariReligiousAfter+religious_knowledge_scale+
          MI_geo_proximity+
          MI_economic_capital+
          MI_human_capital+
           MI_economic_capital*familyBariReligiousAfter, data=d, family = negbinomial(link = "log", link_shape = "identity"),
        prior = c(set_prior("normal(0,2)", class = "b")),
        warmup = 1000, iter = 5000, chains = 4,
        control = list(adapt_delta = 0.95))

print(summary(M1a, prob=0.95,priors=TRUE), digits = 6)

path<- (paste0("results/"))
filename <- "Childcare_help_rels_w_intx.rds"

saveRDS(M1a, paste0(path, filename))

fit1 <- add_criterion(M1, c("loo","waic"))
fit2 <- add_criterion(M1a, c("loo","waic"),moment_match = TRUE)
## model comparison
loo_compare(fit1, fit2,criterion = "loo")

# elpd_diff se_diff
# fit2  0.0       0.0   
# fit1 -1.9       2.3 

# get the weights
model_weights(fit1, fit2,
              weights = "waic") %>% 
  round(digits = 3)

# fit1  fit2 
# 0.134 0.866 

#These do not need to be run - just see if the above interaction are negative to test


# **P5) Alternatively, more market-integrated households may be better able to harness secular institutions
# to buffer the negative impact of living further away from relatives (Boyer, 1991) and more religious individuals
# may be better able to replace kin with unrelated co-religionists.  If true, then, religiosity will be less 
# important for obtaining alloparenting support from either kin or non-kin. In this case there may be a negative
# relationship between the interaction of market integration with religiosity and the kin density of one's social
# network, such that individuals who are both more dependent on markets and more religious will

#P5a) live further from relatives, 

#P5b) receive less emotional support from relatives and 

#P5c) receive less alloparenting support 
# from relatives and more from non-relatives (i.e. a positive relationship between the interaction of religiosity
#  with market integration and density of kin networks) 
