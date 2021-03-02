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
data <- data %>% distinct(idwife, .keep_all = TRUE)
# 1) center and scale variables for easier interpretability fo parameter estimates
data$religious_knowledge_scale <-  data$religious_knowledge_scale-mean(data$religious_knowledge_scale, na.rm=T)
data$hh_total  <- data$hh_total-mean(data$hh_total, na.rm=T)  
data$kids_in_hh  <- data$kids_in_hh-mean(data$kids_in_hh, na.rm=T)

data$kids_in_hh[is.na(data$kids_in_hh)]<- 0
data$religious_knowledge_scale[is.na(data$religious_knowledge_scale)]<-0
data$religion[is.na(data$religion)]<-0
data$sex <- NULL
data <- data[c(4,6:8,9,10,18,20,22,28,34,35,36,37,39,41,43,44,45,46,48)] 
d <- data[complete.cases(data), ] 


hist(d$NW_total)

### Model 1 (Total NW size) - Model with interaction a little better
d$NW_total <- as.numeric(d$NW_total)
### try model
model1 <- brm(NW_total ~ kids_in_hh+
                age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
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
                age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
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


### Model 2 (percent rels in NW)  - model without the interaction is quiet a bit better!!!

d$percent_rels_in_NW<- d$percent_rels_in_NW+0.01
## run as log normal  
model2 <- brm(percent_rels_in_NW ~ kids_in_hh+
                age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
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
                 age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
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
### Rels in NW model 3 (similar weights interaction slightly better)
d$rels_in_NW<- d$rels_in_NW+0.01
 
model3 <- brm(rels_in_NW ~ kids_in_hh+
                  age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
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
                age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
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

# Model 4 (Non rels in NW) - model with interaction a little bit better

d$non_rels<- d$non_rels+0.01

model4 <- brm(non_rels ~ kids_in_hh+
                age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
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
                 age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
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