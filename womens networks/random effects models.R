library(tidyverse)
library(brms)
library(readr)
#####READ in and filter newdata
setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh")
newdata <- read_csv("newdata.csv")
options(scipen=999)

# number of siblings is column 51
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
## Brms models - Model 1 NW total
d <- newdata[c(10,47,5,7,8,9,44,45,49,51)] 

d <- d[complete.cases(d), ]  

d$NW_total <- as.numeric(d$NW_total)
### try model
fixed <- brm(NW_total ~ kids_in_hh+R_NUM_SIBS+
                religion+familyBariReligiousAfter+religious_knowledge_scale+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital, data=d, 
             brmsfamily("lognormal"),
              prior = c(set_prior("normal(0,1)", class = "b")),
              warmup = 200, iter = 500, chains = 2,
              sample_prior=T,
              control = list(adapt_delta = 0.95))
print(summary(fixed, prob=0.95,priors=TRUE), digits = 6)


#R_NUM_SIBS                 0.003579  0.005980 -0.007810 0.014639 0.997798      849      536
#familyBariReligiousAfter   0.114912  0.021730  0.073733 0.158903 0.999308      511      507


random <- brm(NW_total ~ kids_in_hh+
               religion+familyBariReligiousAfter+religious_knowledge_scale+
               MI_geo_proximity+
               MI_economic_capital+
               MI_human_capital+(1|R_NUM_SIBS), data=d, 
             family = lognormal(),
             prior = c(set_prior("normal(0,1)", class = "b")),
             warmup = 1000, iter = 5000, chains = 4,
             sample_prior=T,
             control = list(adapt_delta = 0.95))

print(summary(random, prob=0.95,priors=TRUE), digits = 6)

#familyBariReligiousAfter   0.116640  0.021276  0.075024  0.158926 0.999966    20903    11997
# Group-Level Effects: 
#   ~R_NUM_SIBS (Number of levels: 12) 
#                 Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.049913  0.026125 0.006931 0.110949 1.000695     4370     4476

random2 <- brm(NW_total ~ (1|kids_in_hh)+
                religion+familyBariReligiousAfter+religious_knowledge_scale+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital+(1|R_NUM_SIBS), data=d, 
              family = lognormal(),
              prior = c(set_prior("normal(0,1)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              sample_prior=T,
              control = list(adapt_delta = 0.95))

print(summary(random2, prob=0.95,priors=TRUE), digits = 6)

#familyBariReligiousAfter   0.116053  0.021775  0.072868  0.159165 1.000352    19464    10771
fit1 <- add_criterion(fixed, "waic")
fit2 <- add_criterion(random, "waic")
fit3 <- add_criterion(random2, "waic")

loo_compare(fit1,fit2,fit3, criterion = "waic")

# get the weights
model_weights(fit1, fit2,fit3,
              weights = "waic") %>% 
  round(digits = 3)


###default should be to put in fixed effects as random effects whenever possible (e.g. they form clusters/ groups)