
#### do interactions with chidlcare for non rel and rels 
library(tidyverse)
library(brms)
library(readr)

## Non rels- Non- interaction a little better and negative
# in rethinking and brms
setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh")
newdata <- read_csv("newdata.csv")

d <- newdata[c(1,5,7,8,9,36,35,44,45,47,49,51)] # add 36 for non-rels and 35 for rels
d <- d[complete.cases(d), ] 

# read in wife NW
# WifeNW <- read_csv("HHQPeopleinNW.csv")
# 
# # key variables are location and relationship
# WifeNW$relationship <- as.numeric(WifeNW$relationship)
# plyr::count(WifeNW$relationship)
# WifeNW$relationship[is.na(WifeNW$relationship)]<- 99
# WifeNW$location[WifeNW$location == 0] <- NA
# WifeNW$location[WifeNW$location >5 ] <- NA
# plyr::count(WifeNW$location)
# 
# 
# z <- WifeNW %>% dplyr::select (2,3,6,8)
# 
# # get location of non relatives
# nr <- z %>% filter (relationship==0)
# r <- z %>% filter (relationship>0 & relationship<8)
# 
# # get non relatives
# # join non-rels to data (d)
# non_rels <- nr %>% left_join (d, by=c("id_Questionaire"="idwife"))
# non_rels <- non_rels[complete.cases(non_rels),]
# 
# non_rels$location[non_rels$location==1|non_rels$location==2] <- 2
# non_rels$location[non_rels$location==3] <- 3
# non_rels$location[non_rels$location==4|non_rels$location==5] <- 4
# # Location Codes
# # 1=khana member,  HH member
# # 2=near bari/neighbor
# # 3=other place in Matlab
# # 4=Other Place in Bangladesh
# # 5=Abroad
# 
# # subset to only neighbors or closer
# non_rels <- non_rels %>% filter(location==2)
# 
# non_rels <- non_rels %>% distinct(id_Questionaire, .keep_all = TRUE)

d$childcare_help_non_rels<- as.integer(d$childcare_help_non_rels)
## Run kin and non-kin living in same neighborhood only
M1<-brm(childcare_help_non_rels ~ kids_in_hh+R_NUM_SIBS+
              religion+familyBariReligiousAfter+religious_knowledge_scale+
              MI_geo_proximity+
              MI_economic_capital+
              MI_human_capital, data=d, family = negbinomial(link = "log", link_shape = "identity"),
            prior = c(set_prior("normal(0,2)", class = "b")),
            warmup = 1000, iter = 5000, chains = 4,
            control = list(adapt_delta = 0.95))

print(summary(M1, prob=0.95,priors=TRUE), digits = 6)

path<- (paste0("results/"))
filename <- "Childcare_help_nonrels.rds"

saveRDS(M1, paste0(path, filename))

## with interaction
M1a<-brm(childcare_help_non_rels ~ kids_in_hh+R_NUM_SIBS+
              religion+familyBariReligiousAfter+religious_knowledge_scale+
              MI_geo_proximity+
              MI_economic_capital+MI_economic_capital*familyBariReligiousAfter+
              MI_human_capital, data=d, family = negbinomial(link = "log", link_shape = "identity"),
            prior = c(set_prior("normal(0,2)", class = "b")),
            warmup = 1000, iter = 5000, chains = 4,
            control = list(adapt_delta = 0.95))

print(summary(M1a, prob=0.95,priors=TRUE), digits = 6)

path<- (paste0("results/"))
filename <- "Childcare_help_nonrels_w_intx.rds"

saveRDS(M1a, paste0(path, filename))

#familyBariReligiousAfter  -1.354227  0.397596 -2.182557 -0.613391 1.000679    12329    10858

# vs
#familyBariReligiousAfter                     -1.383730  0.406541 -2.238154 -0.637917 1.000030    13696    12386
# familyBariReligiousAfter:MI_economic_capital  0.007088  0.330671 -0.650467  0.657276 1.000209    17170    11541

fit1 <- add_criterion(M1, c("loo","waic"))
fit2 <- add_criterion(M1a, c("loo","waic"))
## model comparison
loo_compare(fit1, fit2,criterion = "loo")

# elpd_diff se_diff
# fit1  0.0       0.0   
# fit2 -1.3       0.5  

# get the weights
model_weights(fit1, fit2,
              weights = "waic") %>% 
  round(digits = 3)

# fit1  fit2 
# 0.756 0.244 

# Do relatives  (with interaction gets more weight)
setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh")
newdata <- read_csv("newdata.csv")

d <- newdata[c(1,5,7,8,9,36,35,44,45,47,49,51)] # add 36 for non-rels and 35 for rels
d <- d[complete.cases(d), ] 
# 
# # read in wife NW
# WifeNW <- read_csv("HHQPeopleinNW.csv")
# 
# # key variables are location and relationship
# WifeNW$relationship <- as.numeric(WifeNW$relationship)
# plyr::count(WifeNW$relationship)
# WifeNW$relationship[is.na(WifeNW$relationship)]<- 99
# WifeNW$location[WifeNW$location == 0] <- NA
# WifeNW$location[WifeNW$location >5 ] <- NA
# plyr::count(WifeNW$location)
# 
# 
# z <- WifeNW %>% dplyr::select (2,3,6,8)
# 
# # get location of non relatives
# nr <- z %>% filter (relationship==0)
# r <- z %>% filter (relationship>0 & relationship<8)
# 
# # get non relatives
# # join non-rels to data (d)
# rels <- r %>% left_join (d, by=c("id_Questionaire"="idwife"))
# rels <- rels[complete.cases(rels),]
# 
# rels$location[rels$location==1|rels$location==2] <- 2
# rels$location[rels$location==3] <- 3
# rels$location[rels$location==4|rels$location==5] <- 4

# subset to only neighbors or closer
#rels <- rels %>% filter(location==2)

#rels <- rels %>% distinct(id_Questionaire, .keep_all = TRUE)

d$childcare_help_rels<- as.integer(d$childcare_help_rels)
## Run kin and non-kin living in same neighborhood only

M2<-brm(childcare_help_rels ~ kids_in_hh+R_NUM_SIBS+
          religion+familyBariReligiousAfter+religious_knowledge_scale+
          MI_geo_proximity+
          MI_economic_capital+
          MI_human_capital, data=d, family = negbinomial(link = "log", link_shape = "identity"),
        prior = c(set_prior("normal(0,2)", class = "b")),
        warmup = 1000, iter = 5000, chains = 4,
        control = list(adapt_delta = 0.95))

print(summary(M2, prob=0.95,priors=TRUE), digits = 6)

path<- (paste0("results/"))
filename <- "Childcare_help_rels_neg_binom.rds"

saveRDS(M2, paste0(path, filename))

# with intx
M2a<-brm(childcare_help_rels ~ kids_in_hh+R_NUM_SIBS+
          religion+familyBariReligiousAfter+religious_knowledge_scale+
          MI_geo_proximity+
          MI_economic_capital+
          MI_human_capital+MI_economic_capital*familyBariReligiousAfter, data=d, family = negbinomial(link = "log", link_shape = "identity"),
        prior = c(set_prior("normal(0,2)", class = "b")),
        warmup = 1000, iter = 5000, chains = 4,
        control = list(adapt_delta = 0.95))

print(summary(M2a, prob=0.95,priors=TRUE), digits = 6)

path<- (paste0("results/"))
filename <- "Childcare_help_rels_w_intx.rds"

saveRDS(M2a, paste0(path, filename))

#familyBariReligiousAfter  -0.015744  0.035220 -0.084517  0.053455 0.999948    16070    11761

# VS

#familyBariReligiousAfter                     -0.026510  0.035387 -0.095298  0.042576 1.000590    17750    12965
#familyBariReligiousAfter:MI_economic_capital -0.083979  0.036825 -0.156535 -0.012892 1.000162    16372    12422

# compare models
fit1 <- add_criterion(M2, c("loo","waic"))
fit2 <- add_criterion(M2a, c("loo","waic"))


## model comparison
loo_compare(fit1, fit2,criterion = "loo")
# elpd_diff se_diff
# fit2  0.0       0.0   
# fit1 -1.5       2.2 

# get the weights
model_weights(fit1, fit2,
              weights = "waic") %>% 
  round(digits = 3)

# fit1  fit2 
# 0.172 0.828 
### do relative but include all of them (neighbors and non)
# A negative interaction between religiosity and market integration on the number of relatives
# who help with childcare.   Less religious households who are more integrated have more relatives 
# helping with childcare (left panel) while more 
# religious households who are more integrated have fewer relatives helping with childcare (right panel)

### add number of kin in network to relatives childcare intx model -see if result is same - compare models (if so or not plot it-
# in Main Figures)


setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh")
newdata <- read_csv("newdata.csv")

d <- newdata[c(1,5,7,8,9,36,35,44,45,47,49,51)] # add 36 for non-rels and 35 for rels
d <- d[complete.cases(d), ] 



### add variable number of kin in neighborhood to d
# read in wife NW
 WifeNW <- read_csv("HHQPeopleinNW.csv")

## key variables are location and relationship
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

#count rels and non rels who location==1 | location==2

nr2 <-nr %>% group_by(id_Questionaire) %>% summarise (neighbor_non_rels = sum(location<3))

# must link nr2 back to d somehow - all missing in nr2 are 0
d2<- d%>% left_join (nr2, by=c("idwife"="id_Questionaire"))
# replace NA's in neighbor_rels with 0
d2$neighbor_non_rels[is.na(d2$neighbor_non_rels)]<-0
d2$childcare_help_non_rels<- as.integer(d3$childcare_help_non_rels)

r2 <- r %>% group_by(id_Questionaire) %>% summarise (neighbor_rels = sum(location<3))
# must link nr2 back to d somehow - all missing in nr2 are 0
d3<- d %>% left_join (r2, by=c("idwife"="id_Questionaire"))
# replace NA's in neighbor_rels with 0
d3$neighbor_rels[is.na(d3$neighbor_rels)]<-0

## Now run  with number of non-kin (d2) and kin (d3) living in same neighborhood only

# Non - kin 1st:  No interaction with number of neighbors is best
M<-brm(childcare_help_non_rels ~ kids_in_hh+R_NUM_SIBS+
          religion+familyBariReligiousAfter+religious_knowledge_scale+
          MI_geo_proximity+
          MI_economic_capital+
          MI_human_capital, data=d2, family = negbinomial(link = "log", link_shape = "identity"),
        prior = c(set_prior("normal(0,2)", class = "b")),
        warmup = 1000, iter = 5000, chains = 4,
        control = list(adapt_delta = 0.95))

print(summary(M, prob=0.95,priors=TRUE), digits = 6)
#familyBariReligiousAfter  -1.359020  0.392521 -2.188454 -0.630265 1.000274    12117    10118

M1<-brm(childcare_help_non_rels ~ kids_in_hh+R_NUM_SIBS+
          religion+familyBariReligiousAfter+religious_knowledge_scale+
          MI_geo_proximity+
          MI_economic_capital+neighbor_non_rels+
          MI_human_capital, data=d2, family = negbinomial(link = "log", link_shape = "identity"),
        prior = c(set_prior("normal(0,2)", class = "b")),
        warmup = 1000, iter = 5000, chains = 4,
        control = list(adapt_delta = 0.95))

print(summary(M1, prob=0.95,priors=TRUE), digits = 6)

#familyBariReligiousAfter  -1.127200  0.315396 -1.763422 -0.528475 1.000262    15010    11247

path<- (paste0("results/"))
filename <- "non_rels_childcare_with_neigbors.rds"
saveRDS(M1, paste0(path, filename))

# with intx
M1a<-brm(childcare_help_non_rels ~ kids_in_hh+R_NUM_SIBS+neighbor_non_rels+
           religion+familyBariReligiousAfter+religious_knowledge_scale+
           MI_geo_proximity+
           MI_economic_capital+
           MI_human_capital+MI_economic_capital*familyBariReligiousAfter, data=d2, family = negbinomial(link = "log", link_shape = "identity"),
         prior = c(set_prior("normal(0,2)", class = "b")),
         warmup = 1000, iter = 5000, chains = 4,
         control = list(adapt_delta = 0.95))

print(summary(M1a, prob=0.95,priors=TRUE), digits = 6)
#familyBariReligiousAfter:MI_economic_capital  0.064872  0.255724 -0.456137  0.547546 1.000401    20131    11382

# path<- (paste0("results/"))
# filename <- "Childcare_help_rels_w_intx.rds"
# 
# saveRDS(M1a, paste0(path, filename))


# compare models
fit <- add_criterion(M, c("loo","waic"))
fit1 <- add_criterion(M1, c("loo","waic"))
fit2 <- add_criterion(M1a, c("loo","waic"))

# elpd_diff se_diff
# fit1   0.0       0.0  
# fit2  -1.4       0.4  
# fit  -45.1      10.2 
## model comparison
loo_compare(fit,fit1, fit2,criterion = "loo")


# get the weights
model_weights(fit, fit1, fit2,
              weights = "waic") %>% 
  round(digits = 3)
# # fit  fit1  fit2 
# 0.000 0.775 0.225 

###### next do relatives
d3$childcare_help_rels<- as.integer(d3$childcare_help_rels)

M.2<-brm(childcare_help_rels ~ kids_in_hh+R_NUM_SIBS+
         religion+familyBariReligiousAfter+religious_knowledge_scale+
         MI_geo_proximity+
         MI_economic_capital+
         MI_human_capital, data=d3, family = negbinomial(link = "log", link_shape = "identity"),
       prior = c(set_prior("normal(0,2)", class = "b")),
       warmup = 1000, iter = 5000, chains = 4,
       control = list(adapt_delta = 0.95))

print(summary(M.2, prob=0.95,priors=TRUE), digits = 6)
#familyBariReligiousAfter  -0.012700  0.035750 -0.083630  0.057692 1.000308    16779    11589

M2<-brm(childcare_help_rels ~ kids_in_hh+R_NUM_SIBS+
          religion+familyBariReligiousAfter+religious_knowledge_scale+
          MI_geo_proximity+
          MI_economic_capital+neighbor_rels+
          MI_human_capital, data=d3, family = negbinomial(link = "log", link_shape = "identity"),
        prior = c(set_prior("normal(0,2)", class = "b")),
        warmup = 1000, iter = 5000, chains = 4,
        control = list(adapt_delta = 0.95))

print(summary(M2, prob=0.95,priors=TRUE), digits = 6)

#familyBariReligiousAfter  -0.037596  0.034692 -0.105041  0.031501 1.000146    16592    11896

# path<- (paste0("results/"))
# filename <- ""
# 
# saveRDS(M1, paste0(path, filename))

# with intx
M2a<-brm(childcare_help_rels ~ kids_in_hh+R_NUM_SIBS+neighbor_rels+
           religion+familyBariReligiousAfter+religious_knowledge_scale+
           MI_geo_proximity+
           MI_economic_capital+
           MI_human_capital+MI_economic_capital*familyBariReligiousAfter, data=d3, family = negbinomial(link = "log", link_shape = "identity"),
         prior = c(set_prior("normal(0,2)", class = "b")),
         warmup = 1000, iter = 5000, chains = 4,
         control = list(adapt_delta = 0.95))

print(summary(M2a, prob=0.95,priors=TRUE), digits = 6)

#

 path<- (paste0("results/"))
 filename <- "Rels_childcare_intx_with_neighbors.rds"
# 
saveRDS(M2a, paste0(path, filename))

#familyBariReligiousAfter                     -0.048321  0.035388 -0.117916  0.020536 1.000053    18614    12647
#familyBariReligiousAfter:MI_economic_capital -0.087446  0.036460 -0.158963 -0.016385 0.999973    17049    13155


# compare models
fit <- add_criterion(M.2, c("loo","waic"))
fit1 <- add_criterion(M2, c("loo","waic"))
fit2 <- add_criterion(M2a, c("loo","waic"))


## model comparison
loo_compare(fit,fit1, fit2,criterion = "loo")

# elpd_diff se_diff
# fit2   0.0       0.0  
# fit1  -2.0       2.2  
# fit  -24.5       7.6 
# get the weights
model_weights(fit, fit1, fit2,
              weights = "waic") %>% 
  round(digits = 3)

# fit  fit1  fit2 
# 0.000 0.117 0.883 


### take home message - it does not matter if we include number of neighbors who are relatives as a covariate

#######Best relatives model with interaction and neighbors only: "Rels_childcare_intx_with_neighbors.rds"
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                                        0.86      0.07     0.74     0.99 1.00    19251    11754
# kids_in_hh                                      -0.01      0.01    -0.04     0.01 1.00    17298    11827
# R_NUM_SIBS                                       0.01      0.01    -0.00     0.03 1.00    20343    12519
# neighbor_rels                                    0.05      0.01     0.04     0.07 1.00    19549    11384
# religion                                        -0.06      0.08    -0.21     0.10 1.00    16295    12919
# familyBariReligiousAfter                        -0.05      0.04    -0.12     0.02 1.00    18614    12647
# religious_knowledge_scale                        0.00      0.01    -0.01     0.02 1.00    17642    12645
# MI_geo_proximity                                -0.12      0.04    -0.21    -0.04 1.00    17662    10286
# MI_economic_capital                              0.04      0.03    -0.02     0.09 1.00    16031    12734
# MI_human_capital                                -0.03      0.03    -0.09     0.03 1.00    16885    11681
# familyBariReligiousAfter:MI_economic_capital    -0.09      0.04    -0.16    -0.02 1.00    17049    13155


### no # of neighbors as predictor: "Childcare_help_rels_w_intx.rds
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                                        1.15      0.05     1.05     1.25 1.00    17647    11646
# kids_in_hh                                      -0.01      0.01    -0.04     0.01 1.00    13741    11976
# R_NUM_SIBS                                       0.01      0.01    -0.00     0.03 1.00    17458    11541
# religion                                        -0.04      0.08    -0.20     0.12 1.00    14309    11886
# familyBariReligiousAfter                        -0.02      0.04    -0.09     0.05 1.00    16961    12439
# religious_knowledge_scale                       -0.00      0.01    -0.02     0.01 1.00    15769    12555
# MI_geo_proximity                                -0.14      0.05    -0.23    -0.06 1.00    18337    11822
# MI_economic_capital                              0.05      0.03    -0.01     0.11 1.00    13950    11545
# MI_human_capital                                -0.02      0.03    -0.08     0.04 1.00    12796    11736
# familyBariReligiousAfter:MI_economic_capital    -0.09      0.04    -0.16    -0.02 1.00    15586    12168


## for non relatives

# Population-Level Effects: 
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                    -4.45      0.55    -5.61    -3.44 1.00    13066    10900
# kids_in_hh                    0.11      0.12    -0.13     0.35 1.00    14389    11800
# R_NUM_SIBS                    0.03      0.09    -0.14     0.21 1.00    17651    11670
# religion                     -1.13      0.77    -2.72     0.29 1.00    15420    11690
# familyBariReligiousAfter     -1.13      0.32    -1.76    -0.53 1.00    15010    11247
# religious_knowledge_scale     0.06      0.04    -0.03     0.14 1.00    13603    10135
# MI_geo_proximity             -0.11      0.35    -0.94     0.44 1.00    14617     8545
# MI_economic_capital           0.16      0.20    -0.23     0.54 1.00    17821    12453
# neighbor_non_rels             0.91      0.12     0.71     1.16 1.00    11755    10618
# MI_human_capital              0.15      0.29    -0.41     0.71 1.00    15166    10230


## try one last time with Number neighbor non rels as DV intx vs non intx - no intx wins

M3<-brm(neighbor_rels ~ kids_in_hh+R_NUM_SIBS+
           religion+familyBariReligiousAfter+religious_knowledge_scale+
           MI_geo_proximity+
           MI_economic_capital+
           MI_human_capital, data=d3, family = negbinomial(link = "log", link_shape = "identity"),
         prior = c(set_prior("normal(0,2)", class = "b")),
         warmup = 1000, iter = 5000, chains = 4,
         control = list(adapt_delta = 0.95))

print(summary(M3, prob=0.95,priors=TRUE), digits = 6)

#                           Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                  1.695088  0.040530  1.615476  1.775533 1.000025    17232    12437
# kids_in_hh                 0.002673  0.011183 -0.019692  0.024070 1.000396    13372    11649
# R_NUM_SIBS                 0.003604  0.008075 -0.012378  0.019565 1.000060    18746    11692
# religion                   0.066011  0.064762 -0.060972  0.194622 1.000940    13459    11122
# familyBariReligiousAfter   0.081683  0.028364  0.026155  0.137095 1.000497    14996    12212
# religious_knowledge_scale -0.013735  0.006832 -0.027401 -0.000643 1.000111    14301    11401
# MI_geo_proximity          -0.047716  0.027998 -0.104648  0.005142 0.999992    15236    10485
# MI_economic_capital        0.039485  0.021442 -0.002232  0.081630 1.000373    14719    12370
# MI_human_capital           0.027477  0.025670 -0.023744  0.077519 0.999994    13224    12644

path<- (paste0("results/"))
filename <- "Rels_who_are_neighbors.rds"
saveRDS(M3, paste0(path, filename))


M3a<-brm(neighbor_rels ~ kids_in_hh+R_NUM_SIBS+
          religion+familyBariReligiousAfter+religious_knowledge_scale+
          MI_geo_proximity+
          MI_economic_capital+
          MI_human_capital+MI_economic_capital*familyBariReligiousAfter, data=d3, family = negbinomial(link = "log", link_shape = "identity"),
        prior = c(set_prior("normal(0,2)", class = "b")),
        warmup = 1000, iter = 5000, chains = 4,
        control = list(adapt_delta = 0.95))

print(summary(M3a, prob=0.95,priors=TRUE), digits = 6)

# path<- (paste0("results/"))
# filename <- "Rels_neighbors_w_intx.rds"
# 
# saveRDS(M3, paste0(path, filename))


# compare models
fit1 <- add_criterion(M3, c("loo","waic"))
fit2 <- add_criterion(M3a, c("loo","waic"))


## model comparison
loo_compare(fit1, fit2,criterion = "loo")
# elpd_diff se_diff
# fit1  0.0       0.0   
# fit2 -0.9       0.4  

# get the weights
model_weights(fit1, fit2,
              weights = "waic") %>% 
  round(digits = 3)
# # fit1  fit2 
# 0.708 0.292


######All the other intx between MI indices and religiosity



setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh")
newdata <- read_csv("newdata.csv")

d <- newdata[c(1,5,7,8,9,36,35,44,45,47,49,51)] # add 36 for non-rels and 35 for rels
d <- d[complete.cases(d), ] 



### add variable number of kin in neighborhood to d
# read in wife NW
WifeNW <- read_csv("HHQPeopleinNW.csv")

## key variables are location and relationship
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

#count rels and non rels who location==1 | location==2

nr2 <-nr %>% group_by(id_Questionaire) %>% summarise (neighbor_non_rels = sum(location<3))

# must link nr2 back to d somehow - all missing in nr2 are 0
d2<- d%>% left_join (nr2, by=c("idwife"="id_Questionaire"))
# replace NA's in neighbor_rels with 0
d2$neighbor_non_rels[is.na(d2$neighbor_non_rels)]<-0
d2$childcare_help_non_rels<- as.integer(d3$childcare_help_non_rels)

r2 <- r %>% group_by(id_Questionaire) %>% summarise (neighbor_rels = sum(location<3))
# must link nr2 back to d somehow - all missing in nr2 are 0
d3<- d %>% left_join (r2, by=c("idwife"="id_Questionaire"))
# replace NA's in neighbor_rels with 0
d3$neighbor_rels[is.na(d3$neighbor_rels)]<-0

###### next do relatives
d3$childcare_help_rels<- as.integer(d3$childcare_help_rels)

M.2<-brm(childcare_help_rels ~ kids_in_hh+R_NUM_SIBS+
           religion+familyBariReligiousAfter+religious_knowledge_scale+
           MI_geo_proximity+
           MI_economic_capital+
           MI_human_capital, data=d3, family = negbinomial(link = "log", link_shape = "identity"),
         prior = c(set_prior("normal(0,2)", class = "b")),
         warmup = 1000, iter = 5000, chains = 4,
         control = list(adapt_delta = 0.95))

print(summary(M.2, prob=0.95,priors=TRUE), digits = 6)
#familyBariReligiousAfter  -0.012700  0.035750 -0.083630  0.057692 1.000308    16779    11589

M2<-brm(childcare_help_rels ~ kids_in_hh+R_NUM_SIBS+
          religion+familyBariReligiousAfter+religious_knowledge_scale+
          MI_geo_proximity+
          MI_economic_capital+neighbor_rels+
          MI_human_capital, data=d3, family = negbinomial(link = "log", link_shape = "identity"),
        prior = c(set_prior("normal(0,2)", class = "b")),
        warmup = 1000, iter = 5000, chains = 4,
        control = list(adapt_delta = 0.95))

print(summary(M2, prob=0.95,priors=TRUE), digits = 6)

#familyBariReligiousAfter  -0.037596  0.034692 -0.105041  0.031501 1.000146    16592    11896

# path<- (paste0("results/"))
# filename <- ""
# 
# saveRDS(M1, paste0(path, filename))

# with intx
M2a<-brm(childcare_help_rels ~ kids_in_hh+R_NUM_SIBS+neighbor_rels+
           religion+familyBariReligiousAfter+religious_knowledge_scale+
           MI_geo_proximity+
           MI_economic_capital+
           MI_human_capital+MI_economic_capital*familyBariReligiousAfter, data=d3, family = negbinomial(link = "log", link_shape = "identity"),
         prior = c(set_prior("normal(0,2)", class = "b")),
         warmup = 1000, iter = 5000, chains = 4,
         control = list(adapt_delta = 0.95))

print(summary(M2a, prob=0.95,priors=TRUE), digits = 6)

# familyBariReligiousAfter                     -0.047632  0.035344 -0.118595  0.020572 1.000276    16398    12156
# familyBariReligiousAfter:MI_economic_capital -0.087159  0.036444 -0.157871 -0.015343 1.000006    16749    12727


M2b<-brm(childcare_help_rels ~ kids_in_hh+R_NUM_SIBS+neighbor_rels+
           religion+familyBariReligiousAfter+religious_knowledge_scale+
           MI_geo_proximity+
           MI_economic_capital+
           MI_human_capital+MI_geo_proximity*familyBariReligiousAfter, data=d3, family = negbinomial(link = "log", link_shape = "identity"),
         prior = c(set_prior("normal(0,2)", class = "b")),
         warmup = 1000, iter = 5000, chains = 4,
         control = list(adapt_delta = 0.95))

print(summary(M2b, prob=0.95,priors=TRUE), digits = 6)

# familyBariReligiousAfter                  -0.037040  0.035356 -0.106694  0.032621 1.000192    16511    11375
# familyBariReligiousAfter:MI_geo_proximity -0.003066  0.072818 -0.138356  0.149798 1.000452    17026    11464

M2c<-brm(childcare_help_rels ~ kids_in_hh+R_NUM_SIBS+neighbor_rels+
           religion+familyBariReligiousAfter+religious_knowledge_scale+
           MI_geo_proximity+
           MI_economic_capital+
           MI_human_capital+MI_human_capital*familyBariReligiousAfter, data=d3, family = negbinomial(link = "log", link_shape = "identity"),
         prior = c(set_prior("normal(0,2)", class = "b")),
         warmup = 1000, iter = 5000, chains = 4,
         control = list(adapt_delta = 0.95))

print(summary(M2c, prob=0.95,priors=TRUE), digits = 6)

#familyBariReligiousAfter                  -0.039730  0.035008 -0.107933  0.029565 1.000147    16915    11812
#familyBariReligiousAfter:MI_human_capital -0.070833  0.038888 -0.147542  0.004805 1.000238    17308    11909

# path<- (paste0("results/"))
# filename <- "Rels_childcare_intx_with_neighbors.rds"
# # 
# saveRDS(M2a, paste0(path, filename))

#familyBariReligiousAfter                     -0.048321  0.035388 -0.117916  0.020536 1.000053    18614    12647
#familyBariReligiousAfter:MI_economic_capital -0.087446  0.036460 -0.158963 -0.016385 0.999973    17049    13155


# compare models
fit <- add_criterion(M.2, c("loo","waic"))
fit1 <- add_criterion(M2, c("loo","waic"))
fit2 <- add_criterion(M2a, c("loo","waic"))
fit3 <- add_criterion(M2b, c("loo","waic"))
fit4 <- add_criterion(M2c, c("loo","waic"))


## model comparison
loo_compare(fit,fit1, fit2,fit3,fit4,criterion = "loo")
#      elpd_diff se_diff
# fit2   0.0       0.0  #  econ capital intx
# fit4  -1.4       2.7  #  human capital intx
# fit1  -2.2       2.2  
# fit3  -3.8       2.5  # geo proximity intx
# fit  -24.6       7.6  
# get the weights
model_weights(fit, fit1, fit2,fit3,fit4,
              weights = "waic") %>% 
  round(digits = 3)

# fit  fit1  fit2  fit3  fit4 
# 0.000 0.082 0.707 0.024 0.187 
