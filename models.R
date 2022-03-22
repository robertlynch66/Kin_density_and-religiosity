library(tidyverse)
library(brms)
library(readr)

husbands_new <- read.csv("C:/Users/robert/Dropbox/Github/Intensive extensive kin networks/data/husbands_new.csv", header = T, sep = ",")

## make sure DV's for husbands are okay
# husbands_new$rels_in_NW <- husbands_new$rels_in_NW +0.001
# husbands_new$non_rels <- husbands_new$non_rels+0.001

data1 <-  husbands_new %>% filter (Respondent_a=="Wife" | Respondent_a=="Husband")
data2 <-  husbands_new %>% filter (Respondent_a=="Wife")
data3 <-  husbands_new %>% filter (Respondent_a=="Husband")


d<- data3 %>% select(8,9,25:54)
cors <- cor(subset(d, select = c(religious_knowledge_scale_men,familyBariReligiousAfter,MI_geo_proximity,MI_human_capital,MI_economic_capital)))
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
## Brms models - Model 1 NW total

library(readr)

d$NW_total <- as.numeric(d$NW_total)
### try model
model1 <- brm(NW_total ~ Kids_a+Mothers_kids_a+
                (1|religion)+
                familyBariReligiousAfter+
                religious_knowledge_scale_men+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital, data=data3, 
              family = lognormal(),
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model1, prob=0.95,priors=TRUE), digits = 6)


#   Population-Level Effects: 
#                                Estimate Est.Error  l-95% CI u-95% CI     Rhat
# Intercept                      2.330482  0.539635  1.162309 3.496492 1.005085
# Kids_a                         0.012948  0.010085 -0.007025 0.032537 1.000188
# Mothers_kids_a                 0.005846  0.004053 -0.001991 0.013802 0.999964
# familyBariReligiousAfter      -0.011497  0.022996 -0.057181 0.034023 1.000181
# religious_knowledge_scale_men  0.006098  0.006725 -0.007154 0.019134 1.001035
# MI_geo_proximity              -0.010636  0.016545 -0.043069 0.021971 1.000314
# MI_economic_capital            0.047424  0.017096  0.013597 0.080743 1.000442
# MI_human_capital               0.101913  0.020972  0.060689 0.142398 1.000377

path<- (paste0("C:/Users/robert/Dropbox/Github/Kin_networks_men/results/"))
filename <- "NW_total_lognormal.rds"
# 
# richer and more educated men men have larger Networks 
saveRDS(model1, paste0(path, filename))

#### Model2
## run as log normal  
data3$percent_rels_in_NW<- data3$rels_in_NW/data3$NW_total
data3$percent_rels_in_NW<- data3$percent_rels_in_NW+0.01
model2 <- brm(percent_rels_in_NW ~ Kids_a+Mothers_kids_a+
                (1|religion)+familyBariReligiousAfter+
                religious_knowledge_scale_men+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital, data=data3, 
              family = lognormal(),
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model2, prob=0.95,priors=TRUE), digits = 6)

#*More religiously knowledgeable men have a slightly lower % of relatives in their social networks

# Group-Level Effects: 
#   ~religion (Number of levels: 2) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.441834  0.559399 0.007518 1.982398 1.007073     1189     1461
# 
# Population-Level Effects: 
#                                 Estimate Est.Error  l-95% CI  u-95% CI
# Intercept                     -0.489893  0.349213 -1.315277  0.332174
# Kids_a                         0.003985  0.014417 -0.023979  0.032641
# Mothers_kids_a                -0.007114  0.005681 -0.018297  0.003926
# familyBariReligiousAfter       0.043119  0.032027 -0.019706  0.104534
# religious_knowledge_scale_men -0.010638  0.008939 -0.027964  0.007028
# MI_geo_proximity               0.012802  0.023105 -0.032281  0.057846
# MI_economic_capital           -0.037437  0.023580 -0.083898  0.009211
# MI_human_capital              -0.086287  0.029937 -0.144610 -0.028472
path<- (paste0("C:/Users/robert/Dropbox/Github/Kin_networks_men/results/"))
filename <- "percent_relatives_in_NW_lognormal.rds"

saveRDS(model2, paste0(path, filename))

# Model 2.1 Relatives in Network
## run as log normal 
data3$rels_in_NW<- data3$rels_in_NW+0.01
model2.1 <- brm(rels_in_NW ~ Kids_a+Mothers_kids_a+
                  (1|religion)+familyBariReligiousAfter+
                  religious_knowledge_scale_men+
                  MI_geo_proximity+
                  MI_economic_capital+
                  MI_human_capital, data=data3, 
                family = lognormal(),
                prior = c(set_prior("normal(0,2)", class = "b")),
                warmup = 1000, iter = 5000, chains = 4,
                control = list(adapt_delta = 0.95))

print(summary(model2.1, prob=0.95,priors=TRUE), digits = 6)


path<- (paste0("C:/Users/robert/Dropbox/Github/Kin_networks_men/results/"))
filename <- "relatives_in_NW_lognormal.rds"
# NO EFFECT

# Group-Level Effects: 
# ~religion (Number of levels: 2) 
#         Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.693735  0.833425 0.017598 3.063926 1.002876     2799     4274
# 
# Population-Level Effects: 
#                                 Estimate Est.Error  l-95% CI u-95% CI     Rhat
# Intercept                      1.812897  0.559364  0.575100 3.070793 1.003213
# Kids_a                         0.018737  0.018766 -0.017900 0.055785 1.000634
# Mothers_kids_a                -0.002038  0.007435 -0.016647 0.012423 1.000086
# familyBariReligiousAfter       0.049830  0.043033 -0.034289 0.133624 1.000057
# religious_knowledge_scale_men -0.005538  0.012209 -0.029217 0.018538 1.000221
# MI_geo_proximity               0.002783  0.030870 -0.057759 0.062849 1.000162
# MI_economic_capital           -0.000400  0.031507 -0.061462 0.062189 1.000135
# MI_human_capital               0.017626  0.038791 -0.059811 0.092839 1.000620

saveRDS(model2.1, paste0(path, filename))


# Model 2.2 Non-relatives in Network
## run as Negative bionomial
data3$non_rels<- data3$non_rels+0.01

model2.2 <- brm(non_rels ~Kids_a+Mothers_kids_a+
                  (1|religion)+
                  #familyBariReligiousAfter+
                  religious_knowledge_scale_men+
                  MI_geo_proximity+
                  MI_economic_capital+
                  MI_human_capital, data=data3, 
                family = lognormal(),
                prior = c(set_prior("normal(0,2)", class = "b")),
                warmup = 1000, iter = 5000, chains = 4,
                control = list(adapt_delta = 0.95))

print(summary(model2.2, prob=0.95,priors=TRUE), digits = 6)
#*More religiously knowledgeable men and more educated men 
#*have more non-relatives in their NW's

# Group-Level Effects: 
#   ~religion (Number of levels: 2) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.851297  0.877211 0.025358 3.224507 1.003860     1686     1813
# 
# Population-Level Effects: 
#                               Estimate Est.Error  l-95% CI  u-95% CI
# Intercept                      1.006091  0.737485 -0.508956  3.081650
# Kids_a                         0.001718  0.043941 -0.084761  0.088246
# Mothers_kids_a                 0.011561  0.017374 -0.022748  0.045403
# familyBariReligiousAfter      -0.209622  0.100771 -0.405732 -0.018875
# religious_knowledge_scale_men  0.047353  0.027988 -0.007371  0.104470
# MI_geo_proximity              -0.012574  0.070499 -0.150704  0.125604
# MI_economic_capital            0.052274  0.073688 -0.096358  0.192415
# MI_human_capital               0.282185  0.090161  0.103213  0.460598

path<- (paste0("C:/Users/robert/Dropbox/Github/Kin_networks_men/results/"))
filename <- "non_relatives_in_NW_neg_bin.rds"
# 
saveRDS(model2.2, paste0(path, filename))

### Geographic distance models
HusbandNW<- read_csv("C:/Users/robert/Dropbox/PSU postdoc/Access text files Bangladesh/HusQnetworkDeID.csv")

# key variables are location and relationship
HusbandNW$relationship <- as.numeric(HusbandNW$relationship)
plyr::count(HusbandNW$relationship)


HusbandNW$relationship[is.na(HusbandNW$relationship)]<- 99
HusbandNW$location[HusbandNW$location == 0] <- NA
HusbandNW$location[HusbandNW$location >5 ] <- NA
plyr::count(HusbandNW$location)

## add id_wife to z
z <- HusbandNW %>% dplyr::select (1,2,3,5,7)

# get location of non relatives
nr <- z %>% filter (relationship==0)
r <- z %>% filter (relationship>0 & relationship<8)

# get non relatives
# join non-rels to data3 (d)
non_rels <- nr %>% left_join (data3, by=c("id_Questionaire"="idhusband"))
#non_rels <- non_rels[complete.cases(non_rels),]

non_rels$location[non_rels$location==1|non_rels$location==2] <- 2
non_rels$location[non_rels$location==3] <- 3
non_rels$location[non_rels$location==4|non_rels$location==5] <- 4
# Location Codes
# 1=khana member,
# 2=near bari/neighbor
# 3=other place in Matlab
# 4=Other Place in Bangladesh
# 5=Abroad

# subset to only neighbors or closer
#non_rels <- non_rels %>% filter(location==2)
# Make model in brms  
library(brms)
model3.1c <- brm(data = non_rels, 
                 family = cumulative("logit"),
                 location ~ 1+MI_geo_proximity+MI_economic_capital+
                   MI_human_capital +
                   Kids_a+Mothers_kids_a+
                   familyBariReligiousAfter+
                   religious_knowledge_scale_men+(1|religion),
                 prior = c(prior(normal(0, 1.5), class = Intercept),
                           prior(normal(0, 0.5), class = b)),
                 iter = 5000, warmup = 1000, cores = 4, chains = 4)
print(model3.1c)

## more religiously knowledgable men live further from 
#non-relatives in their NW

# Family: cumulative 
# Links: mu = logit; disc = identity 
# Formula: location ~ 1 + MI_geo_proximity + MI_economic_capital + MI_human_capital + Kids_a + Mothers_kids_a + familyBariReligiousAfter + religious_knowledge_scale_men + (1 | religion) 
# Data: non_rels (Number of observations: 2268) 
# Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# total post-warmup samples = 16000
# 
# Group-Level Effects: 
#   ~religion (Number of levels: 2) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.94      0.82     0.05     3.17 1.00     2581      818
# 
# Population-Level Effects: 
#                                  Estimate Est.Error l-95% CI u-95% CI Rhat
# Intercept[1]                     -6.53      0.77    -8.02    -4.92 1.00
# Intercept[2]                      1.62      0.60     0.68     2.99 1.00
# Intercept[3]                      2.57      0.60     1.64     3.94 1.00
# MI_geo_proximity                  0.04      0.08    -0.13     0.20 1.00
# MI_economic_capital               0.05      0.05    -0.05     0.16 1.00
# MI_human_capital                  0.58      0.07     0.45     0.73 1.00
# Kids_a                           -0.04      0.04    -0.12     0.04 1.00
# Mothers_kids_a                   -0.03      0.01    -0.06    -0.01 1.00
# familyBariReligiousAfter         -0.01      0.08    -0.17     0.14 1.00
# religious_knowledge_scale_men     0.08      0.02     0.04     0.12 1.00


# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# disc     1.00      0.00     1.00     1.00 1.00    16000    16000


path<- (paste0("C:/Users/robert/Dropbox/Github/Kin_networks_men/results/"))
filename <- "Geo_distance_non_relatives_ord_cum.rds"

saveRDS(model3.1c, paste0(path, filename))

###############################################################################################
###############################################################################################3
## relatives geo_distance
rels <- r %>% left_join (data3, by=c("id_Questionaire"="idhusband"))


rels$location[rels$location==1|rels$location==2] <- 2
rels$location[rels$location==3] <- 3
rels$location[rels$location==4|rels$location==5] <- 4
# Make model in brms  

model3.2c <- 
  brm(data = rels, 
      family = cumulative("logit"),
      location ~ 1+MI_geo_proximity+MI_economic_capital+
        MI_human_capital +
        Kids_a+Mothers_kids_a+
        familyBariReligiousAfter+
        religious_knowledge_scale_men+(1|religion),
      prior = c(prior(normal(0, 1.5), class = Intercept),
                prior(normal(0, 0.5), class = b)),
      iter = 5000, warmup = 1000, cores = 4, chains = 4)

print(model3.2c)

### More religiously knowledgeable men live further away from 
#relatives in their NW


# Family: cumulative 
# Links: mu = logit; disc = identity 
# Formula: location ~ 1 + MI_geo_proximity + MI_economic_capital + MI_human_capital + Kids_a + Mothers_kids_a + familyBariReligiousAfter + religious_knowledge_scale_men + (1 | religion) 
# Data: rels (Number of observations: 3554) 
# Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# total post-warmup samples = 16000
# 
# Group-Level Effects: 
#   ~religion (Number of levels: 2) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.91      0.81     0.02     2.95 1.01     1272     2400
# 
# Population-Level Effects: 
#                                 Estimate Est.Error l-95% CI u-95% CI Rhat
# Intercept[1]                     -6.49      0.76    -7.95    -4.88 1.01
# Intercept[2]                      1.59      0.58     0.74     2.93 1.01
# Intercept[3]                      2.21      0.59     1.36     3.56 1.01
# MI_geo_proximity                 -0.05      0.05    -0.14     0.03 1.00
# MI_economic_capital               0.06      0.04    -0.03     0.15 1.00
# MI_human_capital                 -0.10      0.06    -0.21     0.01 1.00
# Kids_a                            0.05      0.03    -0.00     0.10 1.00
# Mothers_kids_a                    0.01      0.01    -0.01     0.03 1.00
# familyBariReligiousAfter         -0.01      0.06    -0.12     0.11 1.00
# religious_knowledge_scale_men     0.04      0.02     0.00     0.07 1.00

# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# disc     1.00      0.00     1.00     1.00 1.00    16000    16000


path<- (paste0("C:/Users/robert/Dropbox/Github/Kin_networks_men/results/"))
filename <- "Geo_distance_relatives_ord_cum.rds"
saveRDS(model3.2c, paste0(path, filename))

############## Econ, emotional and work help

# model 4 percent_rels_econ_help
library(tidyverse)
library(brms)
library(readr)


## try beta - transform first
# beta transformation
# transform to be between 0 1nd 1 for beta distribution

data3$percent_rels_econ_help <- (data3$percent_rels_econ_help  * (484) + 0.5) / 484

model4<-brm(percent_rels_econ_help ~Kids_a+
              Mothers_kids_a+
              (1|religion)+
              familyBariReligiousAfter+
              religious_knowledge_scale_men+
              MI_geo_proximity+
              MI_economic_capital+
              MI_human_capital, data=data3, family = "lognormal",
            prior = c(set_prior("normal(0,2)", class = "b")),
            warmup = 1000, iter = 5000, chains = 4,
            control = list(adapt_delta = 0.95))
print(summary(model4, prob=0.95,priors=TRUE), digits = 6)
#More educated men have fewer % of relatives in their NW's

# Group-Level Effects: 
#   ~religion (Number of levels: 2) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.942785  1.060647 0.023720 3.973933 1.000741     3801     6005
# 
# Population-Level Effects: 
#                                Estimate Est.Error  l-95% CI  u-95% CI
# Intercept                     -1.016040  0.840680 -2.592110  0.983371
# Kids_a                         0.050100  0.073918 -0.095458  0.196368
# Mothers_kids_a                -0.014784  0.028986 -0.071998  0.041714
# familyBariReligiousAfter       0.092873  0.166140 -0.232540  0.415956
# religious_knowledge_scale_men -0.016000  0.045048 -0.103906  0.074078
# MI_geo_proximity               0.100091  0.116988 -0.127596  0.330230
# MI_economic_capital           -0.056184  0.122376 -0.294753  0.183672
# MI_human_capital              -0.351353  0.151417 -0.652848 -0.052954

path<- (paste0("C:/Users/robert/Dropbox/Github/Kin_networks_men/results/"))
filename <- "Percent_rels_econ_help_beta.rds"

saveRDS(model4, paste0(path, filename))


# 4.1) non rel econ help
library(tidyverse)
library(brms)
library(readr)

# hist(d$non_rels_econ_help)
data3$non_rels_econ_help <- as.integer(data3$non_rels_econ_help)

model4.1<-brm(non_rels_econ_help ~Kids_a+
                Mothers_kids_a+
                (1|religion)+
                familyBariReligiousAfter+
                religious_knowledge_scale_men+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital, data=data3, family = "poisson",
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))
print(summary(model4.1, prob=0.95,priors=TRUE), digits = 6)
#*Wealthier and more educated men get more economic help from non-relatives



# Group-Level Effects: 
#   ~religion (Number of levels: 2) 
#      Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.796170  0.946116 0.015480 3.387087 1.005075     2269     3158
# 
# Population-Level Effects: 
#                                 Estimate Est.Error  l-95% CI  u-95% CI
# Intercept                      0.065779  0.705625 -1.841065  1.286403
# Kids_a                        -0.081318  0.035585 -0.150532 -0.012120
# Mothers_kids_a                 0.018127  0.011965 -0.005576  0.041295
# familyBariReligiousAfter      -0.028376  0.073576 -0.169145  0.120701
# religious_knowledge_scale_men -0.002486  0.019016 -0.039292  0.035276
# MI_geo_proximity              -0.238087  0.105680 -0.466255 -0.052698
# MI_economic_capital            0.179278  0.044160  0.089973  0.265083
# MI_human_capital               0.309579  0.063859  0.186132  0.435100

path<- (paste0("C:/Users/robert/Dropbox/Github/Kin_networks_men/results/"))
filename <- "Non_rels_econ_help_neg_binom.rds"
# 
saveRDS(model4.1, paste0(path, filename))

####
# 4.2) rels_econ_help
library(tidyverse)
library(brms)
library(readr)

model4.2<-brm(rels_econ_help ~ Kids_a+
                Mothers_kids_a+
                (1|religion)+
                familyBariReligiousAfter+
                religious_knowledge_scale_men+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital, data=data3, family = "poisson",
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model4.2, prob=0.95,priors=TRUE), digits = 6)
#*No effect
#*
#*
# Group-Level Effects: 
#   ~religion (Number of levels: 2) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.830711  0.850330 0.045545 3.186508 1.001171     2450      940
# 
# Population-Level Effects: 
#                                Estimate Est.Error  l-95% CI u-95% CI     Rhat
# Intercept                      0.926063  0.618915 -0.406946 2.350704 1.001779
# Kids_a                         0.027455  0.018545 -0.009202 0.063481 1.000419
# Mothers_kids_a                -0.011201  0.007680 -0.026144 0.003997 1.000356
# familyBariReligiousAfter       0.029373  0.044305 -0.056661 0.115854 0.999958
# religious_knowledge_scale_men  0.000905  0.013189 -0.025265 0.026466 1.000366
# MI_geo_proximity              -0.008086  0.033628 -0.082258 0.051523 1.002047
# MI_economic_capital            0.036961  0.031925 -0.026726 0.098879 1.000434
# MI_human_capital              -0.022753  0.040013 -0.099401 0.055915 1.001121

path<- (paste0("C:/Users/robert/Dropbox/Github/Kin_networks_men/results/"))
filename <- "rels_econ_help_poisson.rds"
# 
saveRDS(model4.2, paste0(path, filename))

# 5) percent_rels_emotional_support
library(tidyverse)
library(brms)
library(readr)

# transform beta to be between 0 and 1
data3$percent_rels_emot_support <- (data3$percent_rels_emot_support  * (484) + 0.5) / 484

model5<-brm(percent_rels_emot_support~Kids_a+
              Mothers_kids_a+
              (1|religion)+
              familyBariReligiousAfter+
              religious_knowledge_scale_men+
              MI_geo_proximity+
              MI_economic_capital+
              MI_human_capital, data=data3, family = "lognormal",
            prior = c(set_prior("normal(0,2)", class = "b")),
            warmup = 1000, iter = 5000, chains = 4,
            control = list(adapt_delta = 0.95))

print(summary(model5, prob=0.95,priors=TRUE), digits = 6)
#*Wealthier men have a higher percentage of relatives who provide emotional support

# Group-Level Effects: 
#   ~religion (Number of levels: 2) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.790055  0.801775 0.036179 3.014068 1.007441     2975     4619
# 
# Population-Level Effects: 
#                                 Estimate Est.Error  l-95% CI u-95% CI     Rhat
# Intercept                     -0.306496  0.592079 -1.548293 1.065655 1.005088
# Kids_a                        -0.020420  0.027388 -0.074531 0.032045 1.000387
# Mothers_kids_a                -0.002720  0.010892 -0.024059 0.018599 1.000373
# familyBariReligiousAfter       0.093639  0.062359 -0.028091 0.216804 1.000420
# religious_knowledge_scale_men -0.009216  0.017837 -0.044196 0.026049 1.000126
# MI_geo_proximity               0.032515  0.042761 -0.051033 0.117245 1.000100
# MI_economic_capital           -0.082540  0.045548 -0.170881 0.008090 1.000420
# MI_human_capital              -0.057936  0.055707 -0.167995 0.052239 1.000326

path<- (paste0("C:/Users/robert/Dropbox/Github/Kin_networks_men/results/"))
 filename <- "percent_rels_emot_support_beta.rds"

saveRDS(model5, paste0(path, filename))
#*Wealthier men have a higher percentage of relatives who provide emotional support

# 5.1) emot_support_non_rels
data3$emot_support_non_rels <- as.integer(data3$emot_support_non_rels)

model5.1<-brm(emot_support_non_rels ~ Kids_a+
                Mothers_kids_a+
                (1|religion)+
                familyBariReligiousAfter+
                religious_knowledge_scale_men+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital,
              data=data3, family = "poisson",
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model5.1, prob=0.95,priors=TRUE), digits = 6)
#*More religiously knowledgeable men, richer, more educated  and those who live further from markets
#* get more emotional support from non-relatives

# Group-Level Effects: 
#   ~religion (Number of levels: 2) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.812950  0.944689 0.017202 3.493183 1.004951     1963     2032
# 
# Population-Level Effects: 
#                                Estimate Est.Error  l-95% CI u-95% CI     Rhat
# Intercept                     -0.151797  0.633930 -1.537319 1.266232 1.005041
# Kids_a                         0.059773  0.027134  0.005717 0.112357 1.000141
# Mothers_kids_a                 0.025169  0.010769  0.004156 0.046683 1.000197
# familyBariReligiousAfter       0.052672  0.064378 -0.072693 0.180368 1.000421
# religious_knowledge_scale_men  0.025954  0.015887 -0.004371 0.057246 1.000912
# MI_geo_proximity              -0.114259  0.073129 -0.275925 0.009615 1.000321
# MI_economic_capital            0.148309  0.039923  0.069181 0.224942 1.000281
# MI_human_capital               0.340274  0.054624  0.235426 0.447694 1.000387

path<- (paste0("C:/Users/robert/Dropbox/Github/Kin_networks_men/results/"))
filename <- "Non_rels_emot_support_neg_binom.rds"
# 
saveRDS(model5.1, paste0(path, filename))

# 5.2) 
library(tidyverse)
library(brms)
library(readr)
options(scipen=999)


data3$emot_support_rels <- as.integer(data3$emot_support_rels)

model5.2<-brm(emot_support_rels ~ Kids_a+
                Mothers_kids_a+
                (1|religion)+
                familyBariReligiousAfter+
                religious_knowledge_scale_men+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital, data=data3, family = "poisson",
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

#*Wealthier men have more relative who provide emotional support
#*
# Group-Level Effects: 
#   ~religion (Number of levels: 2) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.662326  0.710995 0.030916 2.651376 1.001082     2580     2887
# 
# Population-Level Effects: 
#                                 Estimate Est.Error  l-95% CI u-95% CI     Rhat
# Intercept                      1.682023  0.500203  0.558725 2.839088 1.000914
# Kids_a                        -0.000067  0.013648 -0.026815 0.026405 1.000357
# Mothers_kids_a                 0.005318  0.005539 -0.005606 0.016119 1.000243
# familyBariReligiousAfter      -0.037846  0.031421 -0.098766 0.024903 1.000833
# religious_knowledge_scale_men  0.005788  0.009303 -0.012536 0.024046 1.000136
# MI_geo_proximity               0.029607  0.018717 -0.008438 0.065034 1.000192
# MI_economic_capital            0.058439  0.022240  0.013884 0.101062 1.000058
# MI_human_capital              -0.031382  0.028016 -0.085437 0.023533 1.000698


path<- (paste0("C:/Users/robert/Dropbox/Github/Kin_networks_men/results/"))
filename <- "rels_emot_support_poisson.rds"

saveRDS(model5.2, paste0(path, filename))


#work help
#transform beta to be between 0 and 1
data3$childcare_work_help_rels_percent <- data3$childcare_work_help_rels/(data3$childcare_work_help_non_rels+data3$childcare_work_help_rels)

data3$childcare_work_help_rels_percent <- (data3$childcare_work_help_rels_percent * (484) + 0.5) / 484

model6<-brm(childcare_work_help_rels_percent~Kids_a+
              Mothers_kids_a+
              (1|religion)+
              familyBariReligiousAfter+
              religious_knowledge_scale_men+
              MI_geo_proximity+
              MI_economic_capital+
              MI_human_capital, data=data3, family = "lognormal",
            prior = c(set_prior("normal(0,2)", class = "b")),
            warmup = 1000, iter = 5000, chains = 4,
            control = list(adapt_delta = 0.95))

print(summary(model5, prob=0.95,priors=TRUE), digits = 6)
#*Wealthier men have a higher percentage of relative who help them with work

# Group-Level Effects: 
#   ~religion (Number of levels: 2) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.790055  0.801775 0.036179 3.014068 1.007441     2975     4619
# 
# Population-Level Effects: 
#                               Estimate Est.Error  l-95% CI u-95% CI     Rhat
# Intercept                     -0.306496  0.592079 -1.548293 1.065655 1.005088
# Kids_a                        -0.020420  0.027388 -0.074531 0.032045 1.000387
# Mothers_kids_a                -0.002720  0.010892 -0.024059 0.018599 1.000373
# familyBariReligiousAfter       0.093639  0.062359 -0.028091 0.216804 1.000420
# religious_knowledge_scale_men -0.009216  0.017837 -0.044196 0.026049 1.000126
# MI_geo_proximity               0.032515  0.042761 -0.051033 0.117245 1.000100
# MI_economic_capital           -0.082540  0.045548 -0.170881 0.008090 1.000420
# MI_human_capital              -0.057936  0.055707 -0.167995 0.052239 1.000326

path<- (paste0("C:/Users/robert/Dropbox/Github/Kin_networks_men/results/"))
filename <- "percent_rels_work_support_beta.rds"

saveRDS(model6, paste0(path, filename))

# 6.1) work_support_non_rels

data3$childcare_work_help_rels_non_rels <- as.integer(data3$childcare_work_help_non_rels)

model6.1<-brm(childcare_work_help_rels_non_rels ~ Kids_a+
                Mothers_kids_a+
                (1|religion)+
                familyBariReligiousAfter+
                religious_knowledge_scale_men+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital,
              data=data3, family = "poisson",
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model6.1, prob=0.95,priors=TRUE), digits = 6)
#*More religiously knowledgeable, poorer and more educated men
# have more non-relatives who help them with work

# Group-Level Effects: 
#   ~religion (Number of levels: 2) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 1.184228  1.133508 0.117106 4.366807 1.001082     3041     2555
# 
# Population-Level Effects: 
#                                 Estimate Est.Error  l-95% CI  u-95% CI
# Intercept                      0.177506  0.963044 -2.373064  1.823546
# Kids_a                        -0.099061  0.034995 -0.168371 -0.030697
# Mothers_kids_a                -0.037496  0.011057 -0.059298 -0.015900
# familyBariReligiousAfter      -0.018754  0.065597 -0.148297  0.108588
# religious_knowledge_scale_men  0.040264  0.017074  0.006004  0.073424
# MI_geo_proximity              -0.159223  0.088468 -0.344828 -0.006114
# MI_economic_capital           -0.073348  0.048536 -0.168024  0.022257
# MI_human_capital               0.503386  0.059335  0.386936  0.618286

path<- (paste0("C:/Users/robert/Dropbox/Github/Kin_networks_men/results/"))
filename <- "Non_rels_work_support_neg_binom.rds"
# 
saveRDS(model6.1, paste0(path, filename))

# 6.2) Work support rels
library(tidyverse)
library(brms)
library(readr)
options(scipen=999)


data3$childcare_work_help_rels <- as.integer(data3$childcare_work_help_rels)

model6.2<-brm(childcare_work_help_rels ~ Kids_a+
                Mothers_kids_a+
                (1|religion)+
                familyBariReligiousAfter+
                religious_knowledge_scale_men+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital, data=data3, family = "poisson",
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model6.2, prob=0.95,priors=TRUE), digits = 6)
#*More educated men have more relatives help them with work 

# Group-Level Effects: 
#   ~religion (Number of levels: 2) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.624892  0.723824 0.013414 2.668263 1.004747     2845     3909
# 
# Population-Level Effects: 
#                                Estimate Est.Error  l-95% CI  u-95% CI
# Intercept                      0.750843  0.506414 -0.419989  1.860381
# Kids_a                        -0.039765  0.025810 -0.090374  0.010326
# Mothers_kids_a                 0.010660  0.009954 -0.008857  0.030051
# familyBariReligiousAfter      -0.007050  0.057741 -0.119778  0.107388
# religious_knowledge_scale_men -0.052371  0.017857 -0.087995 -0.018260
# MI_geo_proximity              -0.166957  0.072248 -0.322312 -0.039935
# MI_economic_capital            0.037493  0.042637 -0.047014  0.119036
# MI_human_capital              -0.111994  0.054074 -0.219691 -0.007338


path<- (paste0("C:/Users/robert/Dropbox/Github/Kin_networks_men/results/"))
filename <- "rels_work_support_poisson.rds"

saveRDS(model6.2, paste0(path, filename))

#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#### re-rerun financial, emotional and childcare help received only

## first link key variables from hnr from 'Kin_density_DV.R' line 455 to 'newdata (at beginning of this file) 

hnr <- hnr %>% select(1,11,12,15,16,19,20,23:28)

# link to data 3 (line 13 above)
new <- data3 %>% left_join(hnr, by=c ("idhusband"="id_Questionaire"))

### run vars 98-109 as DV's in the following model

require(lme4)
new$nonrels_help_work_h<- as.integer(new$nonrels_help_work_h)


model<-glmer.nb(rels_aid_h~ Kids_a+
                  (1|religion)+familyBariReligiousAfter+religious_knowledge_scale_men+
                  MI_geo_proximity+
                  MI_economic_capital+
                  MI_human_capital, data=new, family = negbinomial,
                control = glmerControl(optimizer = "bobyqa"))

summary(model)
# "nonrels_help_work_h"      No effect but more educated get more help from non rels
# Fixed effects:
#                                Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                   -0.35965    0.32324  -1.113    0.266    
# Kids_a                        -0.05342    0.07574  -0.705    0.481    
# familyBariReligiousAfter       0.01771    0.15746   0.112    0.910    
# religious_knowledge_scale_men -0.00419    0.03333  -0.126    0.900    
# MI_geo_proximity              -0.22682    0.22105  -1.026    0.305    
# MI_economic_capital           -0.13928    0.11747  -1.186    0.236    
# MI_human_capital               0.70162    0.15228   4.608 4.07e-06 ***

#  "rels_help_work_h"   Less religiously knowledgeable men and *******************************
                        #less educated get more work help from their relatives 
# Fixed effects:
#                                 Estimate Std. Error z value Pr(>|z|)   
# (Intercept)                    0.45348    0.17450   2.599  0.00936 **
# Kids_a                        -0.03046    0.03888  -0.783  0.43337   
# familyBariReligiousAfter      -0.07073    0.09155  -0.773  0.43974   
# religious_knowledge_scale_men -0.04416    0.02181  -2.025  0.04289 * 
# MI_geo_proximity              -0.18865    0.10443  -1.806  0.07084 . 
# MI_economic_capital            0.09550    0.06283   1.520  0.12851   
# MI_human_capital              -0.16872    0.08348  -2.021  0.04327 *


# "non_rels_loan_h"        Nothing except more educated get more loans from non rels
#                                 Estimate Std. Error z value Pr(>|z|)   
# (Intercept)                   -0.584729   0.292597  -1.998  0.04567 * 
# Kids_a                         0.006032   0.067006   0.090  0.92827   
# familyBariReligiousAfter      -0.104579   0.145227  -0.720  0.47146   
# religious_knowledge_scale_men -0.014910   0.032405  -0.460  0.64542   
# MI_geo_proximity              -0.206869   0.188302  -1.099  0.27194   
# MI_economic_capital            0.021263   0.094882   0.224  0.82268   
# MI_human_capital               0.365863   0.135032   2.709  0.00674 **

#  "rels_loan_h"           Less religiously knowledgeable men get more loans from relatives*******************************
#                                 Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                    0.64723    0.11229   5.764 8.21e-09 ***
# Kids_a                         0.02357    0.02497   0.944   0.3453    
# familyBariReligiousAfter       0.01923    0.05822   0.330   0.7412    
# religious_knowledge_scale_men -0.02630    0.01433  -1.835   0.0665 .  
# MI_geo_proximity               0.02249    0.03770   0.597   0.5508    
# MI_economic_capital           -0.02492    0.04294  -0.580   0.5617    
# MI_human_capital              -0.08281    0.05374  -1.541   0.1233 

#  "non_rels_ask_advice_h"    Noting except more educated seek more advice from non rels
#                               Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                   -1.07016    0.29857  -3.584 0.000338 ***
# Kids_a                         0.11590    0.06599   1.756 0.079024 .  
# familyBariReligiousAfter      -0.09431    0.15141  -0.623 0.533353    
# religious_knowledge_scale_men -0.01506    0.03199  -0.471 0.637701    
# MI_geo_proximity              -0.19774    0.19529  -1.013 0.311275    
# MI_economic_capital            0.07505    0.11224   0.669 0.503714    
# MI_human_capital               0.31380    0.13639   2.301 0.021406 * 

#  "rels_ask_advice_h"        Less religious (according to wife) seek more advice from rels*******************************
                              #and men who are closer to markets seek more advice from rels
#                                 Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                    1.09528    0.09871  11.096   <2e-16 ***
# Kids_a                        -0.02940    0.02251  -1.306   0.1916    
# familyBariReligiousAfter      -0.10171    0.05026  -2.024   0.0430 *  
# religious_knowledge_scale_men -0.01669    0.01213  -1.376   0.1690    
# MI_geo_proximity               0.05653    0.02829   1.998   0.0457 *  
# MI_economic_capital            0.03912    0.03615   1.082   0.2792    
# MI_human_capital              -0.07301    0.04631  -1.577   0.1149

#  "non_rels_prestige_h"  (#q9 = Who do you know personally who has local social influence, prestige, or wealth?)
#                          Less religious (according to wife) have more prestigious non rels*******************************

#                                  Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                    0.8400603  0.0941912   8.919   <2e-16 ***
# Kids_a                         0.0068439  0.0211336   0.324   0.7461    
# familyBariReligiousAfter      -0.0963790  0.0489161  -1.970   0.0488 *  
# religious_knowledge_scale_men  0.0002151  0.0111362   0.019   0.9846    
# MI_geo_proximity              -0.0003840  0.0342018  -0.011   0.9910    
# MI_economic_capital            0.0272491  0.0352008   0.774   0.4389    
# MI_human_capital              -0.0141483  0.0444606  -0.318   0.7503 

#  "rels_prestige_h"         Less religiously knowledgeable men have more prestigious relatives*******************************
#                                 Estimate Std. Error z value Pr(>|z|)  
# (Intercept)                   -0.60816    0.26573  -2.289   0.0221 *
# Kids_a                         0.03805    0.05948   0.640   0.5224  
# familyBariReligiousAfter       0.20121    0.13528   1.487   0.1369  
# religious_knowledge_scale_men -0.07283    0.03351  -2.174   0.0297 *
# MI_geo_proximity               0.01238    0.09332   0.133   0.8944  
# MI_economic_capital            0.16585    0.09230   1.797   0.0724 .
# MI_human_capital               0.22170    0.12124   1.829   0.0674 .

#*********************************************************************************************
#  "non_rels_time_spent_h"    More religiously knowledgeable men have more non rels they spend time with
#*********************************************************************************************
#                                 Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                   -1.74649    0.43397  -4.024 5.71e-05 ***
# Kids_a                        -0.01875    0.09996  -0.188    0.851    
# familyBariReligiousAfter       0.13818    0.22961   0.602    0.547    
# religious_knowledge_scale_men  0.08074    0.04392   1.838    0.066 .  
# MI_geo_proximity              -0.21051    0.32433  -0.649    0.516    
# MI_economic_capital           -0.04807    0.16482  -0.292    0.771    
# MI_human_capital               0.24446    0.20368   1.200    0.230 

#  "rels_time_spent_h"      No effects except men in more educated hh's have more rels they spend time with
#                               Estimate Std. Error z value Pr(>|z|)  
# (Intercept)                    0.033490   0.142743   0.235   0.8145  
# Kids_a                         0.005206   0.032398   0.161   0.8723  
# familyBariReligiousAfter      -0.017280   0.072777  -0.237   0.8123  
# religious_knowledge_scale_men  0.004842   0.015785   0.307   0.7591  
# MI_geo_proximity               0.054701   0.042116   1.299   0.1940  
# MI_economic_capital           -0.013279   0.052553  -0.253   0.8005  
# MI_human_capital               0.150497   0.064654   2.328   0.0199 *

#   "non_rels_aid_h"        No effects
#                                 Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                    0.596215   0.159413   3.740 0.000184 ***
# Kids_a                         0.004483   0.035339   0.127 0.899060    
# familyBariReligiousAfter       0.084966   0.087225   0.974 0.330005    
# religious_knowledge_scale_men  0.001106   0.017879   0.062 0.950695    
# MI_geo_proximity              -0.116064   0.082172  -1.412 0.157818    
# MI_economic_capital            0.062870   0.059392   1.059 0.289800    
# MI_human_capital               0.125963   0.076658   1.643 0.100344 


#  "rels_aid_h"                No effects
#                                 Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                    1.625421   0.062747  25.905   <2e-16 ***
# Kids_a                         0.012197   0.013988   0.872   0.3832    
# familyBariReligiousAfter      -0.011003   0.032636  -0.337   0.7360    
# religious_knowledge_scale_men -0.005053   0.007573  -0.667   0.5046    
# MI_geo_proximity              -0.001205   0.023354  -0.052   0.9589    
# MI_economic_capital           -0.023917   0.024186  -0.989   0.3227    
# MI_human_capital              -0.049232   0.029819  -1.651   0.0987 . 

#### break down religious knowledge scale
religion <- read_csv("C:/Users/robert/Dropbox/PSU postdoc/Access text files Bangladesh/HusQMainDeID.csv")
religion <- religion %>% select(1,2,31:54)
plyr::count(religion$religiousEducationReadTextLevel) # 
religion$religiousEducationReadTextLevel <- dplyr::recode(religion$religiousEducationReadTextLevel, "Can" = 1,"CAN"=1,"Good" = 2, "Very good"=3)
religion$religiousEducationReadTextLevel[is.na(religion$religiousEducationReadTextLevel)]<- 0 # these are the can't read I am guessing

religion$religiousEducationMemorizeTextLevel <- dplyr::recode(religion$religiousEducationMemorizeTextLevel, "Nothing" = 0,"Little" = 1, "Half"=2,
                                                              "All"=3)
religion$religiousEducationMemorizeTextLevel[is.na(religion$religiousEducationMemorizeTextLevel)]<- 0 # these are the can't read I am guessing
# 704 are 0's

religion[, 3:26][is.na(religion[, 3:26])] <- 0

summary(religion)

d <- data3 %>% left_join (religion,by= c("idhusband"="idhusband")) 

d <- d%>% select(28,29,35:47,51:54,99:122)

## run correlations
#cors <- cor(subset(d, select = c(religious_knowledge_scale_men,familyBariReligiousAfter,MI_geo_proximity,MI_human_capital,MI_economic_capital)))

#1:13 with 17:44
# "NW_total"                                "non_rels"                               
# [3] "geo_distance_non_rels"                   "geo_distance_rels"                      
# [5] "rels_in_NW"                              "percent_rels_in_NW"                     
# [7] "rels_econ_help"                          "non_rels_econ_help"                     
# [9] "percent_rels_econ_help"                  "emot_support_rels"                      
# [11] "emot_support_non_rels"                   "percent_rels_emot_support"              
# [13] "childcare_work_help_rels"                "childcare_work_help_non_rels"           
# [15] "childcare_work_help_rels_percent" 
cors <- cor(d[ , colnames(d) != "NW_total"],  # Calculate correlations
            d$NW_total)
cors
#religiousEducationMuslim                 0.1164872289
#religiousEducationReadTextSanskrit       0.1093165294

cors <- cor(d[ , colnames(d) != "non_rels"],  # Calculate correlations
                d$non_rels)
cors
#religiousEducationNumberHolyPlaces        0.117804072  X
# religiousEducationReadReligiousText      0.084440621  X
# religiousEducationReadTextLevel          0.082800081  X
# religiousEducationReadTextBangla         0.055293069
# religiousEducationReadTextSanskrit       0.082834998
# religiousEducationReciteText             0.078163296
# religiousEducationMemorizeTextLevel      0.088246869 X
# religiousEducationGoneOnUmrah           -0.053125730 XX
# religiousEducationGoneOnHajj            -0.070852886 X
## holy places and umrah and haji negatively related to non rels

cors <- cor(d[ , colnames(d) != "rels_in_NW"],  # Calculate correlations
            d$rels_in_NW)
cors
#religion                                 -0.123580706
# religiousEducationGoneToHolyPlaces      -0.101081134
# religiousEducationNumberPilgrimageIndia -0.079087196
# religiousEducationReadOtherHolyBooks    -0.120835305
# religiousEducationReadTextBangla        -0.115591009
# religiousEducationMuslim                 0.123580706
# religiousEducationFromSchool             0.077029815
# religiousEducationFromReligiousLeader    0.058855774
# religiousEducationFromFamily            -0.107966910
#religiousEducationNumberFamilyUmrah       0.090489836
#religiousEducationGoneOnUmrah             0.062104365 XX
#religiousEducationGoneOnHajj              0.006955584
## going to holly places negatively related to rels in NW
####Counts
plyr::count(religion$religiousEducationNumberHolyPlaces) # 46 - 1 or more
plyr::count(religion$religiousEducationGoneToHolyPlaces) # 48 yes
plyr::count(religion$religiousEducationReadTextBangla) # 28 men
plyr::count(religion$religiousEducationReadTextSanskrit) #  4 men
plyr::count(religion$religiousEducationReadReligiousText) #126 can read
plyr::count(religion$religiousEducationReadTextLevel) #most (114) are level 1 (a little)
plyr::count(religion$religiousEducationMemorizeTextLevel) #24 are lowest level can


#Did you ever learn to recite from the Qur'an?  -- "religiousEducationReciteText" 
plyr::count(religion$religiousEducationReciteText)#  38 yes

#   If Yes, how much of it did you memorize?? -- "religiousEducationMemorizeTextLevel"
# All   half    A little Nothing
plyr::count(religion$religiousEducationMemorizeTextLevel) #471 are no


# Have you gone on Umrah? Yes No  -- "religiousEducationGoneOnUmrah"
plyr::count(religion$religiousEducationGoneOnUmrah)# 32 yes, 37 men

#Have you gone on hajj? -- "religiousEducationGoneOnHajj"
plyr::count(religion$religiousEducationGoneOnHajj) #  25 men  yes

#How many people in your family have gone on Umrah? -- "religiousEducationNumberFamilyUmrah" 
plyr::count(religion$religiousEducationNumberFamilyUmrah) # 418 are 0
#On hajj? -- "religiousEducationNumberFamilyHajj" 
plyr::count(religion$religiousEducationNumberFamilyHajj) # 448 are 0 

## extra questions:
# "religiousEducationReadOtherHolyBooks" 
plyr::count(religion$religiousEducationReadOtherHolyBooks) # 24 yes
#  "religiousEducationGoneOnPilgrimageIndia" 
plyr::count(religion$religiousEducationGoneOnPilgrimageIndia) # 9 yes
#"religiousEducationGoneToHolyPlaces"   
plyr::count(religion$religiousEducationGoneToHolyPlaces) # 451 none, 48 yes
#  "religiousEducationNumberPilgrimageIndia"
plyr::count(religion$religiousEducationNumberPilgrimageIndia) # 486 none - but these are hindus
#"religiousEducationNumberHolyPlaces"
plyr::count(religion$religiousEducationNumberHolyPlaces) #453 none
#####
religion$religious_knowledge_scale <- religion$religiousEducationFromSchool+religion$religiousEducationFromTemple+
  religion$religiousEducationFromReligiousLeader+religion$religiousEducationFromFamily+religion$religiousEducationFromRelatives-
  religion$religiousEducationFromNoOne-religion$religiousEducationNeverLearned+religion$religiousEducationReadReligiousText+
  religion$religiousEducationReadTextLevel+(2*religion$religiousEducationReadTextBangla)+(3*religion$religiousEducationReadTextSanskrit)+
  religion$religiousEducationReciteText+religion$religiousEducationMemorizeTextLevel+religion$religiousEducationGoneOnUmrah+
  religion$religiousEducationGoneOnHajj+(religion$religiousEducationNumberFamilyUmrah*0.5)+(religion$religiousEducationNumberFamilyHajj*0.5)+
  religion$religiousEducationReadOtherHolyBooks+religion$religiousEducationGoneOnPilgrimageIndia+religion$religiousEducationGoneToHolyPlaces+
  religion$religiousEducationNumberPilgrimageIndia+religion$religiousEducationNumberHolyPlaces


plyr::count(religion$religious_knowledge_scale) 

# calculate mens religious knowledge score next maybe (from h1)
# Next step - link to new data
religion <- religion %>% select (1,2,105)

newdata <- newdata %>% select (1:60)

#### make 2 subscales -- pilgrimages and education
religion$religious_pilgrimmage <-religion$religiousEducationGoneOnUmrah+
  religion$religiousEducationGoneOnHajj+
  (religion$religiousEducationNumberFamilyUmrah*0.5)+
  (religion$religiousEducationNumberFamilyHajj*0.5)+
  religion$religiousEducationGoneOnPilgrimageIndia+
  religion$religiousEducationGoneToHolyPlaces+
  religion$religiousEducationNumberPilgrimageIndia+
  religion$religiousEducationNumberHolyPlaces
  
religion$religious_education <- religion$religiousEducationFromSchool+
  religion$religiousEducationFromTemple+
  religion$religiousEducationFromReligiousLeader+
  religion$religiousEducationFromFamily+
  religion$religiousEducationFromRelatives-
  religion$religiousEducationFromNoOne-
  religion$religiousEducationNeverLearned+
  religion$religiousEducationReadOtherHolyBooks+
  religion$religiousEducationReadReligiousText+
  religion$religiousEducationReadTextLevel+
  (2*religion$religiousEducationReadTextBangla)+
  (3*religion$religiousEducationReadTextSanskrit)+
  religion$religiousEducationReciteText+
  religion$religiousEducationMemorizeTextLevel

religion <- religion %>% select (2,28,29)
## link religion to to data3 by 'idhusband'

data4 <- data3 %>% left_join(religion, by=c("idhusband"="idhusband"))

write.csv(data4,"religion.csv", row.names = FALSE)

## get correlations with pilgrimages 
data <- data3 %>% select(8,9,25,26,27,28,29,35,36,37,38,39:47,51,54:56)
# the predictors -- 
#"familyBariReligiousAfter"    
#"religious_knowledge_scale_men"   
# "religious_pilgrimage_men"     
#"religious_education_men" 

cors <- cor(data[ , colnames(data) != "familyBariReligiousAfter"],  # Calculate correlations
            data$familyBariReligiousAfter)
# cors
# Kids_a                                     NA
# Mothers_kids_a                             NA
# MI_geo_proximity                 -0.061258421
# MI_economic_capital               0.288951827
# MI_human_capital                  0.008838999
# NW_total                          0.031308376
# non_rels                          0.001535761
# geo_distance_non_rels                      NA
# geo_distance_rels                          NA
# rels_in_NW                        0.033823356
# percent_rels_in_NW                         NA
# rels_econ_help                    0.040222406
# non_rels_econ_help                0.009131533
# percent_rels_econ_help                     NA
# emot_support_rels                -0.018783588
# emot_support_non_rels                      NA
# percent_rels_emot_support                  NA
# childcare_work_help_rels         -0.022858471
# childcare_work_help_non_rels     -0.001095389
# childcare_work_help_rels_percent           NA
# religious_knowledge_scale_men     0.180072530
# religious_pilgrimage_men          0.100212745
# religious_education_men           0.195452936

cors <- cor(data[ , colnames(data) != "religious_knowledge_scale_men"],  # Calculate correlations
            data$religious_knowledge_scale_men)
cors
# Kids_a                                    NA
# Mothers_kids_a                            NA
# MI_geo_proximity                  0.01744385
# MI_economic_capital               0.18239494
# MI_human_capital                  0.18073199
# NW_total                          0.03558744
# non_rels                          0.10299822
# geo_distance_non_rels                     NA
# geo_distance_rels                         NA
# rels_in_NW                       -0.06585315
# percent_rels_in_NW                        NA
# rels_econ_help                   -0.03461397
# non_rels_econ_help                0.03307327
# percent_rels_econ_help                    NA
# emot_support_rels                -0.01498093
# emot_support_non_rels                     NA
# percent_rels_emot_support                 NA
# childcare_work_help_rels         -0.08838211
# childcare_work_help_non_rels      0.08734732
# childcare_work_help_rels_percent          NA
# familyBariReligiousAfter          0.18007253
# religious_pilgrimage_men          0.80891111
# religious_education_men           0.88801275

cors <- cor(data[ , colnames(data) != "religious_pilgrimage_men"],  # Calculate correlations
            data$religious_pilgrimage_men)
cors
# Kids_a                                    NA
# Mothers_kids_a                            NA
# MI_geo_proximity                  0.02148702
# MI_economic_capital               0.21019907
# MI_human_capital                  0.05677543
# NW_total                          0.02680886
# non_rels                          0.06077049
# geo_distance_non_rels                     NA
# geo_distance_rels                         NA
# rels_in_NW                       -0.03520438
# percent_rels_in_NW                        NA
# rels_econ_help                   -0.02168898
# non_rels_econ_help                0.01110700
# percent_rels_econ_help                    NA
# emot_support_rels                -0.01443005
# emot_support_non_rels                     NA
# percent_rels_emot_support                 NA
# childcare_work_help_rels         -0.05979131
# childcare_work_help_non_rels      0.02750775
# childcare_work_help_rels_percent          NA
# familyBariReligiousAfter          0.10021275
# religious_knowledge_scale_men     0.80891111
# religious_education_men           0.44798166

cors <- cor(data[ , colnames(data) != "religious_education_men"],  # Calculate correlations
            data$religious_education_men,use="pairwise.complete.obs")
cors
# Kids_a                           -0.105790594
# Mothers_kids_a                    0.045448887
# MI_geo_proximity                  0.009721232
# MI_economic_capital               0.112964595
# MI_human_capital                  0.230427921
# NW_total                          0.033149257
# non_rels                          0.109096822
# geo_distance_non_rels             0.182719065
# geo_distance_rels                 0.018969992
# rels_in_NW                       -0.072606990
# percent_rels_in_NW               -0.014717959
# rels_econ_help                   -0.035673184
# non_rels_econ_help                0.041606435
# percent_rels_econ_help           -0.090935825
# emot_support_rels                -0.011495198
# emot_support_non_rels             0.069871797
# percent_rels_emot_support        -0.043837856
# childcare_work_help_rels         -0.087636505
# childcare_work_help_non_rels      0.111311809
# childcare_work_help_rels_percent -0.157701762
# familyBariReligiousAfter          0.195452936
# religious_knowledge_scale_men     0.888012752
# religious_pilgrimage_men          0.447981657