#######################################################################################
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
newdata <- read_csv("newdata.csv")  # this is fucked up no
#  Not sure why
options(scipen=999)

cors <- cor(subset(newdata, select = c(religious_knowledge_scale.x,familyBariReligiousAfter.x,MI_geo_proximity,MI_human_capital,MI_economic_capital)))
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
model1 <- brm(NW_total ~ kids_in_hh+R_NUM_SIBS+
                (1|religion.x)+familyBariReligiousAfter.x+religious_knowledge_scale+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital, data=d, 
              family = lognormal(),
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model1, prob=0.95,priors=TRUE), digits = 6)

# Family: lognormal 
# Links: mu = identity; sigma = identity 
# Formula: NW_total ~ kids_in_hh + R_NUM_SIBS + (1 | religion) + familyBariReligiousAfter + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# Data: d (Number of observations: 752) 
# Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# total post-warmup samples = 16000
# 
# Priors: 
#   b ~ normal(0,2)
# Intercept ~ student_t(3, 2.3, 2.5)
# sd ~ student_t(3, 0, 2.5)
# sigma ~ student_t(3, 0, 2.5)
# 
# Group-Level Effects: 
#   ~religion (Number of levels: 2) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS
# sd(Intercept) 0.696890  0.754397 0.020144 2.333473 1.125802       22
# Tail_ESS
# sd(Intercept)      122
# 
# Population-Level Effects: 
#                             Estimate Est.Error  l-95% CI u-95% CI     Rhat
# Intercept                  2.315594  0.516707  1.401422 3.415147 1.195609
# kids_in_hh                -0.003462  0.007885 -0.019196 0.012595 1.100160
# R_NUM_SIBS                 0.004167  0.005804 -0.007830 0.014836 1.031972
# familyBariReligiousAfter   0.115703  0.020250  0.075294 0.156532 1.028696
# religious_knowledge_scale  0.002833  0.004444 -0.006024 0.011757 1.031531
# MI_geo_proximity          -0.018795  0.017424 -0.054086 0.014845 1.038881
# MI_economic_capital        0.017599  0.014934 -0.013361 0.047453 1.094439
# MI_human_capital           0.026417  0.019498 -0.013141 0.060695 1.067543

# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sigma 0.354440  0.009390 0.338282 0.373962 1.059644       42     6196
path<- (paste0("results/"))
filename <- "NW_total_lognormal.rds"

saveRDS(model1, paste0(path, filename))


#### Model2
library(tidyverse)
library(brms)
library(readr)

d <- newdata[c(21,47,5,7,8,9,44,45,49,51)] 
d <- d[complete.cases(d), ] 
hist(d$percent_rels_in_NW)
d <- d[complete.cases(d), ]  

d$percent_rels_in_NW<- d$percent_rels_in_NW+0.01
## run as log normal  
model2 <- brm(percent_rels_in_NW ~ kids_in_hh+R_NUM_SIBS+
                (1|religion)+familyBariReligiousAfter+religious_knowledge_scale+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital, data=d, 
              family = lognormal(),
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))



print(summary(model2, prob=0.95,priors=TRUE), digits = 6)
# Family: lognormal 
# Links: mu = identity; sigma = identity 
# Formula: percent_rels_in_NW ~ (1 | kids_in_hh) + (1 | R_NUM_SIBS) + (1 | religion) + familyBariReligiousAfter + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# Data: d (Number of observations: 752) 
# Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# total post-warmup samples = 16000
# 
# Priors: 
#   b ~ normal(0,2)
# Intercept ~ student_t(3, -0.2, 2.5)
# sd ~ student_t(3, 0, 2.5)
# sigma ~ student_t(3, 0, 2.5)
# 
# Group-Level Effects: 
#   ~kids_in_hh (Number of levels: 13) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.020633  0.013949 0.001153 0.054156 1.001313     4971     5460
# 
# ~R_NUM_SIBS (Number of levels: 12) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.024591  0.019757 0.000982 0.073432 1.001333     3171     5885
# 
# ~religion (Number of levels: 2) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.333076  0.530617 0.002684 1.886335 1.011674      639      203
# 
# Population-Level Effects: 
#   Estimate Est.Error  l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                 -0.171892  0.307240 -0.678661 0.334311 1.011447      798      414
# familyBariReligiousAfter   0.026276  0.013610 -0.000195 0.052606 1.000210    11906     8455
# religious_knowledge_scale -0.002256  0.002839 -0.007845 0.003321 1.001609    21854    11387
# MI_geo_proximity           0.008788  0.011922 -0.014712 0.032310 1.000260    15302     9232
# MI_economic_capital        0.023365  0.010076  0.003556 0.043375 1.000440    17160    10156
# MI_human_capital          -0.005058  0.011072 -0.026385 0.016981 1.000828    10617     3657
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sigma 0.225590  0.006071 0.214122 0.237908 1.001429     6816     3791
# 
path<- (paste0("results/"))
filename <- "percent_relatives_in_NW_lognormal.rds"

saveRDS(model2, paste0(path, filename))

# Model 2.1 Relatives in Network

library(tidyverse)
library(brms)
library(readr)


d <- newdata[c(19,47,4,5,7,8,9,44,45,49,51)] 
d <- d[complete.cases(d), ] 

hist(d$rels_in_NW) # log normal okay



d$rels_in_NW<- d$rels_in_NW+0.01
## run as log normal  
model2.1 <- brm(rels_in_NW ~ kids_in_hh+ R_NUM_SIBS+
                  (1|religion)+familyBariReligiousAfter+religious_knowledge_scale+
                  MI_geo_proximity+
                  MI_economic_capital+
                  MI_human_capital, data=d, 
                family = lognormal(),
                prior = c(set_prior("normal(0,2)", class = "b")),
                warmup = 1000, iter = 5000, chains = 4,
                control = list(adapt_delta = 0.95))

print(summary(model2.1, prob=0.95,priors=TRUE), digits = 6)

# Family: lognormal 
# Links: mu = identity; sigma = identity 
# Formula: rels_in_NW ~ (1 | kids_in_hh) + (1 | R_NUM_SIBS) + (1 | religion) + familyBariReligiousAfter + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# Data: d (Number of observations: 752) 
# Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# total post-warmup samples = 16000
# 
# Priors: 
#   b ~ normal(0,2)
# Intercept ~ student_t(3, 2.2, 2.5)
# sd ~ student_t(3, 0, 2.5)
# sigma ~ student_t(3, 0, 2.5)
# 
# Group-Level Effects: 
#   ~kids_in_hh (Number of levels: 13) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.021823  0.019224 0.000680 0.072172 1.000224     6372     5978
# 
# ~R_NUM_SIBS (Number of levels: 12) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.048660  0.029804 0.003702 0.119226 1.000303     4274     4634
# 
# ~religion (Number of levels: 2) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.674044  0.818919 0.020270 2.964864 1.003275     2744     4238
# 
# Population-Level Effects: 
#   Estimate Est.Error  l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                  2.071700  0.564986  0.873460 3.408663 1.002989     2766     2118
# familyBariReligiousAfter   0.134974  0.024930  0.085342 0.183095 1.000055    14380    10275
# religious_knowledge_scale -0.001981  0.005360 -0.012338 0.008414 1.000279    17021    12114
# MI_geo_proximity          -0.022547  0.021316 -0.064682 0.019571 1.000059    18763    11981
# MI_economic_capital        0.027004  0.018409 -0.009058 0.063344 1.000151    13932     9519
# MI_human_capital           0.021041  0.019045 -0.015649 0.058606 1.000037    15239    11573
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sigma 0.413632  0.010729 0.393204 0.434923 1.000516    19255    12343

path<- (paste0("results/"))
filename <- "relatives_in_NW_lognormal.rds"

saveRDS(model2.1, paste0(path, filename))


# Model 2.2 Non-relatives in Network
library(tidyverse)
library(brms)
library(readr)
d <- newdata[c(11,47,4,5,7,8,9,44,45,49,51)] 

d <- d[complete.cases(d), ] 

hist(d$non_rels) # Negative binomial
library(fitdistrplus)
library(logspline)
descdist(d$non_rels, discrete = TRUE, boot=500)

## run as Negative bionomial
model2.2 <- brm(non_rels ~(1|kids_in_hh)+(1|R_NUM_SIBS)+
                  (1|religion)+familyBariReligiousAfter+religious_knowledge_scale+
                  MI_geo_proximity+
                  MI_economic_capital+
                  MI_human_capital, data=d, 
                family = negbinomial(link = "log", link_shape = "identity"),
                prior = c(set_prior("normal(0,2)", class = "b")),
                warmup = 1000, iter = 5000, chains = 4,
                control = list(adapt_delta = 0.95))

print(summary(model2.2, prob=0.95,priors=TRUE), digits = 6)

# Family: negbinomial 
# Links: mu = log; shape = identity 
# Formula: non_rels ~ (1 | kids_in_hh) + (1 | R_NUM_SIBS) + (1 | religion) + familyBariReligiousAfter + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# Data: d (Number of observations: 752) 
# Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# total post-warmup samples = 16000
# 
# Priors: 
#   b ~ normal(0,2)
# Intercept ~ student_t(3, 0, 2.5)
# sd ~ student_t(3, 0, 2.5)
# shape ~ gamma(0.01, 0.01)
# 
# Group-Level Effects: 
#   ~kids_in_hh (Number of levels: 13) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.099399  0.084362 0.003936 0.312712 1.008262      758     5928
# 
# ~R_NUM_SIBS (Number of levels: 12) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.104996  0.085814 0.003495 0.332190 1.024905      133       76
# 
# ~religion (Number of levels: 2) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.808589  1.053949 0.012154 4.035212 1.019837      137       74
# 
# Population-Level Effects: 
#   Estimate Est.Error  l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                 -0.079070  0.545707 -1.619636 1.021312 1.021782      139       61
# familyBariReligiousAfter   0.007044  0.087650 -0.155249 0.203738 1.025306      123       21
# religious_knowledge_scale  0.014704  0.014452 -0.013586 0.044642 1.003254     2702     8359
# MI_geo_proximity           0.079579  0.076029 -0.069872 0.233623 1.004223     2621     9001
# MI_economic_capital       -0.051966  0.059143 -0.162917 0.063006 1.014672      244      822
# MI_human_capital           0.100016  0.059376 -0.017652 0.217097 1.002458     6055     8885
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# shape 1.389145  0.199194 1.060856 1.843710 1.004548     7414     9561
path<- (paste0("results/"))
filename <- "non_relatives_in_NW_neg_bin.rds"

saveRDS(model2.2, paste0(path, filename))

# Model 3.1 geo_distance_non_rels: + p=0.003268 ** 
# 
# d <- newdata[c(17,11,47,4,5,7,8,9,44,45,49,51)] 
# # missing alot here (WHY!!!) if you have no non rels in NW what is the geo distance to them?
# # because we are missing 571 here (NA's)
# # solution - make NA's max mean of geo_distance_non_rels which is 5
# # add dummy for when non rels <- 0
# d$dummy_no_non_rels <- ifelse(d$non_rels==0,1,0)
# 
# 
# d$geo_distance_non_rels[is.na(d$geo_distance_non_rels)] <- 5
# d <- d[complete.cases(d), ] 
# 
# 
# 
# 
# model3.1<-brm(geo_distance_non_rels ~ (1|kids_in_hh)+(1|R_NUM_SIBS)+dummy_no_non_rels+
#                 religion+familyBariReligiousAfter+religious_knowledge_scale+
#                 MI_geo_proximity+
#                 MI_economic_capital+
#                 MI_human_capital, data=d, family = "lognormal",
#               prior = c(set_prior("normal(0,2)", class = "b"),
#                         set_prior("normal(0,10)", class="b",coef="age_wife")),
#               warmup = 1000, iter = 5000, chains = 4,
#               control = list(adapt_delta = 0.95))
# 
# print(summary(model3.1, prob=0.95,priors=TRUE), digits = 6)
# 
# # lognormal
# # 
# # Family: lognormal 
# # Links: mu = identity; sigma = identity 
# # Formula: geo_distance_non_rels ~ kids_in_hh + sex + dummy_no_non_rels + age_wife + religion + familyBariReligiousAfter + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# # Data: d (Number of observations: 762) 
# # Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# # total post-warmup samples = 16000
# # 
# # Priors: 
# #   b ~ normal(0,2)
# # b_age_wife ~ normal(0,10)
# # Intercept ~ student_t(3, 1.1, 2.5)
# # sigma ~ student_t(3, 0, 2.5)
# # 
# # Population-Level Effects: 
# #   Estimate Est.Error  l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# # Intercept                  0.801777  0.042105  0.718716 0.884244 1.000197    19526    14135
# # kids_in_hh                -0.001025  0.003785 -0.008449 0.006489 1.000048    25312    11864
# # sex                       -0.009179  0.012642 -0.034122 0.015743 1.000215    16068    10791
# # dummy_no_non_rels          0.818128  0.011443  0.795652 0.840458 1.000120    17504    11898
# # age_wife                  -0.000190  0.000814 -0.001772 0.001410 1.000171    18913    13554
# # religion                  -0.018016  0.020931 -0.059649 0.022436 1.000178    14554    10691
# # familyBariReligiousAfter   0.022284  0.009243  0.004210 0.040338 1.000272    17899    12162
# # religious_knowledge_scale -0.001273  0.001966 -0.005117 0.002603 1.000169    17174    13479
# # MI_geo_proximity           0.016885  0.008047  0.000985 0.032598 1.000430    18914    11289
# # MI_economic_capital        0.006190  0.007118 -0.007725 0.020148 1.000486    16262    11928
# # MI_human_capital           0.006615  0.011482 -0.015828 0.029291 1.000004    17886    12741
# # 
# # Family Specific Parameters: 
# #   Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# # sigma 0.154844  0.004074 0.147050 0.162946 1.000035    17115    11941
# path<- (paste0("results/"))
# filename <- "Geo_distance_non_relatives_lognormal.rds"
# 
# saveRDS(model3.1, paste0(path, filename))
# 
# 
# # 3.2) geo_distance_rels: 
# d <- newdata[c(18,11,47,4,5,7,8,9,44,45,49,51)] 
# d <- d[complete.cases(d), ] 
# hist(d$geo_distance_rels)
# 
# 
# 
# model3.2<-brm(geo_distance_rels ~ (1|kids_in_hh)+(1|R_NUM_SIBS)+
#                 religion+ familyBariReligiousAfter+religious_knowledge_scale+
#                 MI_geo_proximity+
#                 MI_economic_capital+
#                 MI_human_capital, data=d, family = "normal",
#               prior = c(set_prior("normal(0,2)", class = "b"),
#                         set_prior("normal(0,10)", class="b",coef="age_wife")),
#               warmup = 1000, iter = 5000, chains = 4,
#               control = list(adapt_delta = 0.95))
# 
# print(summary(model3.2, prob=0.95,priors=TRUE), digits = 6)
# 
# # Family: gaussian 
# # Links: mu = identity; sigma = identity 
# # Formula: geo_distance_rels ~ kids_in_hh + sex + age_wife + religion + familyBariReligiousAfter + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# # Data: d (Number of observations: 761) 
# # Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# # total post-warmup samples = 16000
# # 
# # Priors: 
# #   b ~ normal(0,2)
# # b_age_wife ~ normal(0,10)
# # Intercept ~ student_t(3, 2.1, 2.5)
# # sigma ~ student_t(3, 0, 2.5)
# # 
# # Population-Level Effects: 
# #                             Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS Tail_ESS
# # Intercept                  2.382457  0.127843  2.136341  2.636724 1.000048    16424    12837
# # kids_in_hh                 0.024972  0.011211  0.003191  0.046862 0.999907    18840    12189
# # sex                       -0.046796  0.037278 -0.119488  0.026750 1.000525    20413    11520
# # age_wife                  -0.004208  0.002470 -0.009152  0.000542 1.000061    16060    12813
# # religion                  -0.311741  0.062008 -0.434718 -0.190776 1.000049    17079    12689
# # familyBariReligiousAfter   0.079619  0.027837  0.024500  0.133060 1.000344    21344    13298
# # religious_knowledge_scale  0.026291  0.005850  0.014873  0.037849 1.000173    16997    13527
# # MI_geo_proximity           0.034708  0.024057 -0.012653  0.081767 1.000034    18873    12590
# # MI_economic_capital       -0.018774  0.021347 -0.060814  0.022919 1.000522    16682    12898
# # MI_human_capital          -0.011440  0.034844 -0.079908  0.057620 1.000189    14660    12223
# # 
# # Family Specific Parameters: 
# #   Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# # sigma 0.459128  0.011915 0.436283 0.483151 1.000079    20367    11978
# 
# 
# path<- (paste0("results/"))
# filename <- "Geo_distance_relatives_normal.rds"
# 
# saveRDS(model3.2, paste0(path, filename))
# 
# ###### NEW MODELS (REVERSED DV)!!!!!
# # MOdeling geo_distance of relatives and non relatives against religiosity
# 
# #### Non rels first
# d <- newdata[c(1,5,7,8,9,11,19,44,45,47,49,51)] 
# d <- d[complete.cases(d), ] 
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
# # Relationship codes
# #0 = not a relative, 1 = nijer poribar (parents and kids), 2 = babar bari (fathers family- 2nd or 3rd degree relatives),
# #3 = mayer bari (mother side - 2nd or 3rd degree relatives), 4 = shoshur bari (in-laws, affinal), 
# #5= babar bari (far relative), 6 = mayer bari (far relative), 7 = shoshur bari (far relative)
# 
# # Location codes
# #1=khana member,(same household)
# #2=near bari/neighbor (walking distance), 
# #3=other place in Matlab,
# #4=Other Place in Bangladesh, 
# #5=Abroad, 
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
# 
# 
# non_rels$familyBariReligiousAfter<- as.numeric(non_rels$familyBariReligiousAfter)
# 
# 
# model3.1b<-brm(familyBariReligiousAfter ~ (1|kids_in_hh)+(1|R_NUM_SIBS)+
#                  religion+location +religious_knowledge_scale+
#                  MI_geo_proximity+
#                  MI_economic_capital+
#                  MI_human_capital, data=non_rels, family = "normal",
#                prior = c(set_prior("normal(0,2)", class = "b"),
#                          # set_prior("cauchy(0,2)", class="sd"),
#                          set_prior("normal(0,10)", class="b",coef="age_wife")),
#                warmup = 1000, iter = 5000, chains = 4,
#                control = list(adapt_delta = 0.95))
# 
# print(summary(model3.1b, prob=0.95,priors=TRUE), digits = 6)
# # Family: gaussian 
# # Links: mu = identity; sigma = identity 
# # Formula: familyBariReligiousAfter ~ kids_in_hh + age_wife + religion + location + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# # Data: non_rels (Number of observations: 796) 
# # Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# # total post-warmup samples = 16000
# # 
# # Priors: 
# #   b ~ normal(0,2)
# # b_age_wife ~ normal(0,10)
# # Intercept ~ student_t(3, 0, 2.5)
# # sigma ~ student_t(3, 0, 2.5)
# # 
# # Population-Level Effects: 
# #   Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS Tail_ESS
# # Intercept                 -0.485334  0.191843 -0.863457 -0.105756 1.000097    16017    12243
# # kids_in_hh                 0.004647  0.016910 -0.028469  0.037976 0.999979    18632    12078
# # age_wife                   0.009669  0.003154  0.003450  0.015973 1.000251    15690    12456
# # religion                   0.139755  0.078716 -0.013126  0.293314 1.000066    15998    12464
# # location                   0.096914  0.040888  0.017458  0.177166 1.000020    19296    12591
# # religious_knowledge_scale  0.004644  0.005855 -0.006848  0.016259 0.999982    17763    12879
# # MI_geo_proximity          -0.024414  0.039781 -0.101951  0.052743 1.000158    17685    11640
# # MI_economic_capital        0.127497  0.027789  0.073466  0.182119 0.999975    16248    13098
# # MI_human_capital           0.075680  0.042950 -0.007153  0.160043 1.000172    14087    12105
# # 
# # Family Specific Parameters: 
# #   Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# # sigma 0.633930  0.015909 0.603522 0.666007 1.000268    19938    11543
# 
# # save model
# path<- (paste0("results/"))
# filename <- "Religiosity_predicted_by_geo_distance_non_relatives_normal.rds"
# 
# saveRDS(model3.1b, paste0(path, filename))
# 
# ## do relatives next
# # get relatives
# # join to rels (data (d))
# rels <- r %>% left_join (d, by=c("id_Questionaire"="idwife"))
# 
# rels <- rels[complete.cases(rels),]
# 
# 
# rels$familyBariReligiousAfter<- as.numeric(rels$familyBariReligiousAfter)
# 
# model3.2b<-brm(familyBariReligiousAfter ~ (1|kids_in_hh)+(1|R_NUM_SIBS)+
#                  religion+location +religious_knowledge_scale+
#                  MI_geo_proximity+
#                  MI_economic_capital+
#                  MI_human_capital, data=rels, family = "normal",
#                prior = c(set_prior("normal(0,2)", class = "b"),
#                          set_prior("normal(0,10)", class="b",coef="age_wife")),
#                warmup = 1000, iter = 5000, chains = 4,
#                control = list(adapt_delta = 0.95))
# 
# print(summary(model3.2b, prob=0.95,priors=TRUE), digits = 6)
# Family: gaussian 
# # Links: mu = identity; sigma = identity 
# # Formula: familyBariReligiousAfter ~ kids_in_hh + age_wife + religion + location + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# # Data: rels (Number of observations: 5851) 
# # Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# # total post-warmup samples = 16000
# # 
# # Priors: 
# #   b ~ normal(0,2)
# # b_age_wife ~ normal(0,10)
# # Intercept ~ student_t(3, 0, 2.5)
# # sigma ~ student_t(3, 0, 2.5)
# # 
# # Population-Level Effects: 
# #   Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS Tail_ESS
# # Intercept                 -0.294561  0.060675 -0.413095 -0.175248 1.000501    17159    12483
# # kids_in_hh                -0.014249  0.005331 -0.024618 -0.003956 1.000003    21414    11927
# # age_wife                   0.010656  0.001106  0.008485  0.012833 1.000522    16753    13342
# # religion                  -0.101828  0.030061 -0.160472 -0.041856 1.000092    15706    12041
# # location                   0.020988  0.007530  0.006452  0.035783 1.000017    19926    11303
# # religious_knowledge_scale  0.013355  0.003054  0.007284  0.019252 1.000266    16884    13723
# # MI_geo_proximity          -0.063704  0.011264 -0.086158 -0.041496 1.000668    18289    11214
# # MI_economic_capital        0.167306  0.009392  0.149057  0.185525 1.000127    15977    11219
# # MI_human_capital           0.076404  0.015696  0.045315  0.107305 1.000909    14558    11550
# # 
# # Family Specific Parameters: 
# #   Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# # sigma 0.585961  0.005454 0.575407 0.596728 1.000496    18374    10797
# # save model
# path<- (paste0("results/"))
# filename <- "Religiosity_predicted_by_geo_distance_relatives_normal.rds"
# 
# saveRDS(model3.2b, paste0(path, filename))
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#### make geo distance and ordinal categorical outcome variable for non relatives

# in rethinking and brms
setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh")
newdata <- read_csv("newdata.csv")
options(scipen=999)

library(tidyverse)
library(brms)
library(readr)

d <- newdata[c(1,5,7,8,9,36,35,44,45,47,49,51)] # add 36 for non-rels and 35 for rels
d <- d[complete.cases(d), ] 

# read in wife NW
#WifeNW <- read_csv("HHQPeopleinNW.csv")
WifeNW <-  read_csv("C:/Users/robert/Dropbox/PSU postdoc/Access text files Bangladesh/HHQPeopleinNW.csv")

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

# get non relatives
# join non-rels to data (d)
non_rels <- nr %>% left_join (d, by=c("id_Questionaire"="idwife"))
non_rels <- non_rels[complete.cases(non_rels),]

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
      location ~ 1 +   MI_geo_proximity + MI_economic_capital + MI_human_capital +
         religious_knowledge_scale +  familyBariReligiousAfter+kids_in_hh+R_NUM_SIBS+
        (1|religion),
      prior = c(prior(normal(0, 1.5), class = Intercept),
                prior(normal(0, 0.5), class = b)),
      iter = 5000, warmup = 1000, cores = 4, chains = 4)

print(model3.1c)

# Family: cumulative 
# Links: mu = logit; disc = identity 
# Formula: location ~ 1 + MI_geo_proximity + MI_economic_capital + MI_human_capital + religious_knowledge_scale + familyBariReligiousAfter + kids_in_hh + R_NUM_SIBS + (1 | religion) 
# Data: non_rels (Number of observations: 779) 
# Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# total post-warmup samples = 16000
# 
# Group-Level Effects: 
#   ~religion (Number of levels: 2) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     1.13      0.93     0.10     3.61 1.00     2773     1575
# 
# Population-Level Effects: 
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept[1]                 -5.76      0.79    -7.38    -4.25 1.00     6508     3861
# Intercept[2]                  2.00      0.59     0.85     3.24 1.00     6605     3985
# Intercept[3]                  3.41      0.60     2.26     4.67 1.00     6552     5611
# MI_geo_proximity              0.23      0.15    -0.08     0.52 1.00    14205     9830
# MI_economic_capital           0.14      0.11    -0.08     0.36 1.00     8280     7140
# MI_human_capital              0.03      0.13    -0.24     0.29 1.00    10759     9543
# religious_knowledge_scale    -0.03      0.03    -0.09     0.03 1.00     8465     8417
# familyBariReligiousAfter      0.52      0.15     0.23     0.82 1.00    14429     9691
# kids_in_hh                   -0.12      0.07    -0.26     0.02 1.00    10788    10192
# R_NUM_SIBS                   -0.03      0.04    -0.12     0.05 1.00    12523     3796
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# disc     1.00      0.00     1.00     1.00 1.00    16000    16000

#save non rels ordinal outcome model results
path<- (paste0("results/"))
filename <- "Geo_distance_non_relatives_ord_cum.rds"

saveRDS(model3.1c, paste0(path, filename))

###############################################################################################
###############################################################################################3
## relatives geo_distance
rels <- r %>% left_join (d, by=c("id_Questionaire"="idwife"))
rels <- rels[complete.cases(rels),]

rels$location[rels$location==1|rels$location==2] <- 2
rels$location[rels$location==3] <- 3
rels$location[rels$location==4|rels$location==5] <- 4
# Make model in brms  

model3.2c <- 
  brm(data = rels, 
      family = cumulative("logit"),
      location ~ 1 +  MI_geo_proximity + MI_economic_capital + MI_human_capital +
         religious_knowledge_scale + familyBariReligiousAfter+kids_in_hh+R_NUM_SIBS+(1|religion),
      prior = c(prior(normal(0, 1.5), class = Intercept),
                prior(normal(0, 0.5), class = b)),
      iter = 5000, warmup = 1000, cores = 4, chains = 4)

print(model3.2c)

# Family: cumulative 
# Links: mu = logit; disc = identity 
# Formula: location ~ 1 + MI_geo_proximity + MI_economic_capital + MI_human_capital + religious_knowledge_scale + familyBariReligiousAfter + (1 | kids_in_hh) + (1 | R_NUM_SIBS) + (1 | religion) 
# Data: rels (Number of observations: 5780) 
# Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# total post-warmup samples = 16000
# 
# Group-Level Effects: 
#   ~kids_in_hh (Number of levels: 13) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.15      0.07     0.05     0.33 1.03      110      714
# 
# ~R_NUM_SIBS (Number of levels: 12) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.05      0.03     0.00     0.13 1.01      651     5543
# 
# ~religion (Number of levels: 2) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     1.64      1.18     0.24     4.16 1.10       29      112
# 
# Population-Level Effects: 
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept[1]                 -1.81      0.71    -3.05    -0.67 1.07       38      335
# Intercept[2]                  0.50      0.72    -0.79     1.65 1.07       34      181
# Intercept[3]                  1.24      0.72    -0.05     2.38 1.07       34      166
# Intercept[4]                  2.60      0.72     1.32     3.74 1.07       35      167
# MI_geo_proximity              0.05      0.03    -0.02     0.12 1.02     4517    10010
# MI_economic_capital          -0.10      0.03    -0.16    -0.04 1.04     8584     8906
# MI_human_capital              0.01      0.03    -0.06     0.07 1.05     8167     9696
# religious_knowledge_scale     0.05      0.01     0.03     0.06 1.01      224     1460
# familyBariReligiousAfter      0.14      0.04     0.06     0.22 1.02      688      926
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# disc     1.00      0.00     1.00     1.00 1.00    16000    16000

#save rels ordinal outcome model results
path<- (paste0("results/"))
filename <- "Geo_distance_relatives_ord_cum.rds"
saveRDS(model3.2c, paste0(path, filename))

#############################################################################################
#############################################################################################
#############################################################################################

# model 4 percent_rels_econ_help:  NS (Not included in top models)
library(tidyverse)
library(brms)
library(readr)

d <- newdata[c(27,11,47,4,5,7,8,9,44,45,49,51)] 
d <- d[complete.cases(d), ] 


## try beta - transform first
# beta transformation
# transform to be between 0 1nd 1 for beta distribution

d$percent_rels_econ_help <- (d$percent_rels_econ_help  * (783) + 0.5) / 784


model4<-brm(percent_rels_econ_help ~ (1|kids_in_hh)+(1|R_NUM_SIBS)+
              (1|religion)+familyBariReligiousAfter+religious_knowledge_scale+
              MI_geo_proximity+
              MI_economic_capital+
              MI_human_capital, data=d, family = "beta",
            prior = c(set_prior("normal(0,2)", class = "b")),
            warmup = 1000, iter = 5000, chains = 4,
            control = list(adapt_delta = 0.95))

print(summary(model4, prob=0.95,priors=TRUE), digits = 6)

# Family: beta 
# Links: mu = logit; phi = identity 
# Formula: percent_rels_econ_help ~ (1 | kids_in_hh) + (1 | R_NUM_SIBS) + (1 | religion) + familyBariReligiousAfter + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# Data: d (Number of observations: 752) 
# Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# total post-warmup samples = 16000
# 
# Priors: 
#   b ~ normal(0,2)
# Intercept ~ student_t(3, 0, 2.5)
# phi ~ gamma(0.01, 0.01)
# sd ~ student_t(3, 0, 2.5)
# 
# Group-Level Effects: 
#   ~kids_in_hh (Number of levels: 13) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.057991  0.049738 0.002398 0.183946 1.000217     8194     7904
# 
# ~R_NUM_SIBS (Number of levels: 12) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.063233  0.050986 0.002431 0.190556 1.000369     7577     6267
# 
# ~religion (Number of levels: 2) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.700355  0.868550 0.013375 3.183154 1.001254     3121     5619
# 
# Population-Level Effects: 
#   Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                  1.382907  0.558313 -0.092059  2.451280 1.001738     3320     2382
# familyBariReligiousAfter   0.036138  0.071563 -0.105071  0.177477 1.000381    14954    11799
# religious_knowledge_scale -0.036372  0.015313 -0.064810 -0.003423 1.000102    13579     9788
# MI_geo_proximity          -0.002117  0.056285 -0.107558  0.112548 1.000346    16958    11671
# MI_economic_capital       -0.015205  0.052034 -0.117652  0.085772 1.000601    14227    12011
# MI_human_capital           0.099292  0.052741 -0.003003  0.203736 1.000138    14228    11717
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# phi 1.041586  0.058490 0.932179 1.161844 1.000096    13832    10761
path<- (paste0("results/"))
filename <- "Percent_rels_econ_help_beta.rds"

saveRDS(model4, paste0(path, filename))

# 4.1) non rel econ help: - (poisson) p=0.06
library(tidyverse)
library(brms)
library(readr)

d <- newdata[c(25,11,47,4,5,7,8,9,44,45,49,51)] 

d <- d[complete.cases(d), ] 

hist(d$non_rels_econ_help)
d$non_rels_econ_help <- as.integer(d$non_rels_econ_help)
# repeat for specific questions (getting financial help) # 67-80
#new$non_rels_loan <- as.integer(new$non_rels_loan)


model4.1<-brm(non_rels_econ_help ~ (1|kids_in_hh)+(1|R_NUM_SIBS)+
                (1|religion)+familyBariReligiousAfte.x+religious_knowledge_scale.x+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital, data=d, family = negbinomial(link = "log", link_shape = "identity"),
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model4.1, prob=0.95,priors=TRUE), digits = 6)
# Family: negbinomial 
# Links: mu = log; shape = identity 
# Formula: non_rels_econ_help ~ (1 | kids_in_hh) + (1 | R_NUM_SIBS) + (1 | religion) + familyBariReligiousAfter + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# Data: d (Number of observations: 752) 
# Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# total post-warmup samples = 16000
# 
# Priors: 
#   b ~ normal(0,2)
# Intercept ~ student_t(3, -2.3, 2.5)
# sd ~ student_t(3, 0, 2.5)
# shape ~ gamma(0.01, 0.01)
# 
# Group-Level Effects: 
#   ~kids_in_hh (Number of levels: 13) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.196382  0.180942 0.006923 0.661025 1.000572     7533     9125
# 
# ~R_NUM_SIBS (Number of levels: 12) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.291504  0.201760 0.015613 0.775269 1.000905     5190     5503
# 
# ~religion (Number of levels: 2) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 1.117158  1.167834 0.033903 4.261522 1.000188     4561     5346
# 
# Population-Level Effects: 
#   Estimate Est.Error  l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                 -1.492886  0.865401 -3.555475 0.201300 1.000398     7032     5090
# familyBariReligiousAfter  -0.204547  0.175564 -0.551022 0.135022 1.000355    27468    12369
# religious_knowledge_scale  0.052759  0.040229 -0.015559 0.140916 1.000675    17779    11643
# MI_geo_proximity           0.205815  0.299104 -0.324873 0.845280 1.000138    25322    10470
# MI_economic_capital        0.008121  0.139815 -0.265716 0.283439 1.000119    24622    12098
# MI_human_capital           0.179327  0.149143 -0.116641 0.470741 1.000139    22232    12029
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# shape 0.169538  0.028875 0.120788 0.233287 1.000038    22434    11760


path<- (paste0("results/"))
filename <- "Non_rels_econ_help_neg_binom.rds"

saveRDS(model4.1, paste0(path, filename))


# 4.2) rels_econ_help: poisson (Not included in top models)
library(tidyverse)
library(brms)
library(readr)

d <- newdata[c(23,11,47,4,5,7,8,9,44,45,49,51)] 

d <- d[complete.cases(d), ] 

hist(d$rels_econ_help)

d$rels_econ_help<-d$rels_econ_help-0.001
d$rels_econ_help<- as.integer(d$rels_econ_help)

model4.2<-brm(rels_econ_help ~ (1|kids_in_hh)+(1|R_NUM_SIBS)+
                (1|religion)+familyBariReligiousAfter+religious_knowledge_scale+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital, data=d, family = "poisson",
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model4.2, prob=0.95,priors=TRUE), digits = 6)
# Family: poisson 
# Links: mu = log 
# Formula: rels_econ_help ~ (1 | kids_in_hh) + (1 | R_NUM_SIBS) + (1 | religion) + familyBariReligiousAfter + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# Data: d (Number of observations: 752) 
# Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# total post-warmup samples = 16000
# 
# Priors: 
#   b ~ normal(0,2)
# Intercept ~ student_t(3, 1.1, 2.5)
# sd ~ student_t(3, 0, 2.5)
# 
# Group-Level Effects: 
#   ~kids_in_hh (Number of levels: 13) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.114950  0.057781 0.026024 0.251002 1.000542     3501     3191
# 
# ~R_NUM_SIBS (Number of levels: 12) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.106083  0.055033 0.021126 0.236506 1.001113     3572     4047
# 
# ~religion (Number of levels: 2) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 1.092960  0.970580 0.162679 3.784114 1.000772     3726     2668
# 
# Population-Level Effects: 
#                             Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                  0.932590  0.770113 -0.673859  2.721042 1.000703     4841     2265
# familyBariReligiousAfter  -0.009429  0.034295 -0.076778  0.057954 0.999960    14911    12342
# religious_knowledge_scale  0.007081  0.007318 -0.007554  0.020870 0.999965    17365    12733
# MI_geo_proximity          -0.002464  0.027554 -0.058205  0.048592 0.999842    15193    11374
# MI_economic_capital       -0.058060  0.027010 -0.111356 -0.004064 1.000424     8970     2765
# MI_human_capital           0.099496  0.030178  0.039618  0.157337 1.000188     8303    10337


path<- (paste0("results/"))
filename <- "rels_econ_help_poisson.rds"

saveRDS(model4.2, paste0(path, filename))

# 5) percent_rels_emotional_support: + 0.0251395  p=  0.602 (Not included in top models)???
library(tidyverse)
library(brms)
library(readr)

d <- newdata[c(33,11,47,4,5,7,8,9,44,45,49,51)] 

d <- d[complete.cases(d), ] 

# hist(d$percent_rels_emot_support)
# 
# library(fitdistrplus)
# library(logspline)
# descdist(d$percent_rels_emot_support, discrete = FALSE, boot=500)  # beta
# library(gamlss)
# fit <- fitDist(d$percent_rels_emot_support, k = 2, type = "realAll", trace = FALSE, try.gamlss = TRUE)
# summary(fit)
# transform beta to be between 0 and 1
d$percent_rels_emot_support <- (d$percent_rels_emot_support  * (783) + 0.5) / 784

model5<-brm(percent_rels_emot_support ~ (1|kids_in_hh)+(1|R_NUM_SIBS)+
              (1|religion)+familyBariReligiousAfter+religious_knowledge_scale+
              MI_geo_proximity+
              MI_economic_capital+
              MI_human_capital, data=d, family = "beta",
            prior = c(set_prior("normal(0,2)", class = "b")),
            warmup = 1000, iter = 5000, chains = 4,
            control = list(adapt_delta = 0.95))

print(summary(model5, prob=0.95,priors=TRUE), digits = 6)

# Family: beta 
# Links: mu = logit; phi = identity 
# Formula: percent_rels_emot_support ~ (1 | kids_in_hh) + (1 | R_NUM_SIBS) + (1 | religion) + familyBariReligiousAfter + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# Data: d (Number of observations: 752) 
# Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# total post-warmup samples = 16000
# 
# Priors: 
#   b ~ normal(0,2)
# Intercept ~ student_t(3, 0, 2.5)
# phi ~ gamma(0.01, 0.01)
# sd ~ student_t(3, 0, 2.5)
# 
# Group-Level Effects: 
#   ~kids_in_hh (Number of levels: 13) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.047763  0.044842 0.001691 0.182882 1.008547      587      144
# 
# ~R_NUM_SIBS (Number of levels: 12) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.041153  0.035879 0.001638 0.132309 1.000980     4440     6089
# 
# ~religion (Number of levels: 2) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.748479  0.941151 0.011012 3.432015 1.009530      617      143
# 
# Population-Level Effects: 
#   Estimate Est.Error  l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                  2.777198  0.711377  0.597732 3.901534 1.009585      493      113
# familyBariReligiousAfter   0.043939  0.063441 -0.079353 0.168696 1.002166     7067     6605
# religious_knowledge_scale  0.001069  0.012261 -0.021401 0.027138 1.001319    14354    10339
# MI_geo_proximity           0.001416  0.051993 -0.089723 0.112717 1.001435     2532     9698
# MI_economic_capital       -0.020864  0.046101 -0.110386 0.069506 1.001581    10994    11200
# MI_human_capital          -0.031656  0.046801 -0.120907 0.060356 1.002453     2447     8703
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# phi 4.406005  0.346895 3.753885 5.121690 1.000413    13461    11534
path<- (paste0("results/"))
filename <- "percent_rels_emot_support_beta.rds"

saveRDS(model5, paste0(path, filename))


# 5.1) emot_support_non_rels:  + B=0.018276   p= 0.869 (Not included in top models)
library(tidyverse)
library(brms)
library(readr)
d <- newdata[c(31,11,47,4,5,7,8,9,44,45,49,51)] 

d <- d[complete.cases(d), ] 

# hist(d$emot_support_non_rels)
# 
# library(fitdistrplus)
# library(logspline)
# descdist(d$emot_support_non_rels, discrete = TRUE, boot=500)  # negative binomial
d$emot_support_non_rels <- as.integer(d$emot_support_non_rels)

model5.1<-brm(emot_support_non_rels ~ (1|kids_in_hh)+(1|R_NUM_SIBS)+(1|religion)+
                familyBariReligiousAfter+religious_knowledge_scale+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital, data=d, family = negbinomial(link = "log", link_shape = "identity"),
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model5.1, prob=0.95,priors=TRUE), digits = 6)

# Family: negbinomial 
# Links: mu = log; shape = identity 
# Formula: emot_support_non_rels ~ (1 | kids_in_hh) + (1 | R_NUM_SIBS) + (1 | religion) + familyBariReligiousAfter + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# Data: d (Number of observations: 752) 
# Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# total post-warmup samples = 16000
# 
# Priors: 
#   b ~ normal(0,2)
# Intercept ~ student_t(3, -2.3, 2.5)
# sd ~ student_t(3, 0, 2.5)
# shape ~ gamma(0.01, 0.01)
# 
# Group-Level Effects: 
#   ~kids_in_hh (Number of levels: 13) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.351789  0.277723 0.015629 1.040125 1.000650     6125     7353
# 
# ~R_NUM_SIBS (Number of levels: 12) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.258287  0.220492 0.009839 0.811467 1.000583     5671     8069
# 
# ~religion (Number of levels: 2) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 1.072075  1.151874 0.027088 4.200423 1.001552     2390     1937
# 
# Population-Level Effects: 
#   Estimate Est.Error  l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                 -1.629575  0.902728 -3.551397 0.403435 1.003267     1430      544
# familyBariReligiousAfter  -0.085231  0.247422 -0.572843 0.391016 1.000427    12860     6713
# religious_knowledge_scale  0.047723  0.068762 -0.080382 0.189515 1.000483    15835    10077
# MI_geo_proximity           0.196316  0.443015 -0.617600 1.141367 1.000040    17896    10746
# MI_economic_capital        0.008942  0.187903 -0.355339 0.384784 1.000539     4635     2476
# MI_human_capital           0.402121  0.227906 -0.032774 0.861917 1.000677    15918    11269
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# shape 0.077556  0.016373 0.050505 0.115531 1.000895     4732     2444

path<- (paste0("results/"))
filename <- "Non_rels_emot_support_neg_binom.rds"

saveRDS(model5.1, paste0(path, filename))


# 5.2) emot_support_rels: + B= 0.179870   p < 0.0000000000000002 **
library(tidyverse)
library(brms)
library(readr)
options(scipen=999)


d <- newdata[c(29,11,47,4,5,7,8,9,44,45,49,51)]

d <- d[complete.cases(d), ]


d$emot_support_rels <- as.integer(d$emot_support_rels)

model5.2<-brm(emot_support_rels ~ (1|kids_in_hh)+(1|R_NUM_SIBS)+
                (1|religion)+familyBariReligiousAfter+religious_knowledge_scale+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital, data=d, family = "poisson",
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model5.2, prob=0.95,priors=TRUE), digits = 6)

# Family: poisson 
# Links: mu = log 
# Formula: emot_support_rels ~ (1 | kids_in_hh) + (1 | R_NUM_SIBS) + (1 | religion) + familyBariReligiousAfter + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# Data: d (Number of observations: 752) 
# Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# total post-warmup samples = 16000
# 
# Priors: 
#   b ~ normal(0,2)
# Intercept ~ student_t(3, 1.6, 2.5)
# sd ~ student_t(3, 0, 2.5)
# 
# Group-Level Effects: 
#   ~kids_in_hh (Number of levels: 13) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.104389  0.050648 0.031132 0.233928 1.001951     3253     4426
# 
# ~R_NUM_SIBS (Number of levels: 12) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.076202  0.031557 0.028378 0.152519 1.000695     4440     6176
# 
# ~religion (Number of levels: 2) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.603442  0.811312 0.010301 3.384512 1.022251      174       47
# 
# Population-Level Effects: 
#   Estimate Est.Error  l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                  1.630059  0.391178  0.725267 2.525262 1.003047     2881     1425
# familyBariReligiousAfter   0.186163  0.025129  0.136977 0.236581 1.000678    12338    11057
# religious_knowledge_scale  0.005805  0.004963 -0.004203 0.015479 1.002155     7395     3461
# MI_geo_proximity          -0.046299  0.026985 -0.101596 0.004939 1.000321     7134     7857
# MI_economic_capital       -0.005542  0.018370 -0.041639 0.030470 1.000829     8357     9334
# MI_human_capital          -0.020864  0.021593 -0.064503 0.020578 1.000737     7591     9784

path<- (paste0("results/"))
filename <- "rels_emot_support_poisson.rds"

saveRDS(model5.2, paste0(path, filename))

# 6) childcare_help_rels_percent: B=0.08 p=0.08 (religious knowledge however is a very significant negative predictor)
library(tidyverse)
library(brms)
library(readr)
d <- newdata[c(37,11,47,4,5,7,8,9,44,45,49,51)] 

d <- d[complete.cases(d), ] 

# hist(d$childcare_help_rels_percent)
# 
# library(fitdistrplus)
# library(logspline)
# descdist(d$childcare_help_rels_percent, discrete = FALSE, boot=500)  # beta
# library(gamlss)
# fit <- fitDist(d$childcare_help_rels_percent, k = 2, type = "real0to1", trace = FALSE, try.gamlss = TRUE)
# #  type = 
# #c("realAll", "realline", "realplus", "real0to1", "counts", "binom","extra")
# summary(fit)
# transform beta to be between 0 and 1
d$childcare_help_rels_percent <- (d$childcare_help_rels_percent * (783) + 0.5) / 784

model6<-brm(childcare_help_rels_percent ~ kids_in_hh+R_NUM_SIBS+
              (1|religion)+familyBariReligiousAfter+religious_knowledge_scale+
              MI_geo_proximity+
              MI_economic_capital+
              MI_human_capital, data=d, family = "beta",
            prior = c(set_prior("normal(0,2)", class = "b")),
            warmup = 1000, iter = 5000, chains = 4,
            control = list(adapt_delta = 0.95))

print(summary(model6, prob=0.95,priors=TRUE), digits = 6)

# Family: beta 
# Links: mu = logit; phi = identity 
# Formula: childcare_help_rels_percent ~ (1 | kids_in_hh) + (1 | R_NUM_SIBS) + (1 | religion) + familyBariReligiousAfter + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# Data: d (Number of observations: 752) 
# Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# total post-warmup samples = 16000
# 
# Priors: 
#   b ~ normal(0,2)
# Intercept ~ student_t(3, 0, 2.5)
# phi ~ gamma(0.01, 0.01)
# sd ~ student_t(3, 0, 2.5)
# 
# Group-Level Effects: 
#   ~kids_in_hh (Number of levels: 13) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.046394  0.039795 0.001712 0.147008 1.000306    10775     8101
# 
# ~R_NUM_SIBS (Number of levels: 12) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.049226  0.040751 0.001800 0.152348 1.000136     9228     8268
# 
# ~religion (Number of levels: 2) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.695425  0.926025 0.010843 3.167533 1.003925     2606     1609
# 
# Population-Level Effects: 
#   Estimate Est.Error  l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                  2.103508  0.548585  0.652914 3.119864 1.005646     4577     3296
# familyBariReligiousAfter   0.051566  0.064569 -0.076603 0.177945 1.000561    16785     7091
# religious_knowledge_scale -0.024965  0.018361 -0.056302 0.016338 1.001013    11175     4320
# MI_geo_proximity          -0.032947  0.059744 -0.144962 0.091985 1.000367    19075    10639
# MI_economic_capital       -0.007128  0.050005 -0.106609 0.090199 1.000132    10095     3735
# MI_human_capital          -0.017854  0.048379 -0.111825 0.075513 1.000197    15278    10631
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# phi 1.792984  0.126353 1.558735 2.052489 1.000443    16271     8787
path<- (paste0("results/"))
filename <- "percent_rels_childcare_help_beta.rds"

saveRDS(model6, paste0(path, filename))

# 6.1) childcare_help_non_rels: B= -1.14444    p= 0.00000000000791 ***
library(tidyverse)
library(brms)
library(readr)

d <- newdata[c(36,11,47,4,5,7,8,9,44,45,49,51)] 

d <- d[complete.cases(d), ] 

d$childcare_help_non_rels<-as.integer(d$childcare_help_non_rels)

model6.1<-brm(childcare_help_non_rels ~ kids_in_hh+R_NUM_SIBS+
                (1|religion)+familyBariReligiousAfter+religious_knowledge_scale+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital, data=d, family = negbinomial(link = "log", link_shape = "identity"),
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model6.1, prob=0.95,priors=TRUE), digits = 6)

# Family: negbinomial 
# Links: mu = log; shape = identity 
# Formula: childcare_help_non_rels ~ kids_in_hh + R_NUM_SIBS + (1 | religion) + familyBariReligiousAfter + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# Data: d (Number of observations: 752) 
# Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# total post-warmup samples = 16000
# 
# Priors: 
#   b ~ normal(0,2)
# Intercept ~ student_t(3, -2.3, 2.5)
# sd ~ student_t(3, 0, 2.5)
# shape ~ gamma(0.01, 0.01)
# 
# Group-Level Effects: 
#   ~religion (Number of levels: 2) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 1.115379  1.138840 0.035970 4.366621 1.001678     3253     1809
# 
# Population-Level Effects: 
#                               Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                 -2.441607  0.986410 -4.473167 -0.236083 1.000451     3296     1641
# kids_in_hh                 0.295618  0.147360  0.018930  0.598181 1.000249     9333     8390
# R_NUM_SIBS                -0.003478  0.108458 -0.215159  0.207683 1.000182    10708     9296
# familyBariReligiousAfter  -1.360190  0.398587 -2.189932 -0.620257 1.000469     9663     9711
# religious_knowledge_scale  0.057322  0.059438 -0.042285  0.193151 1.000891     8438     4352
# MI_geo_proximity           0.770426  0.663981 -0.340313  2.231493 1.000334    10181     8727
# MI_economic_capital        0.112392  0.229883 -0.343108  0.561602 1.000408    12493    11071
# MI_human_capital           0.574118  0.330503 -0.052628  1.241909 1.000260    11355    10973
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# shape 0.076820  0.024253 0.040997 0.135011 1.000057    10848    10617

path<- (paste0("results/"))
filename <- "childcare_help_non_rels_neg_binom.rds"

saveRDS(model6.1, paste0(path, filename))

# 6.2) childcare_help_rels: B= 0.012237   p=0.631751  (Not included in top model)
#** #the result here is that less religious people have more childcare help from non relatives
#*library(tidyverse)
library(brms)
library(readr)

d <- newdata[c(35,11,47,4,5,7,8,9,44,45,49,51)] 

d <- d[complete.cases(d), ] 


d$childcare_help_rels<-as.integer(d$childcare_help_rels)
model6.2<-brm(childcare_help_rels ~ (1|kids_in_hh)+(1|R_NUM_SIBS)+
                (1|religion)+familyBariReligiousAfter+religious_knowledge_scale+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital, data=d, family = negbinomial(link = "log", link_shape = "identity"),
              prior = c(set_prior("normal(0,1)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model6.2, prob=0.95,priors=TRUE), digits = 6)

# Family: negbinomial 
# Links: mu = log; shape = identity 
# Formula: childcare_help_rels ~ (1 | kids_in_hh) + (1 | R_NUM_SIBS) + (1 | religion) + familyBariReligiousAfter + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# Data: d (Number of observations: 752) 
# Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# total post-warmup samples = 16000
# 
# Priors: 
#   b ~ normal(0,1)
# Intercept ~ student_t(3, 1.1, 2.5)
# sd ~ student_t(3, 0, 2.5)
# shape ~ gamma(0.01, 0.01)
# 
# Group-Level Effects: 
#   ~kids_in_hh (Number of levels: 13) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.036148  0.029637 0.001648 0.111015 1.041538     1990     5269
# 
# ~R_NUM_SIBS (Number of levels: 12) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.051806  0.038442 0.003304 0.146712 1.051313       54     4037
# 
# ~religion (Number of levels: 2) 
# Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sd(Intercept) 0.701772  0.824110 0.006715 2.861438 1.213625       13       19
# 
# Population-Level Effects: 
#   Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                  1.351652  0.566878  0.377776  2.584656 1.220684       13       14
# familyBariReligiousAfter  -0.009114  0.033486 -0.077320  0.056738 1.055781      769     7781
# religious_knowledge_scale -0.003898  0.008039 -0.017503  0.012100 1.084995       29     3515
# MI_geo_proximity          -0.132369  0.041632 -0.221904 -0.051062 1.083152     7369     8792
# MI_economic_capital        0.016019  0.024803 -0.035239  0.065004 1.049889      485     7406
# MI_human_capital          -0.004166  0.026244 -0.052973  0.050068 1.034794       78     8169
# 
# Family Specific Parameters: 
#   Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS Tail_ESS
# shape 31.815896 25.167661 12.686169 99.092424 1.040042      528     2902

path<- (paste0("results/"))
filename <- "childcare_help_rels_neg_binom.rds"

saveRDS(model6.2, paste0(path, filename))


# # 7) percent_overall_help_rels:  B=0.070260   p= 0.12377 
# library(tidyverse)
# library(brms)
# library(readr)
# 
# d <- newdata[c(42,11,47,4,5,7,8,9,44,45,49,51)] 
# 
# d <- d[complete.cases(d), ] 
# 
# hist(d$percent_overall_help_rels)
# 
# d$percent_overall_help_rels <- (d$percent_overall_help_rels * (783) + 0.5) / 784
# 
# 
# model7<-brm(percent_overall_help_rels ~ (1|kids_in_hh)+(1|R_NUM_SIBS)+
#               (1|religion)+familyBariReligiousAfter+religious_knowledge_scale+
#               MI_geo_proximity+
#               MI_economic_capital+
#               MI_human_capital, data=d, family = "gamma",
#             prior = c(set_prior("normal(0,2)", class = "b"),
#                       set_prior("normal(0,10)", class="b",coef="age_wife")),
#             warmup = 1000, iter = 5000, chains = 4,
#             control = list(adapt_delta = 0.95))
# 
# print(summary(model7, prob=0.95,priors=TRUE), digits = 6)
# 
# 
# path<- (paste0("results/"))
# filename <- "percent_overall_help_rels_gamma.rds"
# 
# saveRDS(model7, paste0(path, filename))


# # 7.1) overall_help_non_rels: B=0.37673    p= 0.001963 **
# library(tidyverse)
# library(brms)
# library(readr)
# 
# d <- newdata[c(40,11,47,4,5,7,8,9,44,45,49,51)] 
# 
# d <- d[complete.cases(d), ] 
# 
# 
# 
# model7.1<-brm(overall_help_non_rels ~ (1|kids_in_hh)+(1|R_NUM_SIBS)+
#                 (1|religion)+familyBariReligiousAfter+religious_knowledge_scale+
#                 MI_geo_proximity+
#                 MI_economic_capital+
#                 MI_human_capital, data=d, family = negbinomial(link = "log", link_shape = "identity"),
#               prior = c(set_prior("normal(0,2)", class = "b"),
#                         set_prior("normal(0,10)", class="b",coef="age_wife")),
#               warmup = 1000, iter = 5000, chains = 4,
#               control = list(adapt_delta = 0.95))
# 
# print(summary(model7.1, prob=0.95,priors=TRUE), digits = 6)
# 
# 
# 
# path<- (paste0("results/"))
# filename <- "overall_help_non_rels_neg_binom.rds"
# 
# saveRDS(model7.1, paste0(path, filename))
# 
# # 7.2) overall_help_rels:   B=1.58011    p=0.0000000444 **
# library(tidyverse)
# library(brms)
# library(readr)
# 
# d <- newdata[c(38,11,47,4,5,7,8,9,44,45,49,51)] 
# 
# d <- d[complete.cases(d), ] 
# 
# 
# model7.2<-brm(overall_help_rels ~ (1|kids_in_hh)+(1|R_NUM_SIBS)+
#                 (1|religion)+familyBariReligiousAfter+religious_knowledge_scale+
#                 MI_geo_proximity+
#                 MI_economic_capital+
#                 MI_human_capital, data=d, family = "poisson",
#               prior = c(set_prior("normal(0,2)", class = "b"),
#                         set_prior("normal(0,10)", class="b",coef="age_wife")),
#               warmup = 1000, iter = 5000, chains = 4,
#               control = list(adapt_delta = 0.95))
# 
# print(summary(model7.2, prob=0.95,priors=TRUE), digits = 6)
# 
# path<- (paste0("results/"))
# filename <- "overall_help_rels_poisson.rds"
# 
# saveRDS(model7.2, paste0(path, filename))

#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

# in rethinking and brms
setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh")
newdata <- read_csv("newdata.csv")
options(scipen=999)

d <- newdata[c(1,5,7,8,9,36,35,44,45,47,49,51)] # add 36 for non-rels and 35 for rels
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

# get non relatives
# join non-rels to data (d)
non_rels <- nr %>% left_join (d, by=c("id_Questionaire"="idwife"))
non_rels <- non_rels[complete.cases(non_rels),]

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
non_rels <- non_rels %>% filter(location==2)

hist(non_rels$childcare_help_non_rels)

# negative binomial

non_rels$childcare_help_non_rels<- as.integer(non_rels$childcare_help_non_rels)
## Run kin and non-kin living in same neighborhood only
model8<-brm(childcare_help_non_rels ~ (1|kids_in_hh)+(1|R_NUM_SIBS)+religion+
              familyBariReligiousAfter+religious_knowledge_scale+
              MI_geo_proximity+
              MI_economic_capital+
              MI_human_capital, data=non_rels, family = negbinomial(link = "log", link_shape = "identity"),
            prior = c(set_prior("normal(0,2)", class = "b"),
                      set_prior("normal(0,10)", class="b",coef="age_wife")),
            warmup = 1000, iter = 5000, chains = 4,
            control = list(adapt_delta = 0.95))

print(summary(model8, prob=0.95,priors=TRUE), digits = 6)
# Family: negbinomial 
# Links: mu = log; shape = identity 
# Formula: childcare_help_non_rels ~ kids_in_hh + age_wife + religion + familyBariReligiousAfter + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# Data: non_rels (Number of observations: 660) 
# Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# total post-warmup samples = 16000
# 
# Priors: 
#   b ~ normal(0,2)
# b_age_wife ~ normal(0,10)
# Intercept ~ student_t(3, -2.3, 2.5)
# shape ~ gamma(0.01, 0.01)
# 
# Population-Level Effects: 
#   Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                 -1.566414  0.884149 -3.289748  0.156475 1.000026    12231    12012
# kids_in_hh                 0.445054  0.082745  0.285927  0.613425 1.000212    13720    12062
# age_wife                   0.007658  0.016890 -0.025605  0.040564 1.000067    12403    12281
# religion                   0.163469  0.364252 -0.554276  0.865492 1.000204    13109    10943
# familyBariReligiousAfter  -1.304154  0.205370 -1.723399 -0.916847 1.000107    12493    11190
# religious_knowledge_scale  0.016682  0.020664 -0.022520  0.058564 1.000430    14245    11072
# MI_geo_proximity           0.829542  0.391047  0.116844  1.649874 1.000079    11574    10332
# MI_economic_capital        0.045226  0.128170 -0.206423  0.295156 1.000336    13899    12152
# MI_human_capital           0.832281  0.227037  0.390867  1.283541 1.000041    12285    11363
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# shape 0.375214  0.069584 0.261216 0.529288 1.000215    15431    10243
path<- (paste0("results/"))
filename <- "Childcare_help_nonrels_neighborsonly_neg_binom.rds"

saveRDS(model8, paste0(path, filename))


# in rethinking and brms
setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh")
newdata <- read_csv("newdata.csv")
options(scipen=999)

d <- newdata[c(1,5,7,8,9,36,35,44,45,47,49,51)] # add 36 for non-rels and 35 for rels
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

# get non relatives
# join non-rels to data (d)
rels <- r %>% left_join (d, by=c("id_Questionaire"="idwife"))
rels <- rels[complete.cases(rels),]

rels$location[rels$location==1|rels$location==2] <- 2
rels$location[rels$location==3] <- 3
rels$location[rels$location==4|rels$location==5] <- 4
# Location Codes
# 1=khana member,
# 2=near bari/neighbor
# 3=other place in Matlab
# 4=Other Place in Bangladesh
# 5=Abroad

# subset to only neighbors or closer
rels <- rels %>% filter(location==2)
  # negative binomial

rels$childcare_help_rels<- as.integer(rels$childcare_help_rels)
## Run kin and non-kin living in same neighborhood only
model8.1<-brm(childcare_help_rels ~ (1|kids_in_hh)+(1|R_NUM_SIBS)+
                religion+familyBariReligiousAfter+religious_knowledge_scale+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital, data=rels, family = negbinomial(link = "log", link_shape = "identity"),
              prior = c(set_prior("normal(0,2)", class = "b"),
                        set_prior("normal(0,10)", class="b",coef="age_wife")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model8.1, prob=0.95,priors=TRUE), digits = 6)
# Family: negbinomial 
# Links: mu = log; shape = identity 
# Formula: childcare_help_rels ~ kids_in_hh + age_wife + religion + familyBariReligiousAfter + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# Data: rels (Number of observations: 3035) 
# Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# total post-warmup samples = 16000
# 
# Priors: 
#   b ~ normal(0,2)
# b_age_wife ~ normal(0,10)
# Intercept ~ student_t(3, 1.4, 2.5)
# shape ~ gamma(0.01, 0.01)
# 
# Population-Level Effects: 
#   Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                  1.938539  0.078303  1.784738  2.091634 1.000071    15819    13678
# kids_in_hh                 0.018973  0.007241  0.004802  0.033238 1.000250    15013    11848
# age_wife                  -0.012644  0.001514 -0.015609 -0.009693 0.999994    15671    13521
# religion                  -0.114531  0.041356 -0.195016 -0.033816 1.000059    12243    12174
# familyBariReligiousAfter  -0.008612  0.017758 -0.043330  0.026442 1.000289    13741    10984
# religious_knowledge_scale  0.014160  0.005019  0.004377  0.023960 1.000070    14077    12371
# MI_geo_proximity          -0.112286  0.021921 -0.157021 -0.070800 1.000430    14742    10774
# MI_economic_capital        0.006563  0.013190 -0.019186  0.032195 1.000189    14061    12197
# MI_human_capital          -0.142173  0.022070 -0.185776 -0.099089 1.000658    13700    11505
# 
# Family Specific Parameters: 
#   Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS Tail_ESS
# shape 27.019834  7.074594 17.692974 44.359297 1.000195    12717     7759


path<- (paste0("results/"))
filename <- "Childcare_help_rels_neighborsonly_neg_binom.rds"

saveRDS(model8.1, paste0(path, filename))
######################################################################################################################
###### seperate childcare to helpers and helpees
######################################################################################################################
###### seperate childcare to helpers and helpees
######################################################################################################################
###### seperate childcare to helpers and helpees
######################################################################################################################
###### seperate childcare to helpers and helpees
######################################################################################################################
###### seperate childcare to helpers and helpees
# split child care help Q1 and childcare helper Q2
library(tidyverse)
library(brms)
library(readr)
setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh")
newdata <- read_csv("newdata.csv")

WifeNW <- read_csv("C:/Users/robert/Dropbox/PSU postdoc/Access text files Bangladesh/HHQPeopleinNW.csv")

l <-WifeNW %>% group_by(id_Questionaire) %>% summarise(nonrels_help_childcare=sum(q01==1 & relationship==0,na.rm=T),
                                                       rels_help_childcare=sum(q01==1 & relationship>0 & relationship<8,na.rm=T),
                                                       help_nonrels_childcare=sum(q02==1 & relationship==0,na.rm=T),
                                                       help_rels_childcare=sum(q02==1 & relationship>0 & relationship<8,na.rm=T))
# link l back to newdata

newdata2 <- newdata %>% left_join(l,by=c('idwife'='id_Questionaire'))

summary(newdata2)

## then analyze helping and getting help with childcare seperately

# use this model
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Rels_childcare_intx_with_neighbors.rds")
#familyBariReligiousAfter                        -0.05      0.04    -0.12     0.02 1.00    18614    12647
#familyBariReligiousAfter:MI_economic_capital    -0.09      0.04    -0.16    -0.02 1.00    17049    13155
#need to get d3

d <- newdata2[c(1,7,8,9,36,35,44,45,47,49,51,53:56)] 
d <- d[complete.cases(d), ] 



### add variable number of kin in neighborhood to d
# read in wife NW
WifeNW <- read_csv("C:/Users/robert/Dropbox/PSU postdoc/Access text files Bangladesh/HHQPeopleinNW.csv")

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
d2$childcare_help_non_rels<- as.integer(d2$childcare_help_non_rels)

r2 <- r %>% group_by(id_Questionaire) %>% summarise (neighbor_rels = sum(location<3))
# must link nr2 back to d somehow - all missing in nr2 are 0
d3<- d %>% left_join (r2, by=c("idwife"="id_Questionaire"))
# replace NA's in neighbor_rels with 0
d3$neighbor_rels[is.na(d3$neighbor_rels)]<-0


#Remake this model for getting helping and giving help
# getting help
M2<-brm(rels_help_childcare ~ kids_in_hh+R_NUM_SIBS+neighbor_rels+
          religion+familyBariReligiousAfter+religious_knowledge_scale+
          MI_geo_proximity+
          MI_economic_capital+
          MI_human_capital+MI_economic_capital*familyBariReligiousAfter, data=d3, family = negbinomial(link = "log", link_shape = "identity"),
        prior = c(set_prior("normal(0,2)", class = "b")),
        warmup = 1000, iter = 5000, chains = 4,
        control = list(adapt_delta = 0.95))

print(summary(M2, prob=0.95,priors=TRUE), digits = 6)
#familyBariReligiousAfter                     -0.030575  0.044380 -0.118071  0.055920 1.000444    17747    11378
#familyBariReligiousAfter:MI_economic_capital -0.108247  0.047259 -0.202349 -0.017051 0.999974    16960    11689


path<- (paste0("results/"))
filename <- "Rels_who_help_w_childcare_intx_with_neighbors.rds"
saveRDS(M2, paste0(path, filename))


###### helping relatives with childcare controlling for neighbors with intx
M3<-brm(help_rels_childcare ~ kids_in_hh+R_NUM_SIBS+neighbor_rels+
          religion+familyBariReligiousAfter+religious_knowledge_scale+
          MI_geo_proximity+
          MI_economic_capital+
          MI_human_capital+MI_economic_capital*familyBariReligiousAfter, data=d3, family = negbinomial(link = "log", link_shape = "identity"),
        prior = c(set_prior("normal(0,2)", class = "b")),
        warmup = 1000, iter = 5000, chains = 4,
        control = list(adapt_delta = 0.95))

print(summary(M3, prob=0.95,priors=TRUE), digits = 6)
#familyBariReligiousAfter                     -0.068534  0.051230 -0.170037  0.029766 1.000309    16067    11030
#familyBariReligiousAfter:MI_economic_capital -0.060699  0.053161 -0.166629  0.041418 1.000298    16313    12005

path<- (paste0("results/"))
filename <- "Rels_you_help_w_childcare_intx_with_neighbors.rds"

saveRDS(M3, paste0(path, filename))

######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################



# compare models


# compare models
fit <- add_criterion(m, c("loo","waic"))
fit1 <- add_criterion(model5, c("loo","waic"))



## model comparison
loo_compare(fit,fit1,criterion = "loo")

# get the weights
model_weights(fit, fit1,
              weights = "waic") %>% 
  round(digits = 3)

#####HERE!!!!!!
help("set_prior")
get_prior(NW_total ~ kids_in_hh+
            age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
            MI_geo_proximity+
            MI_economic_capital+
            MI_human_capital, data=d, 
          family = lognormal())

fit1 <- brm(formula = time | cens(censored) ~ age * sex + disease
            + (1 + age|patient),
            data = kidney, family = lognormal(),
            prior = c(set_prior("normal(0,5)", class = "b"),
                      set_prior("cauchy(0,2)", class = "sd"),
                      set_prior("lkj(2)", class = "cor")),
            warmup = 1000, iter = 2000, chains = 4,
            control = list(adapt_delta = 0.95))


# set priors in brms
# If we want to have a normal prior with mean 0 and standard deviation 5 for x1, and 
# a unit student-t prior with 10 degrees of freedom for x2, we can specify this via 
# set_prior("normal(0,5)", class = "b", coef = "x1") and 
# set_prior("student_t(10, 0, 1)", class = "b", coef = "x2"). To put the same prior 
# on all population-level effects at once, we may write as a shortcut set_prior("<prior>", class = "b").

# priors for all slopes (b terms) in main model
c(bhyp, bob, bs, bed, bag, btech, bfact, bserv, boff, bbus, btrans, bage, bpop,
  bobs,bobage,bobhyp) ~ dnorm(0,1)

# a_birthplace[birthplace_id_seq] ~ dnorm(0, sigma),
# sigma ~ dcauchy(0,1),
# a ~ dnorm (0,1),

# priors for all slopes (b terms) in main model
c(bhyp, bob, bs, bed, bag, btech, bfact, bserv, boff, bbus, btrans, bage, bpop,
  bobs,bobage,bobhyp) ~ dnorm(0,1)

model1 <- brm(formula = time | cens(censored) ~ age * sex + disease
              + (1 + age|patient),
              data = kidney, family = lognormal(),
              prior = c(set_prior("normal(0,5)", class = "b"),
                        set_prior("cauchy(0,2)", class = "sd"),
                        set_prior("lkj(2)", class = "cor")),
              warmup = 1000, iter = 2000, chains = 4,
              control = list(adapt_delta = 0.95))



#kids in HH and hh_total cannot go in together

#log.glm = glm(y ~ x, family=gaussian(link="log"), data=my.dat)

model1<-glm(NW_total ~ kids_in_hh+sex+
              age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
              MI_geo_proximity+
              MI_economic_capital+
              MI_human_capital, data=d, family = "gaussian"(link="log"))
summary(model1)


## lognormal
lognormal(link = "identity", link_sigma = "log")

## exgaussian
exgaussian(link = "identity", link_sigma = "log", link_beta = "log")

# custom families of functions
beta_binomial2 <- custom_family(
  "beta_binomial2", dpars = c("mu", "phi"),
  links = c("logit", "log"), lb = c(NA, 0),
  type = "int", vars = "vint1[n]"
)

## zero inflated models - dual processes
d$camper <- factor(zinb$camper, labels = c("no", "yes"))

# Many groups may not catch any fish just
# because they do not try and so we fit a zero-in
# flated Poisson model. For now, we assume a
# constant zero-in
# ation probability across observations.
fit_zinb1 <- brm(count ~ persons + child + camper, data = zinb,
                 family = zero_inflated_poisson("log"))

# in the next example the number of children in the party are treated as another process producing zero's - no fish caught

fit_zinb2 <- brm(bf(count ~ persons + child + camper, zi ~ child),
                 data = zinb, family = zero_inflated_poisson())

library(loo)
LOO(fit_zinb1, fit_zinb2)



# get model summary
summary(fit_zinb1)

# visualize results
conditional_effects(fit_zinb1)

###############################################################

## draw a Dag for our model

library(dagitty)
Kin_dag <-dagitty("dag{
Religiosity -> Kin_density
Religion -> Kin_density
Kin_density -> Religiosity
Unobserved -> Religiosity
Unobserved-> Kin_density
}")
coordinates( Kin_dag)<-list(x=c(Religiosity=0,Kin_density=2,Unobserved=1.5,Religion=1),
                            y=c(Religiosity=1,"Kin_density"=2,Unobserved=3,Religion=4) )
drawdag( Kin_dag)



### Do a DAG for our models to get confounding variables
library(dagitty)
kin_density_dag <-dagitty("dag{
U [unobserved]
religiosity ->kin_density
kids_in_hh -> kin_density
religion -> kin_density
age_wife -> kin_density
religiosity <-U<-neighborhood->house->kin_density
religion <- religious knowledge -> religiosity
MI_economic_capital<- U<- MI_human_capital -> MI_geo_proximity
religiosity <-U<-MI_human_capital->religion->kin_density
age_wife ->U<-kin_density
}")
adjustmentSets( kin_density_dag,exposure="religiosity",outcome="kin_density")

## example

library(dagitty)
dag_6.1 <-dagitty("dag{
U [unobserved]
religiosity ->kin_density
religiosity <-U<-relgion->market_intergration->kin_density
U ->siblings<-kids
}")
adjustmentSets( dag_6.1,exposure="religiosity",outcome="kin_density")

#### DAG Examples from Chapter 6

library(ggdag)

dag_coords <-
  tibble(name = c("L", "D", "F", "K"),
         x    = c(1, 2, 3, 2),
         y    = c(2, 2, 2, 1))

dagify(L ~ D,
       F ~ D,
       K ~ L + F,
       coords = dag_coords) %>%
  
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_point(aes(color = name == "D"),
                 alpha = 1/2, size = 6.5, show.legend = F) +
  geom_point(x = 2, y = 2, 
             size = 6.5, shape = 1, stroke = 1, color = "orange") +
  geom_dag_text(color = "black") +
  geom_dag_edges() +
  scale_color_manual(values = c("steelblue", "orange")) +
  scale_x_continuous(NULL, breaks = NULL, expand = c(.1, .1)) +
  scale_y_continuous(NULL, breaks = NULL, expand = c(.1, .1))


# Number 2
# define our coordinates
dag_coords <-
  tibble(name = c("H0", "T", "F", "H1"),
         x    = c(1, 5, 4, 3),
         y    = c(2, 2, 1.5, 1))

# save our DAG
dag <-
  dagify(F ~ T,
         H1 ~ H0 + F,
         coords = dag_coords)

# plot 
dag %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_point(color = "steelblue", alpha = 1/2, size = 6.5) +
  geom_dag_text(color = "black") +
  geom_dag_edges() + 
  theme_dag()


## function to help with DAG's

gg_simple_dag <- function(d) {
  
  d %>% 
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_point(color = "steelblue", alpha = 1/2, size = 6.5) +
    geom_dag_text(color = "black") +
    geom_dag_edges() + 
    theme_dag()
  
}

## And
gg_fancy_dag <- function(d, x = 1, y = 1, circle = "U") {
  
  d %>% 
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_point(aes(color = name == circle),
                   alpha = 1/2, size = 6.5, show.legend = F) +
    geom_point(x = x, y = y, 
               size = 6.5, shape = 1, stroke = 1, color = "orange") +
    geom_dag_text(color = "black") +
    geom_dag_edges() + 
    scale_color_manual(values = c("steelblue", "orange")) +
    theme_dag()
  
}

###THIS ONE IS A GOOD MODEL TO USE

dag_coords <-
  tibble(name = c("A", "B", "C", "Unobserved", "X", "Y"),
         x    = c(2, 2, 3, 1, 1, 3),
         y    = c(4, 2, 3, 3, 1, 1))

dagify(B ~ C + Unobserved,
       C ~ A,
       Unobserved ~ A,
       X ~ Unobserved,
       Y ~ C + X,
       coords = dag_coords) %>%
  gg_fancy_dag(x = 1, y = 3, circle = "Unobserved")

## Or this one but you need to use the functions made above (e.g. gg_simple_data and gg_fancy_dag)
dag_coords <-
  tibble(name = c("A", "D", "M", "S", "W"),
         x    = c(1, 3, 2, 1, 3),
         y    = c(1, 1, 2, 3, 3))

dagify(A ~ S,
       D ~ A + M + W,
       M ~ A + S,
       W ~ S,
       coords = dag_coords) %>%
  gg_simple_dag()



#### brms code for varying slopes and varying intercepts

# our first model has both varying intercepts and afternoon slopes. 
# I should point out that the (1 + afternoon | cafe) syntax specifies that we'd 
# like brm() to fit the random effects for 1 (i.e., the intercept) and the afternoon
# slope as correlated. Had we wanted to fit a model
# in which they were orthogonal, we'd have coded (1 + afternoon || cafe).

b13.1 <- 
  brm(data = d, family = gaussian,
      wait ~ 1 + religion + (1 + religiosity | religion), 
      prior = c(prior(normal(0, 10), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(cauchy(0, 2), class = sd),
                prior(cauchy(0, 2), class = sigma),
                prior(lkj(2), class = cor)),
      iter = 5000, warmup = 2000, chains = 2, cores = 2,
      seed = 13)

(1+religiosity | religion) + (1|religion)


#### re-rerun financial, emotional and childcare help received only

## first link key variables from wnr from 'Kin_density_DV.R' line 447 to 'newdata (at beginning of this file) 

wnr <- wnr %>% select(1,11,12,15,16,19,20,21,22,27,28,,33,34,35,36)

new <- newdata %>% left_join(wnr, by=c ("idwife"="id_Questionaire"))
# [1] "idwife"                      "couple_id"                   "idhusband"                  
# [4] "sex"                         "age_wife"                    "age_h"                      
# [7] "MI_geo_proximity"            "MI_economic_capital"         "MI_human_capital"           
# [10] "NW_total"                    "non_rels"                    "parents_kids"               
# [13] "pat_rels"                    "mat_rels"                    "in_laws"                    
# [16] "far_rels"                    "geo_distance_non_rels"       "geo_distance_rels"          
# [19] "non_rels_asking_advice_h"    "rels_asking_advice_h"        "non_rels_prestige_h"        
# [22] "rels_prestige_h"             "non_rels_time_spent_h"       "rels_time_spent_h"          
# [25] "non_rels_aid_h"              "rels_aid_h"                  "rels_in_NW"                 
# [28] "rels_in_NW_h"                "percent_rels_in_NW"          "percent_rels_in_NW_h"       
# [31] "rels_econ_help"              "rels_econ_help_h"            "non_rels_econ_help"         
# [34] "non_rels_econ_help_h"        "percent_rels_econ_help"      "percent_rels_econ_help_h"   
# [37] "emot_support_rels"           "emot_support_rels_h"         "emot_support_non_rels"      
# [40] "emot_support_non_rels_h"     "percent_rels_emot_support"   "percent_rels_emot_support_h"
# [43] "childcare_help_rels"         "childcare_help_non_rels"     "childcare_help_rels_percent"
# [46] "overall_help_rels"           "overall_help_rels_h"         "overall_help_non_rels"      
# [49] "overall_help_non_rels_h"     "percent_overall_help_rels"   "percent_overall_help_rels_h"
# [52] "rels_small_loans"            "non_rels_small_loans"        "percent_rels_small_loans"
# [55] "rels_large_loans"            "non_rels_large_loans"        "percent_rels_large_loans"   
# [58] "familyBariReligiousBefore.x" "familyBariReligiousAfter.x"  "religious_change.x"         
# [61] "religion.x"                  "religious_knowledge_scale.x" "hh_total"                   
# [64] "kids_in_hh"                  "religion.y"                  "religious_knowledge_scale.y"
# [67] "nonrels_help_childcare"      "rels_help_childcare"         "non_rels_loan"              
# [70] "rels_loan"                   "non_rels_small_loan"         "non_rels_large_loan"        
# [73] "rels_small_loan"             "rels_large_loan"             "non_rels_ask_advice"        
# [76] "rels_ask_advice"             "non_rels_time_spent"         "rels_time_spent"            
# [79] "non_rels_aid"                "rels_aid"           


# repeat for specific questions (getting financial help) # 67-80
require(lme4)
new$rels_aid<- as.integer(new$rels_aid)


model<-glmer.nb(rels_aid~ (1|kids_in_hh)+
                (1|religion.x)+familyBariReligiousAfter.x+religious_knowledge_scale.x+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital, data=new, family = negbinomial,
             control = glmerControl(optimizer = "bobyqa"))
          
summary(model)

#67    #nonrels_help_childcare   Yes (very similar to old model)
# familyBariReligiousAfter.x  -1.16996    0.35288   0.000915 ***
# VS BRMS (OlD model)
# familyBariReligiousAfter  -1.360190   -2.189932 -0.620257     

#68  rels_help_childcare  Nothing
#familyBariReligiousAfter.x   0.005189   0.043439   0.119   0.9049 
#MI_geo_proximity            -0.120548   0.055909  -2.156   0.0311 *  (further from markets more rels
# help with childcare)

#69   #non_rels_loan  Nothing
#familyBariReligiousAfter.x  -0.10314    0.18003  -0.573    0.567 

#70  rels_loan  Nothing
# familyBariReligiousAfter.x   0.004214   0.047454   0.089   0.9292 
# MI_economic_capital         -0.122198   0.037135  -3.291   0.0010 *** (poor people list more rels who
# give them loans)

#71 "non_rels_small_loan"   NOTHING    
# familyBariReligiousAfter.x  -0.28522    0.23602  -1.208    0.227  

#72 "non_rels_large_loan"   NOTHING
#  familyBariReligiousAfter.x   0.03044    0.24131   0.126    0.900   

#73 "rels_small_loan"     Nothing 
# familyBariReligiousAfter.x  -0.0007358  0.0513192  -0.014   0.9886    
# MI_economic_capital         -0.1800221  0.0417340  -4.314 1.61e-05 *** (poorer get more small loans from rels)

#74 "rels_large_loan"  Nothing
# familyBariReligiousAfter.x   0.05253    0.08264   0.636   0.5250  
# religious_knowledge_scale.x  0.03450    0.01791   1.926   0.0541 . (more religious knowledge means
# more big loans from rels)

#75 "non_rels_ask_advice"  Nothing
# familyBariReligiousAfter.x  -0.24111    0.33584  -0.718    0.473    
# religious_knowledge_scale.x  0.11331    0.05881   1.927    0.054 . (more RK means more advice from non rels)

#76 "rels_ask_advice"  YES (SAME - STRONG EFFECT)
#familyBariReligiousAfter.x   0.139926   0.041457   3.375 0.000738 ***
# VS
# familyBariReligiousAfter   0.186163    0.136977 0.236581 (all emotional)

#77 "non_rels_time_spent"   # NOTHING   
# familyBariReligiousAfter.x  -0.01456    0.23096  -0.063   0.9497   
# MI_human_capital             0.44275    0.18467   2.398   0.0165 *  (more educated spend more time with non-rels)


#78 "rels_time_spent"   # YES (SAME STRONG EFFECT)
#familyBariReligiousAfter.x   0.213135   0.041572   5.127 2.95e-07 ***
# VS
# familyBariReligiousAfter   0.186163    0.136977 0.236581 (all emotional)

#79 "non_rels_aid" NOTHING
# familyBariReligiousAfter.x  -0.433524   0.384339  -1.128    0.259 

#80 "rels_aid" (If some problem happens, who comes to your aid to help you?)  YES (SAME STRONG EFFECT)
# familyBariReligiousAfter.x   0.152175   0.038858   3.916    9e-05 ***
## # VS
# familyBariReligiousAfter   0.186163    0.136977 0.236581  (all emotional)
