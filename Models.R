
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
newdata <- read_csv("newdata.csv")
options(scipen=999)
# remove duplicated women
newdata <- newdata %>% distinct(idwife, .keep_all = TRUE)
# 1) center and scale variables for easier interpretability fo parameter estimates
newdata$religious_knowledge_scale <-  newdata$religious_knowledge_scale-mean(newdata$religious_knowledge_scale, na.rm=T)
newdata$hh_total  <- newdata$hh_total-mean(newdata$hh_total, na.rm=T)  
newdata$kids_in_hh  <- newdata$kids_in_hh-mean(newdata$kids_in_hh, na.rm=T)

########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
## Brms models - Model 1 NW total
d <- newdata[c(10,47,5,7,8,9,44,45,49)] 

d <- d[complete.cases(d), ]  

hist(d$NW_total)
library(fitdistrplus)
library(logspline)
descdist(d$NW_total, discrete = TRUE, boot=500)


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

# Family: lognormal 
# Links: mu = identity; sigma = identity 
# Formula: NW_total ~ kids_in_hh + age_wife + religion + familyBariReligiousAfter + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# Data: d (Number of observations: 762) 
# Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# total post-warmup samples = 16000
# 
# Priors: 
#   b ~ normal(0,2)
# b_age_wife ~ normal(0,10)
# Intercept ~ student_t(3, 2.3, 2.5)
# sigma ~ student_t(3, 0, 2.5)
# 
# Population-Level Effects: 
#                             Estimate Est.Error  l-95% CI  u-95% CI  Rhat     Bulk_ESS Tail_ESS
# Intercept                  2.600852  0.096484  2.407384  2.788214 1.000117    15917    13264
# kids_in_hh                 0.004440  0.008459 -0.012357  0.020953 0.999967    17615    12402
# age_wife                  -0.006747  0.001834 -0.010332 -0.003112 1.000106    16083    13208
# religion                  -0.105896  0.047781 -0.198771 -0.012367 0.999943    13143    12526
# familyBariReligiousAfter   0.120417  0.021203  0.078869  0.161534 1.000052    17034    11791
# religious_knowledge_scale  0.006115  0.004540 -0.002915  0.015070 1.000374    14538    13255
# MI_geo_proximity          -0.014275  0.018496 -0.050677  0.021829 1.000538    17442    12540
# MI_economic_capital        0.032800  0.016275  0.000792  0.064487 1.000266    13624    12579
# MI_human_capital          -0.045201  0.026457 -0.097087  0.006613 1.000425    13516    11181
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sigma 0.353314  0.009158 0.335886 0.371556 1.000244    16441    11623

path<- (paste0("results/"))
filename <- "NW_total_lognormal.rds"

saveRDS(model1, paste0(path, filename))


#### Model2
library(tidyverse)
library(brms)
library(readr)

d <- newdata[c(21,47,5,7,8,9,44,45,49)] 
d <- d[complete.cases(d), ] 
hist(d$percent_rels_in_NW)
d <- d[complete.cases(d), ]  

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
# Family: lognormal 
# Links: mu = identity; sigma = identity 
# Formula: percent_rels_in_NW ~ kids_in_hh + age_wife + religion + familyBariReligiousAfter + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# Data: d (Number of observations: 762) 
# Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# total post-warmup samples = 16000
# 
# Priors: 
#   b ~ normal(0,2)
# b_age_wife ~ normal(0,10)
# Intercept ~ student_t(3, -0.2, 2.5)
# sigma ~ student_t(3, 0, 2.5)
# 
# Population-Level Effects: 
#   Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                 -0.129691  0.061007 -0.249339 -0.011155 1.000065    15760    13696
# kids_in_hh                 0.010848  0.005448  0.000305  0.021654 1.000146    18348    11670
# age_wife                  -0.001237  0.001161 -0.003495  0.001027 1.000060    15647    13959
# religion                   0.013091  0.030357 -0.046637  0.072118 1.000362    13526    11212
# familyBariReligiousAfter   0.030248  0.013455  0.004055  0.056667 1.000045    16139    11806
# religious_knowledge_scale -0.001828  0.002858 -0.007524  0.003705 0.999995    16600    13358
# MI_geo_proximity           0.010047  0.011788 -0.013322  0.033088 1.000087    16676    12449
# MI_economic_capital        0.021238  0.010116  0.001500  0.041135 1.000108    14846    11437
# MI_human_capital          -0.009360  0.016786 -0.042526  0.023334 0.999971    13367    11580
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sigma 0.225518  0.005771 0.214568 0.237180 1.000576    16399    10734
# 
path<- (paste0("results/"))
filename <- "percent_relatives_in_NW_lognormal.rds"

saveRDS(model2, paste0(path, filename))


# Model 2.1 Relatives in Network

library(tidyverse)
library(brms)
library(readr)


d <- newdata[c(19,47,4,5,7,8,9,44,45,49)] 
d <- d[complete.cases(d), ] 

hist(d$rels_in_NW) # log normal okay

library(fitdistrplus)
library(logspline)
descdist(d$rels_in_NW, discrete = TRUE, boot=500)

d$rels_in_NW<- d$rels_in_NW+0.01
## run as log normal  
model2.1 <- brm(rels_in_NW ~ kids_in_hh+
                 age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
                 MI_geo_proximity+
                 MI_economic_capital+
                 MI_human_capital, data=d, 
               family = lognormal(),
               prior = c(set_prior("normal(0,2)", class = "b"),
                         set_prior("normal(0,10)", class="b",coef="age_wife")),
               warmup = 1000, iter = 5000, chains = 4,
               control = list(adapt_delta = 0.95))

print(summary(model2.1, prob=0.95,priors=TRUE), digits = 6)

# Family: lognormal 
# Links: mu = identity; sigma = identity 
# Formula: rels_in_NW ~ kids_in_hh + age_wife + religion + familyBariReligiousAfter + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# Data: d (Number of observations: 762) 
# Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# total post-warmup samples = 16000
# 
# Priors: 
#   b ~ normal(0,2)
# b_age_wife ~ normal(0,10)
# Intercept ~ student_t(3, 2.2, 2.5)
# sigma ~ student_t(3, 0, 2.5)
# 
# Population-Level Effects: 
#   Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                  2.455257  0.112045  2.239544  2.671768 1.000300    17474    13331
# kids_in_hh                 0.009180  0.009971 -0.010497  0.028664 1.000593    18949    11189
# age_wife                  -0.006441  0.002130 -0.010510 -0.002312 1.000330    17169    13771
# religion                  -0.102839  0.055391 -0.212627  0.005685 0.999885    14606    12459
# familyBariReligiousAfter   0.140336  0.024843  0.092132  0.189373 1.000117    16298    11916
# religious_knowledge_scale  0.000428  0.005258 -0.009768  0.010773 1.000005    15827    12661
# MI_geo_proximity          -0.019500  0.021699 -0.061550  0.023518 1.000152    19257    12167
# MI_economic_capital        0.041069  0.018773  0.004201  0.077467 1.000240    15075    12227
# MI_human_capital          -0.044903  0.030818 -0.105491  0.015597 1.000236    14609    12291
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sigma 0.412954  0.010594 0.393120 0.434311 1.000023    19477    11994

path<- (paste0("results/"))
filename <- "relatives_in_NW_lognormal.rds"

saveRDS(model2.1, paste0(path, filename))


# Model 2.2 Non-relatives in Network
library(tidyverse)
library(brms)
library(readr)
d <- newdata[c(11,47,4,5,7,8,9,44,45,49)] 

d <- d[complete.cases(d), ] 

hist(d$non_rels) # Negative bionomial
library(fitdistrplus)
library(logspline)
descdist(d$non_rels, discrete = TRUE, boot=500)

## run as Negative bionomial
model2.2 <- brm(non_rels ~ kids_in_hh+
                 age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
                 MI_geo_proximity+
                 MI_economic_capital+
                 MI_human_capital, data=d, 
               family = negbinomial(link = "log", link_shape = "identity"),
               prior = c(set_prior("normal(0,2)", class = "b"),
                         set_prior("normal(0,10)", class="b",coef="age_wife")),
               warmup = 1000, iter = 5000, chains = 4,
               control = list(adapt_delta = 0.95))

print(summary(model2.2, prob=0.95,priors=TRUE), digits = 6)

# Family: negbinomial 
# Links: mu = log; shape = identity 
# Formula: non_rels ~ kids_in_hh + age_wife + religion + familyBariReligiousAfter + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# Data: d (Number of observations: 762) 
# Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# total post-warmup samples = 16000
# 
# Priors: 
#   b ~ normal(0,2)
# b_age_wife ~ normal(0,10)
# Intercept ~ student_t(3, 0, 2.5)
# shape ~ gamma(0.01, 0.01)
# 
# Population-Level Effects: 
#   Estimate Est.Error  l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                  0.409569  0.351785 -0.273854 1.102458 1.000001    16533    12634
# kids_in_hh                -0.024304  0.033364 -0.088885 0.041016 1.000142    21873    11493
# age_wife                  -0.007022  0.006710 -0.020202 0.006069 1.000087    16452    12480
# religion                  -0.023742  0.171297 -0.358900 0.307297 1.000380    20539    12857
# familyBariReligiousAfter  -0.008389  0.077034 -0.158232 0.141720 1.000122    22632    11616
# religious_knowledge_scale  0.020065  0.015235 -0.008946 0.050877 0.999878    19636    12435
# MI_geo_proximity           0.088159  0.080093 -0.058548 0.256504 1.000224    23245    10613
# MI_economic_capital       -0.017115  0.060309 -0.135743 0.100594 1.000365    19101    12719
# MI_human_capital          -0.004774  0.095393 -0.191785 0.183160 0.999986    15875    12234
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# shape 1.374163  0.194694 1.042604 1.806808 0.999856    20765    10100
path<- (paste0("results/"))
filename <- "non_relatives_in_NW_neg_bin.rds"

saveRDS(model2.2, paste0(path, filename))

# Model 3.1 geo_distance_non_rels: + p=0.003268 ** 

d <- newdata[c(17,11,47,4,5,7,8,9,44,45,49)] 
# missing alot here (WHY!!!) if you have no non rels in NW what is the geo distance to them?
# because we are missing 571 here (NA's)
# solution - make NA's max mean of geo_distance_non_rels which is 5
# add dummy for when non rels <- 0
d$dummy_no_non_rels <- ifelse(d$non_rels==0,1,0)


d$geo_distance_non_rels[is.na(d$geo_distance_non_rels)] <- 5
d <- d[complete.cases(d), ] 

hist(d$geo_distance_non_rels)
descdist(d$geo_distance_non_rels, discrete = FALSE, boot=500)


model3.1<-brm(geo_distance_non_rels ~ kids_in_hh+sex+dummy_no_non_rels+
              age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
              MI_geo_proximity+
              MI_economic_capital+
              MI_human_capital, data=d, family = "lognormal",
            prior = c(set_prior("normal(0,2)", class = "b"),
                      set_prior("normal(0,10)", class="b",coef="age_wife")),
            warmup = 1000, iter = 5000, chains = 4,
            control = list(adapt_delta = 0.95))

print(summary(model3.1, prob=0.95,priors=TRUE), digits = 6)

# lognormal
# 
# Family: lognormal 
# Links: mu = identity; sigma = identity 
# Formula: geo_distance_non_rels ~ kids_in_hh + sex + dummy_no_non_rels + age_wife + religion + familyBariReligiousAfter + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# Data: d (Number of observations: 762) 
# Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# total post-warmup samples = 16000
# 
# Priors: 
#   b ~ normal(0,2)
# b_age_wife ~ normal(0,10)
# Intercept ~ student_t(3, 1.1, 2.5)
# sigma ~ student_t(3, 0, 2.5)
# 
# Population-Level Effects: 
#   Estimate Est.Error  l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                  0.801777  0.042105  0.718716 0.884244 1.000197    19526    14135
# kids_in_hh                -0.001025  0.003785 -0.008449 0.006489 1.000048    25312    11864
# sex                       -0.009179  0.012642 -0.034122 0.015743 1.000215    16068    10791
# dummy_no_non_rels          0.818128  0.011443  0.795652 0.840458 1.000120    17504    11898
# age_wife                  -0.000190  0.000814 -0.001772 0.001410 1.000171    18913    13554
# religion                  -0.018016  0.020931 -0.059649 0.022436 1.000178    14554    10691
# familyBariReligiousAfter   0.022284  0.009243  0.004210 0.040338 1.000272    17899    12162
# religious_knowledge_scale -0.001273  0.001966 -0.005117 0.002603 1.000169    17174    13479
# MI_geo_proximity           0.016885  0.008047  0.000985 0.032598 1.000430    18914    11289
# MI_economic_capital        0.006190  0.007118 -0.007725 0.020148 1.000486    16262    11928
# MI_human_capital           0.006615  0.011482 -0.015828 0.029291 1.000004    17886    12741
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sigma 0.154844  0.004074 0.147050 0.162946 1.000035    17115    11941
path<- (paste0("results/"))
filename <- "Geo_distance_non_relatives_lognormal.rds"

saveRDS(model3.1, paste0(path, filename))


# 3.2) geo_distance_rels:   + 0.00118 ** 
d <- newdata[c(18,11,47,4,5,7,8,9,44,45,49)] 
d <- d[complete.cases(d), ] 
hist(d$geo_distance_rels)

library(fitdistrplus)
library(logspline)
descdist(d$geo_distance_rels, discrete = FALSE, boot=500)

model3.2<-brm(geo_distance_rels ~ kids_in_hh+sex+
                age_wife+religion+ familyBariReligiousAfter+religious_knowledge_scale+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital, data=d, family = "normal",
              prior = c(set_prior("normal(0,2)", class = "b"),
                        set_prior("normal(0,10)", class="b",coef="age_wife")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model3.2, prob=0.95,priors=TRUE), digits = 6)

# Family: gaussian 
# Links: mu = identity; sigma = identity 
# Formula: geo_distance_rels ~ kids_in_hh + sex + age_wife + religion + familyBariReligiousAfter + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# Data: d (Number of observations: 761) 
# Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# total post-warmup samples = 16000
# 
# Priors: 
#   b ~ normal(0,2)
# b_age_wife ~ normal(0,10)
# Intercept ~ student_t(3, 2.1, 2.5)
# sigma ~ student_t(3, 0, 2.5)
# 
# Population-Level Effects: 
#   Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                  2.382457  0.127843  2.136341  2.636724 1.000048    16424    12837
# kids_in_hh                 0.024972  0.011211  0.003191  0.046862 0.999907    18840    12189
# sex                       -0.046796  0.037278 -0.119488  0.026750 1.000525    20413    11520
# age_wife                  -0.004208  0.002470 -0.009152  0.000542 1.000061    16060    12813
# religion                  -0.311741  0.062008 -0.434718 -0.190776 1.000049    17079    12689
# familyBariReligiousAfter   0.079619  0.027837  0.024500  0.133060 1.000344    21344    13298
# religious_knowledge_scale  0.026291  0.005850  0.014873  0.037849 1.000173    16997    13527
# MI_geo_proximity           0.034708  0.024057 -0.012653  0.081767 1.000034    18873    12590
# MI_economic_capital       -0.018774  0.021347 -0.060814  0.022919 1.000522    16682    12898
# MI_human_capital          -0.011440  0.034844 -0.079908  0.057620 1.000189    14660    12223
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sigma 0.459128  0.011915 0.436283 0.483151 1.000079    20367    11978


path<- (paste0("results/"))
filename <- "Geo_distance_relatives_normal.rds"

saveRDS(model3.2, paste0(path, filename))

###### NEW MODELS (REVERSED DV)!!!!!
# MOdeling geo_distance of relatives and non relatives against religiosity

#### Non rels first
d <- newdata[c(1,5,7,8,9,11,19,44,45,47,49)] 
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
# Relationship codes
#0 = not a relative, 1 = nijer poribar (parents and kids), 2 = babar bari (fathers family- 2nd or 3rd degree relatives),
#3 = mayer bari (mother side - 2nd or 3rd degree relatives), 4 = shoshur bari (in-laws, affinal), 
#5= babar bari (far relative), 6 = mayer bari (far relative), 7 = shoshur bari (far relative)

# Location codes
#1=khana member,(same household)
#2=near bari/neighbor (walking distance), 
#3=other place in Matlab,
#4=Other Place in Bangladesh, 
#5=Abroad, 

z <- WifeNW %>% dplyr::select (2,3,6,8)

# get location of non relatives
nr <- z %>% filter (relationship==0)
r <- z %>% filter (relationship>0 & relationship<8)

# get non relatives
# join non-rels to data (d)
non_rels <- nr %>% left_join (d, by=c("id_Questionaire"="idwife"))
non_rels <- non_rels[complete.cases(non_rels),]


hist(non_rels$familyBariReligiousAfter)
library(fitdistrplus)
library(logspline)
descdist(non_rels$familyBariReligiousAfter, discrete = TRUE, boot=500)

non_rels$familyBariReligiousAfter<- as.numeric(non_rels$familyBariReligiousAfter)


model3.1b<-brm(familyBariReligiousAfter ~ kids_in_hh+
                 age_wife+religion+location +religious_knowledge_scale+
                 MI_geo_proximity+
                 MI_economic_capital+
                 MI_human_capital, data=non_rels, family = "normal",
               prior = c(set_prior("normal(0,2)", class = "b"),
                        # set_prior("cauchy(0,2)", class="sd"),
                         set_prior("normal(0,10)", class="b",coef="age_wife")),
               warmup = 1000, iter = 5000, chains = 4,
               control = list(adapt_delta = 0.95))

print(summary(model3.1b, prob=0.95,priors=TRUE), digits = 6)
# Family: gaussian 
# Links: mu = identity; sigma = identity 
# Formula: familyBariReligiousAfter ~ kids_in_hh + age_wife + religion + location + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# Data: non_rels (Number of observations: 796) 
# Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# total post-warmup samples = 16000
# 
# Priors: 
#   b ~ normal(0,2)
# b_age_wife ~ normal(0,10)
# Intercept ~ student_t(3, 0, 2.5)
# sigma ~ student_t(3, 0, 2.5)
# 
# Population-Level Effects: 
#   Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                 -0.485334  0.191843 -0.863457 -0.105756 1.000097    16017    12243
# kids_in_hh                 0.004647  0.016910 -0.028469  0.037976 0.999979    18632    12078
# age_wife                   0.009669  0.003154  0.003450  0.015973 1.000251    15690    12456
# religion                   0.139755  0.078716 -0.013126  0.293314 1.000066    15998    12464
# location                   0.096914  0.040888  0.017458  0.177166 1.000020    19296    12591
# religious_knowledge_scale  0.004644  0.005855 -0.006848  0.016259 0.999982    17763    12879
# MI_geo_proximity          -0.024414  0.039781 -0.101951  0.052743 1.000158    17685    11640
# MI_economic_capital        0.127497  0.027789  0.073466  0.182119 0.999975    16248    13098
# MI_human_capital           0.075680  0.042950 -0.007153  0.160043 1.000172    14087    12105
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sigma 0.633930  0.015909 0.603522 0.666007 1.000268    19938    11543

# save model
path<- (paste0("results/"))
filename <- "Religiosity_predicted_by_geo_distance_non_relatives_normal.rds"

saveRDS(model3.1b, paste0(path, filename))

## do relatives next
# get relatives
# join to rels (data (d))
rels <- r %>% left_join (d, by=c("id_Questionaire"="idwife"))

rels <- rels[complete.cases(rels),]


hist(rels$familyBariReligiousAfter)
library(fitdistrplus)
library(logspline)
descdist(rels$familyBariReligiousAfter, discrete = TRUE, boot=500)

rels$familyBariReligiousAfter<- as.numeric(rels$familyBariReligiousAfter)

model3.2b<-brm(familyBariReligiousAfter ~ kids_in_hh+
                 age_wife+religion+location +religious_knowledge_scale+
                 MI_geo_proximity+
                 MI_economic_capital+
                 MI_human_capital, data=rels, family = "normal",
               prior = c(set_prior("normal(0,2)", class = "b"),
                         set_prior("normal(0,10)", class="b",coef="age_wife")),
               warmup = 1000, iter = 5000, chains = 4,
               control = list(adapt_delta = 0.95))

print(summary(model3.2b, prob=0.95,priors=TRUE), digits = 6)
Family: gaussian 
# Links: mu = identity; sigma = identity 
# Formula: familyBariReligiousAfter ~ kids_in_hh + age_wife + religion + location + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# Data: rels (Number of observations: 5851) 
# Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# total post-warmup samples = 16000
# 
# Priors: 
#   b ~ normal(0,2)
# b_age_wife ~ normal(0,10)
# Intercept ~ student_t(3, 0, 2.5)
# sigma ~ student_t(3, 0, 2.5)
# 
# Population-Level Effects: 
#   Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                 -0.294561  0.060675 -0.413095 -0.175248 1.000501    17159    12483
# kids_in_hh                -0.014249  0.005331 -0.024618 -0.003956 1.000003    21414    11927
# age_wife                   0.010656  0.001106  0.008485  0.012833 1.000522    16753    13342
# religion                  -0.101828  0.030061 -0.160472 -0.041856 1.000092    15706    12041
# location                   0.020988  0.007530  0.006452  0.035783 1.000017    19926    11303
# religious_knowledge_scale  0.013355  0.003054  0.007284  0.019252 1.000266    16884    13723
# MI_geo_proximity          -0.063704  0.011264 -0.086158 -0.041496 1.000668    18289    11214
# MI_economic_capital        0.167306  0.009392  0.149057  0.185525 1.000127    15977    11219
# MI_human_capital           0.076404  0.015696  0.045315  0.107305 1.000909    14558    11550
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# sigma 0.585961  0.005454 0.575407 0.596728 1.000496    18374    10797
# save model
path<- (paste0("results/"))
filename <- "Religiosity_predicted_by_geo_distance_relatives_normal.rds"

saveRDS(model3.2b, paste0(path, filename))
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#### make geo distance and ordinal categorical outcome variable for non relatives

# in rethinking and brms
setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh")
newdata <- read_csv("newdata.csv")
options(scipen=999)
# remove duplicated women
newdata <- newdata %>% distinct(idwife, .keep_all = TRUE)
# 1) center and scale variables for easier interpretability fo parameter estimates
newdata$religious_knowledge_scale <-  newdata$religious_knowledge_scale-mean(newdata$religious_knowledge_scale, na.rm=T)
newdata$hh_total  <- newdata$hh_total-mean(newdata$hh_total, na.rm=T)  
newdata$kids_in_hh  <- newdata$kids_in_hh-mean(newdata$kids_in_hh, na.rm=T)
d <- newdata[c(1,5,7,8,9,36,35,44,45,47,49)] # add 36 for non-rels and 35 for rels
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
# Make model in brms  

model3.1c <- 
  brm(data = non_rels, 
      family = cumulative("logit"),
      location ~ 1 + age_wife +  MI_geo_proximity + MI_economic_capital + MI_human_capital +
        religion+ religious_knowledge_scale + kids_in_hh + familyBariReligiousAfter,
      prior = c(prior(normal(0, 1.5), class = Intercept),
                prior(normal(0, 0.5), class = b)),
      iter = 5000, warmup = 1000, cores = 4, chains = 4,
      seed = 12,
      file = "results/Geo_distance_non_rels_ordinal_cumulative")

print(model3.1c)

#save non rels ordinal outcome model results
path<- (paste0("results/"))
filename <- "Geo_distance_non_relatives_ord_cum.rds"

saveRDS(model3.1c, paste0(path, filename))

###############################################################################################
###############################################################################################3
## relatives geo_distance
rels <- r %>% left_join (d, by=c("id_Questionaire"="idwife"))
rels <- rels[complete.cases(rels),]


# Make model in brms  

model3.2c <- 
  brm(data = rels, 
      family = cumulative("logit"),
      location ~ 1 + age_wife +  MI_geo_proximity + MI_economic_capital + MI_human_capital +
        religion+ religious_knowledge_scale + kids_in_hh + familyBariReligiousAfter,
      prior = c(prior(normal(0, 1.5), class = Intercept),
                prior(normal(0, 0.5), class = b)),
      iter = 5000, warmup = 1000, cores = 4, chains = 4,
      seed = 12,
      file = "results/Geo_distance_rels_ordinal_cumulative")

print(model3.2c)

# Family: cumulative 
# Links: mu = logit; disc = identity 
# Formula: location ~ 1 + age_wife + MI_geo_proximity + MI_economic_capital + MI_human_capital + religion + religious_knowledge_scale + kids_in_hh + familyBariReligiousAfter 
# Data: rels (Number of observations: 5851) 
# Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# total post-warmup samples = 16000
# 
# Population-Level Effects: 
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept[1]                 -1.51      0.19    -1.88    -1.15 1.00    17603    13074
# Intercept[2]                  0.80      0.19     0.44     1.17 1.00    17761    12655
# Intercept[3]                  1.55      0.19     1.18     1.92 1.00    17665    12832
# Intercept[4]                  2.89      0.20     2.50     3.27 1.00    18100    13005
# age_wife                     -0.01      0.00    -0.01     0.00 1.00    17682    12486
# MI_geo_proximity              0.06      0.04    -0.01     0.13 1.00    19471    11431
# MI_economic_capital          -0.10      0.03    -0.16    -0.04 1.00    18677    13246
# MI_human_capital             -0.03      0.05    -0.13     0.07 1.00    16944    11968
# religion                     -0.51      0.10    -0.70    -0.33 1.00    18595    12382
# religious_knowledge_scale     0.05      0.01     0.03     0.07 1.00    18545    11836
# kids_in_hh                    0.03      0.02    -0.01     0.06 1.00    18561    11447
# familyBariReligiousAfter      0.15      0.04     0.07     0.23 1.00    21774    12218
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

d <- newdata[c(27,11,47,4,5,7,8,9,44,45,49)] 
d <- d[complete.cases(d), ] 

hist(d$percent_rels_econ_help)

library(fitdistrplus)
library(logspline)
descdist(d$percent_rels_econ_help, discrete = FALSE, boot=500)  # beta
library(gamlss)
fit <- fitDist(d$percent_rels_econ_help, k = 2, type = "realAll", trace = FALSE, try.gamlss = TRUE)
summary(fit)
#Family:   c("exGAUS", "ex-Gaussian") 

## try beta - transform first
# beta transformation
# transform to be between 0 1nd 1 for beta distribution

d$percent_rels_econ_help <- (d$percent_rels_econ_help  * (783) + 0.5) / 784


model4<-brm(percent_rels_econ_help ~ kids_in_hh+
                age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital, data=d, family = "beta",
              prior = c(set_prior("normal(0,2)", class = "b"),
                        set_prior("normal(0,10)", class="b",coef="age_wife")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model4, prob=0.95,priors=TRUE), digits = 6)

# Family: beta 
# Links: mu = logit; phi = identity 
# Formula: percent_rels_econ_help ~ kids_in_hh + age_wife + religion + familyBariReligiousAfter + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# Data: d (Number of observations: 658) 
# Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# total post-warmup samples = 16000
# 
# Priors: 
#   b ~ normal(0,2)
# b_age_wife ~ normal(0,10)
# Intercept ~ student_t(3, 0, 2.5)
# phi ~ gamma(0.01, 0.01)
# 
# Population-Level Effects: 
#   Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                  1.715099  0.342775  1.048217  2.390072 1.000137    16985    11914
# kids_in_hh                 0.018363  0.030742 -0.041015  0.080125 1.000176    20723    11403
# age_wife                  -0.000992  0.006496 -0.013867  0.011716 1.000135    17085    11799
# religion                   0.088282  0.174672 -0.254517  0.431915 1.000010    18776    11741
# familyBariReligiousAfter   0.048912  0.076953 -0.100290  0.199978 1.000042    20248    11685
# religious_knowledge_scale -0.046130  0.015488 -0.076245 -0.014656 1.000275    18286    12463
# MI_geo_proximity           0.002139  0.057769 -0.107232  0.119339 1.000244    21189    10690
# MI_economic_capital       -0.004426  0.056165 -0.114226  0.107256 1.001262    19569    12251
# MI_human_capital           0.020535  0.089091 -0.151126  0.198876 1.000397    16871    11955
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# phi 0.990256  0.063538 0.870652 1.122063 1.000030    15227    11436
path<- (paste0("results/"))
filename <- "Percent_rels_econ_help_beta.rds"

saveRDS(model4, paste0(path, filename))

# 4.1) non rel econ help: - (poisson) p=0.06
library(tidyverse)
library(brms)
library(readr)

d <- newdata[c(25,11,47,4,5,7,8,9,44,45,49)] 

d <- d[complete.cases(d), ] 

hist(d$non_rels_econ_help)

library(fitdistrplus)
library(logspline)
descdist(d$non_rels_econ_help, discrete = TRUE, boot=500)  # negative binomial

model4.1<-brm(non_rels_econ_help ~ kids_in_hh+
              age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
              MI_geo_proximity+
              MI_economic_capital+
              MI_human_capital, data=d, family = negbinomial(link = "log", link_shape = "identity"),
            prior = c(set_prior("normal(0,2)", class = "b"),
                      set_prior("normal(0,10)", class="b",coef="age_wife")),
            warmup = 1000, iter = 5000, chains = 4,
            control = list(adapt_delta = 0.95))

print(summary(model4.1, prob=0.95,priors=TRUE), digits = 6)
# Family: negbinomial 
# Links: mu = log; shape = identity 
# Formula: non_rels_econ_help ~ kids_in_hh + sex + age_wife + religion + familyBariReligiousAfter + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# Data: d (Number of observations: 762) 
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
#   Estimate Est.Error  l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                 -0.115372  0.921145 -1.928218 1.701771 1.000021    14865    12155
# kids_in_hh                -0.074202  0.086931 -0.242515 0.096076 1.000093    22262    12043
# sex                        0.339695  0.264274 -0.170814 0.864005 0.999943    21086    12212
# age_wife                  -0.021971  0.017915 -0.057269 0.013287 1.000105    14596    12179
# religion                  -0.638422  0.438070 -1.503258 0.214978 1.000451    20930    12783
# familyBariReligiousAfter  -0.181211  0.175531 -0.523596 0.159640 1.000662    21810    11708
# religious_knowledge_scale  0.063578  0.038920 -0.005212 0.145369 1.000238    18613    11487
# MI_geo_proximity           0.271924  0.312385 -0.285036 0.936861 1.000004    21373    10745
# MI_economic_capital        0.079619  0.146651 -0.205127 0.374982 1.000361    19588    12509
# MI_human_capital          -0.096403  0.230207 -0.549128 0.354184 1.000111    14538    11446
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# shape 0.166442  0.027709 0.119378 0.227576 1.000209    19906    11652


path<- (paste0("results/"))
filename <- "Non_rels_econ_help_neg_binom.rds"

saveRDS(model4.1, paste0(path, filename))


# 4.2) rels_econ_help: poisson (Not included in top models)
library(tidyverse)
library(brms)
library(readr)

d <- newdata[c(23,11,47,4,5,7,8,9,44,45,49)] 

d <- d[complete.cases(d), ] 

hist(d$rels_econ_help)

library(fitdistrplus)
library(logspline)
descdist(d$rels_econ_help, discrete = TRUE, boot=500)  # poisson
d$rels_econ_help<-d$rels_econ_help-0.001
d$rels_econ_help<- as.integer(d$rels_econ_help)

model4.2<-brm(rels_econ_help ~ kids_in_hh+
                age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital, data=d, family = "poisson",
              prior = c(set_prior("normal(0,2)", class = "b"),
                        set_prior("normal(0,10)", class="b",coef="age_wife")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model4.2, prob=0.95,priors=TRUE), digits = 6)

# Family: poisson 
# Links: mu = log 
# Formula: rels_econ_help ~ kids_in_hh + age_wife + religion + familyBariReligiousAfter + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# Data: d (Number of observations: 762) 
# Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# total post-warmup samples = 16000
# 
# Priors: 
#   b ~ normal(0,2)
# b_age_wife ~ normal(0,10)
# Intercept ~ student_t(3, 1.1, 2.5)
# 
# Population-Level Effects: 
#   Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                  2.386207  0.154401  2.081094  2.686762 1.000023    16735    13248
# kids_in_hh                -0.011031  0.014652 -0.039833  0.017325 1.000091    19701    12528
# age_wife                  -0.023700  0.003000 -0.029609 -0.017798 1.000018    16415    13163
# religion                  -0.452535  0.087507 -0.629676 -0.283505 1.000053    16149    12057
# familyBariReligiousAfter   0.016055  0.034052 -0.049592  0.082759 0.999938    17421    11651
# religious_knowledge_scale  0.013786  0.006924 -0.000250  0.027083 1.000271    16522    12522
# MI_geo_proximity           0.010606  0.026288 -0.043689  0.059746 1.000485    21388    11441
# MI_economic_capital       -0.008937  0.026201 -0.060111  0.041967 1.000542    15586    12352
# MI_human_capital          -0.161264  0.042572 -0.244504 -0.078166 1.000283    14196    12697

path<- (paste0("results/"))
filename <- "rels_econ_help_poisson.rds"

saveRDS(model4.2, paste0(path, filename))

# 5) percent_rels_emotional_support: + 0.0251395  p=  0.602 (Not included in top models)???
library(tidyverse)
library(brms)
library(readr)

d <- newdata[c(33,11,47,4,5,7,8,9,44,45,49)] 

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

model5<-brm(percent_rels_emot_support ~ kids_in_hh+
                age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital, data=d, family = "beta",
              prior = c(set_prior("normal(0,2)", class = "b"),
                        set_prior("normal(0,10)", class="b",coef="age_wife")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model5, prob=0.95,priors=TRUE), digits = 6)

# Family: beta 
# Links: mu = logit; phi = identity 
# Formula: percent_rels_emot_support ~ kids_in_hh + age_wife + religion + familyBariReligiousAfter + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# Data: d (Number of observations: 759) 
# Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# total post-warmup samples = 16000
# 
# Priors: 
#   b ~ normal(0,2)
# b_age_wife ~ normal(0,10)
# Intercept ~ student_t(3, 0, 2.5)
# phi ~ gamma(0.01, 0.01)
# 
# Population-Level Effects: 
#   Estimate Est.Error  l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                  3.009154  0.279116  2.465469 3.547226 1.000271    14250    13339
# kids_in_hh                -0.004387  0.024461 -0.052195 0.044140 1.000493    19050    11619
# age_wife                   0.001317  0.005159 -0.008742 0.011396 1.000560    14968    12966
# religion                  -0.082205  0.137531 -0.344011 0.188871 1.000315    16695    12610
# familyBariReligiousAfter   0.024218  0.062608 -0.097613 0.144935 1.001538    18564    11859
# religious_knowledge_scale  0.000252  0.012496 -0.022650 0.026356 0.999964    15568    12411
# MI_geo_proximity           0.004315  0.050865 -0.087277 0.112052 1.000253    17890    10179
# MI_economic_capital       -0.020327  0.046754 -0.111369 0.073120 1.000281    17115    12875
# MI_human_capital          -0.028004  0.076009 -0.174278 0.122600 1.000351    13444    11828
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# phi 4.394473  0.344992 3.746410 5.094373 1.000260    12013    11416
path<- (paste0("results/"))
filename <- "percent_rels_emot_support_beta.rds"

saveRDS(model5, paste0(path, filename))


# 5.1) emot_support_non_rels:  + B=0.018276   p= 0.869 (Not included in top models)
library(tidyverse)
library(brms)
library(readr)
d <- newdata[c(31,11,47,4,5,7,8,9,44,45,49)] 

d <- d[complete.cases(d), ] 

# hist(d$emot_support_non_rels)
# 
# library(fitdistrplus)
# library(logspline)
# descdist(d$emot_support_non_rels, discrete = TRUE, boot=500)  # negative binomial


model5.1<-brm(emot_support_non_rels ~ kids_in_hh+
                age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital, data=d, family = negbinomial(link = "log", link_shape = "identity"),
              prior = c(set_prior("normal(0,2)", class = "b"),
                        set_prior("normal(0,10)", class="b",coef="age_wife")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model5.1, prob=0.95,priors=TRUE), digits = 6)

# Family: negbinomial 
# Links: mu = log; shape = identity 
# Formula: emot_support_non_rels ~ kids_in_hh + age_wife + religion + familyBariReligiousAfter + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# Data: d (Number of observations: 762) 
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
#   Estimate Est.Error  l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                 -0.973201  1.402356 -3.714522 1.831276 1.000296    13602    11604
# kids_in_hh                 0.074395  0.115824 -0.156193 0.305244 1.000601    19193    11060
# age_wife                  -0.014680  0.026704 -0.067365 0.037658 1.000257    13615    11509
# religion                   0.380615  0.567130 -0.698584 1.524262 1.000000    18147    12565
# familyBariReligiousAfter  -0.051815  0.244715 -0.530654 0.437974 1.000369    21636    11862
# religious_knowledge_scale  0.038937  0.066889 -0.085891 0.178876 1.000603    16567    11906
# MI_geo_proximity           0.252664  0.445713 -0.554034 1.200149 1.000006    18041    11180
# MI_economic_capital        0.034707  0.202631 -0.357201 0.437940 1.000014    15492    11820
# MI_human_capital           0.328063  0.341550 -0.342146 1.003171 1.000287    12857    11276
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# shape 0.077330  0.015600 0.051080 0.111977 1.000091    18567    10920

path<- (paste0("results/"))
filename <- "Non_rels_emot_support_neg_binom.rds"

saveRDS(model5.1, paste0(path, filename))


# 5.2) emot_support_rels: + B= 0.179870   p < 0.0000000000000002 **
# library(tidyverse)
# library(brms)
# library(readr)
# options(scipen=999)
# 
# 
# d <- newdata[c(29,11,47,4,5,7,8,9,44,45,49)] 
# 
# d <- d[complete.cases(d), ] 
# 
# # hist(d$emot_support_rels)
# # 
# # library(fitdistrplus)
# # library(logspline)
# # descdist(d$emot_support_rels, discrete = TRUE, boot=500)  # poisson
# 
# 
# model5.2<-brm(emot_support_rels ~ kids_in_hh+
#                 age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
#                 MI_geo_proximity+
#                 MI_economic_capital+
#                 MI_human_capital, data=d, family = "poisson",
#               prior = c(set_prior("normal(0,2)", class = "b"),
#                         set_prior("normal(0,10)", class="b",coef="age_wife")),
#               warmup = 1000, iter = 5000, chains = 4,
#               control = list(adapt_delta = 0.95))
# 
# print(summary(model5.2, prob=0.95,priors=TRUE), digits = 6)

# Family: poisson 
# Links: mu = log 
# Formula: emot_support_rels ~ kids_in_hh + age_wife + religion + familyBariReligiousAfter + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# Data: d (Number of observations: 762) 
# Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# total post-warmup samples = 16000
# 
# Priors: 
#   b ~ normal(0,2)
# b_age_wife ~ normal(0,10)
# Intercept ~ student_t(3, 1.6, 2.5)
# 
# Population-Level Effects: 
#   Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                  1.916416  0.110735  1.699687  2.132299 1.000253    15025    12392
# kids_in_hh                -0.018952  0.010042 -0.038823  0.000579 1.000787    18281    11979
# age_wife                  -0.003421  0.002107 -0.007517  0.000700 1.000344    14980    12446
# religion                  -0.093120  0.055341 -0.201898  0.015351 1.000050    13419    11541
# familyBariReligiousAfter   0.194796  0.025190  0.145311  0.244229 1.000120    15264    11286
# religious_knowledge_scale  0.008025  0.004704 -0.001513  0.016989 0.999971    14589    12319
# MI_geo_proximity          -0.047658  0.026322 -0.100423  0.002223 1.001009    18453    11760
# MI_economic_capital       -0.000632  0.018610 -0.037448  0.035938 1.000240    14858    11523
# MI_human_capital          -0.068100  0.030347 -0.127085 -0.008726 0.999983    12843    11701

path<- (paste0("results/"))
filename <- "rels_emot_support_poisson.rds"

saveRDS(model5.2, paste0(path, filename))
  
# 6) childcare_help_rels_percent: B=0.08 p=0.08 (religious knowledge however is a very significant negative predictor)
library(tidyverse)
library(brms)
library(readr)


d <- newdata[c(37,11,47,4,5,7,8,9,44,45,49)] 

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

model6<-brm(childcare_help_rels_percent ~ kids_in_hh+
              age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
              MI_geo_proximity+
              MI_economic_capital+
              MI_human_capital, data=d, family = "beta",
            prior = c(set_prior("normal(0,2)", class = "b"),
                      set_prior("normal(0,10)", class="b",coef="age_wife")),
            warmup = 1000, iter = 5000, chains = 4,
            control = list(adapt_delta = 0.95))

print(summary(model6, prob=0.95,priors=TRUE), digits = 6)

# Family: beta 
# Links: mu = logit; phi = identity 
# Formula: childcare_help_rels_percent ~ kids_in_hh + age_wife + religion + familyBariReligiousAfter + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# Data: d (Number of observations: 716) 
# Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# total post-warmup samples = 16000
# 
# Priors: 
#   b ~ normal(0,2)
# b_age_wife ~ normal(0,10)
# Intercept ~ student_t(3, 0, 2.5)
# phi ~ gamma(0.01, 0.01)
# 
# Population-Level Effects: 
#   Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                  2.381231  0.306640  1.786864  2.983401 1.000015    12369    12148
# kids_in_hh                -0.002102  0.026701 -0.054639  0.049662 1.000096    20155    12724
# age_wife                  -0.000131  0.005722 -0.011221  0.011008 1.000270    12476    11019
# religion                   0.190324  0.159536 -0.121477  0.504302 1.000197    14414    11892
# familyBariReligiousAfter   0.086154  0.065696 -0.041044  0.215616 1.001234    19161    12394
# religious_knowledge_scale -0.044112  0.016702 -0.072496 -0.005171 1.000119    12340     9386
# MI_geo_proximity           0.004833  0.065500 -0.112914  0.140338 1.000428    19167    10901
# MI_economic_capital       -0.004017  0.050937 -0.104779  0.097392 0.999997    18989    11762
# MI_human_capital           0.005461  0.082057 -0.152148  0.168284 1.000201    12710    10788
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# phi 1.947342  0.148932 1.671007 2.255540 1.000432     9930    11087
path<- (paste0("results/"))
filename <- "percent_rels_childcare_help_beta.rds"

saveRDS(model6, paste0(path, filename))

# 6.1) childcare_help_non_rels: B= -1.14444    p= 0.00000000000791 ***
library(tidyverse)
library(brms)
library(readr)

d <- newdata[c(36,11,47,4,5,7,8,9,44,45,49)] 

d <- d[complete.cases(d), ] 

# hist(d$childcare_help_non_rels)
# 
# library(fitdistrplus)
# library(logspline)
# descdist(d$childcare_help_non_rels, discrete = TRUE, boot=500)  # negative binomial
# library(gamlss)
# fit <- fitDist(d$childcare_help_non_rels, k = 2, type = "counts", trace = FALSE, try.gamlss = TRUE)
# #  type = c("realAll", "realline", "realplus", "real0to1", "counts", "binom","extra")
# summary(fit)

model6.1<-brm(childcare_help_non_rels ~ kids_in_hh+
                age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital, data=d, family = negbinomial(link = "log", link_shape = "identity"),
              prior = c(set_prior("normal(0,2)", class = "b"),
                        set_prior("normal(0,10)", class="b",coef="age_wife")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model6.1, prob=0.95,priors=TRUE), digits = 6)

# Family: negbinomial 
# Links: mu = log; shape = identity 
# Formula: childcare_help_non_rels ~ kids_in_hh + age_wife + religion + familyBariReligiousAfter + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# Data: d (Number of observations: 762) 
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
# Intercept                 -2.187209  1.740669 -5.598983  1.314764 1.000361    12081    10853
# kids_in_hh                 0.295496  0.148914  0.010089  0.602753 1.000388    16799    11294
# age_wife                  -0.004650  0.033048 -0.070200  0.060241 1.000439    12178    11157
# religion                  -0.294103  0.725623 -1.719387  1.125603 1.000584    15894    12130
# familyBariReligiousAfter  -1.367604  0.380401 -2.158242 -0.657826 1.000209    14342    11246
# religious_knowledge_scale  0.058157  0.056889 -0.038374  0.184913 1.000440    13740    10056
# MI_geo_proximity           0.797958  0.672256 -0.316514  2.267564 0.999980    14029    10034
# MI_economic_capital        0.109987  0.241524 -0.360311  0.582658 1.000237    14643    11960
# MI_human_capital           0.538504  0.445790 -0.322812  1.424744 1.000429    11998    11260
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# shape 0.079698  0.024912 0.042887 0.138174 1.000080    13641    11028


path<- (paste0("results/"))
filename <- "childcare_help_non_rels_neg_binom.rds"

saveRDS(model6.1, paste0(path, filename))

# 6.2) childcare_help_rels: B= 0.012237   p=0.631751  (Not included in top model)
#** #the result here is that less religious people have more childcare help from non relatives
#*library(tidyverse)
library(brms)
library(readr)

d <- newdata[c(35,11,47,4,5,7,8,9,44,45,49)] 

d <- d[complete.cases(d), ] 

# hist(d$childcare_help_rels)
# library(fitdistrplus)
# library(logspline)
# descdist(d$childcare_help_rels, discrete = TRUE, boot=500)  # poisson
# library(gamlss)
# fit <- fitDist(d$childcare_help_rels, k = 2, type = "counts", trace = FALSE, try.gamlss = TRUE)
# #  type = c("realAll", "realline", "realplus", "real0to1", "counts", "binom","extra")
# summary(fit)


model6.2<-brm(childcare_help_rels ~ kids_in_hh+
                age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital, data=d, family = negbinomial(link = "log", link_shape = "identity"),
              prior = c(set_prior("normal(0,2)", class = "b"),
                        set_prior("normal(0,10)", class="b",coef="age_wife")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model6.2, prob=0.95,priors=TRUE), digits = 6)
# Family: negbinomial 
# Links: mu = log; shape = identity 
# Formula: childcare_help_rels ~ kids_in_hh + age_wife + religion + familyBariReligiousAfter + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# Data: d (Number of observations: 762) 
# Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# total post-warmup samples = 16000
# 
# Priors: 
#   b ~ normal(0,2)
# b_age_wife ~ normal(0,10)
# Intercept ~ student_t(3, 1.1, 2.5)
# shape ~ gamma(0.01, 0.01)
# 
# Population-Level Effects: 
#   Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                  1.939671  0.157187  1.629918  2.250595 1.000176    16982    11983
# kids_in_hh                 0.009725  0.014109 -0.018127  0.036799 1.000363    19280    11599
# age_wife                  -0.014165  0.003019 -0.020140 -0.008202 1.000190    16827    12319
# religion                  -0.053436  0.079160 -0.209405  0.100919 1.000580    14974    11467
# familyBariReligiousAfter   0.003640  0.034681 -0.065048  0.070895 1.000056    18307    12292
# religious_knowledge_scale  0.002612  0.007651 -0.012778  0.017319 1.000332    15155    12941
# MI_geo_proximity          -0.116221  0.043477 -0.207523 -0.035719 1.000062    20534    11793
# MI_economic_capital        0.042804  0.026032 -0.008941  0.092974 1.000154    15678    11663
# MI_human_capital          -0.158839  0.043432 -0.244876 -0.074509 1.000089    14204    11989
# 
# Family Specific Parameters: 
#   Estimate Est.Error  l-95% CI   u-95% CI     Rhat Bulk_ESS Tail_ESS
# shape 42.591673 35.527565 14.519537 139.470068 1.000275    15484     8796


path<- (paste0("results/"))
filename <- "childcare_help_rels_neg_binom.rds"

saveRDS(model6.2, paste0(path, filename))


# 7) percent_overall_help_rels:  B=0.070260   p= 0.12377 
library(tidyverse)
library(brms)
library(readr)

d <- newdata[c(42,11,47,4,5,7,8,9,44,45,49)] 

d <- d[complete.cases(d), ] 

hist(d$percent_overall_help_rels)

d$percent_overall_help_rels <- (d$percent_overall_help_rels * (783) + 0.5) / 784
# library(fitdistrplus)
# library(logspline)
# descdist(d$percent_overall_help_rels, discrete = FALSE, boot=500)  # beta
# library(gamlss)
# fit <- fitDist(d$percent_overall_help_rels, k = 2, type = "real0to1", trace = FALSE, try.gamlss = TRUE)
# #  type = c("realAll", "realline", "realplus", "real0to1", "counts", "binom","extra")
# summary(fit)


model7<-brm(percent_overall_help_rels ~ kids_in_hh+
                age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital, data=d, family = "gamma",
              prior = c(set_prior("normal(0,2)", class = "b"),
                        set_prior("normal(0,10)", class="b",coef="age_wife")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model7, prob=0.95,priors=TRUE), digits = 6)
# Family: gamma 
# Links: mu = log; shape = identity 
# Formula: percent_overall_help_rels ~ kids_in_hh + age_wife + religion + familyBariReligiousAfter + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# Data: d (Number of observations: 762) 
# Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# total post-warmup samples = 16000
# 
# Priors: 
#   b ~ normal(0,2)
# b_age_wife ~ normal(0,10)
# Intercept ~ student_t(3, 0, 2.5)
# shape ~ gamma(0.01, 0.01)
# 
# Population-Level Effects: 
#   Estimate Est.Error  l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                 -0.057922  0.051692 -0.158640 0.044635 1.000038    19457    13384
# kids_in_hh                 0.000042  0.004612 -0.009034 0.008914 1.000319    22209    11627
# age_wife                   0.000265  0.000982 -0.001697 0.002183 1.000048    19438    12757
# religion                  -0.001281  0.025651 -0.051319 0.049428 0.999960    15594    12073
# familyBariReligiousAfter   0.015760  0.011599 -0.006906 0.038666 1.000059    17028    11205
# religious_knowledge_scale -0.003799  0.002477 -0.008651 0.001087 0.999844    18229    14138
# MI_geo_proximity          -0.000875  0.009516 -0.019183 0.018127 1.000536    18900    11575
# MI_economic_capital       -0.003061  0.008754 -0.020170 0.014075 1.000250    17616    12519
# MI_human_capital          -0.004720  0.014096 -0.032600 0.023036 1.000112    16689    11530
# 
# Family Specific Parameters: 
#   Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS Tail_ESS
# shape 28.007308  1.440771 25.270943 30.894420 1.000176    16992    11492

path<- (paste0("results/"))
filename <- "percent_overall_help_rels_gamma.rds"

saveRDS(model7, paste0(path, filename))


# 7.1) overall_help_non_rels: B=0.37673    p= 0.001963 **
library(tidyverse)
library(brms)
library(readr)

d <- newdata[c(40,11,47,4,5,7,8,9,44,45,49)] 

d <- d[complete.cases(d), ] 

# hist(d$overall_help_non_rels)
# 
# library(fitdistrplus)
# library(logspline)
# descdist(d$overall_help_non_rels, discrete = TRUE, boot=500)  # negative binomial
# library(gamlss)
# fit <- fitDist(d$overall_help_non_rels, k = 2, type = "counts", trace = FALSE, try.gamlss = TRUE)
# #  type = c("realAll", "realline", "realplus", "real0to1", "counts", "binom","extra")
# summary(fit)  # Family:  c("NBI", "Negative Binomial type I") 


model7.1<-brm(overall_help_non_rels ~ kids_in_hh+
                age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital, data=d, family = negbinomial(link = "log", link_shape = "identity"),
              prior = c(set_prior("normal(0,2)", class = "b"),
                        set_prior("normal(0,10)", class="b",coef="age_wife")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model7.1, prob=0.95,priors=TRUE), digits = 6)

# Family: negbinomial 
# Links: mu = log; shape = identity 
# Formula: overall_help_non_rels ~ kids_in_hh + age_wife + religion + familyBariReligiousAfter + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# Data: d (Number of observations: 762) 
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
#   Estimate Est.Error  l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                  0.364147  0.978960 -1.531130 2.305295 1.000305    14811    11968
# kids_in_hh                 0.013051  0.085784 -0.156125 0.180099 1.000279    22624    11529
# age_wife                  -0.015139  0.018624 -0.052169 0.021140 1.000320    14786    12200
# religion                  -0.083347  0.432081 -0.908840 0.801390 0.999989    17308    12438
# familyBariReligiousAfter  -0.268359  0.175016 -0.610292 0.071280 1.000696    21549    10608
# religious_knowledge_scale  0.038347  0.039545 -0.029532 0.123435 1.000162    16531    11043
# MI_geo_proximity           0.501856  0.386707 -0.190258 1.319309 0.999958    18116    11478
# MI_economic_capital        0.061901  0.143994 -0.218640 0.345730 1.000167    15430    12432
# MI_human_capital           0.121733  0.245614 -0.358864 0.600378 0.999891    14182    11443
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# shape 0.118039  0.014154 0.092634 0.147866 0.999972    22745    11394

path<- (paste0("results/"))
filename <- "overall_help_non_rels_neg_binom.rds"

saveRDS(model7.1, paste0(path, filename))

# 7.2) overall_help_rels:   B=1.58011    p=0.0000000444 **
library(tidyverse)
library(brms)
library(readr)

d <- newdata[c(38,11,47,4,5,7,8,9,44,45,49)] 

d <- d[complete.cases(d), ] 

# hist(d$overall_help_rels)
# 
# library(fitdistrplus)
# library(logspline)
# descdist(d$overall_help_rels, discrete = TRUE, boot=500)  # poisson
# library(gamlss)
# fit <- fitDist(d$overall_help_rels, k = 2, type = "counts", trace = FALSE, try.gamlss = TRUE)
# #  type = c("realAll", "realline", "realplus", "real0to1", "counts", "binom","extra")
# summary(fit)


model7.2<-brm(overall_help_rels ~ kids_in_hh+
                age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital, data=d, family = "poisson",
              prior = c(set_prior("normal(0,2)", class = "b"),
                        set_prior("normal(0,10)", class="b",coef="age_wife")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model7.2, prob=0.95,priors=TRUE), digits = 6)

# Family: poisson 
# Links: mu = log 
# Formula: overall_help_rels ~ kids_in_hh + age_wife + religion + familyBariReligiousAfter + religious_knowledge_scale + MI_geo_proximity + MI_economic_capital + MI_human_capital 
# Data: d (Number of observations: 762) 
# Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1;
# total post-warmup samples = 16000
# 
# Priors: 
#   b ~ normal(0,2)
# b_age_wife ~ normal(0,10)
# Intercept ~ student_t(3, 2.6, 2.5)
# 
# Population-Level Effects: 
#   Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                  3.195316  0.070135  3.058124  3.334211 0.999914    15055    13366
# kids_in_hh                -0.003549  0.006356 -0.015977  0.009051 1.000293    14822    11347
# age_wife                  -0.009658  0.001341 -0.012314 -0.007031 0.999903    15112    13373
# religion                  -0.129389  0.035580 -0.200567 -0.060205 1.000082    12583    11059
# familyBariReligiousAfter   0.105730  0.015763  0.075047  0.136891 1.000361    14246    12100
# religious_knowledge_scale  0.006030  0.003153 -0.000217  0.012112 0.999973    16690    12421
# MI_geo_proximity          -0.037394  0.015559 -0.068979 -0.007931 1.000121    14230    11212
# MI_economic_capital        0.005608  0.011641 -0.017172  0.028375 1.000056    13120    11813
# MI_human_capital          -0.090597  0.019152 -0.128502 -0.053131 1.000322    12816    11594
path<- (paste0("results/"))
filename <- "overall_help_rels_poisson.rds"

saveRDS(model7.2, paste0(path, filename))

#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

# in rethinking and brms
setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh")
newdata <- read_csv("newdata.csv")
options(scipen=999)
# remove duplicated women
newdata <- newdata %>% distinct(idwife, .keep_all = TRUE)
# 1) center and scale variables for easier interpretability fo parameter estimates
newdata$religious_knowledge_scale <-  newdata$religious_knowledge_scale-mean(newdata$religious_knowledge_scale, na.rm=T)
newdata$hh_total  <- newdata$hh_total-mean(newdata$hh_total, na.rm=T)  
newdata$kids_in_hh  <- newdata$kids_in_hh-mean(newdata$kids_in_hh, na.rm=T)
d <- newdata[c(1,5,7,8,9,36,35,44,45,47,49)] # add 36 for non-rels and 35 for rels
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

library(fitdistrplus)
library(logspline)
descdist(non_rels$childcare_help_non_rels, discrete = TRUE, boot=500)  # negative binomial

non_rels$childcare_help_non_rels<- as.integer(non_rels$childcare_help_non_rels)
## Run kin and non-kin living in same neighborhood only
model8<-brm(childcare_help_non_rels ~ kids_in_hh+
                age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
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
# remove duplicated women
newdata <- newdata %>% distinct(idwife, .keep_all = TRUE)
# 1) center and scale variables for easier interpretability fo parameter estimates
newdata$religious_knowledge_scale <-  newdata$religious_knowledge_scale-mean(newdata$religious_knowledge_scale, na.rm=T)
newdata$hh_total  <- newdata$hh_total-mean(newdata$hh_total, na.rm=T)  
newdata$kids_in_hh  <- newdata$kids_in_hh-mean(newdata$kids_in_hh, na.rm=T)
d <- newdata[c(1,5,7,8,9,36,35,44,45,47,49)] # add 36 for non-rels and 35 for rels
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

hist(rels$childcare_help_rels)

library(fitdistrplus)
library(logspline)
descdist(rels$childcare_help_rels, discrete = TRUE, boot=500)  # negative binomial

rels$childcare_help_rels<- as.integer(rels$childcare_help_rels)
## Run kin and non-kin living in same neighborhood only
model8.1<-brm(childcare_help_rels ~ kids_in_hh+
              age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
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


# compare models


library(loo)
LOO(a, b)

a <- kfold( model2a , k=10 )
b <- kfold( model2b, k=5 )

fit1 <- add_criterion(model2a, "waic")
fit2 <- add_criterion(model2b, "waic")

loo_compare(fit1,fit2, criterion = "waic")


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
