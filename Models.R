library(tidyverse)
library(brms)
library(readr)
library(sjPlot)
library(sjmisc)
library(sjlabelled)                             # Install & load scales
library("scales")

husbands_new <- read.csv("C:/Users/robert/Dropbox/Github/Intensive extensive kin networks/data/husbands_new.csv", header = T, sep = ",")

husbands_new$percent_rels_in_NW<- as.numeric(husbands_new$percent_rels_in_NW)

husbands_new$religious_knowledge_scale<- scales::rescale(husbands_new$religious_knowledge_scale,to=c(-1,1))



data1 <-  husbands_new %>% filter (Respondent_a=="Wife" | Respondent_a=="Husband")
data2 <-  husbands_new %>% filter (Respondent_a=="Wife")
data3 <-  husbands_new %>% filter (Respondent_a=="Husband")

## make dataset with sex as (husband or wife) link data 2 to data 3
# data2 <- data2 %>% select (2,3,7:10,25:53,57:59)
# 
# data3 <- data3 %>% select(2,3,7,28:50,54:56,59)
# 
# #rename data 3 file Age_a, "NW_total"   
# 
# data4 <- rename(data3, Age_a_m = Age_a,
#        NW_total_m= NW_total,
#        non_rels_m= non_rels,
#        parents_kids_m=parents_kids,                     
#        pat_rels_m=pat_rels,
#        mat_rels_m=mat_rels,                           
#        in_laws_m=in_laws,
#        far_rels_m=far_rels,                          
#        geo_distance_non_rels_m=geo_distance_non_rels,
#        geo_distance_rels_m=geo_distance_rels,                  
#        rels_in_NW_m=rels_in_NW,
#        percent_rels_in_NW_m=percent_rels_in_NW,
#        rels_econ_help_m=rels_econ_help,               
#        non_rels_econ_help_m=non_rels_econ_help,
#        percent_rels_econ_help_m=percent_rels_econ_help,
#        emot_support_rels_m=emot_support_rels,                  
#        emot_support_non_rels_m=emot_support_non_rels,
#        percent_rels_emot_support_m=percent_rels_emot_support,          
#        childcare_work_help_rels_m=childcare_work_help_rels,
#        childcare_work_help_non_rels_m=childcare_work_help_non_rels,       
#        childcare_work_help_rels_percent_m=childcare_work_help_rels_percent,
#        overall_help_rels_m=overall_help_rels, 
#        overall_help_non_rels_m=overall_help_non_rels,
#        percent_overall_help_rels_m=percent_overall_help_rels,
#        Birth_hh_size_m=Birth_hh_size) 
# 
# ## left join data2 to data4
# combined <- data2 %>% left_join(data4, by=c("idwife_a"="idwife_a"))

### either used combined or data 1 (probably data1) to run models using sex to predict Socil
# NW outcomes (add sex to model below)

library(readr)

d<-data1[c(2,28,6,8,9,51,52,54,25:27)]

d$NW_total <- as.numeric(d$NW_total)
### try model
model1 <- brm(NW_total ~ gender_F0_M1_a*familyBariReligiousAfter+
                gender_F0_M1_a*religious_knowledge_scale+
                Kids_a+
                Mothers_kids_a+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital+
                (1|religion)+
                (1|idwife_a), 
              data=d, 
              family = lognormal(),
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model1, prob=0.95,priors=TRUE), digits = 6)
tab_model(model1)

# Population-Level Effects: 
#                                           Estimate Est.Error  l-95% CI  u-95% CI     Rhat
# Intercept                                 2.209917  0.488130  1.154086  3.340761 1.005091
# gender_F0_M1_a                            0.396759  0.182598  0.037730  0.754865 1.000552
# familyBariReligiousAfter                  0.106674  0.020138  0.067263  0.146283 1.000091
# religious_knowledge_scale                 0.053990  0.126792 -0.190127  0.303514 1.000696
# Kids_a                                   -0.000773  0.006773 -0.014122  0.012526 1.000713
# Mothers_kids_a                            0.006064  0.003251 -0.000348  0.012292 1.000166
# MI_geo_proximity                         -0.016864  0.012956 -0.042235  0.008496 1.000077
# MI_economic_capital                       0.030468  0.012097  0.007052  0.054313 1.000134
# MI_human_capital                          0.052428  0.014778  0.023417  0.081506 1.001038
# gender_F0_M1_a:familyBariReligiousAfter  -0.108927  0.030242 -0.168213 -0.049941 1.000083
# gender_F0_M1_a:religious_knowledge_scale  0.175864  0.205867 -0.226740  0.579283 1.000435

# 1) Women have smaller NW's than men
# 2) More religious households have larger networks overall
# 3) Relative religiosity only increases the size of women's social networks -- no impact on men's 

path<- (paste0("results/"))
filename <- "NW_total_lognormal_sex.rds"

saveRDS(model1, paste0(path, filename))

### Percent relatives run
d <- data1[c(2,38,6,8,9,52,51,54,53,25:27)] 
# 
d$percent_rels_in_NW <- as.numeric(d$percent_rels_in_NW)
d$percent_rels_in_NW<- d$percent_rels_in_NW+0.01
d <- d[complete.cases(d), ] 
## run as log normal  
model2 <- brm(percent_rels_in_NW ~ #religious_belief+
                gender_F0_M1_a*familyBariReligiousAfter+
                gender_F0_M1_a*religious_knowledge_scale+
                Kids_a+
                Mothers_kids_a+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital+
                (1|religion)+
                (1|idwife_a), 
              data=d, 
              family = lognormal(),
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95,
                             max_treedepth=13))



print(summary(model2, prob=0.95,priors=TRUE), digits = 6)
tab_model(model2)

# men and women
# Population-Level Effects: 
#                                            Estimate Est.Error  l-95% CI  u-95% CI     Rhat
# Intercept                                 0.314371  0.875550 -0.488074  1.850455 1.593100
# gender_F0_M1_a                           -0.660175  0.170249 -1.011120 -0.411831 1.209662
# familyBariReligiousAfter                  0.026362  0.015572 -0.006097  0.059926 1.419911
# religious_knowledge_scale                -0.105202  0.117952 -0.277245  0.132661 1.208124
# Kids_a                                    0.009810  0.008882 -0.005207  0.022880 1.545305
# Mothers_kids_a                           -0.002974  0.002624 -0.008411  0.002470 1.325128
# MI_geo_proximity                          0.005760  0.009918 -0.015517  0.024887 1.236574
# MI_economic_capital                      -0.007705  0.009714 -0.028874  0.009739 1.070610
# MI_human_capital                         -0.029823  0.015010 -0.059055 -0.010490 1.386112
# gender_F0_M1_a:familyBariReligiousAfter   0.000562  0.026266 -0.044952  0.057343 1.089221
# gender_F0_M1_a:religious_knowledge_scale -0.265037  0.193283 -0.661990  0.012084 1.221803s_belief -0.007414  0.026119 -0.058564  0.044715 1.000279    11781

# 1)  Females have more relatives in their NW's than men
# 2)  More religious people have more relatives in their NW's
# 3)  Intx- More religious knowledge reduces kin density of men' NW's and increases
# kin density of women's networks

#  men only
# Percent  Rels in NW:
#         # familyBariReligiousAfter                 0.043178  -0.019192  0.105203 
#         # religious_knowledge_scale_men           -0.009809  -0.027292  0.007367 
#         # MI_geo_proximity                           0.012798  -0.032001  0.057797 
#         # MI_economic_capital                       -0.036178  -0.082384  0.011327 
#         # MI_human_capital                         -0.082990  -0.139924 -0.025870
#     
path<- (paste0("results/"))
filename <- "percent_relatives_in_NW_lognormal_sex.rds"
setwd("C:/Users/robert/Dropbox/Github/Kin_networks_men")
saveRDS(model2, paste0(path, filename))

# Model 2.1 Relatives in Network

library(tidyverse)
library(brms)
library(readr)

d <- data1[c(2,37,6,8,9,52,51,54,53,25:27)] 
d <- d[complete.cases(d), ] 

d$rels_in_NW<- d$rels_in_NW+0.01
## run as log normal  
model2.1 <- brm(rels_in_NW ~ gender_F0_M1_a*familyBariReligiousAfter+
                  gender_F0_M1_a*religious_knowledge_scale+
                  Kids_a+
                  Mothers_kids_a+
                  MI_geo_proximity+
                  MI_economic_capital+
                  MI_human_capital+
                  (1|religion)+
                  (1|idwife_a), 
                data=d, 
                family = lognormal(),
                prior = c(set_prior("normal(0,2)", class = "b")),
                warmup = 1000, iter = 5000, chains = 4,
                control = list(adapt_delta = 0.95,
                               max_treedepth=13))

print(summary(model2.1, prob=0.95,priors=TRUE), digits = 6)
tab_model(model2.1)

# Population-Level Effects: 
#                                           Estimate Est.Error  l-95% CI  u-95% CI     Rhat
# Intercept                                 2.028842  0.500267  0.918800  3.190022 1.000673
# gender_F0_M1_a                           -0.377006  0.253129 -0.872426  0.119847 1.000007
# familyBariReligiousAfter                  0.135539  0.028547  0.078775  0.191924 1.000713
# religious_knowledge_scale                -0.002297  0.177902 -0.355348  0.342957 1.000247
# Kids_a                                    0.005557  0.009488 -0.012961  0.024207 1.000375
# Mothers_kids_a                            0.002470  0.004627 -0.006552  0.011529 1.000081
# MI_geo_proximity                         -0.011946  0.018182 -0.047535  0.024007 1.000443
# MI_economic_capital                       0.016415  0.017212 -0.017691  0.049996 0.999962
# MI_human_capital                          0.016042  0.020631 -0.024617  0.056906 1.000131
# gender_F0_M1_a:familyBariReligiousAfter  -0.090175  0.043026 -0.174349 -0.006211 1.000238
# gender_F0_M1_a:religious_knowledge_scale -0.197426  0.285243 -0.755310  0.359825 0.999994

#1) Women have more relatives in their networks than men
#2) More religious families have more relatives in their networks
#3) More religious women have more relatives in their NW's, no effect for men

path<- (paste0("results/"))
filename <- "relatives_in_NW_lognormal_sex.rds"

saveRDS(model2.1, paste0(path, filename))


# Model 2.2 Non-relatives in Network
library(tidyverse)
library(brms)
library(readr)


d <- data1[c(2,29,6,8,9,52,51,54,53,25:27)] 
d <- d[complete.cases(d), ] 

#d$non_rels<- d$non_rels+0.01
## run as Negative bionomial
model2.2 <- brm(non_rels ~ gender_F0_M1_a*familyBariReligiousAfter+
                  gender_F0_M1_a*religious_knowledge_scale+
                  Kids_a+
                  Mothers_kids_a+
                  MI_geo_proximity+
                  MI_economic_capital+
                  MI_human_capital+
                  (1|religion)+
                  (1|idwife_a), 
                data=d,
                family = negbinomial(link = "log", link_shape = "identity"),
                prior = c(set_prior("normal(0,2)", class = "b")),
                warmup = 1000, iter = 5000, chains = 4,
                control = list(adapt_delta = 0.95,
                               max_treedepth=13))

print(summary(model2.2, prob=0.95,priors=TRUE), digits = 6)
tab(model2.2)

# Population-Level Effects: 
#                                           Estimate Est.Error  l-95% CI u-95% CI     Rhat
# Intercept                                 0.575823  0.894131 -0.871817 2.449720 1.142129
# gender_F0_M1_a                            1.781248  0.367683  1.042099 2.538245 1.082307
# familyBariReligiousAfter                 -0.044612  0.060884 -0.167557 0.079484 1.043593
# religious_knowledge_scale                 0.403846  0.314303 -0.249326 0.986365 1.015359
# Kids_a                                   -0.017077  0.017819 -0.053984 0.017436 1.032266
# Mothers_kids_a                            0.012683  0.007810 -0.003021 0.028419 1.037805
# MI_geo_proximity                         -0.019951  0.034469 -0.088788 0.050823 1.027366
# MI_economic_capital                       0.036461  0.029848 -0.023494 0.096470 1.052361
# MI_human_capital                          0.160469  0.038863  0.080046 0.229994 1.027966
# gender_F0_M1_a:familyBariReligiousAfter   0.015828  0.075155 -0.137084 0.162432 1.012588
# gender_F0_M1_a:religious_knowledge_scale  0.252715  0.415847 -0.580497 1.103363 1.071272scale  0.011140  0.015073 -0.017831 0.041605 1.006548


#1) males have more non-relatives in their NW's
#2) more educate people have more non-relatives in their networks

path<- (paste0("results/"))
filename <- "non_relatives_in_NW_neg_bin_sex.rds"

saveRDS(model2.2, paste0(path, filename))

#########################################################################
# Geographic location models
## Distance from non-relatives
#Husband's first
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
non_rels<- non_rels[c(1,2,4,5,9,10,12,13,58,55,29,30,31,56)]

## Wives next
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
non_rels2 <- nr %>% left_join (data2, by=c("id_Questionaire"="idwife_a"))
#non_rels2 <- non_rels2[complete.cases(non_rels2),]

non_rels2$location[non_rels2$location==1|non_rels2$location==2] <- 2
non_rels2$location[non_rels2$location==3] <- 3
non_rels2$location[non_rels2$location==4|non_rels2$location==5] <- 4

non_rels2<- non_rels2[c(1,2,3,4,8,9,11,12,57,54,28:30,55)]
### join non-rels to nonrels2 

d <- rbind(non_rels,non_rels2)
# Make model in brms  
library(brms)
model3.1 <- brm(data = d, 
                family = cumulative("logit"),
                location ~ 1+
                  gender_F0_M1_a*familyBariReligiousAfter+
                  gender_F0_M1_a*religious_knowledge_scale+
                  MI_geo_proximity+MI_economic_capital+
                  MI_human_capital +
                  Kids_a+
                  Mothers_kids_a+
                  familyBariReligiousAfter+
                  (1|religion)+
                  (1|id_Questionaire),
                prior = c(prior(normal(0, 1.5), class = Intercept),
                          prior(normal(0, 0.5), class = b)),
                iter = 5000, warmup = 1000, cores = 4, chains = 4,
                control = list(adapt_delta = 0.95,
                               max_treedepth=13))

print(model3.1)
#-Level Effects: 
#                                            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
# Intercept[1]                                -9.00      0.91   -10.77    -7.15 1.00     9870
# Intercept[2]                                 2.40      0.77     1.03     4.07 1.00    10319
# Intercept[3]                                 3.98      0.77     2.60     5.67 1.00    10303
# gender_F0_M1_a                               0.51      0.35    -0.17     1.19 1.00    14310
# familyBariReligiousAfter                     0.49      0.22     0.07     0.92 1.00    10444
# religious_knowledge_scale                    0.09      0.46    -0.80     0.97 1.00    18404
# MI_geo_proximity                             0.09      0.14    -0.19     0.36 1.00    12638
# MI_economic_capital                          0.11      0.12    -0.13     0.34 1.00     7636
# MI_human_capital                             0.61      0.15     0.31     0.91 1.00     6535
# Kids_a                                      -0.09      0.08    -0.24     0.06 1.00     7003
# Mothers_kids_a                              -0.04      0.03    -0.11     0.03 1.00     6664
# gender_F0_M1_a:familyBariReligiousAfter     -0.24      0.26    -0.76     0.28 1.00     9081
# gender_F0_M1_a:religious_knowledge_scale     0.06      0.38    -0.69     0.81 1.00    15119
# 
path<- (paste0("results/"))
filename <- "Geo_distance_non_relatives_ord_cum_sex.rds"

saveRDS(model3.1, paste0(path, filename))

#########################################################################
# Geographic location models
## Distance from relatives
#Husband's first
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
rels <- r %>% left_join (data3, by=c("id_Questionaire"="idhusband"))
#non_rels <- non_rels[complete.cases(non_rels),]

rels$location[rels$location==1|rels$location==2] <- 2
rels$location[rels$location==3] <- 3
rels$location[rels$location==4|rels$location==5] <- 4
# Location Codes
# 1=khana member,
# 2=near bari/neighbor
# 3=other place in Matlab
# 4=Other Place in Bangladesh
# 5=Abroad
rels<- rels[c(1,2,4,5,9,10,12,13,58,55,29,30,31,56)]

## Wives next
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
rels2 <- r %>% left_join (data2, by=c("id_Questionaire"="idwife_a"))
#non_rels2 <- non_rels2[complete.cases(non_rels2),]

rels2$location[rels2$location==1|rels2$location==2] <- 2
rels2$location[rels2$location==3] <- 3
rels2$location[rels2$location==4|rels2$location==5] <- 4

rels2<- rels2[c(1,2,3,4,8,9,11,12,57,54,28:30,55)]
### join non-rels to nonrels2 

d <- rbind(rels,rels2)
# Make model in brms  
library(brms)
model3.2 <- brm(data = d, 
                family = cumulative("logit"),
                location ~ 1+
                  gender_F0_M1_a*familyBariReligiousAfter+
                  gender_F0_M1_a*religious_knowledge_scale+
                  MI_geo_proximity+MI_economic_capital+
                  MI_human_capital +
                  Kids_a+
                  Mothers_kids_a+
                  familyBariReligiousAfter+
                  (1|religion)+
                  (1|id_Questionaire),
                prior = c(prior(normal(0, 1.5), class = Intercept),
                          prior(normal(0, 0.5), class = b)),
                iter = 5000, warmup = 1000, cores = 4, chains = 4,
                control = list(adapt_delta = 0.95,
                               max_treedepth=13))

print(model3.2)
# Population-Level Effects: 
#                                            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
# Intercept[1]                                -8.26      0.88    -9.85    -6.32 1.00    10461
# Intercept[2]                                 1.28      0.80     0.11     3.15 1.00     8125
# Intercept[3]                                 2.03      0.80     0.87     3.91 1.00     8126
# gender_F0_M1_a                               0.44      0.29    -0.12     1.03 1.00    17604
# familyBariReligiousAfter                     0.16      0.07     0.02     0.30 1.00    15125
# religious_knowledge_scale                    1.23      0.33     0.59     1.88 1.00    15833
# MI_geo_proximity                             0.02      0.04    -0.07     0.11 1.00    17446
# MI_economic_capital                          0.05      0.04    -0.03     0.13 1.00    15379
# MI_human_capital                            -0.00      0.05    -0.10     0.10 1.00    14452
# Kids_a                                       0.05      0.02     0.00     0.10 1.00    14505
# Mothers_kids_a                               0.01      0.01    -0.01     0.04 1.00    15371
# gender_F0_M1_a:familyBariReligiousAfter     -0.12      0.11    -0.32     0.09 1.00    14694
# gender_F0_M1_a:religious_knowledge_scale     0.00      0.33    -0.63     0.65 1.00    17687

path<- (paste0("results/"))
filename <- "Geo_distance_relatives_ord_cum_sex.rds"

saveRDS(model3.2, paste0(path, filename))

#### type of help received models
husbands_new <- read.csv("C:/Users/robert/Dropbox/Github/Intensive extensive kin networks/data/husbands_new.csv", header = T, sep = ",")

husbands_new$percent_rels_in_NW<- as.numeric(husbands_new$percent_rels_in_NW)

husbands_new$religious_knowledge_scale<- scales::rescale(husbands_new$religious_knowledge_scale,to=c(-1,1))



data1 <-  husbands_new %>% filter (Respondent_a=="Wife" | Respondent_a=="Husband")
data2 <-  husbands_new %>% filter (Respondent_a=="Wife")
data3 <-  husbands_new %>% filter (Respondent_a=="Husband")

### Economic help
# model 4 percent relatives who provide economic help
library(tidyverse)
library(brms)
library(readr)


d <- data1[c(41,2,6,8,9,52,51,54,53,25:27)] 
# 
d$percent_rels_econ_help <- as.numeric(d$percent_rels_econ_help)
d$percent_rels_econ_help<- d$percent_rels_econ_help+0.01
### try model
model4 <- brm(percent_rels_econ_help ~ gender_F0_M1_a*familyBariReligiousAfter+
                gender_F0_M1_a*religious_knowledge_scale+
                Kids_a+
                Mothers_kids_a+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital+
                (1|religion)+
                (1|idwife_a), 
              data=d, 
              family = lognormal(),
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95,
                             max_treedepth=13))

print(summary(model4, prob=0.95,priors=TRUE), digits = 6)
# Population-Level Effects: 
#                                           Estimate Est.Error  l-95% CI  u-95% CI     Rhat
# Intercept                                -1.219224  0.621242 -2.374901  0.440235 1.013236
# gender_F0_M1_a                           -0.180161  0.531209 -1.233381  0.890845 1.000988
# familyBariReligiousAfter                 -0.001123  0.062839 -0.126303  0.120752 1.000362
# religious_knowledge_scale                -1.098107  0.378104 -1.836870 -0.331849 1.001817
# Kids_a                                    0.019874  0.020893 -0.020341  0.061159 1.001897
# Mothers_kids_a                           -0.001177  0.010506 -0.021965  0.019403 1.001512
# MI_geo_proximity                          0.038495  0.039870 -0.039140  0.116706 1.003587
# MI_economic_capital                      -0.027808  0.037844 -0.102266  0.045180 1.001908
# MI_human_capital                         -0.072359  0.045582 -0.161079  0.018293 1.000350
# gender_F0_M1_a:familyBariReligiousAfter   0.061267  0.099106 -0.129597  0.255387 1.001972
# gender_F0_M1_a:religious_knowledge_scale  0.382402  0.598279 -0.807939  1.587815 1.001097


path<- (paste0("results/"))
filename <- "Percent_rels_econ_help_beta_sex.rds"

saveRDS(model4, paste0(path, filename))


### Number of relatives economic help
library(tidyverse)
library(brms)
library(readr)

d <- data1[c(2,39,6,8,9,52,51,54,53,25:27)] 
d <- d[complete.cases(d), ] 

d$rels_econ_help<- d$rels_econ_help+0.01
## run as log normal  
model4.1 <- brm(rels_econ_help ~ gender_F0_M1_a*familyBariReligiousAfter+
                  gender_F0_M1_a*religious_knowledge_scale+
                  Kids_a+
                  Mothers_kids_a+
                  MI_geo_proximity+
                  MI_economic_capital+
                  MI_human_capital+
                  (1|religion)+
                  (1|idwife_a), 
                data=d, 
                family = lognormal(),
                prior = c(set_prior("normal(0,2)", class = "b")),
                warmup = 1000, iter = 5000, chains = 4,
                control = list(adapt_delta = 0.95,
                               max_treedepth=13))

print(summary(model4.1, prob=0.95,priors=TRUE), digits = 6)

# Population-Level Effects: 
#                                           Estimate Est.Error  l-95% CI u-95% CI     Rhat
# Intercept                                -0.163583  1.202310 -2.379224 2.536857 1.000616
# gender_F0_M1_a                           -0.083096  0.884973 -1.838938 1.649457 1.000034
# familyBariReligiousAfter                 -0.077925  0.128716 -0.333696 0.175469 1.000033
# religious_knowledge_scale                -0.392439  0.726029 -1.807077 1.043646 1.000065
# Kids_a                                   -0.003886  0.042045 -0.086705 0.077636 1.000189
# Mothers_kids_a                            0.003608  0.021274 -0.038138 0.045524 1.000055
# MI_geo_proximity                          0.037362  0.081120 -0.121376 0.196132 1.000145
# MI_economic_capital                      -0.049619  0.076685 -0.199587 0.099727 1.000040
# MI_human_capital                          0.210135  0.092814  0.029102 0.392723 1.000352
# gender_F0_M1_a:familyBariReligiousAfter   0.196033  0.199484 -0.195838 0.587418 1.000177
# gender_F0_M1_a:religious_knowledge_scale  0.122953  0.996354 -1.845961 2.086494 1.000098


path<- (paste0("results/"))
filename <- "econ_help_rels_sex.rds"

saveRDS(model4.1, paste0(path, filename))
### Number of non-relatives economic help
library(tidyverse)
library(brms)
library(readr)

d <- data1[c(2,40,6,8,9,52,51,54,53,25:27)] 
d <- d[complete.cases(d), ] 

d$non_rels_econ_help<- d$non_rels_econ_help+0.01
## run as log normal  
model4.2 <- brm(non_rels_econ_help ~ gender_F0_M1_a*familyBariReligiousAfter+
                  gender_F0_M1_a*religious_knowledge_scale+
                  Kids_a+
                  Mothers_kids_a+
                  MI_geo_proximity+
                  MI_economic_capital+
                  MI_human_capital+
                  (1|religion)+
                  (1|idwife_a), 
                data=d, 
                family = lognormal(),
                prior = c(set_prior("normal(0,2)", class = "b")),
                warmup = 1000, iter = 5000, chains = 4,
                control = list(adapt_delta = 0.95,
                               max_treedepth=13))

print(summary(model4.2, prob=0.95,priors=TRUE), digits = 6)

# Population-Level Effects: 
#                                           Estimate Est.Error  l-95% CI  u-95% CI     Rhat
# Intercept                                -2.880877  1.015918 -5.194334 -0.954550 1.005048
# gender_F0_M1_a                            1.650331  0.878143 -0.068646  3.370191 1.001185
# familyBariReligiousAfter                 -0.252063  0.125019 -0.493520 -0.005223 1.001066
# religious_knowledge_scale                 1.031374  0.730692 -0.395321  2.495310 1.000142
# Kids_a                                   -0.094810  0.042871 -0.178078 -0.010389 1.000497
# Mothers_kids_a                            0.035860  0.020907 -0.005776  0.077013 1.000430
# MI_geo_proximity                         -0.111767  0.081897 -0.272880  0.051328 1.000171
# MI_economic_capital                       0.164836  0.076588  0.013814  0.312912 1.002187
# MI_human_capital                          0.252506  0.093372  0.068756  0.437205 1.001897
# gender_F0_M1_a:familyBariReligiousAfter   0.137016  0.192828 -0.238553  0.519389 1.000295
# gender_F0_M1_a:religious_knowledge_scale  0.223381  0.988035 -1.724280  2.173109 1.001453


path<- (paste0("results/"))
filename <- "econ_help_non_rels_sex.rds"

saveRDS(model4.2, paste0(path, filename))
###percent relatives who provide childcare/ work help
d <- data1[c(47,2,6,8,9,52,51,54,53,25:27)] 
# 
d$childcare_work_help_rels_percent <- as.numeric(d$childcare_work_help_rels_percent)
d$childcare_work_help_rels_percent<- d$childcare_work_help_rels_percent+0.01
### try model
model5 <- brm(childcare_work_help_rels_percent ~ gender_F0_M1_a*familyBariReligiousAfter+
                gender_F0_M1_a*religious_knowledge_scale+
                Kids_a+
                Mothers_kids_a+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital+
                (1|religion)+
                (1|idwife_a), 
              data=d, 
              family = lognormal(),
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95,
                             max_treedepth=13))

print(summary(model5, prob=0.95,priors=TRUE), digits = 6)
#         Population-Level Effects: 
#                                              Estimate Est.Error  l-95% CI  u-95% CI     Rhat
# Intercept                                -0.611524  0.687401 -2.027759  0.861699 1.000361
# gender_F0_M1_a                           -2.718831  0.571075 -3.814590 -1.603532 1.000272
# familyBariReligiousAfter                  0.028423  0.063164 -0.094028  0.152784 1.000452
# religious_knowledge_scale                -0.620221  0.392549 -1.397059  0.143320 1.000138
# Kids_a                                   -0.004406  0.021880 -0.047173  0.038684 1.000050
# Mothers_kids_a                            0.015020  0.011299 -0.007447  0.037163 1.000071
# MI_geo_proximity                          0.020309  0.046542 -0.070199  0.110692 1.000681
# MI_economic_capital                       0.004554  0.039015 -0.070794  0.081428 1.000090
# MI_human_capital                         -0.251932  0.048212 -0.346904 -0.158569 1.000098
# gender_F0_M1_a:familyBariReligiousAfter  -0.013355  0.109484 -0.227533  0.202101 1.000106
# gender_F0_M1_a:religious_knowledge_scale -1.945415  0.643036 -3.193510 -0.683505 1.000320

path<- (paste0("results/"))
filename <- "Percent_childcare_work_help_sex.rds"

saveRDS(model5, paste0(path, filename))
### Number of relatives childcare/ work help
d <- data1[c(2,45,6,8,9,52,51,54,53,25:27)] 
d <- d[complete.cases(d), ] 

d$childcare_work_help_rels<- d$childcare_work_help_rels+0.01
## run as log normal  
model5.1 <- brm(childcare_work_help_rels ~ gender_F0_M1_a*familyBariReligiousAfter+
                  gender_F0_M1_a*religious_knowledge_scale+
                  Kids_a+
                  Mothers_kids_a+
                  MI_geo_proximity+
                  MI_economic_capital+
                  MI_human_capital+
                  (1|religion)+
                  (1|idwife_a), 
                data=d, 
                family = lognormal(),
                prior = c(set_prior("normal(0,2)", class = "b")),
                warmup = 1000, iter = 5000, chains = 4,
                control = list(adapt_delta = 0.95,
                               max_treedepth=13))

print(summary(model5.1, prob=0.95,priors=TRUE), digits = 6)
# Population-Level Effects: 
#                                           Estimate Est.Error  l-95% CI  u-95% CI     Rhat
# Intercept                                 0.178749  0.846570 -1.409057  1.952378 1.002444
# gender_F0_M1_a                           -2.588367  0.855063 -4.279871 -0.906792 1.000130
# familyBariReligiousAfter                 -0.108288  0.121683 -0.343434  0.133490 1.000730
# religious_knowledge_scale                -0.670322  0.695155 -2.021412  0.705773 1.000351
# Kids_a                                   -0.032383  0.041528 -0.113735  0.048545 1.000325
# Mothers_kids_a                            0.027390  0.019955 -0.011126  0.066769 0.999986
# MI_geo_proximity                         -0.158793  0.079866 -0.317066 -0.003392 1.000029
# MI_economic_capital                       0.055069  0.073227 -0.087294  0.200224 1.000681
# MI_human_capital                         -0.166148  0.089738 -0.342587  0.010121 1.001263
# gender_F0_M1_a:familyBariReligiousAfter   0.090666  0.187820 -0.280340  0.456319 1.000654
# gender_F0_M1_a:religious_knowledge_scale -0.513367  0.963531 -2.412174  1.386512 1.000135

path<- (paste0("results/"))
filename <- "Childcare_work_help_rels_sex.rds"

saveRDS(model5.1, paste0(path, filename))

### Number of non-relatives childcare/ work help


d <- data1[c(2,46,6,8,9,52,51,54,53,25:27)] 
d <- d[complete.cases(d), ] 

d$childcare_work_help_non_rels<-d$childcare_work_help_non_rels+0.01
## run as log normal  
model5.2 <- brm(childcare_work_help_non_rels ~ gender_F0_M1_a*familyBariReligiousAfter+
                  gender_F0_M1_a*religious_knowledge_scale+
                  Kids_a+
                  Mothers_kids_a+
                  MI_geo_proximity+
                  MI_economic_capital+
                  MI_human_capital+
                  (1|religion)+
                  (1|idwife_a), 
                data=d, 
                family = lognormal(),
                prior = c(set_prior("normal(0,2)", class = "b")),
                warmup = 1000, iter = 5000, chains = 4,
                control = list(adapt_delta = 0.95,
                               max_treedepth=13))

print(summary(model5.2, prob=0.95,priors=TRUE), digits = 6)

# Population-Level Effects: 
#                                           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                                   -3.91      0.80    -5.57    -2.29 1.00     6034     6116
# gender_F0_M1_a                               2.46      0.81     0.84     4.06 1.00    10731    10669
# familyBariReligiousAfter                    -0.30      0.11    -0.52    -0.09 1.00    15254    12067
# religious_knowledge_scale                    0.26      0.64    -0.99     1.54 1.00    13822    12124
# Kids_a                                      -0.01      0.04    -0.08     0.06 1.00    15719    12222
# Mothers_kids_a                              -0.01      0.02    -0.05     0.02 1.00    20826    11457
# MI_geo_proximity                            -0.07      0.07    -0.21     0.07 1.00    21999    10930
# MI_economic_capital                          0.05      0.07    -0.08     0.18 1.00    20191    11654
# MI_human_capital                             0.46      0.08     0.30     0.61 1.00    13731    12910
# gender_F0_M1_a:familyBariReligiousAfter      0.22      0.17    -0.11     0.56 1.00    16849    12082
# gender_F0_M1_a:religious_knowledge_scale     1.11      0.92    -0.69     2.91 1.00    10652    10632

path<- (paste0("results/"))
filename <- "Childcare_work_help_non_rels_sex.rds"

saveRDS(model5.2, paste0(path, filename))

###percent relatives who provide emotional support
d <- data1[c(44,2,6,8,9,52,51,54,53,25:27)] 
# 
d$percent_rels_emot_support <- as.numeric(d$percent_rels_emot_support )
d$percent_rels_emot_support <- d$percent_rels_emot_support +0.01
### try model
model6 <- brm(percent_rels_emot_support~ gender_F0_M1_a*familyBariReligiousAfter+
                gender_F0_M1_a*religious_knowledge_scale+
                Kids_a+
                Mothers_kids_a+
                MI_geo_proximity+
                MI_economic_capital+
                MI_human_capital+
                (1|religion)+
                (1|idwife_a), 
              data=d, 
              family = lognormal(),
              prior = c(set_prior("normal(0,2)", class = "b")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95,
                             max_treedepth=13))

print(summary(model6, prob=0.95,priors=TRUE), digits = 6)

# Population-Level Effects: 
#                                            Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS
# Intercept                                -0.036205  0.557499 -1.125639  1.190189 1.036693      115
# gender_F0_M1_a                           -0.716904  0.241249 -1.160229 -0.260120 1.033103      138
# familyBariReligiousAfter                  0.037629  0.024757 -0.011756  0.086508 1.014323    13249
# religious_knowledge_scale                 0.060928  0.156040 -0.247678  0.365885 1.002851     7336
# Kids_a                                   -0.006610  0.008316 -0.022364  0.010060 1.007395      757
# Mothers_kids_a                           -0.001523  0.004155 -0.009461  0.006766 1.006235     1162
# MI_geo_proximity                          0.014957  0.015707 -0.015315  0.046257 1.004031     2953
# MI_economic_capital                      -0.026000  0.014836 -0.055728  0.002739 1.002990    14945
# MI_human_capital                         -0.038834  0.017656 -0.074327 -0.004045 1.017361    15217
# gender_F0_M1_a:familyBariReligiousAfter   0.006673  0.038298 -0.067631  0.083620 1.004117    10863
# gender_F0_M1_a:religious_knowledge_scale -0.490591  0.271298 -0.972829  0.026632 1.032272      135

path<- (paste0("results/"))
filename <- "Percent_emotional_support_sex.rds"

saveRDS(model6, paste0(path, filename))

### Number of relatives who provide emotional support
d <- data1[c(42,2,6,8,9,52,51,54,53,25:27)] 
d <- d[complete.cases(d), ] 

d$emot_support_rels<- d$emot_support_rels +0.01
## run as log normal  
model6.1 <- brm(emot_support_rels~gender_F0_M1_a*familyBariReligiousAfter+
                  gender_F0_M1_a*religious_knowledge_scale+
                  Kids_a+
                  Mothers_kids_a+
                  MI_geo_proximity+
                  MI_economic_capital+
                  MI_human_capital+
                  (1|religion)+
                  (1|idwife_a), 
                data=d, 
                family = lognormal(),
                prior = c(set_prior("normal(0,2)", class = "b")),
                warmup = 1000, iter = 5000, chains = 4,
                control = list(adapt_delta = 0.95,
                               max_treedepth=13))

print(summary(model6.1, prob=0.95,priors=TRUE), digits = 6)
#       Population-Level Effects: 
#                                            Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS
# Intercept                                 1.815547  0.652246  0.469984  3.193406 1.003240     8110
# gender_F0_M1_a                           -0.669886  0.434789 -1.512262  0.180184 1.000054    13360
# familyBariReligiousAfter                  0.237394  0.048967  0.141318  0.333275 1.000322    19572
# religious_knowledge_scale                 0.298506  0.309226 -0.305491  0.901069 1.000149    17510
# Kids_a                                   -0.041215  0.016503 -0.073293 -0.009018 1.000057    19597
# Mothers_kids_a                            0.014810  0.008188 -0.001216  0.030935 1.000096    24262
# MI_geo_proximity                         -0.002971  0.031852 -0.064848  0.059637 0.999992    20754
# MI_economic_capital                       0.023235  0.029848 -0.035139  0.082807 1.000466    21271
# MI_human_capital                         -0.025306  0.035871 -0.094919  0.045146 1.000141    19009
# gender_F0_M1_a:familyBariReligiousAfter  -0.134348  0.074911 -0.280861  0.013580 1.000328    18158
# gender_F0_M1_a:religious_knowledge_scale -0.707217  0.489556 -1.661950  0.254648 1.000123    13254

filename <- "emotional_support_rels_sex.rds"

saveRDS(model6.1, paste0(path, filename))
### Number of non-relatives who provide emotional support
d <- data1[c(43,2,6,8,9,52,51,54,53,25:27)] 
d <- d[complete.cases(d), ] 

d$emot_support_non_rels<-d$emot_support_non_rels +0.01
## run as log normal  
model6.2 <- brm(emot_support_non_rels ~ gender_F0_M1_a*familyBariReligiousAfter+
                  gender_F0_M1_a*religious_knowledge_scale+
                  Kids_a+
                  Mothers_kids_a+
                  MI_geo_proximity+
                  MI_economic_capital+
                  MI_human_capital+
                  (1|religion)+
                  (1|idwife_a), 
                data=d, 
                family = lognormal(),
                prior = c(set_prior("normal(0,2)", class = "b")),
                warmup = 1000, iter = 5000, chains = 4,
                control = list(adapt_delta = 0.95,
                               max_treedepth=13))

print(summary(model6.2, prob=0.95,priors=TRUE), digits = 6)

# Population-Level Effects: 
#                                           Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS
# Intercept                                -4.236664  0.872694 -6.142586 -2.623954 1.000540     7188
# gender_F0_M1_a                            2.238178  0.846819  0.614495  3.888820 0.999951    10658
# familyBariReligiousAfter                 -0.140284  0.123565 -0.379449  0.102303 1.000124    16175
# religious_knowledge_scale                 0.190158  0.674106 -1.145339  1.519086 1.000004    16797
# Kids_a                                    0.018363  0.039927 -0.058570  0.096121 1.000049    16690
# Mothers_kids_a                            0.037478  0.020230 -0.001818  0.077797 1.000284    23425
# MI_geo_proximity                         -0.113573  0.077473 -0.264992  0.038341 1.000072    22856
# MI_economic_capital                       0.187497  0.071767  0.046746  0.328239 1.000161    18806
# MI_human_capital                          0.315566  0.087200  0.145321  0.483654 1.000138    14505
# gender_F0_M1_a:familyBariReligiousAfter   0.220176  0.187155 -0.144832  0.589196 1.000135    15788
# gender_F0_M1_a:religious_knowledge_scale  0.068502  0.952148 -1.758682  1.920228 0.999902    10623

path<- (paste0("results/"))
filename <- "emotional_support_non_rels_sex.rds"

saveRDS(model6.2, paste0(path, filename))


