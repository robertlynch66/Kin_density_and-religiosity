library(tidyverse)
library(brms)
library(readr)

husbands_new <- read.csv("C:/Users/robert/Dropbox/Github/Intensive extensive kin networks/data/husbands_new.csv", header = T, sep = ",")

husbands_new$percent_rels_in_NW<- as.numeric(husbands_new$percent_rels_in_NW)

husbands_new$religious_knowledge_scale<- scales::rescale(husbands_new$religious_knowledge_scale,to=c(-1,1))

## make sure DV's for husbands are okay
# husbands_new$rels_in_NW <- husbands_new$rels_in_NW +0.001

data1 <-  husbands_new %>% filter (Respondent_a=="Wife" | Respondent_a=="Husband")
data2 <-  husbands_new %>% filter (Respondent_a=="Wife")
data3 <-  husbands_new %>% filter (Respondent_a=="Husband")


### Sex differences only
# get model (Model 1)
## total NW size
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_men/results/model_intx_both_sexes/NW_total_lognormal_sex.rds")

# percentage relatives in NW
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_men/results/model_intx_both_sexes/percent_relatives_in_NW_lognormal_sex.rds")

# number of relatives in NW
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_men/results/model_intx_both_sexes/relatives_in_NW_lognormal_sex.rds")

# Number of non-relatives in NW
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_men/results/model_intx_both_sexes/non_relatives_in_NW_neg_bin_sex.rds")


# familyBariReligiousAfter                     0.49      0.22     0.07     0.92 1.00    10444     8478
#MI_human_capital                             0.61      0.15     0.31     0.91 1.00     6535     9421
Sex_seq <- c("female","male")
religious_seq <- c("low","medium","high","low","medium","high")
mi_seq <- c("low","high","low","high")
# new df
attach(data1)
data <- tidyr::crossing(gender_F0_M1_a = c(0,1),
                        Kids_a=mean(Kids_a, na.rm=T),
                        Mothers_kids_a=mean(Mothers_kids_a, na.rm=T),
                        religion = mean(religion,na.rm=T),
                        familyBariReligiousAfter=mean(familyBariReligiousAfter),
                        religious_knowledge_scale=mean(religious_knowledge_scale),
                        MI_geo_proximity=mean(MI_geo_proximity,na.rm=T),
                        MI_economic_capital=mean(MI_economic_capital,na.rm=T),
                        MI_human_capital=mean(MI_human_capital,na.rm=T)) %>%
  as.data.frame()

detach(data1)

summary <- fitted(M1, newdata = data, allow_new_levels=TRUE,probs = c(0.11, 0.89)) %>%
  as_tibble() %>% bind_cols(Sex_seq)

%>% bind_cols(mi_seq)

names(summary) <- c("estimate","error","5CI","95CI","sex")

summary
## geo proximity below

j1 <- summary %>% select (1:4,17,18)
j1$location <- 1
names(j1) <- c("estimate","error","5CI","95CI","sex","edu","location")

j2 <- summary %>% select (5:8,17,18)
j2$location <- 2
names(j2) <- c("estimate","error","5CI","95CI","sex","edu","location")

j3 <- summary %>% select (9:12,17,18)
j3$location <- 3
names(j3) <- c("estimate","error","5CI","95CI","sex","edu","location")

j4 <- summary %>% select (13:16,17,18)
j4$location <- 4
names(j4) <- c("estimate","error","5CI","95CI","sex","edu","location")

summary <- rbind(j1,j2,j3,j4)


summary

### Geographic distances
# distance from non-relatives
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_men/results/Geo_distance_non_relatives_ord_cum_sex.rds")
# distance from relatives
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_men/results/Geo_distance_relatives_ord_cum_sex.rds")


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
#####################################################################################
### religious differences
# get model (Model 1)
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_men/results/model_intx_both_sexes/NW_total_lognormal_sex.rds")

Sex_seq <- c("female","male","female","male","female","male","female","male")
RR_seq <- c("low","low","high","high","low","low","high","high")
Rk_seq <- c("low","high","low","high","low","high","low","high")
# new df
attach(data1)
data <- tidyr::crossing(gender_F0_M1_a = c(0,1),
                   Kids_a=mean(Kids_a, na.rm=T),
                   Mothers_kids_a=mean(Mothers_kids_a, na.rm=T),
                   religion = mean(religion,na.rm=T),
                   familyBariReligiousAfter=c(-1,1),
                   religious_knowledge_scale=c(-1,1),
                   MI_geo_proximity=mean(MI_geo_proximity,na.rm=T),
                   MI_economic_capital=mean(MI_economic_capital,na.rm=T),
                   MI_human_capital=mean(MI_human_capital,na.rm=T)) %>%
  as.data.frame()

detach(data1)

summary <- fitted(M1, newdata = data, allow_new_levels=TRUE,probs = c(0.11, 0.89)) %>%
  as_tibble() %>% bind_cols(Sex_seq) %>% bind_cols(RR_seq) %>% bind_cols(Rk_seq)


colnames(summary) <- c('Estimate','Error','Q5','Q95','Sex','Religiosity',"Religious knowledge")

summary

### table 2
# group religious knowledge scale


data3<-data3 %>% mutate(rank=ntile(data3$religious_knowledge_scale,3))

#women
aggregate(childcare_work_help_rels_percent~familyBariReligiousAfter,data=data2, mean)
#men
aggregate(childcare_work_help_rels_percent~rank,data=data3, mean)


religious_knowledge_scale
familyBariReligiousAfter
NW_total
non_rels
rels_in_NW
percent_rels_in_NW
# "pat_rels"                           
# [32] "mat_rels"                           
# [33] "in_laws"
# "geo_distance_non_rels"              
# [36] "geo_distance_rels"                  
# [37] "rels_in_NW"                         
# [38] "percent_rels_in_NW"                 
# [39] "rels_econ_help"                     
# [40] "non_rels_econ_help"                 
# [41] "percent_rels_econ_help"             
# [42] "emot_support_rels"                  
# [43] "emot_support_non_rels"              
# [44] "percent_rels_emot_support"          
# [45] "childcare_work_help_rels"           
# [46] "childcare_work_help_non_rels"       
# [47] "childcare_work_help_rels_percent" 

duplicated(HusbandNW$id)
HusbandNW$id[duplicated(HusbandNW$id)]

duplicated(WifeNW$id)
WifeNW$id[duplicated(WifeNW$id)]
