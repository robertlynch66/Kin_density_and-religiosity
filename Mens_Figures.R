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

### Precent relatives run
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
                      (1|religion), #+
                      #(1|idwife_a), 
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

path<- (paste0("results/"))
filename <- "non_relatives_in_NW_neg_bin_sex.rds"

saveRDS(model2.2, paste0(path, filename))


### plot intx between sex and religiosity for model1 (total NW size)

## make the plots
#######################################FIGURES##################################
#######################################FIGURES##################################
#######################################FIGURES##################################
#######################################FIGURES##################################
#######################################FIGURES##################################
library(tidyverse)
library(brms)
library(readr)
library(scales)
##  make new data frame
husbands_new <- read.csv("C:/Users/robert/Dropbox/Github/Intensive extensive kin networks/data/husbands_new.csv", header = T, sep = ",")

husbands_new$religious_knowledge_scale<- scales::rescale(husbands_new$religious_knowledge_scale,to=c(-1,1))


data1 <-  husbands_new %>% filter (Respondent_a=="Wife" | Respondent_a=="Husband")
data2 <-  husbands_new %>% filter (Respondent_a=="Wife")
data3 <-  husbands_new %>% filter (Respondent_a=="Husband")

d <- data1[c(2,28,6,8,9,52,51,54,25:27)] 
d <- d[complete.cases(d), ] 

d<-d %>% mutate(rank=ntile(d$religious_knowledge_scale,6))
library(ggthemes)

## make the top plot
Sex_seq <- rep(0:1, each=6)
Religiosity_seq <- rep(1:6,2)


#read in  the model
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_men/results/NW_total_lognormal_sex.rds")


attach(d)
newdata1 <- tidyr::crossing(
  religion=mean(religion),
  familyBariReligiousAfter = mean(familyBariReligiousAfter),
  gender_F0_M1_a = c(0,1),
  religious_knowledge_scale=c(-1.0,-0.9,-0.8,-0.7,-0.6,-0.5),
  #religious_knowledge_scale=religious_knowledge_scale,
  MI_geo_proximity=mean(MI_geo_proximity),
  MI_economic_capital=mean(MI_economic_capital),
  MI_human_capital=mean(MI_human_capital),
  Kids_a=mean(Kids_a),
  Mothers_kids_a=mean(Mothers_kids_a)) %>%
  as.data.frame()
detach(d)


mu_summary <-
  fitted(M1, 
         newdata = newdata1, allow_new_levels=TRUE, probs=c(0.05,0.95)) %>%
  as_tibble() %>%
  # let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
  bind_cols(Religiosity_seq) %>% bind_cols(Sex_seq)
# let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
mu_summary

colnames(mu_summary) <- c('Estimate','Error','Q5','Q95','Religiosity','Sex')

mu_summary


Data1 <- d %>% left_join (mu_summary, by =c("rank"="Religiosity","gender_F0_M1_a"="Sex"))

library(plotrix)
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggstance)
library(rstan)
library(tidybayes)
library(emmeans)
library(broom)
library(brms)
library(modelr)
library(forcats)
library(ggdist)


cols <- c("0" = "#000000", "1" = "#0072B2", "2" = "#000000", "3" = "#0072B2") #,

show_col("#000000")
show_col("#0072B2")
# relabel sex fror facet grid
new <- c("Men", "Women")
names(new) <- c(1,0)


plot1<- ggplot(Data1, aes(x=rank, y=Estimate,group=factor(gender_F0_M1_a),
                          fill=factor(gender_F0_M1_a),
                     color=factor(gender_F0_M1_a))) +
  
    geom_ribbon(aes(ymin = Q5, ymax = Q95,alpha=0.7)) +
  
    geom_line() +
  
  geom_jitter(data=Data1,shape=1,size=0.9,width=0.6,aes(group=factor(gender_F0_M1_a),colour=factor(gender_F0_M1_a),
                                                        x = rank, y = NW_total)) +
  
  facet_wrap(~gender_F0_M1_a, ncol = 2, labeller=labeller(gender_F0_M1_a=new))+
  
  
  # scale_color_manual(name=NULL, breaks=c(0,1),values=cols,
  #                    labels=c("Predicted (95% CI)","Observed")) +
  
  scale_color_manual(name = NULL,
                     values = cols,
                     breaks=c(1,0),
                     labels = c("Predicted (95% CI)", "Observed"),
                     guide = guide_legend(override.aes = list(linetype = c(1, 0),
                                                              shape = c(NA, 1),
                                                              color = "grey")))+
  
  scale_fill_manual(name="", breaks=c(0),values=cols,
                      labels=c("Predicted (95% CI)")) +
  scale_x_continuous(name="Religious Knowledge",limits=c(0.5,6.5),breaks=c(1.5,3.5,5.5),
                     labels=c("","","")) +
  scale_y_continuous(name="Total network size",breaks=c(7,8,9,10,11,12,13,14,15),limits=c(7,17),
                     labels=c("7","8","9","10",'11','12','13','14','15')) +
  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=16,face="bold"),
        legend.title = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        legend.text = element_text(size = 12, face = "bold"),
          strip.text.x = element_text(
            size = 12, color = "black", face = "bold"),
          strip.text.y = element_text(
            size = 12, color = "black", face = "bold"),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
 
  guides(alpha="none")+
  guides(fill="none")
plot1


library(ggthemes)

## make the bottom plot
d <- data1[c(2,28,6,8,9,52,51,54,25:27)] 
d <- d[complete.cases(d), ] 
Sex_seq <- c(0,1,0,1,0,1)
Religiosity_seq <- c(-1,-1,0,0,1,1)
#read in  the model
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_men/results/NW_total_lognormal_sex.rds")


attach(d)
newdata2 <- tidyr::crossing(
  religion=mean(religion),
  familyBariReligiousAfter = c(-1,0,1),
  gender_F0_M1_a = c(0,1),
  religious_knowledge_scale=mean(religious_knowledge_scale),
  MI_geo_proximity=mean(MI_geo_proximity),
  MI_economic_capital=mean(MI_economic_capital),
  MI_human_capital=mean(MI_human_capital),
  Kids_a=mean(Kids_a),
  Mothers_kids_a=mean(Mothers_kids_a)) %>%
    as.data.frame()
detach(d)

mu_summary <-
  fitted(M1, 
         newdata = newdata2, allow_new_levels=TRUE, probs=c(0.05,0.95)) %>%
  as_tibble() %>%
  # let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
  bind_cols(Religiosity_seq) %>% bind_cols(Sex_seq)
  # let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
mu_summary

colnames(mu_summary) <- c('Estimate','Error','Q5','Q95','Religiosity','Sex')

mu_summary


Data2 <- d %>% left_join (mu_summary, by =c("familyBariReligiousAfter"="Religiosity","gender_F0_M1_a"="Sex"))

library(plotrix)
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggstance)
library(rstan)
library(tidybayes)
library(emmeans)
library(broom)
library(brms)
library(modelr)
library(forcats)
library(ggdist)
#0072B2
cols <- c("0" = "#000000", "1" = "#0072B2", "2" = "#000000", "3" = "#0072B2")
plot1a <- ggplot() +
 
  geom_point(data=Data2,size=3,aes(group=factor(gender_F0_M1_a),colour=factor(gender_F0_M1_a),
                                                                x = familyBariReligiousAfter, y = Estimate)) +

  geom_errorbar(data=Data2, aes(group=factor(gender_F0_M1_a),x=familyBariReligiousAfter,
                              colour=factor(gender_F0_M1_a),ymin=(Q5),
                              ymax=(Q95)),alpha=0.7, width=0.6,size=1.2) +
  
  

  geom_line(data=Data2,size=1.2,aes(group=factor(gender_F0_M1_a),colour=factor(gender_F0_M1_a),
                         x = familyBariReligiousAfter, y = Estimate))+
  
  geom_jitter(data=Data2,shape=1,size=0.6,width=0.2,aes(group=factor(gender_F0_M1_a),colour=factor(gender_F0_M1_a),
   x = familyBariReligiousAfter, y = NW_total)) +
  
  facet_wrap(~gender_F0_M1_a, ncol = 2, labeller=labeller(gender_F0_M1_a=new))+


  # scale_color_manual(name=NULL, breaks=c(0,1),values=cols,
  #                    labels=c("Predicted (95% CI)","Observed")) +
  
  scale_color_manual(name = NULL,
                     values = cols,
                     breaks=c(1,0),
                     labels = c("Predicted (95% CI)", "Observed"),
                     guide = guide_legend(override.aes = list(linetype = c(1, 0),
                                                              shape = c(NA, 1),
                                                              color = "black")))+

  
  
  
  scale_x_continuous(name="Relative religiosity",limits=c(-1.3,1.3),breaks=c(-1,0,1),
                     labels=c("Low","Medium","High")) +
  scale_y_continuous(name="Total network size",breaks=c(7,8,9,10,11,12,13,14,15),limits=c(4,18),
                     labels=c("7","8","9","10",'11','12','13','14','15')) +
  
theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=16,face="bold"),
        legend.title = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        
        strip.text.x = element_blank(),
        
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  # guides( colour = guide_legend(override.aes = list(linetype=c(1,0)
  #                                                   , shape=c(NA,16))))+
  
  
  guides(alpha="none")


plot1a

library("gridExtra")
library(ggpubr)

m <- ggarrange(plot1, plot1a, 
               labels = c("A", "B"),
               ncol = 1, nrow = 2)
# m1 <-annotate_figure(m,
#                      top = text_grob("Women who are higher in religiosity\n have larger overall social networks", color = "black", face = "bold",
#                                      size = 14))
require(grid)   # for the textGrob() function

figure <- ggarrange(plot1 + rremove("ylab"), plot1a + rremove("ylab"),
                    labels = NULL,
                    ncol = 1, nrow = 2,
                    common.legend = TRUE, legend = "bottom",
                    align = "hv", 
                    font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))

figure1 <- annotate_figure(figure, left = textGrob("Total Network Size", rot = 90, vjust = 1, gp = gpar(cex = 1.6)))


ggsave(figure1, filename = "figures/Figure 1 (Total NW size).png", width = 11, height = 6, device = "png", dpi = 600,units = "in")

#### Do percent relatives in NW ### Figure 2
## just make the new figs with newdata1 and newdata2

d <- data1[c(2,38,6,8,9,52,51,54,25:27)] 
d$percent_rels_in_NW<-as.numeric(d$percent_rels_in_NW)
d <- d[complete.cases(d), ] 



d<-d %>% mutate(rank=ntile(d$religious_knowledge_scale,6))
library(ggthemes)

## make the top plot
Sex_seq <- rep(0:1, each=6)
Religiosity_seq <- rep(1:6,2)


M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_men/results/percent_relatives_in_NW_lognormal_sex.rds")

mu_summary <-
  fitted(M1, 
         newdata = newdata1, allow_new_levels=TRUE, probs=c(0.05,0.95)) %>%
  as_tibble() %>%
  # let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
  bind_cols(Religiosity_seq) %>% bind_cols(Sex_seq)
# let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
mu_summary

colnames(mu_summary) <- c('Estimate','Error','Q5','Q95','Religiosity','Sex')

mu_summary


Data1 <- d %>% left_join (mu_summary, by =c("rank"="Religiosity","gender_F0_M1_a"="Sex"))

library(plotrix)
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggstance)
library(rstan)
library(tidybayes)
library(emmeans)
library(broom)
library(brms)
library(modelr)
library(forcats)
library(ggdist)


cols <- c("0" = "#000000", "1" = "#0072B2", "2" = "#000000", "3" = "#0072B2") #,

# relabel sex for facet grid
new <- c("Men", "Women")
names(new) <- c(1,0)



plot2<- ggplot(Data1, aes(x=rank, y=Estimate,group=factor(gender_F0_M1_a),
                          fill=factor(gender_F0_M1_a),
                          color=factor(gender_F0_M1_a))) +
  
  geom_ribbon(aes(ymin = Q5, ymax = Q95,alpha=0.7)) +
  
  geom_line() +
  
  geom_jitter(data=Data1,shape=1,size=0.9,width=0.6,aes(group=factor(gender_F0_M1_a),colour=factor(gender_F0_M1_a),
                                                        x = rank, y = percent_rels_in_NW)) +
  
  facet_wrap(~gender_F0_M1_a, ncol = 2, labeller=labeller(gender_F0_M1_a=new))+
  
  
  # scale_color_manual(name=NULL, breaks=c(0,1),values=cols,
  #                    labels=c("Predicted (95% CI)","Observed")) +
  
  scale_color_manual(name = NULL,
                     values = cols,
                     breaks=c(1,0),
                     labels = c("Predicted (95% CI)", "Observed"),
                     guide = guide_legend(override.aes = list(linetype = c(1, 0),
                                                              shape = c(NA, 1),
                                                              color = "grey")))+
  
  scale_fill_manual(name="", breaks=c(0),values=cols,
                    labels=c("Predicted (95% CI)")) +
  scale_x_continuous(name="Religious Knowledge",limits=c(0.5,6.5),breaks=c(1.5,3.5,5.5),
                     labels=c("","","")) +
  scale_y_continuous(name="Percentage of Relatives in Network",breaks=c(0.5,0.6,0.7,0.8,0.9),limits=c(0.45,1.01),
                     labels=c("50","60%",'70%','80%','90%')) +
  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=16,face="bold"),
        legend.title = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        legend.text = element_text(size = 12, face = "bold"),
        strip.text.x = element_text(
          size = 12, color = "black", face = "bold"),
        strip.text.y = element_text(
          size = 12, color = "black", face = "bold"),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  
  guides(alpha="none")+
  guides(fill="none")
plot2


### bottom plot
## make the bottom plot

Sex_seq <- c(0,1,0,1,0,1)
Religiosity_seq <- c(-1,-1,0,0,1,1)

mu_summary <-
  fitted(M1, 
         newdata = newdata2, allow_new_levels=TRUE, probs=c(0.05,0.95)) %>%
  as_tibble() %>%
  # let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
  bind_cols(Religiosity_seq) %>% bind_cols(Sex_seq)
# let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
mu_summary

colnames(mu_summary) <- c('Estimate','Error','Q5','Q95','Religiosity','Sex')

mu_summary


Data2 <- d %>% left_join (mu_summary, by =c("familyBariReligiousAfter"="Religiosity","gender_F0_M1_a"="Sex"))

library(plotrix)
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggstance)
library(rstan)
library(tidybayes)
library(emmeans)
library(broom)
library(brms)
library(modelr)
library(forcats)
library(ggdist)
#0072B2
cols <- c("0" = "#000000", "1" = "#0072B2", "2" = "#000000", "3" = "#0072B2")
plot2a <- ggplot() +
  
  geom_point(data=Data2,size=3,aes(group=factor(gender_F0_M1_a),colour=factor(gender_F0_M1_a),
                                   x = familyBariReligiousAfter, y = Estimate)) +
  
  geom_errorbar(data=Data2, aes(group=factor(gender_F0_M1_a),x=familyBariReligiousAfter,
                                colour=factor(gender_F0_M1_a),ymin=(Q5),
                                ymax=(Q95)),alpha=0.7, width=0.6,size=1.2) +
  
  
  
  geom_line(data=Data2,size=1.2,aes(group=factor(gender_F0_M1_a),colour=factor(gender_F0_M1_a),
                                    x = familyBariReligiousAfter, y = Estimate))+
  
  geom_jitter(data=Data2,shape=1,size=0.6,width=0.2,aes(group=factor(gender_F0_M1_a),colour=factor(gender_F0_M1_a),
                                                        x = familyBariReligiousAfter, y = percent_rels_in_NW)) +
  
  facet_wrap(~gender_F0_M1_a, ncol = 2, labeller=labeller(gender_F0_M1_a=new))+
  
  
  # scale_color_manual(name=NULL, breaks=c(0,1),values=cols,
  #                    labels=c("Predicted (95% CI)","Observed")) +
  
  scale_color_manual(name = NULL,
                     values = cols,
                     breaks=c(1,0),
                     labels = c("Predicted (95% CI)", "Observed"),
                     guide = guide_legend(override.aes = list(linetype = c(1, 0),
                                                              shape = c(NA, 1),
                                                              color = "black")))+
  
  
  
  
  scale_x_continuous(name="Relative religiosity",limits=c(-1.3,1.3),breaks=c(-1,0,1),
                     labels=c("Low","Medium","High")) +
  scale_y_continuous(name="Percentage of Relatives in Network",breaks=c(0.5,0.6,0.7,0.8,0.9),limits=c(0.45,1.01),
                     labels=c("50%","60%",'70%','80%','90%')) +
  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=16,face="bold"),
        legend.title = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        
        strip.text.x = element_blank(),
        
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  # guides( colour = guide_legend(override.aes = list(linetype=c(1,0)
  #                                                   , shape=c(NA,16))))+
  
  
  guides(alpha="none")
plot2a

require(grid)   # for the textGrob() function

figure <- ggarrange(plot2 + rremove("ylab"), plot2a + rremove("ylab"),
                    labels = NULL,
                    ncol = 1, nrow = 2,
                    common.legend = TRUE, legend = "bottom",
                    align = "hv", 
                    font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))

figure2 <- annotate_figure(figure, left = textGrob("Percentage of Relatives in Network", rot = 90, vjust = 1, gp = gpar(cex = 1.6)))


ggsave(figure2, filename = "figures/Figure 2 (Percent Relative in NW).png", width = 11, height = 6, device = "png", dpi = 600,units = "in")


####  Number of relatives in NW ### Figure 3

d <- data1[c(2,37,6,8,9,52,51,54,25:27)] 

d <- d[complete.cases(d), ] 



d<-d %>% mutate(rank=ntile(d$religious_knowledge_scale,6))
library(ggthemes)

## make the top plot
Sex_seq <- rep(0:1, each=6)
Religiosity_seq <- rep(1:6,2)


M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_men/results/relatives_in_NW_lognormal_sex.rds")

mu_summary <-
  fitted(M1, 
         newdata = newdata1, allow_new_levels=TRUE, probs=c(0.05,0.95)) %>%
  as_tibble() %>%
  # let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
  bind_cols(Religiosity_seq) %>% bind_cols(Sex_seq)
# let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
mu_summary

colnames(mu_summary) <- c('Estimate','Error','Q5','Q95','Religiosity','Sex')

mu_summary


Data1 <- d %>% left_join (mu_summary, by =c("rank"="Religiosity","gender_F0_M1_a"="Sex"))

cols <- c("0" = "#000000", "1" = "#0072B2", "2" = "#000000", "3" = "#0072B2") #,

# relabel sex for facet grid
new <- c("Men", "Women")
names(new) <- c(1,0)



plot3<- ggplot(Data1, aes(x=rank, y=Estimate,group=factor(gender_F0_M1_a),
                          fill=factor(gender_F0_M1_a),
                          color=factor(gender_F0_M1_a))) +
  
  geom_ribbon(aes(ymin = Q5, ymax = Q95,alpha=0.7)) +
  
  geom_line() +
  
  geom_jitter(data=Data1,shape=1,size=0.9,width=0.6,aes(group=factor(gender_F0_M1_a),colour=factor(gender_F0_M1_a),
                                                        x = rank, y = rels_in_NW)) +
  
  facet_wrap(~gender_F0_M1_a, ncol = 2, labeller=labeller(gender_F0_M1_a=new))+
  
  
  # scale_color_manual(name=NULL, breaks=c(0,1),values=cols,
  #                    labels=c("Predicted (95% CI)","Observed")) +
  
  scale_color_manual(name = NULL,
                     values = cols,
                     breaks=c(1,0),
                     labels = c("Predicted (95% CI)", "Observed"),
                     guide = guide_legend(override.aes = list(linetype = c(1, 0),
                                                              shape = c(NA, 1),
                                                              color = "grey")))+
  
  scale_fill_manual(name="", breaks=c(0),values=cols,
                    labels=c("Predicted (95% CI)")) +
  scale_x_continuous(name="Religious Knowledge",limits=c(0.5,6.5),breaks=c(1.5,3.5,5.5),
                     labels=c("","","")) +
  scale_y_continuous(name="Number of Relatives in Network",breaks=c(5,6,7,8,9,10,11),limits=c(5,11.7),
                     labels=c("5","6",'7','8','9','10','11')) +
  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=16,face="bold"),
        legend.title = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        legend.text = element_text(size = 12, face = "bold"),
        strip.text.x = element_text(
          size = 12, color = "black", face = "bold"),
        strip.text.y = element_text(
          size = 12, color = "black", face = "bold"),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  
  guides(alpha="none")+
  guides(fill="none")
plot3


### bottom plot
## make the bottom plot

Sex_seq <- c(0,1,0,1,0,1)
Religiosity_seq <- c(-1,-1,0,0,1,1)

mu_summary <-
  fitted(M1, 
         newdata = newdata2, allow_new_levels=TRUE, probs=c(0.05,0.95)) %>%
  as_tibble() %>%
  # let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
  bind_cols(Religiosity_seq) %>% bind_cols(Sex_seq)
# let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
mu_summary

colnames(mu_summary) <- c('Estimate','Error','Q5','Q95','Religiosity','Sex')

mu_summary


Data2 <- d %>% left_join (mu_summary, by =c("familyBariReligiousAfter"="Religiosity","gender_F0_M1_a"="Sex"))

#0072B2
cols <- c("0" = "#000000", "1" = "#0072B2", "2" = "#000000", "3" = "#0072B2")
plot3a <- ggplot() +
  
  geom_point(data=Data2,size=3,aes(group=factor(gender_F0_M1_a),colour=factor(gender_F0_M1_a),
                                   x = familyBariReligiousAfter, y = Estimate)) +
  
  geom_errorbar(data=Data2, aes(group=factor(gender_F0_M1_a),x=familyBariReligiousAfter,
                                colour=factor(gender_F0_M1_a),ymin=(Q5),
                                ymax=(Q95)),alpha=0.7, width=0.6,size=1.2) +
  
  
  
  geom_line(data=Data2,size=1.2,aes(group=factor(gender_F0_M1_a),colour=factor(gender_F0_M1_a),
                                    x = familyBariReligiousAfter, y = Estimate))+
  
  geom_jitter(data=Data2,shape=1,size=0.6,width=0.2,aes(group=factor(gender_F0_M1_a),colour=factor(gender_F0_M1_a),
                                                        x = familyBariReligiousAfter, y = rels_in_NW)) +
  
  facet_wrap(~gender_F0_M1_a, ncol = 2, labeller=labeller(gender_F0_M1_a=new))+
  
  
  # scale_color_manual(name=NULL, breaks=c(0,1),values=cols,
  #                    labels=c("Predicted (95% CI)","Observed")) +
  
  scale_color_manual(name = NULL,
                     values = cols,
                     breaks=c(1,0),
                     labels = c("Predicted (95% CI)", "Observed"),
                     guide = guide_legend(override.aes = list(linetype = c(1, 0),
                                                              shape = c(NA, 1),
                                                              color = "black")))+
  
  
  
  
  scale_x_continuous(name="Relative religiosity",limits=c(-1.3,1.3),breaks=c(-1,0,1),
                     labels=c("Low","Medium","High")) +
  scale_y_continuous(name="Number of Relatives in Network",breaks=c(5,6,7,8,9,10,11,12),limits=c(5.0,12.7),
                     labels=c("5","6",'7','8','9','10','11','12')) +
  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=16,face="bold"),
        legend.title = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        
        strip.text.x = element_blank(),
        
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  # guides( colour = guide_legend(override.aes = list(linetype=c(1,0)
  #                                                   , shape=c(NA,16))))+
  
  
  guides(alpha="none")
plot3a

require(grid)   # for the textGrob() function

figure <- ggarrange(plot3 + rremove("ylab"), plot3a + rremove("ylab"),
                    labels = NULL,
                    ncol = 1, nrow = 2,
                    common.legend = TRUE, legend = "bottom",
                    align = "hv", 
                    font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))

figure3 <- annotate_figure(figure, left = textGrob("Number of Relatives in Network", rot = 90, vjust = 1, gp = gpar(cex = 1.6)))


ggsave(figure3, filename = "figures/Figure 3 (Relatives in NW).png", width = 11, height = 6, device = "png", dpi = 600,units = "in")


#### Non relatives in network Figure 4
####  Number of relatives in NW ### Figure 3

d <- data1[c(2,29,6,8,9,52,51,54,25:27)] 

d <- d[complete.cases(d), ] 

d<-d %>% mutate(rank=ntile(d$religious_knowledge_scale,6))
library(ggthemes)

## make the top plot
Sex_seq <- rep(0:1, each=6)
Religiosity_seq <- rep(1:6,2)


M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_men/results/non_relatives_in_NW_neg_bin_sex.rds")

mu_summary <-fitted(M1, 
         newdata = newdata1, allow_new_levels=TRUE, probs=c(0.05,0.95)) %>%
  as_tibble() %>%
  # let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
  bind_cols(Religiosity_seq) %>% bind_cols(Sex_seq)
# let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
mu_summary

colnames(mu_summary) <- c('Estimate','Error','Q5','Q95','Religiosity','Sex')

mu_summary


Data1 <- d %>% left_join (mu_summary, by =c("rank"="Religiosity","gender_F0_M1_a"="Sex"))

cols <- c("0" = "#000000", "1" = "#0072B2", "2" = "#000000", "3" = "#0072B2") #,

# relabel sex for facet grid
new <- c("Men", "Women")
names(new) <- c(1,0)



plot4<- ggplot(Data1, aes(x=rank, y=Estimate,group=factor(gender_F0_M1_a),
                          fill=factor(gender_F0_M1_a),
                          color=factor(gender_F0_M1_a))) +
  
  geom_ribbon(aes(ymin = Q5, ymax = Q95,alpha=0.7)) +
  
  geom_line() +
  
  geom_jitter(data=Data1,shape=1,size=0.9,width=0.6,aes(group=factor(gender_F0_M1_a),colour=factor(gender_F0_M1_a),
                                                        x = rank, y = non_rels)) +
  
  facet_wrap(~gender_F0_M1_a, ncol = 2, labeller=labeller(gender_F0_M1_a=new))+
  
  
  # scale_color_manual(name=NULL, breaks=c(0,1),values=cols,
  #                    labels=c("Predicted (95% CI)","Observed")) +
  
  scale_color_manual(name = NULL,
                     values = cols,
                     breaks=c(1,0),
                     labels = c("Predicted (95% CI)", "Observed"),
                     guide = guide_legend(override.aes = list(linetype = c(1, 0),
                                                              shape = c(NA, 1),
                                                              color = "grey")))+
  
  scale_fill_manual(name="", breaks=c(0),values=cols,
                    labels=c("Predicted (95% CI)")) +
  scale_x_continuous(name="Religious Knowledge",limits=c(0.5,6.5),breaks=c(1.5,3.5,5.5),
                     labels=c("","","")) +
  scale_y_continuous(name="Number of Non-relatives in Network",breaks=c(1,3,5,7,9),limits=c(0,10),
                     labels=c("1","3",'5','7','9')) +
  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=16,face="bold"),
        legend.title = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        legend.text = element_text(size = 12, face = "bold"),
        strip.text.x = element_text(
          size = 12, color = "black", face = "bold"),
        strip.text.y = element_text(
          size = 12, color = "black", face = "bold"),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  
  guides(alpha="none")+
  guides(fill="none")
plot4


### bottom plot

Sex_seq <- c(0,1,0,1,0,1)
Religiosity_seq <- c(-1,-1,0,0,1,1)

mu_summary <-
  fitted(M1, 
         newdata = newdata2, allow_new_levels=TRUE, probs=c(0.05,0.95)) %>%
  as_tibble() %>%
  # let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
  bind_cols(Religiosity_seq) %>% bind_cols(Sex_seq)
# let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
mu_summary

colnames(mu_summary) <- c('Estimate','Error','Q5','Q95','Religiosity','Sex')

mu_summary


Data2 <- d %>% left_join (mu_summary, by =c("familyBariReligiousAfter"="Religiosity","gender_F0_M1_a"="Sex"))

#0072B2
cols <- c("0" = "#000000", "1" = "#0072B2", "2" = "#000000", "3" = "#0072B2")
plot4a <- ggplot() +
  
  geom_point(data=Data2,size=3,aes(group=factor(gender_F0_M1_a),colour=factor(gender_F0_M1_a),
                                   x = familyBariReligiousAfter, y = Estimate)) +
  
  geom_errorbar(data=Data2, aes(group=factor(gender_F0_M1_a),x=familyBariReligiousAfter,
                                colour=factor(gender_F0_M1_a),ymin=(Q5),
                                ymax=(Q95)),alpha=0.7, width=0.6,size=1.2) +
  
  
  
  geom_line(data=Data2,size=1.2,aes(group=factor(gender_F0_M1_a),colour=factor(gender_F0_M1_a),
                                    x = familyBariReligiousAfter, y = Estimate))+
  
  geom_jitter(data=Data2,shape=1,size=0.6,width=0.2,aes(group=factor(gender_F0_M1_a),colour=factor(gender_F0_M1_a),
                                                        x = familyBariReligiousAfter, y = non_rels)) +
  
  facet_wrap(~gender_F0_M1_a, ncol = 2, labeller=labeller(gender_F0_M1_a=new))+
  
  
  # scale_color_manual(name=NULL, breaks=c(0,1),values=cols,
  #                    labels=c("Predicted (95% CI)","Observed")) +
  
  scale_color_manual(name = NULL,
                     values = cols,
                     breaks=c(1,0),
                     labels = c("Predicted (95% CI)", "Observed"),
                     guide = guide_legend(override.aes = list(linetype = c(1, 0),
                                                              shape = c(NA, 1),
                                                              color = "black")))+
  
  
  
  
  scale_x_continuous(name="Relative religiosity",limits=c(-1.3,1.3),breaks=c(-1,0,1),
                     labels=c("Low","Medium","High")) +
  scale_y_continuous(name="Number of Relatives in Network",breaks=c(1,3,5,7,9),limits=c(0,10),
                     labels=c("1","3",'5','7','9')) +
  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=16,face="bold"),
        legend.title = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        
        strip.text.x = element_blank(),
        
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  # guides( colour = guide_legend(override.aes = list(linetype=c(1,0)
  #                                                   , shape=c(NA,16))))+
  
  
  guides(alpha="none")
plot4a

require(grid)   # for the textGrob() function

figure <- ggarrange(plot4 + rremove("ylab"), plot4a + rremove("ylab"),
                    labels = NULL,
                    ncol = 1, nrow = 2,
                    common.legend = TRUE, legend = "bottom",
                    align = "hv", 
                    font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))

figure4 <- annotate_figure(figure, left = textGrob("Number of Non-relatives in Network", rot = 90, vjust = 1, gp = gpar(cex = 1.6)))


ggsave(figure4, filename = "figures/Figure 4 (Non-relatives in NW).png", width = 11, height = 6, device = "png", dpi = 600,units = "in")



#### try to make interactive figures in the scracth pad -- try reducing the CI's to 89% or making two
# CI's e.g. 50% and 80 and 89