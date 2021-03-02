#### Figure 1 - facet plot 
# get data
library(tidyr)
library(brms)
library(readr)
library(magrittr)
library(dplyr)
library(purrr)
library(forcats)
library(modelr)
library(ggdist)
library(tidybayes)
library(ggplot2)
library(cowplot)
library(rstan)
library(ggrepel)
library(RColorBrewer)
library(gganimate)

theme_set(theme_tidybayes() + panel_border())

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


library(tidyverse)
library(brms)
library(readr)
#####READ in and filter newdata
setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh")
newdata <- read_csv("newdata.csv")
options(scipen=999)
# remove duplicated women
newdata <- newdata %>% distinct(idwife, .keep_all = TRUE)
# 1) center and scale variables for easier interpretation of parameter estimates
newdata$religious_knowledge_scale <-  newdata$religious_knowledge_scale-mean(newdata$religious_knowledge_scale, na.rm=T)
newdata$hh_total  <- newdata$hh_total-mean(newdata$hh_total, na.rm=T)  
newdata$kids_in_hh  <- newdata$kids_in_hh-mean(newdata$kids_in_hh, na.rm=T)

# [1] "idwife"                      "couple_id"                   "idhusband"                  
# [4] "sex"                         "age_wife"                    "age_h"                      
# [7] "MI_geo_proximity"            "MI_economic_capital"         "MI_human_capital"           
# [10] "NW_total"                    "non_rels"                    "parents_kids"               
# [13] "pat_rels"                    "mat_rels"                    "in_laws"                    
# [16] "far_rels"                    "geo_distance_non_rels"       "geo_distance_rels"          
# [19] "rels_in_NW"                  "rels_in_NW_h"                "percent_rels_in_NW"         
# [22] "percent_rels_in_NW_h"        "rels_econ_help"              "rels_econ_help_h"           
# [25] "non_rels_econ_help"          "non_rels_econ_help_h"        "percent_rels_econ_help"     
# [28] "percent_rels_econ_help_h"    "emot_support_rels"           "emot_support_rels_h"        
# [31] "emot_support_non_rels"       "emot_support_non_rels_h"     "percent_rels_emot_support"  
# [34] "percent_rels_emot_support_h" "childcare_help_rels"         "childcare_help_non_rels"    
# [37] "childcare_help_rels_percent" "overall_help_rels"           "overall_help_rels_h"        
# [40] "overall_help_non_rels"       "overall_help_non_rels_h"     "percent_overall_help_rels"  
# [43] "percent_overall_help_rels_h" "religion"                    "religious_knowledge_scale"  
# [46] "hh_total"                    "kids_in_hh"                  "familyBariReligiousBefore"  
# [49] "familyBariReligiousAfter"    "religious_change" 

d <- newdata[c(10,11,47,4,5,7,8,9,44,45,49)] 

d <- d[complete.cases(d), ] 

# get model
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/NW_total_lognormal.rds")

get_variables(M1)
# #] "b_Intercept"                 "b_kids_in_hh"                "b_age_wife"                 
# [4] "b_religion"                  "b_familyBariReligiousAfter"  "b_religious_knowledge_scale"
# [7] "b_MI_geo_proximity"          "b_MI_economic_capital"       "b_MI_human_capital"  

# see samples and chains
M2 %>% spread_draws(b_familyBariReligiousAfter) %>% head(10)

# If we want the mean and 95% quantile interval of the variables,
M1 %>%
  spread_draws(b_familyBariReligiousAfter) %>%
  mean_qi(b_familyBariReligiousAfter)

d1 <- M1 %>%
  gather_draws(b_Intercept,b_familyBariReligiousAfter,b_religious_knowledge_scale,b_religion,
               b_kids_in_hh,b_age_wife,b_MI_geo_proximity,b_MI_economic_capital,b_MI_human_capital)

#.variable   .value



# posterior plots


d1 %>%
  spread_draws(.variable) %>%
  #mutate(condition_mean = b_Intercept + b_familyBariReligiousAfter) %>%
  ggplot(aes(y = .variable, x = .valueClassTest(), fill = stat(abs(x) < .8))) +
  stat_halfeye() +
  geom_vline(xintercept = c(-.8, .8), linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue"))


M1 %>%
  gather_draws(b_Intercept,b_familyBariReligiousAfter,b_religious_knowledge_scale,b_religion,
               b_kids_in_hh,b_age_wife,b_MI_geo_proximity,b_MI_economic_capital,b_MI_human_capital) %>%
  
  spread_draws(.variable)
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
#Posterior distributions
library("bayesplot")

#run models and make vectors
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/NW_total_lognormal.rds")

# get religiosity for total network size
get_variables(M1)
posterior1 <- as.array(M1)
dimnames(posterior1)

dim(posterior1)
posterior1 <- posterior1[, , 5]

# get religiosity for number of non-relatives in NW
M2 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/non_relatives_in_NW_neg_bin.rds")
get_variables(M2)
posterior2 <- as.array(M2)
dimnames(posterior2)
dim(posterior2)
posterior2 <- posterior2[, , 5]

# get religiosity for number of relatives in NW
M3 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/relatives_in_NW_lognormal.rds")
get_variables(M3)
posterior3 <- as.array(M3)
dimnames(posterior3)
dim(posterior3)
posterior3 <- posterior3[, , 5]

# get religiosity for percent relatives in NW
M4 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/percent_relatives_in_NW_lognormal.rds")
get_variables(M4)
posterior4 <- as.array(M4)
dimnames(posterior4)
dim(posterior4)
posterior4 <- posterior4[, , 5]

# get mean geo distance  non rels
M5 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Geo_distance_non_relatives_lognormal.rds")
get_variables(M5)
posterior5 <- as.array(M5)
dimnames(posterior5)
dim(posterior5)
posterior5 <- posterior5[, , 7]

# get mean geo distance rels
M6 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Geo_distance_relatives_lognormal.rds")
get_variables(M6)
posterior6 <- as.array(M6)
dimnames(posterior6)
dim(posterior6)
posterior6 <- posterior6[, , 6]

# get non-relatives who give financial support
M7 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Non_rels_econ_help_neg_binom.rds")
get_variables(M7)
posterior7 <- as.array(M7)
dimnames(posterior7)
dim(posterior7)
posterior7 <- posterior7[, , 6]

# Relatives who provide financial support
M8 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/rels_econ_help_poisson.rds")
get_variables(M8)
posterior8 <- as.array(M8)
dimnames(posterior8)
dim(posterior8)
posterior8 <- posterior8[, , 6]

# Percent relatives who provide financial support
M9 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Percent_rels_econ_help_beta.rds")
get_variables(M9)
posterior9 <- as.array(M9)
dimnames(posterior9)
dim(posterior9)
posterior9 <- posterior9[, , 6]

# Non-relatives who provide emotional support
M10 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Non_rels_emot_support_neg_binom.rds")
get_variables(M10)
posterior10 <- as.array(M10)
dimnames(posterior10)
dim(posterior10)
posterior10 <- posterior10[, , 6]

# Relatives who provide emotional support
M11 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/rels_emot_support_poisson.rds")
get_variables(M11)
posterior11 <- as.array(M11)
dimnames(posterior11)
dim(posterior11)
posterior11 <- posterior11[, , 6]

# Percent relatives who provide emotional support
M12 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/percent_rels_emot_support_beta.rds")
get_variables(M12)
posterior12 <- as.array(M12)
dimnames(posterior12)
dim(posterior12)
posterior12 <- posterior12[, , 6]

# Non-relatives who help with childcare
M13 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/childcare_help_non_rels_neg_binom.rds")
get_variables(M13)
posterior13 <- as.array(M13)
dimnames(posterior13)
dim(posterior13)
posterior13 <- posterior13[, , 6]

# Relatives who help with childcare
M14 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/childcare_help_rels_neg_binom.rds")
get_variables(M14)
posterior14 <- as.array(M14)
dimnames(posterior14)
dim(posterior14)
posterior14 <- posterior14[, , 6]

# Percent relatives who help with childcare
M15 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/percent_rels_childcare_help_beta.rds")
get_variables(M15)
posterior15 <- as.array(M15)
dimnames(posterior15)
dim(posterior15)
posterior15 <- posterior15[, , 6]

# Overall help from non-relatives
M16 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/overall_help_non_rels_neg_binom.rds")
get_variables(M16)
posterior16 <- as.array(M16)
dimnames(posterior16)
dim(posterior16)
posterior16 <- posterior16[, , 6]

# Overall help from relatives
M17 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/overall_help_rels_poisson.rds")
get_variables(M17)
posterior17 <- as.array(M17)
dimnames(posterior17)
dim(posterior17)
posterior17 <- posterior17[, , 6]

# Percent overall help from relatives
M18 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/percent_overall_help_rels_gamma.rds")
get_variables(M18)
posterior18 <- as.array(M18)
dimnames(posterior18)
dim(posterior18)
posterior18 <- posterior18[, , 6]
#### ########################################################3

column.names <- c("chain:1","chain:2","chain:3","chain:4")
row.names <- NULL
# name all the posteriors as the DV used in the model
matrix.names <- c("Total NW Size","Non-relatives in NW", "Relatives in NW","Percent relatives in NW",
                  "Geographic distance non-relatives","Geographic distance relatives",
                  "Non-relatives financial support",
                  "Relatives financial support",
                  "Percent relatives financial support",
                  "Non-relatives emotional support",
                  "Relatives emotional support",
                  "Percent relatives emotional support",
                  "Non-relatives childcare",
                  "Relatives childcare",
                  "Percent relatives childcare",
                  "Overall help non-relatives",
                  "Overall help relatives",
                  "Percent overall help relatives")


# Take these vectors as input to the array.
result <- array(c(posterior1,posterior2,posterior3,posterior4,posterior5,posterior6,
                  posterior7,posterior8,posterior9,posterior10,posterior11,posterior12,
                  posterior13,posterior14,posterior15,posterior16,posterior17,
                  posterior18),dim = c(4000,4,18),
                dimnames = list(row.names,column.names,matrix.names))

dimnames(result)

# save result as posterior_dists_array
saveRDS(result, "posterior_dists_array.rds")


# Start below
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
# Make posterior distributions figure

library(tidyr)
library(brms)
library(readr)
library(magrittr)
library(dplyr)
library(purrr)
library(forcats)
library(modelr)
library(ggdist)
library(tidybayes)
library(ggplot2)
library(cowplot)
library(rstan)
library(ggrepel)
library(RColorBrewer)
library(gganimate)
library(bayesplot)

theme_set(theme_tidybayes() + panel_border())
result <- readRDS("posterior_dists_array.rds")

rhat_values <- c(0.99, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00,1.00,1.00)

## make plots
color_scheme_set("green")
post_dists <- mcmc_intervals(result, pars = c("Total NW Size","Non-relatives in NW","Relatives in NW","Percent relatives in NW",
                                              "Geographic distance non-relatives","Geographic distance relatives",
                                              "Non-relatives financial support",
                                              "Relatives financial support",
                                              "Percent relatives financial support",
                                              "Non-relatives emotional support",
                                              "Relatives emotional support",
                                              "Percent relatives emotional support",
                                              "Non-relatives childcare",
                                              "Relatives childcare",
                                              "Percent relatives childcare",
                                              "Overall help non-relatives",
                                              "Overall help relatives",
                                              "Percent overall help relatives"),
                             prob=0.5, #rhat=rhat_values,
                             prob_outer = 0.89,point_est = "median")
post_dists
ggsave(post_dists, filename = "Figures/post_dists.pdf")
ggsave(post_dists, filename = "Figures/post_dists.png")
# alternative plot (fewer DV's)
color_scheme_set("green")
post_dists2 <- mcmc_intervals(result, pars = c("Total NW Size",
                                               "Percent relatives in NW",
                                               "Geographic distance non-relatives",
                                               "Geographic distance relatives",
                                               "Percent relatives financial support",
                                               "Percent relatives emotional support",
                                               "Percent relatives childcare",
                                               "Percent overall help relatives"),
                              prob=0.5, #rhat=rhat_values,
                              prob_outer = 0.89,point_est = "median")
post_dists2
ggsave(post_dists2, filename = "Figures/post_dists2.pdf")
ggsave(post_dists2, filename = "Figures/post_dists2.png")
#ggsave(post_dists2, filename = "Figures/post_dists2.pdf", width = 8, height = 12)

### Old geo dist plots for rels and non rels using mean distance

# get mean geo distance  non rels
# M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Geo_distance_non_relatives_lognormal.rds")
# 
# ## add the dummy variable back
# data$dummy_no_non_rels <- ifelse(data$non_rels==0,1,0)
# data$geo_distance_non_rels[is.na(data$geo_distance_non_rels)] <- 5
# data$dummy_no_non_rels[is.na(data$dummy_no_non_rels)] <- 0
# 
# attach(data)
# newdata <- data.frame(
#   kids_in_hh = mean(kids_in_hh),
#   age_wife = mean(age_wife),
#   religion=mean(religion),
#   sex=0,
#   dummy_no_non_rels=mean(dummy_no_non_rels),
#   familyBariReligiousAfter = c(-1,0,1),
#   religious_knowledge_scale=mean(religious_knowledge_scale),
#   MI_geo_proximity=mean(MI_geo_proximity),
#   MI_economic_capital=mean(MI_economic_capital),
#   MI_human_capital=mean(MI_human_capital)
# )
# detach(data)
# 
# mu_summary <-
#   fitted(M1, 
#          newdata = newdata, probs=c(0.05,0.95)) %>%
#   as_tibble() %>%
#   bind_cols(religious_seq)
# 
# mu_summary
# 
# 
# data$religiosity <- data$familyBariReligiousAfter
# 
# data2 <- data %>% left_join (mu_summary, by =c("religiosity"="religiosity"))
# data2$religiosity[is.na(data2$religiosity)]<- 0
# 
# #1=khana member,(same household)
# #2=near bari/neighbor (walking distance), 
# #3=other place in Matlab,
# #4=Other Place in Bangladesh, 
# #5=Abroad, 
# library(wesanderson)
# fig5 <- ggplot(data2, aes(x=factor(religiosity), y=geo_distance_non_rels,fill=factor(religiosity), colour=factor(religiosity))) +
#   geom_boxplot(show.legend=T, width=0.2,fill="transparent",aes(colour=factor(religiosity)))+
#   geom_jitter(width = 0.2,show.legend=T)+
#   
#   
#   geom_smooth(method = 'lm', aes(group=1,y=Estimate,ymin=Q5,ymax=Q95,colour="Credibility interval"),
#               stat = "identity",fill="grey16",colour="black",show.legend=T)+
#   #theme(legend.position = "none")+
#   
#   scale_x_discrete(breaks=c("-1","0","1"),name="Religiosity",
#                    labels=c("Less", "Equal", "More"))+
#   scale_y_continuous(name="Geographic distance non relatives",
#                      breaks=c(1,2,3,4,5), labels=c("Same house","Neighbor","Matlab","Bangladesh","Abroad"),limits=c(1,5))+
#   theme_bw() +
#   
#   # scale_colour_discrete(name = "Credibility Interval", labels = c("+/- 97.5%")) +
#   scale_fill_manual(name = "Boxplots", labels = c("Less religious","Equally religious", "More religious"),
#                     
#                     values=wes_palette(n=4, name="FantasticFox1"),
#                     
#                     
#                     guide = guide_legend(override.aes = list(linetype = c(0, 0,0))))+
#   
#   
#   scale_colour_manual(name = "Credibility Interval", labels = c("+/- 97.5%", "", ""),
#                       values = c(wes_palette(n=4, name="FantasticFox1"),"black"),
#                       
#                       
#                       guide = guide_legend(override.aes = list(linetype = c(1,0,0))))+
#   
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         legend.key.size = unit(0.15, "in"),
#         legend.key = element_rect(colour = "transparent", fill = NA),
#         legend.box.background = element_blank(),
#         
#         axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
#         axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
#         axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
#         axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
#   theme(legend.position="none")+theme(axis.title.x=element_blank(),
#                                       axis.text.x=element_blank(),
#                                       axis.ticks.x=element_blank())
# 
# fig5
# ############################################################################################################################################
# # reversed axes
# # get mean geo distance  non rels
# M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Religiosity_predicted_by_geo_distance_non_relatives_normal.rds")
# 
# ## add the dummy variable back
# data$dummy_no_non_rels <- ifelse(data$non_rels==0,1,0)
# data$geo_distance_non_rels[is.na(data$geo_distance_non_rels)] <- 5
# data$dummy_no_non_rels[is.na(data$dummy_no_non_rels)] <- 0
# 
# 
# ### link data 2 to the location data in Models (for alternative figure)
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
# non_rels <- nr %>% left_join (data, by=c("id_Questionaire"="idwife"))
# 
# 
# non_rels$location [is.na(non_rels$location)] <- 2
# hist(non_rels$familyBariReligiousAfter)
# 
# attach(non_rels)
# newdata <- data.frame(
#   kids_in_hh = mean(kids_in_hh),
#   age_wife = mean(age_wife),
#   religion=mean(religion),
#   sex=0,
#   dummy_no_non_rels=mean(dummy_no_non_rels),
#   location = c(1,2,3,4,5),
#   religious_knowledge_scale=mean(religious_knowledge_scale),
#   MI_geo_proximity=mean(MI_geo_proximity),
#   MI_economic_capital=mean(MI_economic_capital),
#   MI_human_capital=mean(MI_human_capital)
# )
# detach(non_rels)
# 
# mu_summary <-
#   fitted(M1, 
#          newdata = newdata, probs=c(0.05,0.95)) %>%
#   as_tibble() 
# 
# mu_summary$location <- c(1,2,3,4,5)
# mu_summary
# 
# 
# 
# 
# data2 <- non_rels %>% left_join (mu_summary, by =c("location"="location"))
# 
# data2$familyBariReligiousAfter<- as.numeric(data2$familyBariReligiousAfter)
# library(wesanderson)
# fig5a <- ggplot(data2, aes(x=factor(location), y=familyBariReligiousAfter,fill=factor(location), colour=factor(location))) +
#   geom_boxplot(show.legend=T, width=0.2,fill="transparent",aes(colour=factor(location)))+
#   geom_jitter(width = 0.2,show.legend=T)+
#   
#   
#   geom_smooth(method = 'lm', aes(group=1,y=Estimate,ymin=Q5,ymax=Q95,colour="Credibility interval"),
#               stat = "identity",fill="grey16",colour="black",show.legend=T)+
#   #theme(legend.position = "none")+
#   
#   
#   
#   scale_y_continuous(name="Religiosity",
#                      breaks=c(-1,0,1), labels=c("Less", "Equal", "More"),limits=c(-1.2,1.2))+
#   
#   
#   scale_x_discrete(name="Geographic distance non relatives",
#                    breaks=c(1,2,3,4,5), labels=c("Same house","Neighbor","Matlab","Bangladesh","Abroad")) +   #,limits=c(1,5))+
#   theme_bw() +
#   
#   # scale_colour_discrete(name = "Credibility Interval", labels = c("+/- 97.5%")) +
#   scale_fill_manual(name = "Boxplots", labels = c("Less religious","Equally religious", "More religious"),
#                     
#                     values=wes_palette(n=5, name="FantasticFox1"),
#                     
#                     
#                     guide = guide_legend(override.aes = list(linetype = c(0, 0,0,0,0))))+
#   
#   
#   scale_colour_manual(name = "Credibility Interval", labels = c("+/- 97.5%", "", ""),
#                       values = c(wes_palette(n=5, name="FantasticFox1"),"black"),
#                       
#                       
#                       guide = guide_legend(override.aes = list(linetype = c(1,0,0,0,0))))+
#   
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         legend.key.size = unit(0.15, "in"),
#         legend.key = element_rect(colour = "transparent", fill = NA),
#         legend.box.background = element_blank(),
#         
#         axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
#         axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
#         axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
#         axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold")) 
# 
# 
# 
# # +
# #   theme(legend.position="none")+theme(axis.title.x=element_blank(),
# #                                        axis.text.x=element_blank(),
# #                                       axis.ticks.x=element_blank())
# 
# fig5a
# 
# 
# #### Fix legend and plot above - then repeat for figure 6a (geographic distance relatives)
# #################################################################################################################
# ####################################################################################################################
# #################################################################################################################
# ####################################################################################################################
# # get mean geo distance rels
# M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Geo_distance_relatives_lognormal.rds")
# attach(data)
# newdata <- data.frame(
#   kids_in_hh = mean(kids_in_hh),
#   age_wife = mean(age_wife),
#   religion=mean(religion),
#   sex=mean(sex),
#   familyBariReligiousAfter = c(-1,0,1),
#   religious_knowledge_scale=mean(religious_knowledge_scale),
#   MI_geo_proximity=mean(MI_geo_proximity),
#   MI_economic_capital=mean(MI_economic_capital),
#   MI_human_capital=mean(MI_human_capital)
# )
# detach(data)
# 
# mu_summary <-
#   fitted(M1, 
#          newdata = newdata, probs=c(0.05,0.95)) %>%
#   as_tibble() %>%
#   # let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
#   bind_cols(religious_seq)
# 
# mu_summary
# 
# 
# data$religiosity <- data$familyBariReligiousAfter
# 
# data2 <- data %>% left_join (mu_summary, by =c("religiosity"="religiosity"))
# 
# #1=khana member,(same household)
# #2=near bari/neighbor (walking distance), 
# #3=other place in Matlab,
# #4=Other Place in Bangladesh, 
# #5=Abroad, 
# 
# fig6 <- ggplot(data2, aes(x=factor(religiosity), y=geo_distance_rels,fill=factor(religiosity), colour=factor(religiosity))) +
#   geom_boxplot(show.legend=T, width=0.2,fill="transparent",aes(colour=factor(religiosity)))+
#   geom_jitter(width = 0.2,show.legend=T)+
#   
#   
#   geom_smooth(method = 'lm', aes(group=1,y=Estimate,ymin=Q5,ymax=Q95,colour="Credibility interval"),
#               stat = "identity",fill="grey16",colour="black",show.legend=T)+
#   #theme(legend.position = "none")+
#   
#   scale_x_discrete(breaks=c("-1","0","1"),name="Religiosity",
#                    labels=c("Less", "Equal", "More"))+
#   scale_y_continuous(name="Geographic distance relatives",
#                      breaks=c(1,2,3,4,5), labels=c("Same house","Neighbor","Matlab","Bangladesh","Abroad"),limits=c(1,5))+
#   theme_bw() +
#   
#   # scale_colour_discrete(name = "Credibility Interval", labels = c("+/- 97.5%")) +
#   scale_fill_manual(name = "Boxplots", labels = c("Less religious","Equally religious", "More religious"),
#                     
#                     values=wes_palette(n=4, name="FantasticFox1"),
#                     
#                     
#                     guide = guide_legend(override.aes = list(linetype = c(0, 0,0))))+
#   
#   
#   scale_colour_manual(name = "Credibility Interval", labels = c("+/- 97.5%", "", ""),
#                       values = c(wes_palette(n=4, name="FantasticFox1"),"black"),
#                       
#                       
#                       guide = guide_legend(override.aes = list(linetype = c(1,0,0))))+
#   
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         legend.key.size = unit(0.15, "in"),
#         legend.key = element_rect(colour = "transparent", fill = NA),
#         legend.box.background = element_blank(),
#         
#         axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
#         axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
#         axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
#         axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
#   theme(legend.position="none")+theme(axis.title.x=element_blank(),
#                                       axis.text.x=element_blank(),
#                                       axis.ticks.x=element_blank())
# 
# fig6
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# 
# ## rerun geo dist models first with DV as religiosity
# library(wesanderson)
# library(tidyverse)
# library(brms)
# 
# setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh")
# data <- read.csv("newdata.csv")
# options(scipen=999)
# data <- data %>% filter (sex==0)
# 
# # get mean geo distance  non rels
# M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Religiosity_predicted_by_geo_distance_non_relatives_normal.rds")
# 
# ## add the dummy variable back
# data$dummy_no_non_rels <- ifelse(data$non_rels==0,1,0)
# data$geo_distance_non_rels[is.na(data$geo_distance_non_rels)] <- 5
# data$dummy_no_non_rels[is.na(data$dummy_no_non_rels)] <- 0
# 
# attach(data)
# newdata <- data.frame(
#   kids_in_hh = mean(kids_in_hh),
#   age_wife = mean(age_wife),
#   religion=mean(religion),
#   sex=mean(sex),
#   dummy_no_non_rels=mean(dummy_no_non_rels),
#   geo_distance_non_rels = c(1,2,3,4,5),
#   religious_knowledge_scale=mean(religious_knowledge_scale),
#   MI_geo_proximity=mean(MI_geo_proximity),
#   MI_economic_capital=mean(MI_economic_capital),
#   MI_human_capital=mean(MI_human_capital)
# )
# detach(data)
# 
# mu_summary <-
#   fitted(M1, 
#          newdata = newdata, probs=c(0.05,0.95)) %>%
#   as_tibble() 
# 
# 
# 
# mu_summary$geo_distance_non_rels2 <- c(1,2,3,4,5)
# 
# mu_summary <- mu_summary[c(2,4,5),]
# 
# # group same house with neighbor and Matlab and Bangladesh together
# data$geo_distance_non_rels <- round(data$geo_distance_non_rels, digits = 0)
# data$geo_distance_non_rels2 <- ifelse(data$geo_distance_non_rels==1,2,data$geo_distance_non_rels)
# data$geo_distance_non_rels2 <- ifelse(data$geo_distance_non_rels2==3,4,data$geo_distance_non_rels2)
# 
# plyr::count(data$geo_distance_non_rels2)
# 
# data2 <- data %>% left_join (mu_summary, by =c("geo_distance_non_rels2"="geo_distance_non_rels2"))
# 
# plyr::count(data2$geo_distance_non_rels2)
# 
# 
# 
# fig5 <- ggplot(data2, aes(x=factor(geo_distance_non_rels2), y=familyBariReligiousAfter,fill=factor(geo_distance_non_rels2), colour=factor(geo_distance_non_rels2))) +
#   geom_boxplot(show.legend=T, width=0.2,fill="transparent",aes(colour=factor(geo_distance_non_rels2)))+
#   geom_jitter(width = 0.2,show.legend=T)+
#   
#   
#   geom_smooth(method = 'lm', aes(group=1,y=Estimate,ymin=Q5,ymax=Q95,colour="Credibility interval"),
#               stat = "identity",fill="grey16",colour="black",show.legend=T)+
#   #theme(legend.position = "none")+
#   
#   
#   
#   scale_x_discrete(name="Geographic distance non-relatives",
#                    breaks=c(2,4,5), labels=c("Same house\n Or Neighbor","Matlab\n Or Bangladesh","Abroad"))+
#   
#   
#   
#   scale_y_continuous(name="Relative Religiosity",
#                      breaks=c(-1,0,1), labels=c("Less","Same","More"),limits=c(-1,1))+
#   theme_bw() +
#   
#   # scale_colour_discrete(name = "Credibility Interval", labels = c("+/- 97.5%")) +
#   scale_fill_manual(name = "Boxplots", labels = c("Less religious","Equally religious", "More religious"),
#                     
#                     values=wes_palette(n=3, name="FantasticFox1"),
#                     
#                     
#                     guide = guide_legend(override.aes = list(linetype = c(0, 0,0))))+
#   
#   
#   scale_colour_manual(name = "Credibility Interval", labels = c("+/- 97.5%", "", ""),
#                       values = c(wes_palette(n=3, name="FantasticFox1"),"black"),
#                       
#                       
#                       guide = guide_legend(override.aes = list(linetype = c(1,0,0))))+
#   
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         legend.key.size = unit(0.15, "in"),
#         legend.key = element_rect(colour = "transparent", fill = NA),
#         legend.box.background = element_blank(),
#         
#         axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
#         axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
#         axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
#         axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
#   theme(legend.position="none") #+
# # theme(axis.title.x=element_blank(),
# #                                axis.text.x=element_blank(),
# #                             axis.ticks.x=element_blank())
# 
# fig5
# 
# #1=khana member,(same household)
# #2=near bari/neighbor (walking distance), 
# #3=other place in Matlab,
# #4=Other Place in Bangladesh, 
# #5=Abroad, 
# 
# #################################################################################################################
# ####################################################################################################################
# #################################################################################################################
# ####################################################################################################################
# # get mean geo distance rels
# library(wesanderson)
# library(tidyverse)
# library(brms)
# 
# setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh")
# data <- read.csv("newdata.csv")
# options(scipen=999)
# data <- data %>% filter (sex==0)
# 
# # get mean geo distance  non rels
# M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Religiosity_predicted_by_geo_distance_relatives_normal.rds")
# 
# ## add the dummy variable back
# 
# attach(data)
# newdata <- data.frame(
#   kids_in_hh = mean(kids_in_hh),
#   age_wife = mean(age_wife),
#   religion=mean(religion),
#   sex=mean(sex),
#   #dummy_no_non_rels=mean(dummy_no_non_rels),
#   geo_distance_rels = c(1,2,3,4,5),
#   religious_knowledge_scale=mean(religious_knowledge_scale),
#   MI_geo_proximity=mean(MI_geo_proximity),
#   MI_economic_capital=mean(MI_economic_capital),
#   MI_human_capital=mean(MI_human_capital)
# )
# detach(data)
# 
# mu_summary <-
#   fitted(M1, 
#          newdata = newdata, probs=c(0.05,0.95)) %>%
#   as_tibble() 
# 
# 
# 
# mu_summary$geo_distance_rels2 <- c(1,2,3,4,5)
# 
# #mu_summary <- mu_summary[c(2,4,5),]
# 
# # group same house with neighbor and Matlab and Bangladesh together
# data$geo_distance_rels <- round(data$geo_distance_rels, digits = 0)
# 
# 
# 
# 
# data$geo_distance_rels2 <- ifelse(data$geo_distance_rels>3,3,data$geo_distance_rels)
# 
# plyr::count(data$geo_distance_rels2)
# ##### HERE!!!!!  There are same houshold, neighbor and 
# 
# 
# data2 <- data %>% left_join (mu_summary, by =c("geo_distance_rels2"="geo_distance_rels2"))
# 
# data2$geo_distance_rels2[is.na(data2$geo_distance_rels2)] = 3
# 
# plyr::count(data2$geo_distance_rels2)
# 
# 
# 
# fig6 <- ggplot(data2, aes(x=factor(geo_distance_rels2), y=familyBariReligiousAfter,fill=factor(geo_distance_rels2), colour=factor(geo_distance_rels2))) +
#   geom_boxplot(show.legend=T, width=0.2,fill="transparent",aes(colour=factor(geo_distance_rels2)))+
#   geom_jitter(width = 0.2,show.legend=T)+
#   
#   
#   geom_smooth(method = 'lm', aes(group=1,y=Estimate,ymin=Q5,ymax=Q95,colour="Credibility interval"),
#               stat = "identity",fill="grey16",colour="black",show.legend=T)+
#   #theme(legend.position = "none")+
#   
#   
#   
#   scale_x_discrete(name="Geographic distance relatives",
#                    breaks=c(1,2,3), labels=c("Same household", "Neighbor","Matlab\n Or Bangladesh"))+
#   
#   
#   
#   scale_y_continuous(name="Relative Religiosity",
#                      breaks=c(-1,0,1), labels=c("Less","Same","More"),limits=c(-1,1))+
#   theme_bw() +
#   
#   # scale_colour_discrete(name = "Credibility Interval", labels = c("+/- 97.5%")) +
#   scale_fill_manual(name = "Boxplots", labels = c("Less religious","Equally religious", "More religious"),
#                     
#                     values=wes_palette(n=3, name="FantasticFox1"),
#                     
#                     
#                     guide = guide_legend(override.aes = list(linetype = c(0, 0,0))))+
#   
#   
#   scale_colour_manual(name = "Credibility Interval", labels = c("+/- 97.5%", "", ""),
#                       values = c(wes_palette(n=3, name="FantasticFox1"),"black"),
#                       
#                       
#                       guide = guide_legend(override.aes = list(linetype = c(1,0,0))))+
#   
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         legend.key.size = unit(0.15, "in"),
#         legend.key = element_rect(colour = "transparent", fill = NA),
#         legend.box.background = element_blank(),
#         
#         axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
#         axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
#         axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
#         axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
#   theme(legend.position="none") #+
# # theme(axis.title.x=element_blank(),
# #                                axis.text.x=element_blank(),
# #                             axis.ticks.x=element_blank())
# 
# fig6
# 
# 
# 
# ## brms figures -- in Mcelreaths book
# #https://bookdown.org/ajkurz/Statistical_Rethinking_recoded/linear-models.html#a-gaussian-model-of-height
# 
# 
# 
# 
# # To change the probability intervals
# 
# fitted(b4.3, newdata = weight.seq, probs = c(.25, .75))
# 
# 
# # Plot the Markov Chains
# plot(M1)
# 
# 
# # Plot the PP checks for key models
# ####HERE!!!!!
# # pp <- predict(M1)
# # 
# # 
# # predict(M1, newdata = newdata)
# 
# 
# 
# 
# ## try to write a function that does all the stuff above but takes a new model each time!!!
# # 
# # mcmc_areas(posterior, pars = c("b_Intercept","b_kids_in_hh","b_age_wife",                
# #                                "b_religion","b_familyBariReligiousAfter","b_religious_knowledge_scale",
# #                                "b_MI_geo_proximity","b_MI_economic_capital","b_MI_human_capital"),
# #            prob=0.8,
# #            prob_outer = 0.99,
# #            point_est = "median")
# # 
# # mcmc_areas(result, pars = c("b_familyBariReligiousAfter","b_religion"))
# # 
# # 
# # 
# # posterior1 <- posterior[, , 5]
# # posterior2 <- posterior[, , 4]
# # 
# # result <- array(c(posterior1,posterior2),dim=c(1000,4,2))
# # 
# # vector1 <- posterior[, , 5]
# # vector2 <- posterior[, , 4]
# # column.names <- c("chain:1","chain:2","chain:3","chain:4")
# # row.names <- NULL
# # matrix.names <- c("b_familyBariReligiousAfter","b_religion")
# # 
# # # Take these vectors as input to the array.
# # result <- array(c(vector1,vector2),dim = c(1000,4,2),dimnames = list(row.names,column.names,
# #                                                                      matrix.names))
# # dimnames(result)[[1]] <- "iterations"
# # 
# # dimnames(ar)[[3]] <- c("G", "H", "I")
# # 
# # mcmc_areas(
# #   result, 
# #   pars = pars = c("b_Intercept","b_kids_in_hh","b_age_wife",                
# #                   "b_religion","b_familyBariReligiousAfter","b_religious_knowledge_scale",
# #                   "b_MI_geo_proximity","b_MI_economic_capital","b_MI_human_capital"),
# #   prob = 0.8, 
# #   prob_outer = 0.99, 
# #   point_est = "median")               
# # 
# # 
# # "b_familyBariReligiousAfter" "b_religion"
# # 
# # #### posterior predictive checks
