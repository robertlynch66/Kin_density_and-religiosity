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
# # remove duplicated women
# newdata <- newdata %>% distinct(idwife, .keep_all = TRUE)
# # 1) center and scale variables for easier interpretation of parameter estimates
# newdata$religious_knowledge_scale <-  newdata$religious_knowledge_scale-mean(newdata$religious_knowledge_scale, na.rm=T)
# newdata$hh_total  <- newdata$hh_total-mean(newdata$hh_total, na.rm=T)  
# newdata$kids_in_hh  <- newdata$kids_in_hh-mean(newdata$kids_in_hh, na.rm=T)

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

d <- newdata[c(10,11,47,4,5,7,8,9,44,45,49,51)] 

d <- d[complete.cases(d), ] 

# get model
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/NW_total_lognormal.rds")

get_variables(M1)
# #] "b_Intercept"                 "b_kids_in_hh"                "b_R_NUM_SIBS"                 
# [4] "b_religion"                  "b_familyBariReligiousAfter"  "b_religious_knowledge_scale"
# [7] "b_MI_geo_proximity"          "b_MI_economic_capital"       "b_MI_human_capital"  

# see samples and chains
# M2 %>% spread_draws(b_familyBariReligiousAfter) %>% head(10)
# 
# # If we want the mean and 95% quantile interval of the variables,
# M1 %>%
#   spread_draws(b_familyBariReligiousAfter) %>%
#   mean_qi(b_familyBariReligiousAfter)
# 
# d1 <- M1 %>%
#   gather_draws(b_Intercept,b_familyBariReligiousAfter,b_religious_knowledge_scale,b_religion,
#                b_kids_in_hh,b_age_wife,b_MI_geo_proximity,b_MI_economic_capital,b_MI_human_capital)
# 
# #.variable   .value
# 
# 
# 
# # posterior plots
# 
# 
# d1 %>%
#   spread_draws(.variable) %>%
#   #mutate(condition_mean = b_Intercept + b_familyBariReligiousAfter) %>%
#   ggplot(aes(y = .variable, x = .valueClassTest(), fill = stat(abs(x) < .8))) +
#   stat_halfeye() +
#   geom_vline(xintercept = c(-.8, .8), linetype = "dashed") +
#   scale_fill_manual(values = c("gray80", "skyblue"))
# 
# 
# M1 %>%
#   gather_draws(b_Intercept,b_familyBariReligiousAfter,b_religious_knowledge_scale,b_religion,
#                b_kids_in_hh,b_age_wife,b_MI_geo_proximity,b_MI_economic_capital,b_MI_human_capital) %>%
#   
#   spread_draws(.variable)
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
posterior1 <- posterior1[, , 2]

# get religiosity for number of non-relatives in NW
M2 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/non_relatives_in_NW_neg_bin.rds")
get_variables(M2)
posterior2 <- as.array(M2)
dimnames(posterior2)
dim(posterior2)
posterior2 <- posterior2[, , 2]

# get religiosity for number of relatives in NW
M3 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/relatives_in_NW_lognormal.rds")
get_variables(M3)
posterior3 <- as.array(M3)
dimnames(posterior3)
dim(posterior3)
posterior3 <- posterior3[, , 2]

# get religiosity for percent relatives in NW
M4 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/percent_relatives_in_NW_lognormal.rds")
get_variables(M4)
posterior4 <- as.array(M4)
dimnames(posterior4)
dim(posterior4)
posterior4 <- posterior4[, , 2]

# get  geo distance  non rels
M5 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Geo_distance_non_relatives_ord_cum.rds")
get_variables(M5)
posterior5 <- as.array(M5)
dimnames(posterior5)
dim(posterior5)
posterior5 <- posterior5[, , 8]

# get  geo distance rels
M6 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Geo_distance_relatives_ord_cum.rds")
get_variables(M6)
posterior6 <- as.array(M6)
dimnames(posterior6)
dim(posterior6)
posterior6 <- posterior6[, , 9]

# get non-relatives who give financial support
M7 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Non_rels_econ_help_neg_binom.rds")
get_variables(M7)
posterior7 <- as.array(M7)
dimnames(posterior7)
dim(posterior7)
posterior7 <- posterior7[, , 2]

# Relatives who provide financial support
M8 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/rels_econ_help_poisson.rds")
get_variables(M8)
posterior8 <- as.array(M8)
dimnames(posterior8)
dim(posterior8)
posterior8 <- posterior8[, , 2]

# Percent relatives who provide financial support
M9 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Percent_rels_econ_help_beta.rds")
get_variables(M9)
posterior9 <- as.array(M9)
dimnames(posterior9)
dim(posterior9)
posterior9 <- posterior9[, , 2]



# Non-relatives who help with childcare
M10 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/childcare_help_non_rels_neg_binom.rds")
get_variables(M10)
posterior10 <- as.array(M10)
dimnames(posterior10)
dim(posterior10)
posterior10 <- posterior10[, , 2]

# Relatives who help with childcare
M11 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/childcare_help_rels_neg_binom.rds")
get_variables(M11)
posterior11 <- as.array(M11)
dimnames(posterior11)
dim(posterior11)
posterior11 <- posterior11[, , 2]

# Percent relatives who help with childcare
M12 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/percent_rels_childcare_help_beta.rds")
get_variables(M12)
posterior12 <- as.array(M12)
dimnames(posterior12)
dim(posterior12)
posterior12 <- posterior12[, , 2]

# Non-relatives who provide emotional support
M13 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Non_rels_emot_support_neg_binom.rds")
get_variables(M13)
posterior13 <- as.array(M13)
dimnames(posterior13)
dim(posterior13)
posterior13 <- posterior13[, , 2]

# Relatives who provide emotional support
M14 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/rels_emot_support_poisson.rds")
get_variables(M14)
posterior14 <- as.array(M14)
dimnames(posterior14)
dim(posterior14)
posterior14 <- posterior14[, , 2]

# Percent relatives who provide emotional support
M15 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/percent_rels_emot_support_beta.rds")
get_variables(M15)
posterior15 <- as.array(M15)
dimnames(posterior15)
dim(posterior15)
posterior15 <- posterior15[, , 2]
# 
# # Overall help from non-relatives
# M16 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/overall_help_non_rels_neg_binom.rds")
# get_variables(M16)
# posterior16 <- as.array(M16)
# dimnames(posterior16)
# dim(posterior16)
# posterior16 <- posterior16[, , 6]
# 
# # Overall help from relatives
# M17 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/overall_help_rels_poisson.rds")
# get_variables(M17)
# posterior17 <- as.array(M17)
# dimnames(posterior17)
# dim(posterior17)
# posterior17 <- posterior17[, , 6]
# 
# # Percent overall help from relatives
# M18 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/percent_overall_help_rels_gamma.rds")
# get_variables(M18)
# posterior18 <- as.array(M18)
# dimnames(posterior18)
# dim(posterior18)
# posterior18 <- posterior18[, , 6]
#### ########################################################3

column.names <- c("chain:1","chain:2","chain:3","chain:4")
row.names <- NULL
# name all the posteriors as the DV used in the model
matrix.names <- c("Geographic distance non-relatives","Geographic distance relatives","Total NW Size",
                  "Non-relatives in NW", "Relatives in NW","Percent relatives in NW",
                  "Non-relatives financial support",
                  "Relatives financial support",
                  "Percent relatives financial support",
                  "Non-relatives childcare",
                  "Relatives childcare",
                  "Percent relatives childcare",
                  "Non-relatives emotional support",
                  "Relatives emotional support",
                  "Percent relatives emotional support")


# Take these vectors as input to the array.
result <- array(c(posterior5,posterior6,posterior1,posterior2,posterior3,posterior4,
                  posterior7,posterior8,posterior9,posterior10,posterior11,posterior12,
                  posterior13,posterior14,posterior15),dim = c(4000,4,15),
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

rhat_values <- c(0.99, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00)

## make plots
color_scheme_set("green")
post_dists <- mcmc_intervals(result, pars = c("Geographic distance non-relatives","Geographic distance relatives",
                                              "Total NW Size","Non-relatives in NW","Relatives in NW",
                                              "Percent relatives in NW",
                                              "Non-relatives financial support",
                                              "Relatives financial support",
                                              "Percent relatives financial support",
                                              "Non-relatives childcare",
                                              "Relatives childcare",
                                              "Percent relatives childcare",
                                              "Non-relatives emotional support",
                                              "Relatives emotional support",
                                              "Percent relatives emotional support"),
                             prob=0.5, #rhat=rhat_values,
                             prob_outer = 0.89,point_est = "median")
post_dists

ggsave(post_dists, filename = "C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures/post_dists.pdf")
ggsave(post_dists, filename = "C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures/post_dists.png")
# alternative plot (fewer DV's)
# color_scheme_set("green")
# post_dists2 <- mcmc_intervals(result, pars = c("Total NW Size",
#                                                "Percent relatives in NW",
#                                                "Geographic distance non-relatives",
#                                                "Geographic distance relatives",
#                                                "Percent relatives financial support",
#                                                "Percent relatives emotional support",
#                                                "Percent relatives childcare",
#                                                "Percent overall help relatives"),
#                               prob=0.5, #rhat=rhat_values,
#                               prob_outer = 0.89,point_est = "median")
# post_dists2
# ggsave(post_dists2, filename = "Figures/post_dists2.pdf")
# ggsave(post_dists2, filename = "Figures/post_dists2.png")


# # Plot the Markov Chains
library(ggplot2)

M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Geo_distance_non_relatives_ord_cum.rds")

posterior <- as.array(M1)
dim(posterior)
dimnames(posterior)

posterior <- posterior[1:4000, 1:4, 1:11]
dimnames(posterior)$parameters <- c("Int 1","Int 2","Int 3","MI\nGeographic","MI\nEcon\nCapital",
                                    "MI\nHum\nCapital","Religious\nKnowledge",
                                    "Religiosity","SD\nKids\nin\nHH\nInt","SD\nSIBS\nInt",                  
                                    "SD\nReligion\nInt")
color_scheme_set("blue")

p1 <- mcmc_trace(posterior, pars = c("Int 1","Int 2","Int 3","MI\nGeographic","MI\nEcon\nCapital",
                                     "MI\nHum\nCapital","Religious\nKnowledge",
                                     "Religiosity","SD\nKids\nin\nHH\nInt","SD\nSIBS\nInt",                  
                                     "SD\nReligion\nInt"), 
                 facet_args = list(ncol = 1, strip.position = "left"))


plot_1<- p1 + 
  
  ggtitle("Trace Plots for Geographic Distance of Non-Relatives in Social Network")+
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.y = element_line(size = 0.08),
        axis.text.y = element_text(colour="grey8",size=4,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))

plot_1
setwd("C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures/")
library(bayesplot)
library("rstan")
ggsave(plot_1, filename = "C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures/Trace_plot_1.png", 
       width = 10, height = 12, device = "png", 
       dpi = 600,units = "in")

#### next one
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Geo_distance_relatives_ord_cum.rds")
posterior <- as.array(M1)
dim(posterior)
dimnames(posterior)

posterior <- posterior[1:4000, 1:4, c(1:3,5:12)]
dimnames(posterior)$parameters <- c("Int 1","Int 2","Int 3","MI\nGeographic","MI\nEcon\nCapital",
                                    "MI\nHum\nCapital","Religious\nKnowledge",
                                    "Religiosity","SD\nKids\nin\nHH\nInt","SD\nSIBS\nInt",                  
                                    "SD\nReligion\nInt")
color_scheme_set("blue")

p1 <- mcmc_trace(posterior, pars = c("Int 1","Int 2","Int 3","MI\nGeographic","MI\nEcon\nCapital",
                                     "MI\nHum\nCapital","Religious\nKnowledge",
                                     "Religiosity","SD\nKids\nin\nHH\nInt","SD\nSIBS\nInt",                  
                                     "SD\nReligion\nInt"), 
                 facet_args = list(ncol = 1, strip.position = "left"))


plot_1<- p1 + 
  
  ggtitle("Trace Plots for Geographic Distance of Relatives in Social Network")+
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.y = element_line(size = 0.08),
        axis.text.y = element_text(colour="grey8",size=4,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))

plot_1
setwd("C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures/")
library(bayesplot)
library("rstan")
ggsave(plot_1, filename = "C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures/Trace_plot_2.png", 
       width = 10, height = 12, device = "png", 
       dpi = 600,units = "in")

# next one
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/NW_total_lognormal.rds")
posterior <- as.array(M1)
dim(posterior)
dimnames(posterior)

posterior <- posterior[1:4000, 1:4, 1:9]
dimnames(posterior)$parameters <- c("Int","Religiosity","Religious\nKnowledge",
                                    "MI\nGeographic","MI\nEcon\nCapital",
                                    "MI\nHum\nCapital",
                                    "SD\nKids\nin\nHH\nInt","SD\nSIBS\nInt",                  
                                    "SD\nReligion\nInt")
color_scheme_set("blue")

p1 <- mcmc_trace(posterior, pars = c("Int","Religiosity","Religious\nKnowledge",
                                     "MI\nGeographic","MI\nEcon\nCapital",
                                     "MI\nHum\nCapital",
                                     "SD\nKids\nin\nHH\nInt","SD\nSIBS\nInt",                  
                                     "SD\nReligion\nInt"), 
                 facet_args = list(ncol = 1, strip.position = "left"))


plot_1<- p1 + 
  
  ggtitle("Trace Plots for Total Size of Social Network")+
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.y = element_line(size = 0.08),
        axis.text.y = element_text(colour="grey8",size=4,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))

plot_1
setwd("C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures/")
library(bayesplot)
library("rstan")
ggsave(plot_1, filename = "C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures/Trace_plot_3.png", 
       width = 10, height = 12, device = "png", 
       dpi = 600,units = "in")


#### next one
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/relatives_in_NW_lognormal.rds")
posterior <- as.array(M1)
dim(posterior)
dimnames(posterior)

posterior <- posterior[1:4000, 1:4, 1:9]
dimnames(posterior)$parameters <- c("Int","Religiosity","Religious\nKnowledge",
                                    "MI\nGeographic","MI\nEcon\nCapital",
                                    "MI\nHum\nCapital",
                                    "SD\nKids\nin\nHH\nInt","SD\nSIBS\nInt",                  
                                    "SD\nReligion\nInt")
color_scheme_set("blue")

p1 <- mcmc_trace(posterior, pars = c("Int","Religiosity","Religious\nKnowledge",
                                     "MI\nGeographic","MI\nEcon\nCapital",
                                     "MI\nHum\nCapital",
                                     "SD\nKids\nin\nHH\nInt","SD\nSIBS\nInt",                  
                                     "SD\nReligion\nInt"), 
                 facet_args = list(ncol = 1, strip.position = "left"))


plot_1<- p1 + 
  
  ggtitle("Trace Plots for Number of Relatives in Social Network")+
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.y = element_line(size = 0.08),
        axis.text.y = element_text(colour="grey8",size=4,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))

plot_1
setwd("C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures/")
library(bayesplot)
library("rstan")
ggsave(plot_1, filename = "C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures/Trace_plot_4.png", 
       width = 10, height = 12, device = "png", 
       dpi = 600,units = "in")

#### next one
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/non_relatives_in_NW_neg_bin.rds")
posterior <- as.array(M1)
dim(posterior)
dimnames(posterior)

posterior <- posterior[1:4000, 1:4, 1:9]
dimnames(posterior)$parameters <- c("Int","Religiosity","Religious\nKnowledge",
                                    "MI\nGeographic","MI\nEcon\nCapital",
                                    "MI\nHum\nCapital",
                                    "SD\nKids\nin\nHH\nInt","SD\nSIBS\nInt",                  
                                    "SD\nReligion\nInt")
color_scheme_set("blue")

p1 <- mcmc_trace(posterior, pars = c("Int","Religiosity","Religious\nKnowledge",
                                     "MI\nGeographic","MI\nEcon\nCapital",
                                     "MI\nHum\nCapital",
                                     "SD\nKids\nin\nHH\nInt","SD\nSIBS\nInt",                  
                                     "SD\nReligion\nInt"), 
                 facet_args = list(ncol = 1, strip.position = "left"))


plot_1<- p1 + 
  
  ggtitle("Trace Plots for Number of Non-Relatives in Social Network")+
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.y = element_line(size = 0.08),
        axis.text.y = element_text(colour="grey8",size=4,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))

plot_1
setwd("C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures/")
library(bayesplot)
library("rstan")
ggsave(plot_1, filename = "C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures/Trace_plot_5.png", 
       width = 10, height = 12, device = "png", 
       dpi = 600,units = "in")

#### next one
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/percent_relatives_in_NW_lognormal.rds")
posterior <- as.array(M1)
dim(posterior)
dimnames(posterior)

posterior <- posterior[1:4000, 1:4, 1:9]
dimnames(posterior)$parameters <- c("Int","Religiosity","Religious\nKnowledge",
                                    "MI\nGeographic","MI\nEcon\nCapital",
                                    "MI\nHum\nCapital",
                                    "SD\nKids\nin\nHH\nInt","SD\nSIBS\nInt",                  
                                    "SD\nReligion\nInt")
color_scheme_set("blue")

p1 <- mcmc_trace(posterior, pars = c("Int","Religiosity","Religious\nKnowledge",
                                     "MI\nGeographic","MI\nEcon\nCapital",
                                     "MI\nHum\nCapital",
                                     "SD\nKids\nin\nHH\nInt","SD\nSIBS\nInt",                  
                                     "SD\nReligion\nInt"), 
                 facet_args = list(ncol = 1, strip.position = "left"))


plot_1<- p1 + 
  
  ggtitle("Trace Plots for Percentage of Relatives in Social Network")+
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.y = element_line(size = 0.08),
        axis.text.y = element_text(colour="grey8",size=4,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))

plot_1
setwd("C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures/")
library(bayesplot)
library("rstan")
ggsave(plot_1, filename = "C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures/Trace_plot_6.png", 
       width = 10, height = 12, device = "png", 
       dpi = 600,units = "in")





###########################INSERT 7 
#### next one
# Next one
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Non_rels_econ_help_neg_binom.rds")

posterior <- as.array(M1)
dim(posterior)
dimnames(posterior)

posterior <- posterior[1:4000, 1:4, 1:9]
dimnames(posterior)$parameters <- c("Int","Religiosity","Religious\nKnowledge",
                                    "MI\nGeographic","MI\nEcon\nCapital",
                                    "MI\nHum\nCapital",
                                    "SD\nKids\nin\nHH\nInt","SD\nSIBS\nInt",                  
                                    "SD\nReligion\nInt")
color_scheme_set("blue")

p1 <- mcmc_trace(posterior, pars = c("Int","Religiosity","Religious\nKnowledge",
                                     "MI\nGeographic","MI\nEcon\nCapital",
                                     "MI\nHum\nCapital",
                                     "SD\nKids\nin\nHH\nInt","SD\nSIBS\nInt",                  
                                     "SD\nReligion\nInt"), 
                 facet_args = list(ncol = 1, strip.position = "left"))


plot_1<- p1 + 
  
  ggtitle("Trace Plots for Number of Non-Relatives in Social Network Providing Financial Help")+
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.y = element_line(size = 0.08),
        axis.text.y = element_text(colour="grey8",size=4,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))

plot_1
setwd("C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures/")
library(bayesplot)
library("rstan")
ggsave(plot_1, filename = "C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures/Trace_plot_7.png", 
       width = 10, height = 12, device = "png", 
       dpi = 600,units = "in")


# Next one
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/rels_econ_help_poisson.rds")

posterior <- as.array(M1)
dim(posterior)
dimnames(posterior)

posterior <- posterior[1:4000, 1:4, 1:9]
dimnames(posterior)$parameters <- c("Int","Religiosity","Religious\nKnowledge",
                                    "MI\nGeographic","MI\nEcon\nCapital",
                                    "MI\nHum\nCapital",
                                    "SD\nKids\nin\nHH\nInt","SD\nSIBS\nInt",                  
                                    "SD\nReligion\nInt")
color_scheme_set("blue")

p1 <- mcmc_trace(posterior, pars = c("Int","Religiosity","Religious\nKnowledge",
                                     "MI\nGeographic","MI\nEcon\nCapital",
                                     "MI\nHum\nCapital",
                                     "SD\nKids\nin\nHH\nInt","SD\nSIBS\nInt",                  
                                     "SD\nReligion\nInt"), 
                 facet_args = list(ncol = 1, strip.position = "left"))


plot_1<- p1 + 
  
  ggtitle("Trace Plots for Number of Relatives in Social Network Providing Financial Help")+
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.y = element_line(size = 0.08),
        axis.text.y = element_text(colour="grey8",size=4,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))

plot_1
setwd("C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures/")
library(bayesplot)
library("rstan")
ggsave(plot_1, filename = "C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures/Trace_plot_8.png", 
       width = 10, height = 12, device = "png", 
       dpi = 600,units = "in")


# Next one
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Percent_rels_econ_help_beta.rds")

posterior <- as.array(M1)
dim(posterior)
dimnames(posterior)

posterior <- posterior[1:4000, 1:4, 1:9]
dimnames(posterior)$parameters <- c("Int","Religiosity","Religious\nKnowledge",
                                    "MI\nGeographic","MI\nEcon\nCapital",
                                    "MI\nHum\nCapital",
                                    "SD\nKids\nin\nHH\nInt","SD\nSIBS\nInt",                  
                                    "SD\nReligion\nInt")
color_scheme_set("blue")

p1 <- mcmc_trace(posterior, pars = c("Int","Religiosity","Religious\nKnowledge",
                                     "MI\nGeographic","MI\nEcon\nCapital",
                                     "MI\nHum\nCapital",
                                     "SD\nKids\nin\nHH\nInt","SD\nSIBS\nInt",                  
                                     "SD\nReligion\nInt"), 
                 facet_args = list(ncol = 1, strip.position = "left"))


plot_1<- p1 + 
  
  ggtitle("Trace Plots for Perecentage in Social Network Providing Financial Help that are relatives")+
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.y = element_line(size = 0.08),
        axis.text.y = element_text(colour="grey8",size=4,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))

plot_1
setwd("C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures/")
library(bayesplot)
library("rstan")
ggsave(plot_1, filename = "C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures/Trace_plot_9.png", 
       width = 10, height = 12, device = "png", 
       dpi = 600,units = "in")
### Insert figs 10 
# Next one
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/childcare_help_non_rels_neg_binom.rds")

posterior <- as.array(M1)
dim(posterior)
dimnames(posterior)

posterior <- posterior[1:4000, 1:4, 1:9]
dimnames(posterior)$parameters <- c("Int","Religiosity","Religious\nKnowledge",
                                    "MI\nGeographic","MI\nEcon\nCapital",
                                    "MI\nHum\nCapital",
                                    "SD\nKids\nin\nHH\nInt","SD\nSIBS\nInt",                  
                                    "SD\nReligion\nInt")
color_scheme_set("blue")

p1 <- mcmc_trace(posterior, pars = c("Int","Religiosity","Religious\nKnowledge",
                                     "MI\nGeographic","MI\nEcon\nCapital",
                                     "MI\nHum\nCapital",
                                     "SD\nKids\nin\nHH\nInt","SD\nSIBS\nInt",                  
                                     "SD\nReligion\nInt"), 
                 facet_args = list(ncol = 1, strip.position = "left"))


plot_1<- p1 + 
  
  ggtitle("Trace Plots for Number of Non-Relatives in Social Network Providing Help with Childcare")+
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.y = element_line(size = 0.08),
        axis.text.y = element_text(colour="grey8",size=4,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))

plot_1
setwd("C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures/")
library(bayesplot)
library("rstan")
ggsave(plot_1, filename = "C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures/Trace_plot_10.png", 
       width = 10, height = 12, device = "png", 
       dpi = 600,units = "in")

# Next one
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/childcare_help_rels_neg_binom.rds")
posterior <- as.array(M1)
dim(posterior)
dimnames(posterior)

posterior <- posterior[1:4000, 1:4, 1:9]
dimnames(posterior)$parameters <- c("Int","Religiosity","Religious\nKnowledge",
                                    "MI\nGeographic","MI\nEcon\nCapital",
                                    "MI\nHum\nCapital",
                                    "SD\nKids\nin\nHH\nInt","SD\nSIBS\nInt",                  
                                    "SD\nReligion\nInt")
color_scheme_set("blue")

p1 <- mcmc_trace(posterior, pars = c("Int","Religiosity","Religious\nKnowledge",
                                     "MI\nGeographic","MI\nEcon\nCapital",
                                     "MI\nHum\nCapital",
                                     "SD\nKids\nin\nHH\nInt","SD\nSIBS\nInt",                  
                                     "SD\nReligion\nInt"), 
                 facet_args = list(ncol = 1, strip.position = "left"))


plot_1<- p1 + 
  
  ggtitle("Trace Plots for Number of Relatives in Social Network Providing Help with Childcare")+
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.y = element_line(size = 0.08),
        axis.text.y = element_text(colour="grey8",size=4,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))

plot_1
setwd("C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures/")
library(bayesplot)
library("rstan")
ggsave(plot_1, filename = "C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures/Trace_plot_11.png", 
       width = 10, height = 12, device = "png", 
       dpi = 600,units = "in")

# insert 12
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/percent_rels_childcare_help_beta.rds")
posterior <- as.array(M1)
dim(posterior)
dimnames(posterior)

posterior <- posterior[1:4000, 1:4, 1:9]
dimnames(posterior)$parameters <- c("Int","Religiosity","Religious\nKnowledge",
                                    "MI\nGeographic","MI\nEcon\nCapital",
                                    "MI\nHum\nCapital",
                                    "SD\nKids\nin\nHH\nInt","SD\nSIBS\nInt",                  
                                    "SD\nReligion\nInt")
color_scheme_set("blue")

p1 <- mcmc_trace(posterior, pars = c("Int","Religiosity","Religious\nKnowledge",
                                     "MI\nGeographic","MI\nEcon\nCapital",
                                     "MI\nHum\nCapital",
                                     "SD\nKids\nin\nHH\nInt","SD\nSIBS\nInt",                  
                                     "SD\nReligion\nInt"), 
                 facet_args = list(ncol = 1, strip.position = "left"))


plot_1<- p1 + 
  
  ggtitle("Trace Plots for Percent in Social Network Providing Help with Childcare that are relatives")+
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.y = element_line(size = 0.08),
        axis.text.y = element_text(colour="grey8",size=4,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))

plot_1
setwd("C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures/")
library(bayesplot)
library("rstan")
ggsave(plot_1, filename = "C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures/Trace_plot_12.png", 
       width = 10, height = 12, device = "png", 
       dpi = 600,units = "in")

### insert plots 13 and 15 - emot support
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Non_rels_emot_support_neg_binom.rds")
posterior <- as.array(M1)
dim(posterior)
dimnames(posterior)

posterior <- posterior[1:4000, 1:4, 1:9]
dimnames(posterior)$parameters <- c("Int","Religiosity","Religious\nKnowledge",
                                    "MI\nGeographic","MI\nEcon\nCapital",
                                    "MI\nHum\nCapital",
                                    "SD\nKids\nin\nHH\nInt","SD\nSIBS\nInt",                  
                                    "SD\nReligion\nInt")
color_scheme_set("blue")

p1 <- mcmc_trace(posterior, pars = c("Int","Religiosity","Religious\nKnowledge",
                                     "MI\nGeographic","MI\nEcon\nCapital",
                                     "MI\nHum\nCapital",
                                     "SD\nKids\nin\nHH\nInt","SD\nSIBS\nInt",                  
                                     "SD\nReligion\nInt"), 
                 facet_args = list(ncol = 1, strip.position = "left"))


plot_1<- p1 + 
  
  ggtitle("Trace Plots for Number of Non-Relatives in Social Network Providing Emotional Support")+
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.y = element_line(size = 0.08),
        axis.text.y = element_text(colour="grey8",size=4,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))

plot_1
setwd("C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures/")
library(bayesplot)
library("rstan")
ggsave(plot_1, filename = "C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures/Trace_plot_13.png", 
       width = 10, height = 12, device = "png", 
       dpi = 600,units = "in")
# Next one
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/rels_emot_support_poisson.rds")

posterior <- as.array(M1)
dim(posterior)
dimnames(posterior)

posterior <- posterior[1:4000, 1:4, 1:9]
dimnames(posterior)$parameters <- c("Int","Religiosity","Religious\nKnowledge",
                                    "MI\nGeographic","MI\nEcon\nCapital",
                                    "MI\nHum\nCapital",
                                    "SD\nKids\nin\nHH\nInt","SD\nSIBS\nInt",                  
                                    "SD\nReligion\nInt")
color_scheme_set("blue")

p1 <- mcmc_trace(posterior, pars = c("Int","Religiosity","Religious\nKnowledge",
                                     "MI\nGeographic","MI\nEcon\nCapital",
                                     "MI\nHum\nCapital",
                                     "SD\nKids\nin\nHH\nInt","SD\nSIBS\nInt",                  
                                     "SD\nReligion\nInt"), 
                 facet_args = list(ncol = 1, strip.position = "left"))


plot_1<- p1 + 
  
  ggtitle("Trace Plots for Number of Relatives in Social Network Providing Emotional Support")+
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.y = element_line(size = 0.08),
        axis.text.y = element_text(colour="grey8",size=4,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))

plot_1
setwd("C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures/")
library(bayesplot)
library("rstan")
ggsave(plot_1, filename = "C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures/Trace_plot_14.png", 
       width = 10, height = 12, device = "png", 
       dpi = 600,units = "in")


# Next one
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/percent_rels_emot_support_beta.rds")

posterior <- as.array(M1)
dim(posterior)
dimnames(posterior)

posterior <- posterior[1:4000, 1:4, 1:9]
dimnames(posterior)$parameters <- c("Int","Religiosity","Religious\nKnowledge",
                                    "MI\nGeographic","MI\nEcon\nCapital",
                                    "MI\nHum\nCapital",
                                    "SD\nKids\nin\nHH\nInt","SD\nSIBS\nInt",                  
                                    "SD\nReligion\nInt")
color_scheme_set("blue")

p1 <- mcmc_trace(posterior, pars = c("Int","Religiosity","Religious\nKnowledge",
                                     "MI\nGeographic","MI\nEcon\nCapital",
                                     "MI\nHum\nCapital",
                                     "SD\nKids\nin\nHH\nInt","SD\nSIBS\nInt",                  
                                     "SD\nReligion\nInt"), 
                 facet_args = list(ncol = 1, strip.position = "left"))


plot_1<- p1 + 
  
  ggtitle("Trace Plots for Percentage of Relatives in Social Network Providing Emotional Support")+
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.y = element_line(size = 0.08),
        axis.text.y = element_text(colour="grey8",size=4,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))

plot_1
setwd("C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures/")
library(bayesplot)
library("rstan")
ggsave(plot_1, filename = "C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures/Trace_plot_15.png", 
       width = 10, height = 12, device = "png", 
       dpi = 600,units = "in")
# # Plot the PP checks for key models
# ## posterior predictive checks
# pp <- predict(fit)
# head(pp)

## First one number 1
setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh")
library(tidyverse)
library(brms)
library(readr)
newdata <- read_csv("newdata.csv")
library(ggplot2)
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Geo_distance_non_relatives_ord_cum.rds")
mean(newdata$geo_distance_non_rels,na.rm=T)
library(bayesplot)

color_scheme_set('purple')

p1 <- pp_check(M1, type = "stat")+  
  
  ggtitle("Posterior Predictive Check\nGeographic distance of Non-relatives in Network")+
  
  scale_x_continuous(name="Predicted", limits=c(2.10,2.32),
                     breaks=c(2.15,2.20,2.25,2.30),
                     labels=c('2.15','2.20','2.25','2.30'))+
  
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        legend.position="none",
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"),
        plot.title = element_text(hjust = 0.5))+
  # mean(z$observed)
  annotate("text", x = 2.28, y = 1200, label = "Mean observed\n2.22", colour="black")
p1

setwd("C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures")

ggsave(p1, filename = "PPC1.png", width = 9, height = 7, device = "png", dpi = 600,units = "in")
ggsave(p1, filename = "PPC1.pdf", width = 9, height = 7, device = "pdf", dpi = 600,units = "in")

### next one - number 2
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Geo_distance_relatives_ord_cum.rds")


mean(newdata$geo_distance_rels,na.rm=T)
#2.133599

library(bayesplot)

color_scheme_set('purple')

p1 <- pp_check(M1, type = "stat")+  
  
  ggtitle("Posterior Predictive Check\nGeographic distance of Relatives in Network")+
  scale_x_continuous(name="Predicted", limits=c(2.10,2.25),
                     breaks=c(2.15,2.20,2.25),
                     labels=c('2.15','2.20','2.25')) +
  
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        legend.position="none",
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"),
        plot.title = element_text(hjust = 0.5))+
  # mean(z$observed)
 
  annotate("text", x = 2.15, y = 1200, label = "Mean observed\n2.17", colour="black")

p1

setwd("C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures")

ggsave(p1, filename = "PPC2.png", width = 9, height = 7, device = "png", dpi = 600,units = "in")
ggsave(p1, filename = "PPC2.pdf", width = 9, height = 7, device = "pdf", dpi = 600,units = "in")

#### next one

library(ggplot2)
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/NW_total_lognormal.rds")

library(dplyr)
library(rstanarm)
# path to the folder with the R data files
library(tidyverse)
library(brms)
library(readr)
#####READ in and filter newdata
setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh")
newdata <- read_csv("newdata.csv")
options(scipen=999)

mean(newdata$NW_total,na.rm=T)
#10.30184

library(bayesplot)

color_scheme_set('purple')

p1 <- pp_check(M1, type = "stat")+  
 
  ggtitle("Posterior Predictive Check\nTotal Size of Network")+
  scale_x_continuous(name="Predicted", limits=c(9.75,11.25),
  breaks=c(10,10.25,10.50,10.75,11.00),
  labels=c('10','10.25','10.5','10.75','11')) +
  
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        legend.position="none",
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"),
        plot.title = element_text(hjust = 0.5))+
  # mean(z$observed)

# annotate("text", x = 10.05, y = 1200, label = "Mean predicted\n10.3", colour="black")+
#   geom_vline(xintercept=10.31, linetype="solid", color="red",size=1.5)+
  annotate("text", x = 10.6, y = 1200, label = "Mean observed\n10.3", colour="black")
  
p1

setwd("C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures")

ggsave(p1, filename = "PPC3.png", width = 9, height = 7, device = "png", dpi = 600,units = "in")
ggsave(p1, filename = "PPC3.pdf", width = 9, height = 7, device = "pdf", dpi = 600,units = "in")


## next one  (4th)
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/non_relatives_in_NW_neg_bin.rds")

library(bayesplot)
library(ggplot2)

names(newdata)
mean(newdata$non_rels,na.rm=T)
#1.048556

color_scheme_set('purple')

p1 <- pp_check(M1, type = "stat")+scale_x_continuous(name="Predicted", limits=c(0.8,1.35),
                                                     breaks=c(0.9,1.0,1.1,1.2,1.3),
                                                     labels=c('0.9','1.0','1.1','1.2','1.3')) +
  
  ggtitle("Posterior Predictive Check\nNumber of Non-Relatives in Network")+
  
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        legend.position="none",
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"),
        plot.title = element_text(hjust = 0.5))+
  # mean(z$observed)
  annotate("text", x = 1.15, y = 1200, label = "Mean observed\n1.05", colour="black")
  

p1

setwd("C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures")

ggsave(p1, filename = "PPC4.png", width = 9, height = 7, device = "png", dpi = 600,units = "in")
ggsave(p1, filename = "PPC4.pdf", width = 9, height = 7, device = "pdf", dpi = 600,units = "in")


## next one  5 (15 in total)
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/relatives_in_NW_lognormal.rds")

library(bayesplot)
library(ggplot2)

names(newdata)
mean(newdata$rels_in_NW,na.rm=T)
#9.253281

color_scheme_set('purple')

p1 <- pp_check(M1, type = "stat")+scale_x_continuous(name="Predicted", limits=c(8.75,10.25),
                                                     breaks=c(9,9.25,9.50,9.75,10.00),
                                                     labels=c('9','9.25','9.5','9.75','10')) +
  
  ggtitle("Posterior Predictive Check\nNumber of Relatives in Network")+
  
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        legend.position="none",
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"),
        plot.title = element_text(hjust = 0.5))+
  # mean(z$observed)
  annotate("text", x = 9.0, y = 1200, label = "Mean observed\n9.25", colour="black")
  #annotate("text", x = 9.6, y = 1200, label = "Mean predicted\n9.33", colour="black")+
  #geom_vline(xintercept=9.25, linetype="solid", color="red",size=1.5)

p1

setwd("C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures")

ggsave(p1, filename = "PPC5.png", width = 9, height = 7, device = "png", dpi = 600,units = "in")
ggsave(p1, filename = "PPC5.pdf", width = 9, height = 7, device = "pdf", dpi = 600,units = "in")





## next one  (6th)
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/percent_relatives_in_NW_lognormal.rds")

library(bayesplot)
library(ggplot2)

names(newdata)
mean(newdata$percent_rels_in_NW,na.rm=T)
#0.8341017

color_scheme_set('purple')

p1 <- pp_check(M1, type = "stat")+scale_x_continuous(name="Predicted", limits=c(0.82,0.88),
                                                     breaks=c(0.83,0.84,0.85,0.86,0.87),
                                                     labels=c('83%','84%','85%','86%','87%')) +
  
  ggtitle("Posterior Predictive Check\nPercentage Relatives in Network")+
  
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        legend.position="none",
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"),
        plot.title = element_text(hjust = 0.5))+
  # mean(z$observed)
  annotate("text", x = 0.838, y = 1100, label = "Mean observed\n84.4%", colour="black")

p1

setwd("C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures")

ggsave(p1, filename = "PPC6.png", width = 9, height = 7, device = "png", dpi = 600,units = "in")
ggsave(p1, filename = "PPC6.pdf", width = 9, height = 7, device = "pdf", dpi = 600,units = "in")

#### insert plot 7 - non rel financial
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Non_rels_econ_help_neg_binom.rds")

mean(newdata$non_rels_econ_help,na.rm=T)
#0.29

library(bayesplot)

color_scheme_set('purple')

p1 <- pp_check(M1, type = "stat")+  
  
  ggtitle("Posterior Predictive Check\nNumber of Non-relatives helping financially")+
  scale_x_continuous(name="Predicted", limits=c(0.18,0.6),
                     breaks=c(0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55),
                     labels=c('0.2','0.25','0.3','0.35','0.4','0.45','0.5','0.55')) +
  
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        legend.position="none",
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"),
        plot.title = element_text(hjust = 0.5))+
  # mean(z$observed)
  annotate("text", x = 0.22, y = 1200, label = "Mean observed\n0.29", colour="black")

p1

setwd("C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures")

ggsave(p1, filename = "PPC7.png", width = 9, height = 7, device = "png", dpi = 600,units = "in")
ggsave(p1, filename = "PPC7.pdf", width = 9, height = 7, device = "pdf", dpi = 600,units = "in")

## next one number 8
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/rels_econ_help_poisson.rds")

mean(newdata$rels_econ_help,na.rm=T)
#3.226722

library(bayesplot)

color_scheme_set('purple')

p1 <- pp_check(M1, type = "stat")+  
  
  ggtitle("Posterior Predictive Check\nNumber of relatives helping financially")+
  scale_x_continuous(name="Predicted", limits=c(2.8,3.5),
                     breaks=c(2.9,3.0,3.1,3.2,3.3,3.4),
                     labels=c('2.9','3.0','3.1','3.2','3.3','3.4')) +
  
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        legend.position="none",
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"),
        plot.title = element_text(hjust = 0.5))+
  # mean(z$observed)
  annotate("text", x = 2.95, y = 1200, label = "Mean observed\n3.23", colour="black")

p1

setwd("C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures")

ggsave(p1, filename = "PPC8.png", width = 9, height = 7, device = "png", dpi = 600,units = "in")
ggsave(p1, filename = "PPC8.pdf", width = 9, height = 7, device = "pdf", dpi = 600,units = "in")

### Insert 9 percent rel financial
M1<- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Percent_rels_econ_help_beta.rds")

names(newdata)
mean(newdata$percent_rels_econ_help,na.rm=T)
#0.8581301

color_scheme_set('purple')

p1 <- pp_check(M1, type = "stat")+scale_x_continuous(name="Predicted", limits=c(0.77,0.88),
                                                     breaks=c(0.80,0.82,0.84,0.86,0.88),
                                                     labels=c('80%','82%','84%','86%','88%')) +
  
  ggtitle("Posterior Predictive Check\nPercentage Relatives helping financially")+
  
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        legend.position="none",
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"),
        plot.title = element_text(hjust = 0.5))+
  # mean(z$observed)
  annotate("text", x = 0.838, y = 1100, label = "Mean observed\n85.8%", colour="black")

p1

setwd("C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures")

ggsave(p1, filename = "PPC9.png", width = 9, height = 7, device = "png", dpi = 600,units = "in")
ggsave(p1, filename = "PPC9.pdf", width = 9, height = 7, device = "pdf", dpi = 600,units = "in")

### insert 10 non rels childcare
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/childcare_help_non_rels_neg_binom.rds")

mean(newdata$childcare_help_non_rels,na.rm=T)
#0.0849895

library(bayesplot)

color_scheme_set('purple')

p1 <- pp_check(M1, type = "stat")+  
  
  ggtitle("Posterior Predictive Check\nNumber of non-relatives helping with childcare")+
  scale_x_continuous(name="Predicted", limits=c(0,0.3),
                     breaks=c(0.05,0.1,0.15,0.2,0.25),
                     labels=c('0.05','0.1','0.15','0.2','0.25')) +
  
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        legend.position="none",
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"),
        plot.title = element_text(hjust = 0.5))+
  # mean(z$observed)
  annotate("text", x = 0.05, y = 900, label = "Mean observed\n0.08", colour="black")

p1

setwd("C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures")

ggsave(p1, filename = "PPC10.png", width = 9, height = 7, device = "png", dpi = 600,units = "in")
ggsave(p1, filename = "PPC10.pdf", width = 9, height = 7, device = "pdf", dpi = 600,units = "in")

## next one 
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/childcare_help_rels_neg_binom.rds")

mean(newdata$childcare_help_rels,na.rm=T)
#3.350081

library(bayesplot)

color_scheme_set('purple')

p1 <- pp_check(M1, type = "stat")+  
  
  ggtitle("Posterior Predictive Check\nNumber of relatives helping with childcare")+
  scale_x_continuous(name="Predicted", limits=c(3,3.7),
                     breaks=c(3.1,3.2,3.3,3.4,3.5,3.6),
                     labels=c('3.1','3.2','3.3','3.4','3.5','3.6')) +
  
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        legend.position="none",
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"),
        plot.title = element_text(hjust = 0.5))+
  # mean(z$observed)
  annotate("text", x = 3.2, y = 1200, label = "Mean observed\n3.35", colour="black")

p1

setwd("C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures")

ggsave(p1, filename = "PPC11.png", width = 9, height = 7, device = "png", dpi = 600,units = "in")
ggsave(p1, filename = "PPC11.pdf", width = 9, height = 7, device = "pdf", dpi = 600,units = "in")

### insert 12 percent rels childcare
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/percent_rels_childcare_help_beta.rds")

mean(newdata$childcare_help_rels_percent,na.rm=T)
#0.9420905

color_scheme_set('purple')

p1 <- pp_check(M1, type = "stat")+scale_x_continuous(name="Predicted", limits=c(0.86,0.95),
                                                     breaks=c(0.88,0.90,0.92,0.94),
                                                     labels=c('88%','90%','92%','94%')) +
  
  ggtitle("Posterior Predictive Check\nPercentage relatives helping with childcare")+
  
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        legend.position="none",
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"),
        plot.title = element_text(hjust = 0.5))+
  # mean(z$observed)
  annotate("text", x = 0.93, y = 1100, label = "Mean observed\n94.2%", colour="black")

p1

setwd("C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures")

ggsave(p1, filename = "PPC12.png", width = 9, height = 7, device = "png", dpi = 600,units = "in")
ggsave(p1, filename = "PPC12.pdf", width = 9, height = 7, device = "pdf", dpi = 600,units = "in")

### insert 13 - non rels emot support

M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Non_rels_emot_support_neg_binom.rds")

mean(newdata$emot_support_non_rels,na.rm=T)
# 0.1912887

library(bayesplot)

color_scheme_set('purple')

p1 <- pp_check(M1, type = "stat")+  
  
  ggtitle("Posterior Predictive Check\nNumber of non-relatives providing emotional support")+
  scale_x_continuous(name="Predicted", limits=c(0.05,0.45),
                     breaks=c(0.1,0.15,0.2,0.25,0.3,0.35,0.4),
                     labels=c('0.1','0.15','0.2','0.25','0.3','0.35','0.4')) +
  
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        legend.position="none",
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"),
        plot.title = element_text(hjust = 0.5))+
  # mean(z$observed)
  annotate("text", x = 0.12, y = 900, label = "Mean observed\n0.19", colour="black")

p1

setwd("C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures")

ggsave(p1, filename = "PPC13.png", width = 9, height = 7, device = "png", dpi = 600,units = "in")
ggsave(p1, filename = "PPC13.pdf", width = 9, height = 7, device = "pdf", dpi = 600,units = "in")




##### HERRREEEEE !!!!!!



## next one number 14
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/rels_emot_support_poisson.rds")
mean(newdata$emot_support_rels,na.rm=T)
# 5.985252

library(bayesplot)

color_scheme_set('purple')

p1 <- pp_check(M1, type = "stat")+  
  
  ggtitle("Posterior Predictive Check\nNumber of relatives providing emotional support")+
  scale_x_continuous(name="Predicted", limits=c(5.6,6.35),
                     breaks=c(5.7,5.8,5.9,6.0,6.1,6.2),
                     labels=c('5.7','5.8','5.9','6.0','6.1','6.2')) +
  
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        legend.position="none",
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"),
        plot.title = element_text(hjust = 0.5))+
  # mean(z$observed)
  annotate("text", x = 5.8, y = 1100, label = "Mean observed\n5.98", colour="black")

p1

setwd("C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures")

ggsave(p1, filename = "PPC14.png", width = 9, height = 7, device = "png", dpi = 600,units = "in")
ggsave(p1, filename = "PPC14.pdf", width = 9, height = 7, device = "pdf", dpi = 600,units = "in")


### insert 15 - percent rels emotional support
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/percent_rels_emot_support_beta.rds")

mean(newdata$percent_rels_emot_support,na.rm=T)
#0.9673699

color_scheme_set('purple')

p1 <- pp_check(M1, type = "stat")+scale_x_continuous(name="Predicted", limits=c(0.93,0.98),
                                                     breaks=c(0.94,0.95,0.96,0.97),
                                                     labels=c('94%','95%','96%','97%')) +
  
  ggtitle("Posterior Predictive Check\nPercentage relatives providing emotional support")+
  
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        legend.position="none",
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"),
        plot.title = element_text(hjust = 0.5))+
  # mean(z$observed)
  annotate("text", x = 0.96, y = 1500, label = "Mean observed\n96.7%", colour="black")

p1

setwd("C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures")

ggsave(p1, filename = "PPC15.png", width = 9, height = 7, device = "png", dpi = 600,units = "in")
ggsave(p1, filename = "PPC15.pdf", width = 9, height = 7, device = "pdf", dpi = 600,units = "in")