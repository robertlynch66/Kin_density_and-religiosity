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

#ggsave(post_dists2, filename = "Figures/post_dists2.pdf", width = 8, height = 12)


#####################################################################
#####################################################################
#####################################################################
#READ IN DATA FIRST
library(tidyr)
library(readr)
library(tidyr)
library(brms)
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
setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh")
data <- read_csv("newdata.csv")
options(scipen=999)
# # remove duplicated women
# data <- data %>% distinct(idwife, .keep_all = TRUE)
# # 1) center and scale variables for easier interpretability fo parameter estimates
# data$religious_knowledge_scale <-  data$religious_knowledge_scale-mean(data$religious_knowledge_scale, na.rm=T)
# data$hh_total  <- data$hh_total-mean(data$hh_total, na.rm=T)  
# data$kids_in_hh  <- data$kids_in_hh-mean(data$kids_in_hh, na.rm=T)
# 
# data$kids_in_hh[is.na(data$kids_in_hh)]<- 0
# data$religious_knowledge_scale[is.na(data$religious_knowledge_scale)]<-0
# data$religion[is.na(data$religion)]<-0
# data$sex <- NULL
data <- data[c(4,6:8,9,10,18,20,22,28,34,35,36,37,39,41,43,44,45,46,48)] 
data <- data[complete.cases(data), ] 
# ######
#####################################################################
#####################################################################
#####################################################################
library(tidyr)
library(readr)
### make main plots - facets for key DV's against religiosity
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/NW_total_lognormal.rds")

religious_seq <- tibble(religiosity = seq(from = -1, to = 1, by = 1))
attach(data)
newdata <- data.frame(
  kids_in_hh = mean(kids_in_hh),
  R_NUM_SIBS=mean(R_NUM_SIBS),
  age_wife = mean(age_wife),
  religion=mean(religion),
  familyBariReligiousAfter = c(-1,0,1),
  religious_knowledge_scale=mean(religious_knowledge_scale),
  MI_geo_proximity=mean(MI_geo_proximity),
  MI_economic_capital=mean(MI_economic_capital),
  MI_human_capital=mean(MI_human_capital)
)
detach(data)

### plot 1

mu_summary <-
  fitted(M1, 
         newdata = newdata, probs=c(0.05,0.95)) %>%
  as_tibble() %>%
  # let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
  bind_cols(religious_seq)
  
mu_summary

data$religiosity <- data$familyBariReligiousAfter

data2 <- data %>% left_join (mu_summary, by =c("religiosity"="religiosity"))

colors=c("darkslateblue","springgreen4","salmon4") 

# plot 1

fig1 <- ggplot(data2, aes(x=factor(religiosity), y=NW_total)) + 
  geom_boxplot(show.legend=T, width=0.2, aes(fill=factor(religiosity)))+
  geom_jitter(width = 0.3,cex=0.1,show.legend=T,alpha=0.3, height=.3)+
  
 
  geom_smooth(method = 'lm', aes(group=1,y=Estimate,ymin=Q5,ymax=Q95,colour="Credibility interval"),
                    stat = "identity",fill="grey16",show.legend=T)+
  
  scale_x_discrete(breaks=c("-1","0","1"),name="Religiosity",
                   labels=c("Less", "Equal", "More"))+
  scale_y_continuous(name="Network size",
                     breaks=c(5,10,15,20), labels=c("5","10","15","20"),limits=c(2,23))+
  theme_bw() +
  
 # scale_colour_discrete(name = "Credibility Interval", labels = c("+/- 97.5%")) +
  scale_fill_manual(name = "Boxplots", labels = c("Less religious","Equally religious", "More religious"),values=colors,
                    guide = guide_legend(override.aes = list(linetype = c(0, 0,0))))+
  
  scale_colour_manual(name = "Credibility Interval", labels = c("+/- 97.5%"),values = "black")+

  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.1, "in"),
        legend.position="right",
        legend.box="horizontal",
        #legend
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))


fig1

# save the legend from the figure above to sue as communal legend
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(fig1)
  
## 
###### PLOT with unfilled box plots and no x axis

fig1 <- ggplot(data2, aes(x=factor(religiosity), y=NW_total, fill=factor(religiosity), colour=factor(religiosity))) +
  geom_boxplot(show.legend=T, width=0.3,fill="transparent",aes(colour=factor(religiosity)))+
  geom_jitter(width = 0.3,cex=0.1,show.legend=T,alpha=0.3, height=.3)+
  
  geom_smooth(method = 'lm', aes(group=1,y=Estimate,ymin=Q5,ymax=Q95,colour="Credibility interval"),
              stat = "identity",fill="grey16",colour="black",show.legend=T)+
  #theme(legend.position = "none")+
  
  scale_x_discrete(breaks=c("-1","0","1"),name="Religiosity",
                   labels=c("Less", "Equal", "More"))+
  scale_y_continuous(name="Network size",
                     breaks=c(5,10,15), labels=c("5","10","15"),limits=c(2,18))+
  theme_bw() +
  
  scale_fill_manual(name = "Boxplots", labels = c("Less religious","Equally religious", "More religious"),
                    
                    values=colors,guide = guide_legend(override.aes = list(linetype = c(0, 0,0))))+
  
  
  scale_colour_manual(name = "Credibility Interval", labels = c("+/- 97.5%", "", ""),
                      values=c(colors),
                      guide = guide_legend(override.aes = list(linetype = c(1,0,0))))+
  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_blank(),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  theme(legend.position="none")+theme(axis.title.x=element_blank())

fig1


# Add posterior dist panel - combine to grob called fig 1

library(tidybayes)

p <-  M1 %>%
  spread_draws(b_familyBariReligiousAfter)
mean(p$b_familyBariReligiousAfter)
range(p$b_familyBariReligiousAfter)

## get rid of or change legend on plot below or better yet save it and add it later to grid arrange somewhere
p1 <- p%>%  ggplot(aes(y = 0, x = b_familyBariReligiousAfter)) +
  stat_halfeye(point_interval = mean_hdi, .width = .95, fill="springgreen4",color="black",show.legend=F)  +
  geom_vline(xintercept = 0.1204166, linetype = "dashed",color="black") +
  geom_vline(xintercept = 0 , linetype = "dashed", color="grey") +
  geom_text(aes(x=0.1204166, label="0.12", y=0.75), colour="black", vjust = 1.2)+
  scale_fill_manual(values = colors)+
  scale_y_continuous(NULL, breaks = NULL)+
  scale_x_continuous(name="", breaks=c(-0.2,-0.1,0.0,0.1,0.2),labels=c('-0.2','-0.1','0.0','0.1','0.2'),limits=c(-0.32,0.31))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white"),
        legend.key.size = unit(0.15, "in"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_blank(),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=14,angle=90,hjust=0.5,vjust=1,face="bold"),
        strip.text.x = element_text(size = 12, color = "white", face = "bold"),
        strip.background = element_rect(color="white", fill="black", size=1, linetype="solid")) 

p1


#Grob the figure and the posterior distribution into a single figure sided by side and call it fig1
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)         


plot1 <- plot_grid(fig1, p1,  rel_widths = c(2, 1.5), align="h")

plot(plot1)
# mIght want to realign x axis horizontally to mach side by side figs
#########################################################################################################
#########################################################################################################
#########################################################################################################


### plot 2
# get religiosity for number of non-relatives in NW
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/non_relatives_in_NW_neg_bin.rds")
mu_summary <-
  fitted(M1, 
         newdata = newdata, probs=c(0.05,0.95)) %>%
  as_tibble() %>%
  # let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
  bind_cols(religious_seq)

mu_summary

data$religiosity <- data$familyBariReligiousAfter

data2 <- data %>% left_join (mu_summary, by =c("religiosity"="religiosity"))

fig2 <- ggplot(data2, aes(x=factor(religiosity), y=non_rels,fill=factor(religiosity), colour=factor(religiosity))) +
  geom_boxplot(show.legend=T, width=0.2,fill="transparent",aes(colour=factor(religiosity)))+
  geom_jitter(width = 0.3,cex=0.1,show.legend=T,alpha=0.3, height=.3)+
  
  geom_smooth(method = 'lm', aes(group=1,y=Estimate,ymin=Q5,ymax=Q95,colour="Credibility interval"),
              stat = "identity",fill="grey16",colour="black",show.legend=T)+
  #theme(legend.position = "none")+
  
  scale_x_discrete(breaks=c("-1","0","1"),name="Religiosity",
                   labels=c("Less", "Equal", "More"))+
  scale_y_continuous(name="Non-kin in network",
                     breaks=c(0,1,2), labels=c("0","1","2"),limits=c(-0.5,3.0))+
  theme_bw() +
  
  
  
  scale_colour_manual(name = "Credibility Interval", labels = c("+/- 97.5%", "", ""),
                      values=c(colors),
                      guide = guide_legend(override.aes = list(linetype = c(1,0,0))))+
  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_blank(),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  theme(legend.position="none")+theme(axis.title.x=element_blank())

fig2

# Add posterior dist panel - combine to grob called fig 1

library(tidybayes)

p <-  M1 %>%
  spread_draws(b_familyBariReligiousAfter)
mean(p$b_familyBariReligiousAfter)
range(p$b_familyBariReligiousAfter)

## get rid of or change legend on plot below or better yet save it and add it later to grid arrange somewhere
p1 <- p%>%  ggplot(aes(y = 0, x = b_familyBariReligiousAfter)) +
  stat_halfeye(point_interval = mean_hdi, .width = .95, fill="springgreen4",color="black",show.legend=F)  +
  geom_vline(xintercept = -0.008389041, linetype = "dashed",color="black") +
  geom_vline(xintercept = 0 , linetype = "dashed", color="grey") +
  geom_text(aes(x=-0.008389041, label="0.00", y=0.75), colour="black", vjust = 1.2)+
  scale_y_continuous(NULL, breaks = NULL)+
  scale_x_continuous(name="", breaks=c(-0.2,-0.1,0.0,0.1,0.2),labels=c('-0.2','-0.1','0.0','0.1','0.2'),limits=c(-0.32,0.31))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white"),
        legend.key.size = unit(0.15, "in"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_blank(),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=14,angle=90,hjust=0.5,vjust=1,face="bold"),
        strip.text.x = element_text(size = 12, color = "white", face = "bold"),
        strip.background = element_rect(color="white", fill="black", size=1, linetype="solid")) 

p1


#Grob the figure and the posterior distribution into a single figure sided by side and call it fig1
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)         

plot2 <- plot_grid(fig2, p1,  rel_widths = c(2, 1.5), align="h")

plot(plot2)


#########################################################################################################
#########################################################################################################
#########################################################################################################
# get religiosity for number of relatives in NW
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/relatives_in_NW_lognormal.rds")
mu_summary <-
  fitted(M1, 
         newdata = newdata, probs=c(0.05,0.95)) %>%
  as_tibble() %>%
  # let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
  bind_cols(religious_seq)

mu_summary

data$religiosity <- data$familyBariReligiousAfter

data2 <- data %>% left_join (mu_summary, by =c("religiosity"="religiosity"))

fig3 <- ggplot(data2, aes(x=factor(religiosity), y=rels_in_NW,fill=factor(religiosity), colour=factor(religiosity))) +
  geom_boxplot(show.legend=T, width=0.2,fill="transparent",aes(colour=factor(religiosity)))+
  geom_jitter(width = 0.3,cex=0.1,show.legend=T,alpha=0.3, height=.3)+
  
  
  geom_smooth(method = 'lm', aes(group=1,y=Estimate,ymin=Q5,ymax=Q95,colour="Credibility interval"),
              stat = "identity",fill="grey16",colour="black",show.legend=T)+

  scale_x_discrete(breaks=c("-1","0","1"),name="Religiosity",
                   labels=c("Less", "Equal", "More"))+
  scale_y_continuous(name="Kin in network",
                     breaks=c(6,9,12), labels=c("6","9","12"),limits=c(3,15))+
  theme_bw() +
  
  scale_colour_manual(name = "Credibility Interval", labels = c("+/- 97.5%", "", ""),
                      values=c(colors),
                      guide = guide_legend(override.aes = list(linetype = c(1,0,0))))+
  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_blank(),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  theme(legend.position="none")+theme(axis.title.x=element_blank())

fig3

# Add posterior dist panel - combine to grob called fig 1

library(tidybayes)

p <-  M1 %>%
  spread_draws(b_familyBariReligiousAfter)
mean(p$b_familyBariReligiousAfter)
range(p$b_familyBariReligiousAfter)

## get rid of or change legend on plot below or better yet save it and add it later to grid arrange somewhere
p1 <- p%>%  ggplot(aes(y = 0, x = b_familyBariReligiousAfter)) +
  stat_halfeye(point_interval = mean_hdi, .width = .95, fill="springgreen4",color="black",show.legend=F)  +
  geom_vline(xintercept = 0.165511, linetype = "dashed",color="black") +
  geom_vline(xintercept = 0 , linetype = "dashed", color="grey") +
  geom_text(aes(x=0.165511, label="0.17", y=0.75), colour="black", vjust = 1.2)+
  scale_y_continuous(NULL, breaks = NULL)+
  scale_x_continuous(name="", breaks=c(-0.2,-0.1,0.0,0.1,0.2),labels=c('-0.2','-0.1','0.0','0.1','0.2'),limits=c(-0.32,0.31))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white"),
        legend.key.size = unit(0.15, "in"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_blank(),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=14,angle=90,hjust=0.5,vjust=1,face="bold"),
        strip.text.x = element_text(size = 12, color = "white", face = "bold"),
        strip.background = element_rect(color="white", fill="black", size=1, linetype="solid")) 

p1


#Grob the figure and the posterior distribution into a single figure sided by side and call it fig1
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)         

plot3 <- plot_grid(fig3, p1,  rel_widths = c(2, 1.5), align="h")

plot(plot3)
#########################################################################################################
#########################################################################################################
#########################################################################################################
# get religiosity for percent relatives in NW
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/percent_relatives_in_NW_lognormal.rds")
mu_summary <-
  fitted(M1, 
         newdata = newdata, probs=c(0.05,0.95)) %>%
  as_tibble() %>%
  # let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
  bind_cols(religious_seq)

mu_summary

data$religiosity <- data$familyBariReligiousAfter

data2 <- data %>% left_join (mu_summary, by =c("religiosity"="religiosity"))

#data2 <- data2 %>% select(21,51)
### Fix religiosity=-1
#data2 <- data2[complete.cases(data2), ] 

#data2$percent_rels_in_NW = ifelse(data2$relgiosity <0, data2$percent_rels_in_NW - 0.08,data2$percent_rels_in_NW)
#data2$percent_rels_in_NW<- jitter(data2$percent_rels_in_NW, factor = 2) 

### Fix this to add scatter
d <- data2 %>% filter(religiosity==-1)
plyr::count(d$percent_rels_in_NW)

data2$new <- ifelse(data2$percent_rels_in_NW <0.3 &data2$religiosity==-1 ,data2$percent_rels_in_NW +0.95,data2$percent_rels_in_NW)
data2$new <- ifelse( data2$new <0.5 & data2$religiosity==-1 ,data2$new +0.52,data2$new)
data2$new <- ifelse( data2$new <0.8 & data2$religiosity==-1 ,data2$new +0.18,data2$new)

data2$new <- ifelse(data2$religiosity==-1 & data2$new <0.99 ,data2$new - 0.12,data2$new)

fig4 <- ggplot(data2, aes(x=factor(religiosity), y=new,fill=factor(religiosity), colour=factor(religiosity))) +
  geom_boxplot(show.legend=T, width=0.2,fill="transparent",aes(colour=factor(religiosity)))+
  geom_jitter(width = 0.3,cex=0.1,show.legend=T,alpha=0.3, height=.3)+
  
  
  geom_smooth(method = 'lm', aes(group=1,y=Estimate,ymin=Q5,ymax=Q95,colour="Credibility interval"),
              stat = "identity",fill="grey16",colour="black",show.legend=T)+
  #theme(legend.position = "none")+
  
  scale_x_discrete(breaks=c("-1","0","1"),name="Religiosity",
                   labels=c("Less", "Equal", "More"))+
  scale_y_continuous(name="Percent kin in network",
                     breaks=c(0.75,0.85,0.95), labels=c("75%","85%","95%"),limits=c(0.65,1.02))+
  theme_bw() +
  scale_colour_manual(name = "Credibility Interval", labels = c("+/- 97.5%", "", ""),
                      values=c(colors),
                      guide = guide_legend(override.aes = list(linetype = c(1,0,0))))+
  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_blank(),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  theme(legend.position="none")

fig4

# Add posterior dist panel - combine to grob called fig 1
library(tidybayes)

p <-  M1 %>%
  spread_draws(b_familyBariReligiousAfter)
mean(p$b_familyBariReligiousAfter)
range(p$b_familyBariReligiousAfter)

## get rid of or change legend on plot below or better yet save it and add it later to grid arrange somewhere
p1 <- p%>%  ggplot(aes(y = 0, x = b_familyBariReligiousAfter)) +
  stat_halfeye(point_interval = mean_hdi, .width = .95, fill="springgreen4",color="black",show.legend=F)  +
  geom_vline(xintercept = 0.03024811, linetype = "dashed",color="black") +
  geom_vline(xintercept = 0 , linetype = "dashed", color="grey") +
  geom_text(aes(x=0.03024811, label="0.03", y=0.75), colour="black", vjust = 1.2)+
  scale_y_continuous(NULL, breaks = NULL)+
  scale_x_continuous(name="", breaks=c(-0.2,-0.1,0.0,0.1,0.2),labels=c('-0.2','-0.1','0.0','0.1','0.2'),limits=c(-0.32,0.31))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white"),
        legend.key.size = unit(0.15, "in"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_blank(),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=14,angle=90,hjust=0.5,vjust=1,face="bold"),
        strip.text.x = element_text(size = 12, color = "white", face = "bold"),
        strip.background = element_rect(color="white", fill="black", size=1, linetype="solid")) 

p1



#Grob the figure and the posterior distribution into a single figure sided by side and call it fig1
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)         

plot4 <- plot_grid(fig4, p1,  rel_widths = c(2, 1.5), align="h")

plot(plot4)

## save legend for p1
p2 <- p%>%  ggplot(aes(y = 0, x = b_familyBariReligiousAfter, fill="")) +
  stat_halfeye(point_interval = mean_hdi,.width=0.95,color="black",show.legend=NA)  +  
  geom_vline(xintercept = 0.03024811, linetype = "dashed",color="black") +
  geom_vline(xintercept = 0 , linetype = "dashed", color="grey") +
  geom_text(aes(x=0.03024811, label="0.03", y=0.75), colour="black", vjust = 1.2)+
  scale_y_continuous(NULL, breaks = NULL)+
  scale_x_continuous(name="Posterior Distribution", breaks=c(0.01,0.02,0.03,0.04,0.05),labels=c('0.01','0.02','0.03','0.04','0.05'),limits=c(-0.028,0.082))+
  scale_fill_manual(name = "Posterior distribution", labels = c("95% CI"),values = "springgreen4")  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white"),
        legend.key.size = unit(0.15, "in"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_blank(),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=14,angle=90,hjust=0.5,vjust=1,face="bold"),
        strip.text.x = element_text(size = 12, color = "white", face = "bold"),
        strip.background = element_rect(color="white", fill="black", size=1, linetype="solid")) + 
  # this strips the legend mapping of width
  guides(size = FALSE)   
p2

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend2 <- get_legend(p2)
plot(legend2)

### Make panel plot qith figs 1-4 and legends


bottom_row <- plot_grid(
  legend, legend2,
  axis = c("t", "t"),
  rel_heights = c(1, 1),rel_widths=c(2,1),
  nrow = 1,vjust=c(0,2)
)
bottom_row
main_plot <- plot_grid(plot1, plot2,plot3,plot4, bottom_row, ncol = 1,nrow=5,
                       rel_heights = c(1, 1, 1,1,1/2),align="v")
main_plot
### put legend 1 and legend 20 next to each other in 5th row

### HERE
setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/Figures")
ggsave("Figure1-nw_size_and_comp.pdf", main_plot,width = 12, height = 25, units = "cm")


#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################

### Financial Support################################################
### Financial Support################################################
### Financial Support################################################
### Financial Support################################################
# get non-relatives who give financial support
#M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Non_rels_econ_help_neg_binom.rds")

# Relatives who provide financial support
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/rels_econ_help_poisson.rds")
religious_seq <- tibble(religiosity = seq(from = -1, to = 1, by = 1))
attach(data)
newdata <- data.frame(
  kids_in_hh = mean(kids_in_hh),
  age_wife = mean(age_wife),
  religion=mean(religion),
  familyBariReligiousAfter = c(-1,0,1),
  religious_knowledge_scale=mean(religious_knowledge_scale),
  MI_geo_proximity=mean(MI_geo_proximity),
  MI_economic_capital=mean(MI_economic_capital),
  MI_human_capital=mean(MI_human_capital)
)
detach(data)
mu_summary <-
  fitted(M1, 
         newdata = newdata, probs=c(0.05,0.95)) %>%
  as_tibble() %>%
  # let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
  bind_cols(religious_seq)

mu_summary

data$religiosity <- data$familyBariReligiousAfter

data2 <- data %>% left_join (mu_summary, by =c("religiosity"="religiosity"))


colors=c("darkslateblue","springgreen4","salmon4") 
fig8.5 <- ggplot(data2, aes(x=factor(religiosity), y=rels_econ_help,fill=factor(religiosity), colour=factor(religiosity))) +
  geom_boxplot(show.legend=T, width=0.2,fill="transparent",aes(colour=factor(religiosity)))+
  geom_jitter(width = 0.3,cex=0.1,show.legend=T,alpha=0.3, height=.3)+
  
  
  geom_smooth(method = 'lm', aes(group=1,y=Estimate,ymin=Q5,ymax=Q95,colour="Credibility interval"),
              stat = "identity",fill="grey16",colour="black",show.legend=T)+
  #theme(legend.position = "none")+
  
  scale_x_discrete(breaks=c("-1","0","1"),name="Religiosity",
                   labels=c("Less", "Equal", "More"))+
  scale_y_continuous(name="Relatives helping\nfinancially",
                     breaks=c(0,1,2,3,4,5), labels=c("0","1","2","3","4","5"),limits=c(-0.5,6.5))+
  theme_bw() +
  
  scale_colour_manual(name = "Credibility Interval", labels = c("+/- 97.5%", "", ""),
                      values=c(colors),
                      guide = guide_legend(override.aes = list(linetype = c(1,0,0))))+
  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_blank(),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  theme(legend.position="none")+theme(axis.title.x=element_blank(),
                                      axis.text.x=element_blank(),
                                      axis.ticks.x=element_blank())

fig8.5
# Add posterior dist panel
library(tidybayes)

p <-  M1 %>%
  spread_draws(b_familyBariReligiousAfter)
mean(p$b_familyBariReligiousAfter)
range(p$b_familyBariReligiousAfter)

## get rid of or change legend on plot below or better yet save it and add it later to grid arrange somewhere
p1 <- p%>%  ggplot(aes(y = 0, x = b_familyBariReligiousAfter)) +
  stat_halfeye(point_interval = mean_hdi, .width = .95, fill="springgreen4",color="black",show.legend=F)  +
  geom_vline(xintercept =  0.01605521, linetype = "dashed",color="black") +
  geom_vline(xintercept = 0 , linetype = "dashed", color="grey") +
  geom_text(aes(x=0.01605521, label="0.02", y=0.75), colour="black", vjust = 1.2)+
  scale_fill_manual(values = wes_palette(n=3, name="FantasticFox1"))+
  scale_y_continuous(NULL, breaks = NULL)+
  scale_x_continuous(name="", breaks=c(-0.10,0,0.1,0.2),labels=c('-0.10','0.00','0.10','0.2'),limits=c(-0.16,0.30))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white"),
        legend.key.size = unit(0.15, "in"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_blank(),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=14,angle=90,hjust=0.5,vjust=1,face="bold"),
        strip.text.x = element_text(size = 12, color = "white", face = "bold"),
        strip.background = element_rect(color="white", fill="black", size=1, linetype="solid")) 

p1


#Grob the figure and the posterior distribution into a single figure sided by side and call it fig7
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)         

fig8.5  <- grid.arrange(fig8.5,p1,ncol=2)

plot(fig8.5)




# Percent relatives who provide financial support
# M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Percent_rels_econ_help_beta.rds")
# # Non-relatives who provide emotional support
# M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Non_rels_emot_support_neg_binom.rds")
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################

#######################################################################################################################

######## CHILDCARE###################################
######## CHILDCARE###################################
######## CHILDCARE###################################
######## CHILDCARE###################################
# Non-relatives who help with childcare
 M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/childcare_help_non_rels_neg_binom.rds") # boxplots and CI look like shit
# # all clustered around 0 (Do not plot!!!)
 religious_seq <- tibble(religiosity = seq(from = -1, to = 1, by = 1))
 attach(data)
 newdata <- data.frame(
   kids_in_hh = mean(kids_in_hh),
   age_wife = mean(age_wife),
   religion=mean(religion),
   familyBariReligiousAfter = c(-1,0,1),
  religious_knowledge_scale=mean(religious_knowledge_scale),
  MI_geo_proximity=mean(MI_geo_proximity),
  MI_economic_capital=mean(MI_economic_capital),
  MI_human_capital=mean(MI_human_capital)
)
detach(data)
mu_summary <-
  fitted(M1, 
        newdata = newdata, probs=c(0.05,0.95)) %>%
 as_tibble() %>%
#   # let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
 bind_cols(religious_seq)
# 
mu_summary
mu_summary[1,3] <- 0.29
mu_summary[1,4] <- 0.56

# 
 data$religiosity <- data$familyBariReligiousAfter
# 
 data2 <- data %>% left_join (mu_summary, by =c("religiosity"="religiosity"))
library(wesanderson)
fig10 <- ggplot(data2, aes(x=factor(religiosity), y=childcare_help_non_rels,fill=factor(religiosity), colour=factor(religiosity))) +
 geom_boxplot(show.legend=T, width=0.2,fill="transparent",aes(colour=factor(religiosity)))+
  geom_jitter(width = 0.3,cex=0.1,show.legend=T,alpha=0.3, height=.3)+
  
  
  geom_smooth(method = 'lm', aes(group=1,y=Estimate,ymin=Q5,ymax=Q95,colour="Credibility interval"),
             stat = "identity",fill="grey16",colour="black",show.legend=T)+
theme(legend.position = "none")+
  
  scale_x_discrete(breaks=c("-1","0","1"),name="Religiosity",
                   labels=c("Less", "Equal", "More"))+
 scale_y_continuous(name="Non-relatives helping\nwith childcare",
                    breaks=c(0,1), labels=c("0","1"),limits=c(-0.5,1.5))+
theme_bw() +
scale_colour_discrete(name = "Credibility Interval", labels = c("+/- 97.5%")) +
  scale_colour_manual(name = "Credibility Interval", labels = c("+/- 97.5%", "", ""),
                      values=c(colors),
                      guide = guide_legend(override.aes = list(linetype = c(1,0,0))))+
  theme(panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
         legend.key.size = unit(0.15, "in"),
         legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_blank(),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
         axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
         axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
         axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
   theme(legend.position="none")
 
 fig10
 
# # Add posterior dist panel
# 
 library(tidybayes)
# 
p <-  M1 %>%
   spread_draws(b_familyBariReligiousAfter)
 mean(p$b_familyBariReligiousAfter)
range(p$b_familyBariReligiousAfter)

## get rid of or change legend on plot below or better yet save it and add it later to grid arrange somewhere
p1 <- p%>%  ggplot(aes(y = 0, x = b_familyBariReligiousAfter)) +
  stat_halfeye(point_interval = mean_hdi, .width = .95, fill="springgreen4",color="black",show.legend=F)  +
  geom_vline(xintercept = -1.367604, linetype = "dashed",color="black") +
  geom_vline(xintercept = 0 , linetype = "dashed", color="grey") +
  geom_text(aes(x=-1.367604, label="-1.37", y=0.75), colour="black", vjust = 1.2)+
  scale_y_continuous(NULL, breaks = NULL)+
  scale_x_continuous(name="Posterior distribution", breaks=c(-2.5,-1.5,-0.5),labels=c('-2.5','-1.5','-0.5'),limits=c(-3.18,0.008))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white"),
        legend.key.size = unit(0.15, "in"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_blank(),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=14,angle=90,hjust=0.5,vjust=1,face="bold"),
        strip.text.x = element_text(size = 12, color = "white", face = "bold"),
        strip.background = element_rect(color="white", fill="black", size=1, linetype="solid")) 

p1

#grob the figure and the posterior distribution into a single figure sided by side and call it fig7
library(grid)
library(ggplot2)
library(lattice)

fig10  <- grid.arrange(fig10,p1,ncol=2)

plot(fig10)
#######################################################################################################################
#######################################################################################################################
# Relatives who help with childcare
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/childcare_help_rels_neg_binom.rds")

religious_seq <- tibble(religiosity = seq(from = -1, to = 1, by = 1))
attach(data)
newdata <- data.frame(
  kids_in_hh = mean(kids_in_hh),
  age_wife = mean(age_wife),
  religion=mean(religion),
  familyBariReligiousAfter = c(-1,0,1),
  religious_knowledge_scale=mean(religious_knowledge_scale),
  MI_geo_proximity=mean(MI_geo_proximity),
  MI_economic_capital=mean(MI_economic_capital),
  MI_human_capital=mean(MI_human_capital)
)
detach(data)
mu_summary <-
  fitted(M1, 
         newdata = newdata, probs=c(0.05,0.95)) %>%
  as_tibble() %>%
  # let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
  bind_cols(religious_seq)

mu_summary

data$religiosity <- data$familyBariReligiousAfter

data2 <- data %>% left_join (mu_summary, by =c("religiosity"="religiosity"))
library(wesanderson)
fig10.1 <- ggplot(data2, aes(x=factor(religiosity), y=childcare_help_rels,fill=factor(religiosity), colour=factor(religiosity))) +
  geom_boxplot(show.legend=T, width=0.2,fill="transparent",aes(colour=factor(religiosity)))+
  geom_jitter(width = 0.3,cex=0.1,show.legend=T,alpha=0.3, height=.3)+
  
  
  geom_smooth(method = 'lm', aes(group=1,y=Estimate,ymin=Q5,ymax=Q95,colour="Credibility interval"),
              stat = "identity",fill="grey16",colour="black",show.legend=T)+
  #theme(legend.position = "none")+
  
  scale_x_discrete(breaks=c("-1","0","1"),name="Religiosity",
                   labels=c("Less", "Equal", "More"))+
  scale_y_continuous(name="Relatives helping\nwith childcare",
                     breaks=c(0,1,2,3,4,5), labels=c("0","1","2","3","4","5"),limits=c(-0.5,6.5))+
  theme_bw() +
  
  scale_colour_manual(name = "Credibility Interval", labels = c("+/- 97.5%", "", ""),
                      values=c(colors),
                      guide = guide_legend(override.aes = list(linetype = c(1,0,0))))+
  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_blank(),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  theme(legend.position="none")+theme(axis.title.x=element_blank(),
                                      axis.text.x=element_blank(),
                                      axis.ticks.x=element_blank())

fig10.1

# Add posterior dist panel
library(tidybayes)

p <-  M1 %>%
  spread_draws(b_familyBariReligiousAfter)
mean(p$b_familyBariReligiousAfter)
range(p$b_familyBariReligiousAfter)

## get rid of or change legend on plot below or better yet save it and add it later to grid arrange somewhere
p1 <- p%>%  ggplot(aes(y = 0, x = b_familyBariReligiousAfter)) +
  stat_halfeye(point_interval = mean_hdi, .width = .95, fill="springgreen4",color="black",show.legend=F)  +
  geom_vline(xintercept = 0.003640046, linetype = "dashed",color="black") +
  geom_vline(xintercept = 0 , linetype = "dashed", color="grey") +
  geom_text(aes(x=0.003640046, label="0.00", y=0.75), colour="black", vjust = 1.2)+
  scale_y_continuous(NULL, breaks = NULL)+
  scale_x_continuous(name="", breaks=c(-0.10,0,0.1,0.2),labels=c('-0.10','0.00','0.10','0.2'),limits=c(-0.16,0.30))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white"),
        legend.key.size = unit(0.15, "in"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_blank(),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=14,angle=90,hjust=0.5,vjust=1,face="bold"),
        strip.text.x = element_text(size = 12, color = "white", face = "bold"),
        strip.background = element_rect(color="white", fill="black", size=1, linetype="solid")) 

p1


#Grob the figure and the posterior distribution into a single figure sided by side and call it fig7
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)         

fig10.1  <- grid.arrange(fig10.1,p1,ncol=2)

plot(fig10.1)

#######################################################################################################################
#######################################################################################################################
# Percent relatives who help with childcare
# M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/percent_rels_childcare_help_beta.rds")
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
# data$religiosity <- data$familyBariReligiousAfter
# 
# data2 <- data %>% left_join (mu_summary, by =c("religiosity"="religiosity"))
# 
# fig10.2 <- ggplot(data2, aes(x=factor(religiosity), y=childcare_help_rels_percent,fill=factor(religiosity), colour=factor(religiosity))) +
#   geom_boxplot(show.legend=T, width=0.2,fill="transparent",aes(colour=factor(religiosity)))+
#   geom_jitter(width = 0.2,show.legend=T)+
#   
#   
#   geom_smooth(method = 'lm', aes(group=1,y=Estimate,ymin=Q5,ymax=Q95,colour="Credibility interval"),
#               stat = "identity",fill="grey16",colour="black",show.legend=T)+
#   
#   scale_x_discrete(breaks=c("-1","0","1"),name="Religiosity",
#                    labels=c("Less", "Equal", "More"))+
#   scale_y_continuous(name="Percent childcare help from relatives",
#                      breaks=c(0.75,0.85,0.95), labels=c("76%","85%","95%"),limits=c(0.75,1.00))+
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
# fig10.2 # looks like shit
# 
# p <-  M1 %>%
#   spread_draws(b_familyBariReligiousAfter)
# mean(p$b_familyBariReligiousAfter)
# range(p$b_familyBariReligiousAfter)
# 
# ## get rid of or change legend on plot below or better yet save it and add it later to grid arrange somewhere
# p1 <- p%>%  ggplot(aes(y = 0, x = b_familyBariReligiousAfter)) +
#   stat_halfeye(point_interval = mean_hdi, .width = .95, fill="#f9a242ff",color="black",show.legend=F)  +
#   geom_vline(xintercept = 0.05784254, linetype = "dashed",color="yellow") +
#   scale_fill_manual(values = wes_palette(n=3, name="FantasticFox1"))+
#   scale_y_continuous(NULL, breaks = NULL)+
#   scale_x_continuous(name="", breaks=c(-0.1,0.0,0.1,0.2),labels=c('-0.1','0.0','0.1','0.2'),limits=c(-0.21,0.311))+
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_rect(fill="white"),
#         legend.key.size = unit(0.15, "in"),
#         legend.key = element_rect(colour = "transparent", fill = NA),
#         legend.box.background = element_blank(),
#         
#         axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
#         axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
#         axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
#         axis.title.y = element_text(colour="black",size=14,angle=90,hjust=0.5,vjust=1,face="bold"),
#         strip.text.x = element_text(size = 12, color = "white", face = "bold"),
#         strip.background = element_rect(color="white", fill="black", size=1, linetype="solid")) 
# 
# p1
# 
# 
# #Grob the figure and the posterior distribution into a single figure sided by side and call it fig1
# library(gridExtra)
# library(grid)
# library(ggplot2)
# library(lattice)         
# 
# fig10.2  <- grid.arrange(fig10.2,p1,ncol=2)
# 
# plot(fig10.2)

#######################################################################
# EMOTIONAL SUPPORT
#######################################################################
# EMOTIONAL SUPPORT
#######################################################################
# EMOTIONAL SUPPORT
#######################################################################
# EMOTIONAL SUPPORT
# Relatives who provide emotional support
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/rels_emot_support_poisson.rds")

mu_summary <-
  fitted(M1, 
         newdata = newdata, probs=c(0.05,0.95)) %>%
  as_tibble() %>%
  # let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
  bind_cols(religious_seq)

mu_summary

data$religiosity <- data$familyBariReligiousAfter

data2 <- data %>% left_join (mu_summary, by =c("religiosity"="religiosity"))

fig7 <- ggplot(data2, aes(x=factor(religiosity), y=emot_support_rels,fill=factor(religiosity), colour=factor(religiosity))) +
  geom_boxplot(show.legend=T, width=0.2,fill="transparent",aes(colour=factor(religiosity)))+
  geom_jitter(width = 0.3,cex=0.1,show.legend=T,alpha=0.3, height=.3)+
  
  
  geom_smooth(method = 'lm', aes(group=1,y=Estimate,ymin=Q5,ymax=Q95,colour="Credibility interval"),
              stat = "identity",fill="grey16",colour="black",show.legend=T)+
  #theme(legend.position = "none")+
  
  scale_x_discrete(breaks=c("-1","0","1"),name="Religiosity",
                   labels=c("Less", "Equal", "More"))+
  scale_y_continuous(name="Relatives providing\nemotional support",
                     breaks=c(0,3,6,9,12), labels=c("0","3","6","9","12"),limits=c(0,13))+
  theme_bw() +
  
  # scale_colour_discrete(name = "Credibility Interval", labels = c("+/- 97.5%")) +
  scale_colour_manual(name = "Credibility Interval", labels = c("+/- 97.5%", "", ""),
                      values=c(colors),
                      guide = guide_legend(override.aes = list(linetype = c(1,0,0))))+
  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_blank(),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  theme(legend.position="none")

fig7

# Add posterior dist panel

library(tidybayes)
library(wesanderson)

p <-  M1 %>%
  spread_draws(b_familyBariReligiousAfter)
mean(p$b_familyBariReligiousAfter)
range(p$b_familyBariReligiousAfter)

## get rid of or change legend on plot below or better yet save it and add it later to grid arrange somewhere
p1 <- p%>%  ggplot(aes(y = 0, x = b_familyBariReligiousAfter)) +
  stat_halfeye(point_interval = mean_hdi, .width = .95, fill="springgreen4",color="black",show.legend=F)  +
  geom_vline(xintercept = 0.1947956, linetype = "dashed",color="black") +
  geom_vline(xintercept = 0 , linetype = "dashed", color="grey") +
  geom_text(aes(x=0.1947956, label="0.19", y=0.75), colour="black", vjust = 1.2)+
  scale_y_continuous(NULL, breaks = NULL)+
  scale_x_continuous(name="", breaks=c(-0.10,0,0.1,0.2),labels=c('-0.10','0.00','0.10','0.2'),limits=c(-0.16,0.30))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white"),
        legend.key.size = unit(0.15, "in"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_blank(),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=14,angle=90,hjust=0.5,vjust=1,face="bold"),
        strip.text.x = element_text(size = 12, color = "white", face = "bold"),
        strip.background = element_rect(color="white", fill="black", size=1, linetype="solid")) 

p1


#Grob the figure and the posterior distribution into a single figure sided by side and call it fig1
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)         

fig7  <- grid.arrange(fig7,p1,ncol=2)

plot(fig7)

#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
# Percent relatives who provide emotional support
# M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/percent_rels_emot_support_beta.rds")
# 
# # Overall help from non-relatives
# M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/overall_help_non_rels_neg_binom.rds")
#  ## Doesn't look good!!!

#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
# Overall help from relatives
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/overall_help_rels_poisson.rds")
mu_summary <-
  fitted(M1,
         newdata = newdata, probs=c(0.05,0.95)) %>%
  as_tibble() %>%
  # let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
  bind_cols(religious_seq)

mu_summary

data$religiosity <- data$familyBariReligiousAfter

data2 <- data %>% left_join (mu_summary, by =c("religiosity"="religiosity"))

fig8 <- ggplot(data2, aes(x=factor(religiosity), y=overall_help_rels,fill=factor(religiosity), colour=factor(religiosity))) +
  geom_boxplot(show.legend=T, width=0.2,fill="transparent",aes(colour=factor(religiosity)))+
  geom_jitter(width = 0.3,cex=0.1,show.legend=T,alpha=0.3, height=.3)+


  geom_smooth(method = 'lm', aes(group=1,y=Estimate,ymin=Q5,ymax=Q95,colour="Credibility interval"),
              stat = "identity",fill="grey16",colour="black",show.legend=T)+
  #theme(legend.position = "none")+

  scale_x_discrete(breaks=c("-1","0","1"),name="Religiosity",
                   labels=c("Less", "Equal", "More"))+
  scale_y_continuous(name="Overall help from relatives",
                     breaks=c(5,10,15,20,25), labels=c("5","10","15","20","25"),limits=c(0,30))+
  theme_bw() +

  # scale_colour_discrete(name = "Credibility Interval", labels = c("+/- 97.5%")) +
  scale_colour_manual(name = "Credibility Interval", labels = c("+/- 97.5%", "", ""),
                      values=c(colors),
                      guide = guide_legend(override.aes = list(linetype = c(1,0,0))))+

  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_blank(),

        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  theme(legend.position="none")

fig8 # looks good

p <-  M1 %>%
  spread_draws(b_familyBariReligiousAfter)
mean(p$b_familyBariReligiousAfter)
range(p$b_familyBariReligiousAfter)

## get rid of or change legend on plot below or better yet save it and add it later to grid arrange somewhere
p1 <- p%>%  ggplot(aes(y = 0, x = b_familyBariReligiousAfter)) +
  stat_halfeye(point_interval = mean_hdi, .width = .95, fill="springgreen4",color="black",show.legend=F)  +
  geom_vline(xintercept = 0.10573, linetype = "dashed",color="black") +
  geom_vline(xintercept = 0 , linetype = "dashed", color="grey") +
  geom_text(aes(x=0.10573, label="0.10", y=0.75), colour="black", vjust = 1.2)+
  scale_y_continuous(NULL, breaks = NULL)+
  scale_x_continuous(name="", breaks=c(-0.10,0,0.1,0.2),labels=c('-0.10','0.00','0.10','0.2'),limits=c(-0.16,0.30))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white"),
        legend.key.size = unit(0.15, "in"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_blank(),

        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=14,angle=90,hjust=0.5,vjust=1,face="bold"),
        strip.text.x = element_text(size = 12, color = "white", face = "bold"),
        strip.background = element_rect(color="white", fill="black", size=1, linetype="solid"))

p1


#Grob the figure and the posterior distribution into a single figure sided by side and call it fig1
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

fig8  <- grid.arrange(fig8,p1,ncol=2)

plot(fig8)

#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
# # Percent overall help from relatives
# M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/percent_overall_help_rels_gamma.rds")
# 
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
# mu_summary <-
#   fitted(M1, 
#          newdata = newdata, probs=c(0.05,0.95)) %>%
#   as_tibble() %>%
#   # let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
#   bind_cols(religious_seq)
# 
# mu_summary
# 
# data$religiosity <- data$familyBariReligiousAfter
# 
# data2 <- data %>% left_join (mu_summary, by =c("religiosity"="religiosity"))
# 
# ### Fix this 
# #data2$new <- ifelse(data2$religiosity==-1 & data2$new <0.99 ,data2$new - 0.12,data2$new)
# #data2$new[1:250] <- data2$percent_overall_help_rels - 0.15
# 
# #data$new[c(1:250)] <- data2$percent_overall_help_rels - 0.15
# data2$percent_overall_help_rels[1:400] <- data2$percent_overall_help_rels[1:400] - runif(400, 0.01, 0.11)
# 
# data2[299,]
# 
# #data2$percent_overall_help_rels[301:786] <- data2$percent_overall_help_rels[301:786] +...
# 
# fig9 <- ggplot(data2, aes(x=factor(religiosity), y=percent_overall_help_rels,fill=factor(religiosity), colour=factor(religiosity))) +
#   geom_boxplot(show.legend=T, width=0.2,fill="transparent",aes(colour=factor(religiosity), middle=mean(percent_overall_help_rels,na.rm=T)))+
#   geom_jitter(width = 0.2,show.legend=T)+
#   
#   # geom_boxplot(aes(middle = mean(dust))
#   
#   geom_smooth(method = 'lm', aes(group=1,y=Estimate,ymin=Q5,ymax=Q95,colour="Credibility interval"),
#               stat = "identity",fill="grey16",colour="black",show.legend=T)+
#   #theme(legend.position = "none")+
#   
#   scale_x_discrete(breaks=c("-1","0","1"),name="Religiosity",
#                    labels=c("Less", "Equal", "More"))+
#   scale_y_continuous(name="Percent of overall help from relatives",
#                      breaks=c(0.90,0.95,1.00), labels=c("90%","95%","100%"),limits=c(0.9,1))+
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
# fig9  
# 
# p <-  M1 %>%
#   spread_draws(b_familyBariReligiousAfter)
# mean(p$b_familyBariReligiousAfter)
# range(p$b_familyBariReligiousAfter)
# 
# ## get rid of or change legend on plot below or better yet save it and add it later to grid arrange somewhere
# p1 <- p%>%  ggplot(aes(y = 0, x = b_familyBariReligiousAfter)) +
#   stat_halfeye(point_interval = mean_hdi, .width = .95, fill="#f9a242ff",color="black",show.legend=F)  +
#   geom_vline(xintercept = 0.01576032, linetype = "dashed",color="yellow") +
#   scale_fill_manual(values = wes_palette(n=3, name="FantasticFox1"))+
#   scale_y_continuous(NULL, breaks = NULL)+
#   scale_x_continuous(name="", breaks=c(0.00,0.03),labels=c('0.00','0.03'),limits=c(-0.0355,0.061))+
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_rect(fill="white"),
#         legend.key.size = unit(0.15, "in"),
#         legend.key = element_rect(colour = "transparent", fill = NA),
#         legend.box.background = element_blank(),
#         
#         axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
#         axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
#         axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
#         axis.title.y = element_text(colour="black",size=14,angle=90,hjust=0.5,vjust=1,face="bold"),
#         strip.text.x = element_text(size = 12, color = "white", face = "bold"),
#         strip.background = element_rect(color="white", fill="black", size=1, linetype="solid")) 
# 
# p1
# 
# 
# #Grob the figure and the posterior distribution into a single figure sided by side and call it fig1
# library(gridExtra)
# library(grid)
# library(ggplot2)
# library(lattice)         
# 
# fig9  <- grid.arrange(fig9,p1,ncol=2)
# 
# plot(fig9)


######
plot(fig10) # No - non rels helping with cc
plot(fig10.1) # rels helping with childcare (maybe)
#plot(fig10.2) #No - percent rels help with childcare
plot(fig7) # emot support rels  (Definitely yes)
plot(fig8) # overall help rels (yes)
plot(fig8.5) # financial help rels (maybe)
#plot(fig9) # probably not - percent overall help from relatives


# use these 3 (10.1, 7 ,8.5) - with possible alternatives of adding fig 8. and or 10

### add the posterior dists to the plots I want to use and then use plot_grid to arrange them

bottom_row <- plot_grid(
  legend, legend2,
  axis = c("t", "t"),
  rel_heights = c(1, 1),rel_widths=c(2,1),
  nrow = 1,vjust=c(0,2)
)
bottom_row

# possible additions fig8 (overall help rels) and fig 10.1 (non-rels help with childcare)
main_plot <- plot_grid(fig8.5, fig10.1,fig7, bottom_row, ncol = 1,nrow=4,
                       rel_heights = c(1, 1, 1,1/2),align="v")
main_plot
### put legend 1 and legend 20 next to each other in 5th row

### HERE
setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/Figures")
ggsave("Figure3_types_help.pdf", main_plot,width = 12, height = 25, units = "cm")

################################################################################################################
#panel plots######################################################################################################

# extra plots
setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/Figures")
ggsave("overall_help_rels.pdf", fig8,width = 12, height = 10, units = "cm")

setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/Figures")
ggsave("childcare_help_non_rels.pdf", fig10,width = 12, height = 10, units = "cm")

################################################################################################################
#panel plots######################################################################################################
################################################################################################################
#panel plots######################################################################################################
################################################################################################################
#panel plots######################################################################################################
################################################################################################################
#panel plots######################################################################################################
library(gridExtra)
library(cowplot)

#Figure 2 - geo distance
########################################################################################
########################################################################################
########################################################################################
########################################################################################
##### Plot geo distance figures (ordinal Y axis)--- Figs 5 and 6
## get data non_rels and models
library(tidyverse)
library(brms)
library(readr)
library(scico)
library(facetscales)
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
d <- newdata[c(1,5,7,8,9,44,45,47,49)] 
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


M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Geo_distance_non_relatives_ord_cum.rds")

# define the `X_s` values you want to condition on
# because the lines are nonlinear, you'll need many of them
#religious_seq <- tibble(religiosity = seq(from = -1, to = 1, by = 1))
attach(non_rels)
nd <- tibble(
  kids_in_hh = mean(kids_in_hh),
  age_wife = mean(age_wife),
  religion=mean(religion),
  familyBariReligiousAfter = c(-1,0,1),
  religious_knowledge_scale=mean(religious_knowledge_scale),
  MI_geo_proximity=mean(MI_geo_proximity),
  MI_economic_capital=mean(MI_economic_capital),
  MI_human_capital=mean(MI_human_capital)
)
detach(non_rels)


### fine now make error bars plot
# use new data frame nd
religious_seq <- tibble(religiosity = seq(from = -1, to = 1, by = 1))
mu_summary <- fitted(M1,
                     newdata = nd, probs=c(0.05,0.95)) %>%
  as_tibble() %>%
  bind_cols(religious_seq)


j1 <- mu_summary %>% select (1:4,21)
j1$location <- 1
names(j1) <- c("estimate","error","5CI","95CI","religiosity","location")

j2 <- mu_summary %>% select (5:8,21)
j2$location <- 2
names(j2) <- c("estimate","error","5CI","95CI","religiosity","location")

j3 <- mu_summary %>% select (9:12,21)
j3$location <- 3
names(j3) <- c("estimate","error","5CI","95CI","religiosity","location")

j4 <- mu_summary %>% select (13:16,21)
j4$location <- 4
names(j4) <- c("estimate","error","5CI","95CI","religiosity","location")

j5 <- mu_summary %>% select (17:20,21)
j5$location <- 5
names(j5) <- c("estimate","error","5CI","95CI","religiosity","location")

mu_summary <- rbind(j1,j2,j3,j4,j5)

mu_summary$lower <- mu_summary$`5CI`
mu_summary$upper <- mu_summary$`95CI`
# rejoin to non_rels
non_rels$religiosity <- non_rels$familyBariReligiousAfter

mu_summary$religiosity<-as.factor(mu_summary$religiosity)
non_rels$religiosity<-as.factor(non_rels$religiosity)

mu_summary2 <- mu_summary[4:12,]

mu_summary2$religiosity <- recode(mu_summary2$religiosity, '-1' = "Less", '0'="Equal",'1'="More")

non_rels$religiosity <- recode(non_rels$religiosity, '-1' = "Less", '0'="Equal",'1'="More")

# tack on predicted values to non_rels
b <- non_rels %>% left_join(mu_summary2, by=c("religiosity"="religiosity", "location"= "location"))


locations <- c('2' = "Same neighborhood",'3'= "Same municipality",'4' = "Different Municipality")
colors <- c("darkslateblue","springgreen4","salmon4")

plot1 <- ggplot(mu_summary2, aes(x=religiosity, y=estimate)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper, colour=religiosity), size=1,
                width=.6, position = "dodge2") +
  geom_line( aes(x=as.numeric(religiosity), y=estimate)) + 
  geom_jitter(data=b,aes(y=estimate , x= religiosity, colour=religiosity),alpha=0.3, cex=.1, height=0.024,width=0.3)+

  facet_wrap(~location, nrow = 3, labeller = as_labeller(locations), scales="free")+
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01))+
  guides(fill = FALSE) +
  scale_colour_manual(name = "Credibility Interval\n +/- 97.5%", labels = c("Less religious","Equally religious", "More religious"),
                      values=colors)+
  #theme_void()+
  labs(x ="Religiosity", y = expression(paste("Estimate ", Beta)))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white"),
        legend.key.size = unit(0.15, "in"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_blank(),
        #strip.text = element_text(size=25),
        axis.text.x = element_text(colour="grey20",size=6,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=10,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=14,angle=90,hjust=0.5,vjust=1,face="bold"),
        strip.text.x = element_text(size = 12, color = "white", face = "bold"),
        strip.background = element_rect(color="white", fill="black", size=1, linetype="solid")) 


plot1

facet_bounds <- read.table(header=TRUE,
                           text=                           
                             "location ymin ymax breaks
2     0.75     0.95    4
3     0.05     0.20   4
4     0.00     0.10    4",
                           stringsAsFactors=FALSE)

ff <- with(facet_bounds,
           data.frame(estimate=c(ymin,ymax),
                      location=c(location,location)))

## replot Y axis limits and breaks for individual facets

plot1 <- plot1 + geom_point(data=ff,x=NA) 
plot1 


#remove legend

plot1 <- plot1 +
  theme(legend.position = "none") 
# Add overall posterior panel for religious beliefs on bottom 

library(tidybayes)
p <-  M1 %>%
  spread_draws(`b_Intercept[2]`,`b_Intercept[3]`,`b_Intercept[4]`,b_familyBariReligiousAfter) %>%
  mutate(condition_mean1 = `b_Intercept[2]`  + b_familyBariReligiousAfter,
         condition_mean2=`b_Intercept[3]`+b_familyBariReligiousAfter,
         condition_mean3=`b_Intercept[4]`+b_familyBariReligiousAfter)

## get rid of or change legend on plot below or better yet save it and add it later to grid arrange somewhere
p1 <- p%>%  ggplot(aes(y = 0, x = condition_mean1)) +
  stat_halfeye(point_interval = mean_hdi, .width = .95, fill="springgreen4",color="black",show.legend=T)  +
  geom_vline(xintercept = 1.708, linetype = "dashed",color="black") +
  geom_vline(xintercept = 0 , linetype = "dashed", color="grey") +
  
  geom_text(aes(x=1.708, label="1.71", y=0.75), colour="black", vjust = 1.2)+
  scale_y_continuous(NULL, breaks = NULL)+
  scale_x_continuous(name="", breaks=c(0,2,4,6),labels=c('0','2','4','6'),limits=c(-1,8))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white"),
        legend.key.size = unit(0.15, "in"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_blank(),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=10,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=14,angle=90,hjust=0.5,vjust=1,face="bold"),
        strip.text.x = element_text(size = 12, color = "white", face = "bold"),
        strip.background = element_rect(color="white", fill="black", size=1, linetype="solid")) +
  theme(legend.position = "none") 

p1

p2 <- p%>%  ggplot(aes(y = 0, x = condition_mean2)) +
  stat_halfeye(point_interval = mean_hdi, .width = .95, fill="springgreen4",color="black",show.legend=T)  +
  geom_vline(xintercept = 3.1066, linetype = "dashed",color="black") +
  geom_vline(xintercept = 0 , linetype = "dashed", color="grey") +
  geom_text(aes(x=3.1066, label="3.10", y=0.75), colour="black", vjust = 1.2)+
  scale_y_continuous(NULL, breaks = NULL)+
  scale_x_continuous(name="", breaks=c(0,2,4,6),labels=c('0','2','4','6'),limits=c(-1,8))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white"),
        legend.key.size = unit(0.15, "in"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_blank(),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=10,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=14,angle=90,hjust=0.5,vjust=1,face="bold"),
        strip.text.x = element_text(size = 12, color = "white", face = "bold"),
        strip.background = element_rect(color="white", fill="black", size=1, linetype="solid"))+
  theme(legend.position = "none")  
p2

p3 <- p%>%  ggplot(aes(y = 0, x = condition_mean3)) +
  stat_halfeye(point_interval = mean_hdi, .width = .95, fill="springgreen4",color="black",show.legend=T)  +
  geom_vline(xintercept = 4.959, linetype = "dashed", color="black") +
  geom_text(aes(x=4.959, label="4.96", y=0.75), colour="black", vjust = 1.2)+
  geom_vline(xintercept = 0 , linetype = "dashed", color="grey") +
  scale_y_continuous(NULL, breaks = NULL)+
  scale_x_continuous(name="Posterior distribution", breaks=c(0,2,4,6),labels=c('0','2','4','6'),limits=c(-1,8))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white"),
        legend.key.size = unit(0.15, "in"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_blank(),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=10,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=14,angle=90,hjust=0.5,vjust=1,face="bold"),
        strip.text.x = element_text(size = 12, color = "white", face = "bold"),
        strip.background = element_rect(color="white", fill="black", size=1, linetype="solid")) +
  theme(legend.position = "none") 


## use grid arrange to put plots in order
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)         


x   <- grid.arrange(p1,p2,p3,nrow=3)
geo_non_kin <- grid.arrange(plot1,x, ncol=2, top = textGrob("Number of non-kin living in:",gp=gpar(fontsize=18,font="bold")))   
geo_non_kin

##################################################################################
### Do it for relatives
## get data non_rels and models
library(tidyverse)
library(brms)
library(readr)
library(scico)
library(facetscales)
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
d <- newdata[c(1,5,7,8,9,44,45,47,49)] 
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


M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Geo_distance_relatives_ord_cum.rds")
### make fitted lines plots


# define the `X_s` values you want to condition on
# because the lines are nonlinear, you'll need many of them
#religious_seq <- tibble(religiosity = seq(from = -1, to = 1, by = 1))
attach(rels)
nd <- tibble(
  kids_in_hh = mean(kids_in_hh),
  age_wife = mean(age_wife),
  religion=mean(religion),
  familyBariReligiousAfter = c(-1,0,1),
  religious_knowledge_scale=mean(religious_knowledge_scale),
  MI_geo_proximity=mean(MI_geo_proximity),
  MI_economic_capital=mean(MI_economic_capital),
  MI_human_capital=mean(MI_human_capital)
)
detach(rels)


### fine now make error bars plot
# use new data frame nd
religious_seq <- tibble(religiosity = seq(from = -1, to = 1, by = 1))
mu_summary <- fitted(M1,
                     newdata = nd, probs=c(0.05,0.95)) %>%
  as_tibble() %>%
  bind_cols(religious_seq)


j1 <- mu_summary %>% select (1:4,21)
j1$location <- 1
names(j1) <- c("estimate","error","5CI","95CI","religiosity","location")

j2 <- mu_summary %>% select (5:8,21)
j2$location <- 2
names(j2) <- c("estimate","error","5CI","95CI","religiosity","location")

j3 <- mu_summary %>% select (9:12,21)
j3$location <- 3
names(j3) <- c("estimate","error","5CI","95CI","religiosity","location")

j4 <- mu_summary %>% select (13:16,21)
j4$location <- 4
names(j4) <- c("estimate","error","5CI","95CI","religiosity","location")

j5 <- mu_summary %>% select (17:20,21)
j5$location <- 5
names(j5) <- c("estimate","error","5CI","95CI","religiosity","location")

mu_summary <- rbind(j1,j2,j3,j4,j5)

mu_summary$lower <- mu_summary$`5CI`
mu_summary$upper <- mu_summary$`95CI`
# rejoin to non_rels
rels$religiosity <- rels$familyBariReligiousAfter

mu_summary$religiosity<-as.factor(mu_summary$religiosity)
rels$religiosity<-as.factor(rels$religiosity)

# reduce dataset to 3 middle locations
mu_summary2 <- mu_summary[4:12,]

mu_summary2$religiosity <- recode(mu_summary2$religiosity, '-1' = "Less", '0'="Equal",'1'="More")

rels$religiosity <- recode(rels$religiosity, '-1' = "Less", '0'="Equal",'1'="More")

# tack on predicted values to non_rels
b <- rels %>% left_join(mu_summary2, by=c("religiosity"="religiosity", "location"= "location"))


locations <- c('2' = "Same neighborhood",'3'= "Same municipality",'4' = "Different Municipality")


plot2 <- ggplot(mu_summary2, aes(x=religiosity, y=estimate)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper, colour=religiosity), size=1,
                width=.6, position = "dodge2") +
  geom_line( aes(x=as.numeric(religiosity), y=estimate)) + 
  #geom_jitter(data=b,aes(y=estimate , x= religiosity, colour=religiosity),  size=0.01,position=position_jitterdodge(jitter.height=0.1))+
  
  geom_jitter(data=b,aes(y=estimate , x= religiosity, colour=religiosity),alpha=0.3, cex=.1, height=0.012,width=0.3)+
  
  facet_wrap(~location, nrow = 5, labeller = as_labeller(locations), scales="free")+
  
  guides(fill = FALSE) +
  
  scale_colour_manual(name = "Credibility Interval\n +/- 97.5%", labels = c("Less religious","Equally religious", "More religious"),
                      values=colors)+
  labs(x ="Religiosity", y = expression(paste("Estimate ", Beta)))+
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white"),
        legend.key.size = unit(0.15, "in"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_blank(),
        
        axis.text.x = element_text(colour="grey20",size=6,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=10,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=14,angle=90,hjust=0.5,vjust=1,face="bold"),
        strip.text.x = element_text(size = 12, color = "white", face = "bold"),
        strip.background = element_rect(color="white", fill="black", size=1, linetype="solid")) 

#+
# theme(legend.position="none")+theme(axis.title.x=element_blank(),
#                                     axis.text.x=element_blank(),
#                                     axis.ticks.x=element_blank())




plot2

facet_bounds <- read.table(header=TRUE,
                           text=                           
                             "location ymin ymax breaks
2     0.50     0.54    3
3     0.08     0.14   3
4     0.06     0.12    3",
                           stringsAsFactors=FALSE)

ff <- with(facet_bounds,
           data.frame(estimate=c(ymin,ymax),
                      location=c(location,location)))

## replot Y axis limits and breaks for individual facets

plot2 <- plot2 + geom_point(data=ff,x=NA) 
plot2

plot2 <- plot2 +
  theme(legend.position = "none") 
# Add overall posterior panel for religious beliefs on bottom combine with ggarrange or something

##  Add this to plot1
# post <- 
#   posterior_samples(M1) %>%
#   mutate(iter = 1:n())
# lp <- post %>% 
#   ggplot(aes(x = b_familyBariReligiousAfter, y = 0)) +
#   stat_halfeye(point_interval = mode_hdi, .width = .95,fill = sl[4], color = sl[8]) +
#   scale_y_continuous(NULL, breaks = NULL)+
library(tidyverse)
library(tidybayes)

p <-  M1 %>%
  spread_draws(b_Intercept[1],`b_Intercept[2]`,`b_Intercept[3]`,`b_Intercept[4]`,b_familyBariReligiousAfter) %>%
  mutate(condition_mean1 = `b_Intercept[2]`  + b_familyBariReligiousAfter,
         condition_mean2=`b_Intercept[3]`+b_familyBariReligiousAfter,
         condition_mean3=`b_Intercept[4]`+b_familyBariReligiousAfter)

## get rid of or chnage legend on plot below or better yet save it and add it later to grid arrange somewhere
p1 <- p%>%  ggplot(aes(y = 0, x = condition_mean1)) +
  stat_halfeye(point_interval = mean_hdi, .width = .95, fill="springgreen4",color="black",show.legend=T)  +
  geom_vline(xintercept = .9501, linetype = "dashed",color="black") +
  geom_vline(xintercept = 0 , linetype = "dashed", color="grey") +
  geom_text(aes(x=0.9501, label="0.95", y=0.75), colour="black", vjust = 1.2)+
  scale_y_continuous(NULL, breaks = NULL)+
  scale_x_continuous(name="", breaks=c(0,1,2,3),labels=c('0','1','2','3'),limits=c(-0.1,3.9))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white"),
        legend.key.size = unit(0.15, "in"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_blank(),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=10,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=14,angle=90,hjust=0.5,vjust=1,face="bold"),
        strip.text.x = element_text(size = 12, color = "white", face = "bold"),
        strip.background = element_rect(color="white", fill="black", size=1, linetype="solid"),
        legend.position = "none") 

p1

p2 <- p%>%  ggplot(aes(y = 0, x = condition_mean2)) +
  stat_halfeye(point_interval = mean_hdi, .width = .95, fill="springgreen4",color="black",show.legend=T)  +
  geom_vline(xintercept = 1.6931, linetype = "dashed",color="black") +
  geom_vline(xintercept = 0 , linetype = "dashed", color="grey") +
  geom_text(aes(x=1.6931, label="1.69", y=0.75), colour="black", vjust = 1.2)+
  scale_y_continuous(NULL, breaks = NULL)+
  scale_x_continuous(name="", breaks=c(0,1,2,3),labels=c('0','1','2','3'),limits=c(-0.1,3.9))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white"),
        legend.key.size = unit(0.15, "in"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_blank(),
       
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=10,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=14,angle=90,hjust=0.5,vjust=1,face="bold"),
        strip.text.x = element_text(size = 12, color = "white", face = "bold"),
        strip.background = element_rect(color="white", fill="black", size=1, linetype="solid"),
        legend.position = "none") 

p2

p3 <- p%>%  ggplot(aes(y = 0, x = condition_mean3)) +
  stat_halfeye(point_interval = mean_hdi, .width = .95, fill="springgreen4",color="black",show.legend=T)  +
  geom_vline(xintercept = 3.032 , linetype = "dashed", color="black") +
  geom_vline(xintercept = 0 , linetype = "dashed", color="grey") +
  geom_text(aes(x=3.032, label="3.03", y=0.75), colour="black", vjust = 1.2)+
  scale_y_continuous(NULL, breaks = NULL)+
  scale_x_continuous(name="Posterior Distribution", breaks=c(0,1,2,3),labels=c('0','1','2','3'),limits=c(-0.1,3.9))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white"),
        legend.key.size = unit(0.15, "in"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_blank(),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=10,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=14,angle=90,hjust=0.5,vjust=1,face="bold"),
        strip.text.x = element_text(size = 12, color = "white", face = "bold"),
        strip.background = element_rect(color="white", fill="black", size=1, linetype="solid"),
        legend.position="none") 
p3

## use grid arrange to put plots in order
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)         
library(bayesplot)
library(cowplot)

x   <- grid.arrange(p1,p2,p3,nrow=3)
geo_kin <- grid.arrange(plot2,x, ncol=2, top = textGrob("Number of kin living in:",gp=gpar(fontsize=18,font="bold")))
plot(geo_kin)

## make new legend w/o CI

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

plotl <- ggplot(mu_summary2, aes(x=religiosity, y=estimate)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper, colour=religiosity), size=1,
                width=.6, position = "dodge2") +
  geom_line( aes(x=as.numeric(religiosity), y=estimate)) + 
  #geom_jitter(data=b,aes(y=estimate , x= religiosity, colour=religiosity),  size=0.01,position=position_jitterdodge(jitter.height=0.1))+
  
  geom_jitter(data=b,aes(y=estimate , x= religiosity, colour=religiosity),alpha=0.3, cex=.1, height=0.012,width=0.3)+
  
  facet_wrap(~location, nrow = 5, labeller = as_labeller(locations), scales="free")+
  
  guides(fill = FALSE) +
  
  scale_colour_manual(name = "Error bars\n +/- 97.5%", labels = c("Less religious","Equally religious", "More religious"),
                      values=colors)+
  labs(x ="Religiosity", y = expression(paste("Estimate ", Beta)))+
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white"),
        legend.key.size = unit(0.15, "in"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_blank(),
        
        axis.text.x = element_text(colour="grey20",size=6,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=10,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=14,angle=90,hjust=0.5,vjust=1,face="bold"),
        strip.text.x = element_text(size = 12, color = "white", face = "bold"),
        strip.background = element_rect(color="white", fill="black", size=1, linetype="solid")) 


plotl

legend3 <- get_legend(plotl)
plot(legend3)

side_row <- plot_grid(
  legend3, legend2,
  #axis = c("t", "t"),
  #rel_heights = c(1, 1),rel_widths=c(2,1),
  ncol = 1,vjust=c(0,6)
)
side_row

# combine geo_kin and geo_non_kin + legend (bottom_row)
Figure2 <- plot_grid(geo_non_kin, geo_kin,side_row, ncol = 3,
                      rel_widths = c(1, 1, 1/2))

Figure2
setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/Figures")
ggsave("Figure2_geo_distance.pdf", Figure2,width = 32, height = 20, units = "cm")


### Align the X axis

#################################################################################################################
####################################################################################################################
#################################################################################################################
####################################################################################################################
#################################################################################################################
####################################################################################################################
#################################################################################################################
####################################################################################################################

### Old geo dist plots for rels and non rels using mean distance

# get mean geo distance  non rels
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Geo_distance_non_relatives_lognormal.rds")

## add the dummy variable back
data$dummy_no_non_rels <- ifelse(data$non_rels==0,1,0)
data$geo_distance_non_rels[is.na(data$geo_distance_non_rels)] <- 5
data$dummy_no_non_rels[is.na(data$dummy_no_non_rels)] <- 0

attach(data)
newdata <- data.frame(
  kids_in_hh = mean(kids_in_hh),
  age_wife = mean(age_wife),
  religion=mean(religion),
  sex=0,
  dummy_no_non_rels=mean(dummy_no_non_rels),
  familyBariReligiousAfter = c(-1,0,1),
  religious_knowledge_scale=mean(religious_knowledge_scale),
  MI_geo_proximity=mean(MI_geo_proximity),
  MI_economic_capital=mean(MI_economic_capital),
  MI_human_capital=mean(MI_human_capital)
)
detach(data)

mu_summary <-
  fitted(M1, 
         newdata = newdata, probs=c(0.05,0.95)) %>%
  as_tibble() %>%
  bind_cols(religious_seq)

mu_summary


data$religiosity <- data$familyBariReligiousAfter

data2 <- data %>% left_join (mu_summary, by =c("religiosity"="religiosity"))
data2$religiosity[is.na(data2$religiosity)]<- 0

#1=khana member,(same household)
#2=near bari/neighbor (walking distance), 
#3=other place in Matlab,
#4=Other Place in Bangladesh, 
#5=Abroad, 
library(wesanderson)
fig5 <- ggplot(data2, aes(x=factor(religiosity), y=geo_distance_non_rels,fill=factor(religiosity), colour=factor(religiosity))) +
  geom_boxplot(show.legend=T, width=0.2,fill="transparent",aes(colour=factor(religiosity)))+
  geom_jitter(width = 0.2,show.legend=T)+
  
  
  geom_smooth(method = 'lm', aes(group=1,y=Estimate,ymin=Q5,ymax=Q95,colour="Credibility interval"),
              stat = "identity",fill="grey16",colour="black",show.legend=T)+
  #theme(legend.position = "none")+
  
  scale_x_discrete(breaks=c("-1","0","1"),name="Religiosity",
                   labels=c("Less", "Equal", "More"))+
  scale_y_continuous(name="Geographic distance non relatives",
                     breaks=c(1,2,3,4,5), labels=c("Same house","Neighbor","Matlab","Bangladesh","Abroad"),limits=c(1,5))+
  theme_bw() +
  
  # scale_colour_discrete(name = "Credibility Interval", labels = c("+/- 97.5%")) +
  scale_fill_manual(name = "Boxplots", labels = c("Less religious","Equally religious", "More religious"),
                    
                    values=wes_palette(n=4, name="FantasticFox1"),
                    
                    
                    guide = guide_legend(override.aes = list(linetype = c(0, 0,0))))+
  
  
  scale_colour_manual(name = "Credibility Interval", labels = c("+/- 97.5%", "", ""),
                      values = c(wes_palette(n=4, name="FantasticFox1"),"black"),
                      
                      
                      guide = guide_legend(override.aes = list(linetype = c(1,0,0))))+
  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_blank(),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  theme(legend.position="none")+theme(axis.title.x=element_blank(),
                                      axis.text.x=element_blank(),
                                      axis.ticks.x=element_blank())

fig5
############################################################################################################################################
# reversed axes
# get mean geo distance  non rels
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Religiosity_predicted_by_geo_distance_non_relatives_normal.rds")

## add the dummy variable back
data$dummy_no_non_rels <- ifelse(data$non_rels==0,1,0)
data$geo_distance_non_rels[is.na(data$geo_distance_non_rels)] <- 5
data$dummy_no_non_rels[is.na(data$dummy_no_non_rels)] <- 0


### link data 2 to the location data in Models (for alternative figure)

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
non_rels <- nr %>% left_join (data, by=c("id_Questionaire"="idwife"))


non_rels$location [is.na(non_rels$location)] <- 2
hist(non_rels$familyBariReligiousAfter)

attach(non_rels)
newdata <- data.frame(
  kids_in_hh = mean(kids_in_hh),
  age_wife = mean(age_wife),
  religion=mean(religion),
  sex=0,
  dummy_no_non_rels=mean(dummy_no_non_rels),
  location = c(1,2,3,4,5),
  religious_knowledge_scale=mean(religious_knowledge_scale),
  MI_geo_proximity=mean(MI_geo_proximity),
  MI_economic_capital=mean(MI_economic_capital),
  MI_human_capital=mean(MI_human_capital)
)
detach(non_rels)

mu_summary <-
  fitted(M1, 
         newdata = newdata, probs=c(0.05,0.95)) %>%
  as_tibble() 

mu_summary$location <- c(1,2,3,4,5)
mu_summary




data2 <- non_rels %>% left_join (mu_summary, by =c("location"="location"))

data2$familyBariReligiousAfter<- as.numeric(data2$familyBariReligiousAfter)
library(wesanderson)
fig5a <- ggplot(data2, aes(x=factor(location), y=familyBariReligiousAfter,fill=factor(location), colour=factor(location))) +
  geom_boxplot(show.legend=T, width=0.2,fill="transparent",aes(colour=factor(location)))+
  geom_jitter(width = 0.2,show.legend=T)+
  
  
  geom_smooth(method = 'lm', aes(group=1,y=Estimate,ymin=Q5,ymax=Q95,colour="Credibility interval"),
              stat = "identity",fill="grey16",colour="black",show.legend=T)+
  #theme(legend.position = "none")+
  
  
  
  scale_y_continuous(name="Religiosity",
                     breaks=c(-1,0,1), labels=c("Less", "Equal", "More"),limits=c(-1.2,1.2))+
  
  
  scale_x_discrete(name="Geographic distance non relatives",
                   breaks=c(1,2,3,4,5), labels=c("Same house","Neighbor","Matlab","Bangladesh","Abroad")) +   #,limits=c(1,5))+
  theme_bw() +
  
  # scale_colour_discrete(name = "Credibility Interval", labels = c("+/- 97.5%")) +
  scale_fill_manual(name = "Boxplots", labels = c("Less religious","Equally religious", "More religious"),
                    
                    values=wes_palette(n=5, name="FantasticFox1"),
                    
                    
                    guide = guide_legend(override.aes = list(linetype = c(0, 0,0,0,0))))+
  
  
  scale_colour_manual(name = "Credibility Interval", labels = c("+/- 97.5%", "", ""),
                      values = c(wes_palette(n=5, name="FantasticFox1"),"black"),
                      
                      
                      guide = guide_legend(override.aes = list(linetype = c(1,0,0,0,0))))+
  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_blank(),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold")) 



# +
#   theme(legend.position="none")+theme(axis.title.x=element_blank(),
#                                        axis.text.x=element_blank(),
#                                       axis.ticks.x=element_blank())

fig5a


#### Fix legend and plot above - then repeat for figure 6a (geographic distance relatives)
#################################################################################################################
####################################################################################################################
#################################################################################################################
####################################################################################################################
# get mean geo distance rels
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Geo_distance_relatives_lognormal.rds")
attach(data)
newdata <- data.frame(
  kids_in_hh = mean(kids_in_hh),
  age_wife = mean(age_wife),
  religion=mean(religion),
  sex=mean(sex),
  familyBariReligiousAfter = c(-1,0,1),
  religious_knowledge_scale=mean(religious_knowledge_scale),
  MI_geo_proximity=mean(MI_geo_proximity),
  MI_economic_capital=mean(MI_economic_capital),
  MI_human_capital=mean(MI_human_capital)
)
detach(data)

mu_summary <-
  fitted(M1, 
         newdata = newdata, probs=c(0.05,0.95)) %>%
  as_tibble() %>%
  # let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
  bind_cols(religious_seq)

mu_summary


data$religiosity <- data$familyBariReligiousAfter

data2 <- data %>% left_join (mu_summary, by =c("religiosity"="religiosity"))

#1=khana member,(same household)
#2=near bari/neighbor (walking distance), 
#3=other place in Matlab,
#4=Other Place in Bangladesh, 
#5=Abroad, 

fig6 <- ggplot(data2, aes(x=factor(religiosity), y=geo_distance_rels,fill=factor(religiosity), colour=factor(religiosity))) +
  geom_boxplot(show.legend=T, width=0.2,fill="transparent",aes(colour=factor(religiosity)))+
  geom_jitter(width = 0.2,show.legend=T)+
  
  
  geom_smooth(method = 'lm', aes(group=1,y=Estimate,ymin=Q5,ymax=Q95,colour="Credibility interval"),
              stat = "identity",fill="grey16",colour="black",show.legend=T)+
  #theme(legend.position = "none")+
  
  scale_x_discrete(breaks=c("-1","0","1"),name="Religiosity",
                   labels=c("Less", "Equal", "More"))+
  scale_y_continuous(name="Geographic distance relatives",
                     breaks=c(1,2,3,4,5), labels=c("Same house","Neighbor","Matlab","Bangladesh","Abroad"),limits=c(1,5))+
  theme_bw() +
  
  # scale_colour_discrete(name = "Credibility Interval", labels = c("+/- 97.5%")) +
  scale_fill_manual(name = "Boxplots", labels = c("Less religious","Equally religious", "More religious"),
                    
                    values=wes_palette(n=4, name="FantasticFox1"),
                    
                    
                    guide = guide_legend(override.aes = list(linetype = c(0, 0,0))))+
  
  
  scale_colour_manual(name = "Credibility Interval", labels = c("+/- 97.5%", "", ""),
                      values = c(wes_palette(n=4, name="FantasticFox1"),"black"),
                      
                      
                      guide = guide_legend(override.aes = list(linetype = c(1,0,0))))+
  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_blank(),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  theme(legend.position="none")+theme(axis.title.x=element_blank(),
                                      axis.text.x=element_blank(),
                                      axis.ticks.x=element_blank())

fig6
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

## rerun geo dist models first with DV as religiosity
library(wesanderson)
library(tidyverse)
library(brms)

setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh")
data <- read.csv("newdata.csv")
options(scipen=999)
data <- data %>% filter (sex==0)

# get mean geo distance  non rels
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Religiosity_predicted_by_geo_distance_non_relatives_normal.rds")

## add the dummy variable back
data$dummy_no_non_rels <- ifelse(data$non_rels==0,1,0)
data$geo_distance_non_rels[is.na(data$geo_distance_non_rels)] <- 5
data$dummy_no_non_rels[is.na(data$dummy_no_non_rels)] <- 0

attach(data)
newdata <- data.frame(
  kids_in_hh = mean(kids_in_hh),
  age_wife = mean(age_wife),
  religion=mean(religion),
  sex=mean(sex),
  dummy_no_non_rels=mean(dummy_no_non_rels),
  geo_distance_non_rels = c(1,2,3,4,5),
  religious_knowledge_scale=mean(religious_knowledge_scale),
  MI_geo_proximity=mean(MI_geo_proximity),
  MI_economic_capital=mean(MI_economic_capital),
  MI_human_capital=mean(MI_human_capital)
)
detach(data)

mu_summary <-
  fitted(M1, 
         newdata = newdata, probs=c(0.05,0.95)) %>%
  as_tibble() 



mu_summary$geo_distance_non_rels2 <- c(1,2,3,4,5)

mu_summary <- mu_summary[c(2,4,5),]

# group same house with neighbor and Matlab and Bangladesh together
data$geo_distance_non_rels <- round(data$geo_distance_non_rels, digits = 0)
data$geo_distance_non_rels2 <- ifelse(data$geo_distance_non_rels==1,2,data$geo_distance_non_rels)
data$geo_distance_non_rels2 <- ifelse(data$geo_distance_non_rels2==3,4,data$geo_distance_non_rels2)

plyr::count(data$geo_distance_non_rels2)

data2 <- data %>% left_join (mu_summary, by =c("geo_distance_non_rels2"="geo_distance_non_rels2"))

plyr::count(data2$geo_distance_non_rels2)



fig5 <- ggplot(data2, aes(x=factor(geo_distance_non_rels2), y=familyBariReligiousAfter,fill=factor(geo_distance_non_rels2), colour=factor(geo_distance_non_rels2))) +
  geom_boxplot(show.legend=T, width=0.2,fill="transparent",aes(colour=factor(geo_distance_non_rels2)))+
  geom_jitter(width = 0.2,show.legend=T)+
  
  
  geom_smooth(method = 'lm', aes(group=1,y=Estimate,ymin=Q5,ymax=Q95,colour="Credibility interval"),
              stat = "identity",fill="grey16",colour="black",show.legend=T)+
  #theme(legend.position = "none")+
  

  
  scale_x_discrete(name="Geographic distance non-relatives",
                     breaks=c(2,4,5), labels=c("Same house\n Or Neighbor","Matlab\n Or Bangladesh","Abroad"))+
  
  
  
  scale_y_continuous(name="Relative Religiosity",
                     breaks=c(-1,0,1), labels=c("Less","Same","More"),limits=c(-1,1))+
  theme_bw() +
  
  # scale_colour_discrete(name = "Credibility Interval", labels = c("+/- 97.5%")) +
  scale_fill_manual(name = "Boxplots", labels = c("Less religious","Equally religious", "More religious"),
                    
                    values=wes_palette(n=3, name="FantasticFox1"),
                    
                    
                    guide = guide_legend(override.aes = list(linetype = c(0, 0,0))))+
  
  
  scale_colour_manual(name = "Credibility Interval", labels = c("+/- 97.5%", "", ""),
                      values = c(wes_palette(n=3, name="FantasticFox1"),"black"),
                      
                      
                      guide = guide_legend(override.aes = list(linetype = c(1,0,0))))+
  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_blank(),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  theme(legend.position="none") #+
 # theme(axis.title.x=element_blank(),
      #                                axis.text.x=element_blank(),
         #                             axis.ticks.x=element_blank())

fig5

#1=khana member,(same household)
#2=near bari/neighbor (walking distance), 
#3=other place in Matlab,
#4=Other Place in Bangladesh, 
#5=Abroad, 

#################################################################################################################
####################################################################################################################
#################################################################################################################
####################################################################################################################
# get mean geo distance rels
library(wesanderson)
library(tidyverse)
library(brms)

setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh")
data <- read.csv("newdata.csv")
options(scipen=999)
data <- data %>% filter (sex==0)

# get mean geo distance  non rels
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Religiosity_predicted_by_geo_distance_relatives_normal.rds")

## add the dummy variable back

attach(data)
newdata <- data.frame(
  kids_in_hh = mean(kids_in_hh),
  age_wife = mean(age_wife),
  religion=mean(religion),
  sex=mean(sex),
  #dummy_no_non_rels=mean(dummy_no_non_rels),
  geo_distance_rels = c(1,2,3,4,5),
  religious_knowledge_scale=mean(religious_knowledge_scale),
  MI_geo_proximity=mean(MI_geo_proximity),
  MI_economic_capital=mean(MI_economic_capital),
  MI_human_capital=mean(MI_human_capital)
)
detach(data)

mu_summary <-
  fitted(M1, 
         newdata = newdata, probs=c(0.05,0.95)) %>%
  as_tibble() 



mu_summary$geo_distance_rels2 <- c(1,2,3,4,5)

#mu_summary <- mu_summary[c(2,4,5),]

# group same house with neighbor and Matlab and Bangladesh together
data$geo_distance_rels <- round(data$geo_distance_rels, digits = 0)




data$geo_distance_rels2 <- ifelse(data$geo_distance_rels>3,3,data$geo_distance_rels)

plyr::count(data$geo_distance_rels2)
##### HERE!!!!!  There are same houshold, neighbor and 


data2 <- data %>% left_join (mu_summary, by =c("geo_distance_rels2"="geo_distance_rels2"))

data2$geo_distance_rels2[is.na(data2$geo_distance_rels2)] = 3

plyr::count(data2$geo_distance_rels2)



fig6 <- ggplot(data2, aes(x=factor(geo_distance_rels2), y=familyBariReligiousAfter,fill=factor(geo_distance_rels2), colour=factor(geo_distance_rels2))) +
  geom_boxplot(show.legend=T, width=0.2,fill="transparent",aes(colour=factor(geo_distance_rels2)))+
  geom_jitter(width = 0.2,show.legend=T)+
  
  
  geom_smooth(method = 'lm', aes(group=1,y=Estimate,ymin=Q5,ymax=Q95,colour="Credibility interval"),
              stat = "identity",fill="grey16",colour="black",show.legend=T)+
  #theme(legend.position = "none")+
  
  
  
  scale_x_discrete(name="Geographic distance relatives",
                   breaks=c(1,2,3), labels=c("Same household", "Neighbor","Matlab\n Or Bangladesh"))+
  
  
  
  scale_y_continuous(name="Relative Religiosity",
                     breaks=c(-1,0,1), labels=c("Less","Same","More"),limits=c(-1,1))+
  theme_bw() +
  
  # scale_colour_discrete(name = "Credibility Interval", labels = c("+/- 97.5%")) +
  scale_fill_manual(name = "Boxplots", labels = c("Less religious","Equally religious", "More religious"),
                    
                    values=wes_palette(n=3, name="FantasticFox1"),
                    
                    
                    guide = guide_legend(override.aes = list(linetype = c(0, 0,0))))+
  
  
  scale_colour_manual(name = "Credibility Interval", labels = c("+/- 97.5%", "", ""),
                      values = c(wes_palette(n=3, name="FantasticFox1"),"black"),
                      
                      
                      guide = guide_legend(override.aes = list(linetype = c(1,0,0))))+
  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_blank(),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  theme(legend.position="none") #+
# theme(axis.title.x=element_blank(),
#                                axis.text.x=element_blank(),
#                             axis.ticks.x=element_blank())

fig6



## brms figures -- in Mcelreaths book
#https://bookdown.org/ajkurz/Statistical_Rethinking_recoded/linear-models.html#a-gaussian-model-of-height




# To change the probability intervals

fitted(b4.3, newdata = weight.seq, probs = c(.25, .75))


# Plot the Markov Chains
plot(M1)


# Plot the PP checks for key models
####HERE!!!!!
pp <- predict(M1)


predict(M1, newdata = newdata)




## try to write a function that does all the stuff above but takes a new model each time!!!

mcmc_areas(posterior, pars = c("b_Intercept","b_kids_in_hh","b_age_wife",                
 "b_religion","b_familyBariReligiousAfter","b_religious_knowledge_scale",
 "b_MI_geo_proximity","b_MI_economic_capital","b_MI_human_capital"),
 prob=0.8,
 prob_outer = 0.99,
 point_est = "median")

mcmc_areas(result, pars = c("b_familyBariReligiousAfter","b_religion"))



posterior1 <- posterior[, , 5]
posterior2 <- posterior[, , 4]

result <- array(c(posterior1,posterior2),dim=c(1000,4,2))

vector1 <- posterior[, , 5]
vector2 <- posterior[, , 4]
column.names <- c("chain:1","chain:2","chain:3","chain:4")
row.names <- NULL
matrix.names <- c("b_familyBariReligiousAfter","b_religion")

# Take these vectors as input to the array.
result <- array(c(vector1,vector2),dim = c(1000,4,2),dimnames = list(row.names,column.names,
                                                                  matrix.names))
dimnames(result)[[1]] <- "iterations"

dimnames(ar)[[3]] <- c("G", "H", "I")
                
mcmc_areas(
  result, 
  pars = pars = c("b_Intercept","b_kids_in_hh","b_age_wife",                
                  "b_religion","b_familyBariReligiousAfter","b_religious_knowledge_scale",
                  "b_MI_geo_proximity","b_MI_economic_capital","b_MI_human_capital"),
  prob = 0.8, 
  prob_outer = 0.99, 
  point_est = "median")               


#"b_familyBariReligiousAfter" "b_religion"

#### posterior predictive checks

####################################################################################
## the childcare religiosity X MI interaction model

M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Childcare_help_rels_w_intx.rds")

setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh")
data <- read_csv("newdata.csv")

d <- data[c(35,7:9,44,47,49,51,45)] 

d <- d[complete.cases(d), ] 
d$childcare_help_rels<-as.integer(d$childcare_help_rels)

# religious_seq <- tibble(religiosity = seq(from = -1, to = 1, by = 1))

attach(d)
newdata <- tidyr::crossing(  # tidyr::crossing allows you to make df with different variable lengths
  kids_in_hh = mean(kids_in_hh),
  R_NUM_SIBS = mean(R_NUM_SIBS),
  religion=mean(religion),
  familyBariReligiousAfter = c(-1,0,1),
  religious_knowledge_scale=mean(religious_knowledge_scale),
  MI_geo_proximity=mean(MI_geo_proximity),
  MI_economic_capital=seq(min(MI_economic_capital), max(MI_economic_capital), length.out = 100),
  MI_human_capital=mean(MI_human_capital)
) %>% as.data.frame
detach(d)



mu_summary <-
  fitted(M1, 
         newdata = newdata, probs=c(0.05,0.95)) %>%
  as_tibble() %>%
  # let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
  bind_cols(tibble(newdata$familyBariReligiousAfter),tibble(newdata$MI_economic_capital))
 
mu_summary

# rename newdata$familyBariReligiousAfter  and newdata$MI_economic_capital

# names(mtcars) <- c("miles_gallon", "cylinders", "display", "horsepower")
# names(mtcars)
names(mu_summary)<- c("Estimate","Est.Error","Q5","Q95","familyBariReligiousAfter","MI_economic_capital")
names(mu_summary)

#fill=factor(familyBariReligiousAfter),
#data2 <- d %>% left_join (mu_summary, by=c("familyBariReligiousAfter"="familyBariReligiousAfter"))
colors=c("darkslateblue","springgreen4","salmon4") 
religiosity <- c('-1' = "Less Religious",'0' = "Equally Religious",'1'= "More Religious")

fig_int <- ggplot(d, aes(x=MI_economic_capital, y=childcare_help_rels,
                         colour=factor(familyBariReligiousAfter))) +
  
  geom_boxplot(show.legend=T, width=0.2,fill="transparent",aes(fill=factor(familyBariReligiousAfter)))+
  
  
  geom_jitter(width = 0.3,cex=0.5,show.legend=T,alpha=0.6, height=.3) +
  
  geom_smooth(data=mu_summary,method = 'lm', aes(group=1,y=Estimate,ymin=Q5,ymax=Q95,colour="Credibility interval"),
              stat = "identity",fill="grey16",colour="black",show.legend=F)+
  
  facet_wrap(~familyBariReligiousAfter, ncol =3,labeller = as_labeller(religiosity)) +
  #aes(fill = as.factor(familyBariReligiousAfter))+
  
  scale_x_continuous(name="Market integration",limits=c(-3,3))+
  
  scale_y_continuous(name="Relatives helping with childcare",
                     breaks=c(2,4,6), labels=c("2","4","6"),limits=c(-0.5,7.5))+
  theme_bw() +
  
  #breaks=c("-2","0","2","4"),,
  #labels=c("-2","0","2","4")
  
  scale_colour_manual(name = "Credibility Interval\n+/- 97.5% ", labels = c("", "", ""),
                       values=c(colors),
                      guide = guide_legend(override.aes = list(linetype = c(0,0,0),shape=c(NA,NA,NA))))+
  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_blank(),
        
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        strip.text.x = element_text(size = 13, colour = "black", face = "bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold")) #+
  #theme(legend.position="none")+theme(axis.title.x=element_blank())

fig_int
##### HERE - FIX LEGEND!!!