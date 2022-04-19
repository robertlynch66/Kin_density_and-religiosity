
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

M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_men/model_intx_both_sexes/NW_total_lognormal_sex.rds")
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
         newdata = newdata1, allow_new_levels=TRUE, probs=c(0.11,0.89)) %>%
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
# relabel sex from facet grid
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
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_men/model_intx_both_sexes/NW_total_lognormal_sex.rds")
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
         newdata = newdata2, allow_new_levels=TRUE, probs=c(0.11,0.89)) %>%
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


ggsave(figure1, filename = "Figures/Figure 1 (Total NW size).png", width = 11, height = 6, device = "png", dpi = 600,units = "in")

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


M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_men/model_intx_both_sexes/percent_relatives_in_NW_lognormal_sex.rds")
mu_summary <-
  fitted(M1, 
         newdata = newdata1, allow_new_levels=TRUE, probs=c(0.11,0.89)) %>%
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
                     labels=c("50%","60%",'70%','80%','90%')) +
  
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
         newdata = newdata2, allow_new_levels=TRUE, probs=c(0.11,0.89)) %>%
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


ggsave(figure2, filename = "Figures/Figure 2 (Percent Relatives in NW).png", width = 11, height = 6, device = "png", dpi = 600,units = "in")


####  Number of relatives in NW ### Figure 3

d <- data1[c(2,37,6,8,9,52,51,54,25:27)] 

d <- d[complete.cases(d), ] 



d<-d %>% mutate(rank=ntile(d$religious_knowledge_scale,6))
library(ggthemes)

## make the top plot
Sex_seq <- rep(0:1, each=6)
Religiosity_seq <- rep(1:6,2)


M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_men/model_intx_both_sexes/relatives_in_NW_lognormal_sex.rds")
mu_summary <-
  fitted(M1, 
         newdata = newdata1, allow_new_levels=TRUE, probs=c(0.11,0.89)) %>%
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
         newdata = newdata2, allow_new_levels=TRUE, probs=c(0.11,0.89)) %>%
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


ggsave(figure3, filename = "Figures/Figure 3 (Relatives in NW).png", width = 11, height = 6, device = "png", dpi = 600,units = "in")


#### Non relatives in network Figure 4


d <- data1[c(2,29,6,8,9,52,51,54,25:27)] 

d <- d[complete.cases(d), ] 

d<-d %>% mutate(rank=ntile(d$religious_knowledge_scale,6))
library(ggthemes)

## make the top plot
Sex_seq <- rep(0:1, each=6)
Religiosity_seq <- rep(1:6,2)


M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_men/model_intx_both_sexes/non_relatives_in_NW_neg_bin_sex.rds")
mu_summary <-fitted(M1, 
         newdata = newdata1, allow_new_levels=TRUE, probs=c(0.11,0.89)) %>%
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
         newdata = newdata2, allow_new_levels=TRUE, probs=c(0.11,0.89)) %>%
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

  
  guides(alpha="none")
plot4a


figure <- ggarrange(plot4 + rremove("ylab"), plot4a + rremove("ylab"),
                    labels = NULL,
                    ncol = 1, nrow = 2,
                    common.legend = TRUE, legend = "bottom",
                    align = "hv", 
                    font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))

figure4 <- annotate_figure(figure, left = textGrob("Number of Non-relatives in Network", rot = 90, vjust = 1, gp = gpar(cex = 1.6)))


ggsave(figure4, filename = "Figures/Figure 4 (Non-relatives in NW).png", width = 11, height = 6, device = "png", dpi = 600,units = "in")

###########################################################################
###########################################################################
###########################################################################
###########################################################################
######Geographic distance figures

husbands_new <- read.csv("C:/Users/robert/Dropbox/Github/Intensive extensive kin networks/data/husbands_new.csv", header = T, sep = ",")

husbands_new$religious_knowledge_scale<- scales::rescale(husbands_new$religious_knowledge_scale,to=c(-1,1))


data1 <-  husbands_new %>% filter (Respondent_a=="Wife" | Respondent_a=="Husband")
data2 <-  husbands_new %>% filter (Respondent_a=="Wife")
data3 <-  husbands_new %>% filter (Respondent_a=="Husband")

### Do it for relatives
data1$religious_knowledge_scale <-  data1$religious_knowledge_scale-mean(data1$religious_knowledge_scale, na.rm=T)

data1$Kids_a  <- data1$Kids_a-mean(data1$Kids_a, na.rm=T)  
data1$Mothers_kids_a  <- data1$Mothers_kids_a-mean(data1$Mothers_kids_a, na.rm=T)

###religious knowledge plot on top
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
nonrels <- nr %>% left_join (data3, by=c("id_Questionaire"="idhusband"))
#non_rels <- non_rels[complete.cases(non_rels),]

nonrels$location[nonrels$location==1|nonrels$location==2] <- 2
nonrels$location[nonrels$location==3] <- 3
nonrels$location[nonrels$location==4|nonrels$location==5] <- 4
# Location Codes
# 1=khana member,
# 2=near bari/neighbor
# 3=other place in Matlab
# 4=Other Place in Bangladesh
# 5=Abroad
nonrels<- nonrels[c(1,2,4,5,9,10,12,13,58,55,29,30,31,56)]

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
nonrels2 <- nr %>% left_join (data2, by=c("id_Questionaire"="idwife_a"))
#non_rels2 <- non_rels2[complete.cases(non_rels2),]

nonrels2$location[nonrels2$location==1|nonrels2$location==2] <- 2
nonrels2$location[nonrels2$location==3] <- 3
nonrels2$location[nonrels2$location==4|nonrels2$location==5] <- 4

nonrels2<- nonrels2[c(1,2,3,4,8,9,11,12,57,54,28:30,55)]
### join non-rels to nonrels2 

d <- rbind(nonrels,nonrels2)
# Location Codes
# 1=khana member,
# 2=near bari/neighbor
# 3=other place in Matlab
# 4=Other Place in Bangladesh
# 5=Abroad

d<-d %>% mutate(rank=ntile(d$religious_knowledge_scale,6))
library(ggthemes)

M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_men/results/Geo_distance_non_relatives_ord_cum_sex.rds")

d <- d[complete.cases(d), ] 
attach(d)
rd <- tidyr::crossing(
  religion=mean(religion),
  familyBariReligiousAfter = mean(familyBariReligiousAfter),
  gender_F0_M1_a = c(0,1),
  religious_knowledge_scale=c(-1.0,-0.9,-0.8,-0.7,-0.6,-0.5),
  MI_geo_proximity=mean(MI_geo_proximity),
  MI_economic_capital=mean(MI_economic_capital),
  MI_human_capital=mean(MI_human_capital),
  Kids_a=mean(Kids_a),
  Mothers_kids_a=mean(Mothers_kids_a)) %>%
  as.data.frame()
detach(d)


### fine now make error bars plot
# use new data frame nd

Sex_seq <- c(0,0,0,0,0,0,1,1,1,1,1,1)
Religiosity_seq <- rep(1:6,2)

mu_summary <-
  fitted(M1, 
         newdata = rd, allow_new_levels=TRUE, probs=c(0.11,0.89)) %>%
  as_tibble() %>%
  # let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
  bind_cols(Religiosity_seq) %>% bind_cols(Sex_seq)
# let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
mu_summary

#colnames(mu_summary) <- c('Estimate','Error','Q5','Q95','Religiosity','Sex')


j1 <- mu_summary %>% select (1:4,17,18)
j1$location <- 1
names(j1) <- c("estimate","error","5CI","95CI","religiosity","sex","location")

j2 <- mu_summary %>% select (5:8,17,18)
j2$location <- 2
names(j2) <- c("estimate","error","5CI","95CI","religiosity","sex","location")

j3 <- mu_summary %>% select (9:12,17,18)
j3$location <- 3
names(j3) <- c("estimate","error","5CI","95CI","religiosity","sex","location")

j4 <- mu_summary %>% select (13:16,17,18)
j4$location <- 4
names(j4) <- c("estimate","error","5CI","95CI","religiosity","sex","location")


mu_summary <- rbind(j1,j2,j3,j4)

mu_summary$lower <- mu_summary$`5CI`
mu_summary$upper <- mu_summary$`95CI`

mu_summary <- mu_summary %>% select(1,2,8,9,5,6,7)

mu_summary <- mu_summary[13:48,]

mu_summary$religiosity<-as.integer(mu_summary$religiosity)
mu_summary$sex<-as.integer(mu_summary$sex)
mu_summary$location<-as.numeric(mu_summary$location)


# rejoin to rels (d) ### it didn't link to locations 3-4
Data1 <- d %>% left_join (mu_summary, by =c("rank"="religiosity","gender_F0_M1_a"="sex","location"= "location"))

locations <- c('2' = "Same neighborhood",'3'= "Same municipality",'4' = "Different Municipality")


new <- c("Men", "Women")
names(new) <- c(1,0)
cols <- c("0" = "#000000", "1" = "#0072B2", "2" = "#000000", "3" = "#0072B2")

plot1a <- ggplot(Data1, aes(x=rank, y=estimate,group=factor(gender_F0_M1_a),
                    fill=factor(gender_F0_M1_a),
                    color=factor(gender_F0_M1_a))) +
  
  geom_ribbon(aes(ymin = lower, ymax = upper,alpha=0.7)) +
  
  geom_line() +
  
  facet_grid(location ~ gender_F0_M1_a,labeller=labeller(gender_F0_M1_a=new,location=locations), 
             scales="free",switch = "y")+
  
  scale_color_manual(name = NULL,
                     values = cols,
                     breaks=c(1,0),
                     labels = c("Predicted (95% CI)", "Mean"),
                     guide = guide_legend(override.aes = list(linetype = c(1, 0),
                                                              shape = c(NA, 1),
                                                              color = "grey")))+
  
  
  scale_fill_manual(name="", breaks=c(0),values=cols,
                    labels=c("Predicted (95% CI)")) +
  
  scale_x_continuous(name="Religious knowledge",breaks=c(4,5,6),limits=c(3.5,6.5),
                     labels=c("Low","Medium","High")) +
  
  
  scale_y_continuous(name="Probability of non-kin living in:",breaks=c(0,0.1,0.2,0.3,0.5,0.7,0.9),
                     labels=c("0%","10%","20%","30%","50%","70%","90%")) +
  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=16,face="bold"),
        legend.title = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        
        #strip.text.x = element_blank(),
        strip.text.x = element_text(
          size = 12, color = "black", face = "bold"),
        
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  # guides( colour = guide_legend(override.aes = list(linetype=c(1,0)
  #                                                   , shape=c(NA,16))))+
  guides(alpha="none")+
  guides(fill="none")

plot1a

### relative religiosity plot on bottom
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
# Location Codes
# 1=khana member,
# 2=near bari/neighbor
# 3=other place in Matlab
# 4=Other Place in Bangladesh
# 5=Abroad


M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_men/results/Geo_distance_non_relatives_ord_cum_sex.rds")
# define the `X_s` values you want to condition on
# because the lines are nonlinear, you'll need many of them
#religious_seq <- tibble(religiosity = seq(from = -1, to = 1, by = 1))

d <- d[complete.cases(d), ] 
attach(d)
nd <- tidyr::crossing(
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
### fine now make error bars plot
# use new data frame nd

Sex_seq <- c(0,1,0,1,0,1)
Religiosity_seq <- c(-1,-1,0,0,1,1)

mu_summary <-
  fitted(M1, 
         newdata = nd, allow_new_levels=TRUE, probs=c(0.11,0.89)) %>%
  as_tibble() %>%
  # let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
  bind_cols(Religiosity_seq) %>% bind_cols(Sex_seq)
# let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
mu_summary


j1 <- mu_summary %>% select (1:4,17,18)
j1$location <- 1
names(j1) <- c("estimate","error","5CI","95CI","religiosity","sex","location")

j2 <- mu_summary %>% select (5:8,17,18)
j2$location <- 2
names(j2) <- c("estimate","error","5CI","95CI","religiosity","sex","location")

j3 <- mu_summary %>% select (9:12,17,18)
j3$location <- 3
names(j3) <- c("estimate","error","5CI","95CI","religiosity","sex","location")

j4 <- mu_summary %>% select (13:16,17,18)
j4$location <- 4
names(j4) <- c("estimate","error","5CI","95CI","religiosity","sex","location")


mu_summary <- rbind(j1,j2,j3,j4)

mu_summary$lower <- mu_summary$`5CI`
mu_summary$upper <- mu_summary$`95CI`

mu_summary <- mu_summary %>% select(1,2,8,9,5,6,7)

mu_summary <- mu_summary[7:24,]

mu_summary$religiosity<-as.integer(mu_summary$religiosity)
mu_summary$sex<-as.integer(mu_summary$sex)
mu_summary$location<-as.numeric(mu_summary$location)

# rejoin to non_rels (d)
Data2 <- d %>% left_join (mu_summary, by =c("familyBariReligiousAfter"="religiosity","gender_F0_M1_a"="sex","location"= "location"))


locations <- c('2' = "Same neighborhood",'3'= "Same municipality",'4' = "Different Municipality")


new <- c("Men", "Women")
names(new) <- c(1,0)
cols <- c("0" = "#000000", "1" = "#0072B2", "2" = "#000000", "3" = "#0072B2")

plot1b <- ggplot() +
  
  geom_point(data=Data2,size=3,aes(group=factor(gender_F0_M1_a),colour=factor(gender_F0_M1_a),
                                   x = familyBariReligiousAfter, y = estimate)) +
  
  geom_errorbar(data=Data2, aes(group=factor(gender_F0_M1_a),x=familyBariReligiousAfter,
                                colour=factor(gender_F0_M1_a),ymin=(lower),
                                ymax=(upper)),alpha=0.7, width=0.6,size=1.2) +
  
  
  
  geom_line(data=Data2,size=1.2,aes(group=factor(gender_F0_M1_a),colour=factor(gender_F0_M1_a),
                                    x = familyBariReligiousAfter, y = estimate))+
  
  geom_jitter(data=Data2,shape=1,size=0.6,width=0.2,aes(group=factor(gender_F0_M1_a),colour=factor(gender_F0_M1_a),
                                                        x = familyBariReligiousAfter, y = estimate)) +
  
  facet_grid(location ~ gender_F0_M1_a,labeller=labeller(gender_F0_M1_a=new,location=locations), 
             scales="free",switch = "y")+

  
  scale_color_manual(name = NULL,
                     values = cols,
                     breaks=c(1,0),
                     labels = c("Predicted (95% CI)", "Observed"),
                     guide = guide_legend(override.aes = list(linetype = c(1, 0),
                                                              shape = c(NA, 1),
                                                              color = "black")))+
  
  
  scale_x_continuous(name="Relative religiosity",breaks=c(-1,0,1),limits=c(-1.3,1.3),
                     labels=c("Low","Medium","High")) +
  
  
  scale_y_continuous(name="Probability of non-kin living in:",breaks=c(0,0.1,0.2,0.3,0.5,0.7,0.9),
                     labels=c("0%","10%","20%","30%","50%","70%","90%")) +
  
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

plot1b

######################################################################
# Combine plot 1a and 1b
require(grid)   #
library(ggpubr)
figure5 <- ggarrange(plot1a + rremove("ylab"), plot1b + rremove("ylab"),
                    labels = NULL,
                    ncol = 1, nrow = 2,
                    common.legend = TRUE, legend = "bottom",
                    align = "hv", 
                    font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))

figure5 <- annotate_figure(figure5, left = textGrob("Distance to non-kin", rot = 90, vjust = 1, gp = gpar(cex = 1.6)))


ggsave(figure5, filename = "Figures/Figure 5 (Geo distance non-relatives).png", width = 11, height = 6, device = "png", dpi = 600,units = "in")

#######################################################################
# Figure 6 Geo distance relatives
husbands_new <- read.csv("C:/Users/robert/Dropbox/Github/Intensive extensive kin networks/data/husbands_new.csv", header = T, sep = ",")

husbands_new$religious_knowledge_scale<- scales::rescale(husbands_new$religious_knowledge_scale,to=c(-1,1))


data1 <-  husbands_new %>% filter (Respondent_a=="Wife" | Respondent_a=="Husband")
data2 <-  husbands_new %>% filter (Respondent_a=="Wife")
data3 <-  husbands_new %>% filter (Respondent_a=="Husband")

### Do it for relatives
data1$religious_knowledge_scale <-  data1$religious_knowledge_scale-mean(data1$religious_knowledge_scale, na.rm=T)

data1$Kids_a  <- data1$Kids_a-mean(data1$Kids_a, na.rm=T)  
data1$Mothers_kids_a  <- data1$Mothers_kids_a-mean(data1$Mothers_kids_a, na.rm=T)

###religious knowledge plot on top
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
### join rels to rels2 

d <- rbind(rels,rels2)

d<-d %>% mutate(rank=ntile(d$religious_knowledge_scale,6))
library(ggthemes)

M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_men/results/Geo_distance_relatives_ord_cum_sex.rds")
# define the `X_s` values you want to condition on
# because the lines are nonlinear, you'll need many of them
#religious_seq <- tibble(religiosity = seq(from = -1, to = 1, by = 1))

d <- d[complete.cases(d), ] 
attach(d)
rd <- tidyr::crossing(
  religion=mean(religion),
  familyBariReligiousAfter = mean(familyBariReligiousAfter),
  gender_F0_M1_a = c(0,1),
  religious_knowledge_scale=c(-1.0,-0.9,-0.8,-0.7,-0.6,-0.5),
  MI_geo_proximity=mean(MI_geo_proximity),
  MI_economic_capital=mean(MI_economic_capital),
  MI_human_capital=mean(MI_human_capital),
  Kids_a=mean(Kids_a),
  Mothers_kids_a=mean(Mothers_kids_a)) %>%
  as.data.frame()
detach(d)


### fine now make error bars plot
# use new data frame nd

Sex_seq <- c(0,0,0,0,0,0,1,1,1,1,1,1)
Religiosity_seq <- rep(1:6,2)

mu_summary <-
  fitted(M1, 
         newdata = rd, allow_new_levels=TRUE, probs=c(0.11,0.89)) %>%
  as_tibble() %>%
  # let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
  bind_cols(Religiosity_seq) %>% bind_cols(Sex_seq)
# let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
mu_summary

#colnames(mu_summary) <- c('Estimate','Error','Q5','Q95','Religiosity','Sex')


j1 <- mu_summary %>% select (1:4,17,18)
j1$location <- 1
names(j1) <- c("estimate","error","5CI","95CI","religiosity","sex","location")

j2 <- mu_summary %>% select (5:8,17,18)
j2$location <- 2
names(j2) <- c("estimate","error","5CI","95CI","religiosity","sex","location")

j3 <- mu_summary %>% select (9:12,17,18)
j3$location <- 3
names(j3) <- c("estimate","error","5CI","95CI","religiosity","sex","location")

j4 <- mu_summary %>% select (13:16,17,18)
j4$location <- 4
names(j4) <- c("estimate","error","5CI","95CI","religiosity","sex","location")


mu_summary <- rbind(j1,j2,j3,j4)

mu_summary$lower <- mu_summary$`5CI`
mu_summary$upper <- mu_summary$`95CI`

mu_summary <- mu_summary %>% select(1,2,8,9,5,6,7)

mu_summary <- mu_summary[13:48,]

mu_summary$religiosity<-as.integer(mu_summary$religiosity)
mu_summary$sex<-as.integer(mu_summary$sex)
mu_summary$location<-as.numeric(mu_summary$location)

# rejoin to rels (d) ### it didn't link to locations 3-4
Data3 <- d %>% left_join (mu_summary, by =c("rank"="religiosity","gender_F0_M1_a"="sex","location"= "location"))

locations <- c('2' = "Same neighborhood",'3'= "Same municipality",'4' = "Different Municipality")


new <- c("Men", "Women")
names(new) <- c(1,0)
cols <- c("0" = "#000000", "1" = "#0072B2", "2" = "#000000", "3" = "#0072B2")

plot2a <- ggplot(Data3, aes(x=rank, y=estimate,group=factor(gender_F0_M1_a),
                            fill=factor(gender_F0_M1_a),
                            color=factor(gender_F0_M1_a))) +
  
  geom_ribbon(aes(ymin = lower, ymax = upper,alpha=0.7)) +
  
  geom_line() +
  
  facet_grid(location ~ gender_F0_M1_a,labeller=labeller(gender_F0_M1_a=new,location=locations), 
             scales="free",switch = "y")+
  
  scale_color_manual(name = NULL,
                     values = cols,
                     breaks=c(1,0),
                     labels = c("Predicted (95% CI)", "Mean"),
                     guide = guide_legend(override.aes = list(linetype = c(1, 0),
                                                              shape = c(NA, 1),
                                                              color = "grey")))+
  
  
  scale_fill_manual(name="", breaks=c(0),values=cols,
                    labels=c("Predicted (95% CI)")) +
  
  scale_x_continuous(name="Religious knowledge",breaks=c(2,4,6),limits=c(0.5,6.5),
                     labels=c("Low","Medium","High")) +
  
  
  scale_y_continuous(name="Probability of kin living in:",breaks=c(0,0.1,0.2,0.3,0.5,0.7,0.9),
                     labels=c("0%","10%","20%","30%","50%","70%","90%")) +
  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=16,face="bold"),
        legend.title = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.15, "in"),
        
        strip.text.y = element_text(
          size = 12, color = "black", face = "bold"),
        strip.text.x = element_text(
          size = 12, color = "black", face = "bold"),
        
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold"))+
  # guides( colour = guide_legend(override.aes = list(linetype=c(1,0)
  #                                                   , shape=c(NA,16))))+
  guides(alpha="none")+
  guides(fill="none")

plot2a

#### relative religiosity and geo distance of RELATIVES
d <- d[complete.cases(d), ] 
attach(d)
nd <- tidyr::crossing(
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
### fine now make error bars plot
# use new data frame nd

Sex_seq <- c(0,1,0,1,0,1)
Religiosity_seq <- c(-1,-1,0,0,1,1)

mu_summary <-
  fitted(M1, 
         newdata = nd, allow_new_levels=TRUE, probs=c(0.11,0.89)) %>%
  as_tibble() %>%
  # let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
  bind_cols(Religiosity_seq) %>% bind_cols(Sex_seq)
# let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
mu_summary


j1 <- mu_summary %>% select (1:4,17,18)
j1$location <- 1
names(j1) <- c("estimate","error","5CI","95CI","religiosity","sex","location")

j2 <- mu_summary %>% select (5:8,17,18)
j2$location <- 2
names(j2) <- c("estimate","error","5CI","95CI","religiosity","sex","location")

j3 <- mu_summary %>% select (9:12,17,18)
j3$location <- 3
names(j3) <- c("estimate","error","5CI","95CI","religiosity","sex","location")

j4 <- mu_summary %>% select (13:16,17,18)
j4$location <- 4
names(j4) <- c("estimate","error","5CI","95CI","religiosity","sex","location")


mu_summary <- rbind(j1,j2,j3,j4)

mu_summary$lower <- mu_summary$`5CI`
mu_summary$upper <- mu_summary$`95CI`

mu_summary <- mu_summary %>% select(1,2,8,9,5,6,7)

mu_summary <- mu_summary[7:24,]

mu_summary$religiosity<-as.integer(mu_summary$religiosity)
mu_summary$sex<-as.integer(mu_summary$sex)
mu_summary$location<-as.numeric(mu_summary$location)

# rejoin to non_rels (d)
Data4 <- d %>% left_join (mu_summary, by =c("familyBariReligiousAfter"="religiosity","gender_F0_M1_a"="sex","location"= "location"))


locations <- c('2' = "Same neighborhood",'3'= "Same municipality",'4' = "Different Municipality")


new <- c("Men", "Women")
names(new) <- c(1,0)
cols <- c("0" = "#000000", "1" = "#0072B2", "2" = "#000000", "3" = "#0072B2")

plot2b <- ggplot() +
  
  geom_point(data=Data4,size=3,aes(group=factor(gender_F0_M1_a),colour=factor(gender_F0_M1_a),
                                   x = familyBariReligiousAfter, y = estimate)) +
  
  geom_errorbar(data=Data4, aes(group=factor(gender_F0_M1_a),x=familyBariReligiousAfter,
                                colour=factor(gender_F0_M1_a),ymin=(lower),
                                ymax=(upper)),alpha=0.7, width=0.6,size=1.2) +
  
  
  
  geom_line(data=Data4,size=1.2,aes(group=factor(gender_F0_M1_a),colour=factor(gender_F0_M1_a),
                                    x = familyBariReligiousAfter, y = estimate))+
  
  geom_jitter(data=Data4,shape=1,size=0.6,width=0.2,aes(group=factor(gender_F0_M1_a),colour=factor(gender_F0_M1_a),
                                                        x = familyBariReligiousAfter, y = estimate)) +
  
  facet_grid(location ~ gender_F0_M1_a,labeller=labeller(gender_F0_M1_a=new,location=locations), 
             scales="free",switch = "y")+
  
  
  scale_color_manual(name = NULL,
                     values = cols,
                     breaks=c(1,0),
                     labels = c("Predicted (95% CI)", "Observed"),
                     guide = guide_legend(override.aes = list(linetype = c(1, 0),
                                                              shape = c(NA, 1),
                                                              color = "black")))+
  
  
  scale_x_continuous(name="Relative religiosity",breaks=c(-1,0,1),limits=c(-1.3,1.3),
                     labels=c("Low","Medium","High")) +
  
  
  scale_y_continuous(name="Probability of non-kin living in:",breaks=c(0,0.1,0.2,0.3,0.5,0.7,0.9),
                     labels=c("0%","10%","20%","30%","50%","70%","90%")) +
  
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

plot2b

######################################################################
# Combine plot 1a and 1b
require(grid)   #
library(ggpubr)
figure6 <- ggarrange(plot2a + rremove("ylab"), plot2b + rremove("ylab"),
                     labels = NULL,
                     ncol = 1, nrow = 2,
                     common.legend = TRUE, legend = "bottom",
                     align = "hv", 
                     font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))

figure6 <- annotate_figure(figure5, left = textGrob("Distance to non-kin", rot = 90, vjust = 1, gp = gpar(cex = 1.6)))


ggsave(figure6, filename = "Figures/Figure 6 (Geo distance relatives).png", width = 11, height = 6, device = "png", dpi = 600,units = "in")

##########################################################################################3

###What figures should I make next 
## Emotional, Financial and childcare
### Posterior predictive Checks
### Markov Chains convergence
### Posterior dists (all in one)
## get code from women's NW's 'Figures.R'