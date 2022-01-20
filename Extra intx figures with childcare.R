### new figures dividing helpers from people you help with childcare
## get data
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


attach(d3)
newdata <- tidyr::crossing(  # tidyr::crossing allows you to make df with different variable lengths
  kids_in_hh = mean(kids_in_hh),
  R_NUM_SIBS = mean(R_NUM_SIBS),
  neighbor_rels=mean(neighbor_rels),
  religion=mean(religion),
  familyBariReligiousAfter = c(-1,0,1),
  religious_knowledge_scale=mean(religious_knowledge_scale),
  MI_geo_proximity=mean(MI_geo_proximity),
  MI_economic_capital=seq(min(MI_economic_capital), max(MI_economic_capital), length.out = 100),
  MI_human_capital=mean(MI_human_capital)
) %>% as.data.frame
detach(d3)

# relatives who help
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Rels_who_help_w_childcare_intx_with_neighbors.rds")
#familyBariReligiousAfter:MI_economic_capital    -0.11      0.05    -0.20    -0.02 1.00    16960    11689
# relative you help
M2 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Rels_you_help_w_childcare_intx_with_neighbors.rds")
#familyBariReligiousAfter:MI_economic_capital    -0.06      0.05    -0.17     0.04 1.00    16313    12005
# first do relative who help (M1)
mu_summary <-
  fitted(M1, 
         newdata = newdata, probs=c(0.05,0.95)) %>%
  as_tibble() %>%
  # let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
  bind_cols(tibble(newdata$familyBariReligiousAfter),tibble(newdata$MI_economic_capital))

mu_summary

# rename newdata$familyBariReligiousAfter  and newdata$MI_economic_capital

names(mu_summary)<- c("Estimate","Est.Error","Q5","Q95","familyBariReligiousAfter","MI_economic_capital")
names(mu_summary)

colors=c("darkslateblue","springgreen4","salmon4") 
religiosity <- c('-1' = "Less Religious",'0' = "Equally Religious",'1'= "More Religious")

fig_int_rels <- ggplot(d3, aes(x=MI_economic_capital, y=rels_help_childcare,
                               colour=factor(familyBariReligiousAfter))) +
  
  geom_boxplot(show.legend=T, width=0.2,fill="transparent",aes(fill=factor(familyBariReligiousAfter)))+
  
  
  geom_jitter(width = 0.3,cex=1,show.legend=T,alpha=0.6, height=.3) +
  
  geom_smooth(data=mu_summary,method = 'lm', aes(group=1,y=Estimate,ymin=Q5,ymax=Q95,colour="Credibility interval"),
              stat = "identity",fill="grey16",colour="black",show.legend=F)+
  
  facet_wrap(~familyBariReligiousAfter, ncol =3,labeller = as_labeller(religiosity)) +
  #aes(fill = as.factor(familyBariReligiousAfter))+
  
  scale_x_continuous(name="Market integration",limits=c(-2,2),breaks=c(-1,0,1), labels=c("-1","0","1"))+
  
  scale_y_continuous(name="Relatives helping with childcare",
                     breaks=c(1,2,3), labels=c("1","2","3"),limits=c(0,4.2))+
  theme_bw() +
  
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
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold")) 

fig_int_rels

setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/Figures")
ggsave("Relative_helping_w_childcare_intx_neighbors_rels.pdf", fig_int_rels,width = 24, height = 20, units = "cm")
ggsave("Relative_helping_w_childcare_intx_neighbors_rels.png", fig_int_rels,width = 24, height = 20, units = "cm")



#### next plot those you help with childcare
# then do relatives you help (M2)
mu_summary <-
  fitted(M2, 
         newdata = newdata, probs=c(0.05,0.95)) %>%
  as_tibble() %>%
  # let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
  bind_cols(tibble(newdata$familyBariReligiousAfter),tibble(newdata$MI_economic_capital))

mu_summary

# rename newdata$familyBariReligiousAfter  and newdata$MI_economic_capital

names(mu_summary)<- c("Estimate","Est.Error","Q5","Q95","familyBariReligiousAfter","MI_economic_capital")
names(mu_summary)

colors=c("darkslateblue","springgreen4","salmon4") 
religiosity <- c('-1' = "Less Religious",'0' = "Equally Religious",'1'= "More Religious")

fig_int_rels <- ggplot(d3, aes(x=MI_economic_capital, y=help_rels_childcare,
                               colour=factor(familyBariReligiousAfter))) +
  
  geom_boxplot(show.legend=T, width=0.2,fill="transparent",aes(fill=factor(familyBariReligiousAfter)))+
  
  
  geom_jitter(width = 0.3,cex=1,show.legend=T,alpha=0.6, height=.3) +
  
  geom_smooth(data=mu_summary,method = 'lm', aes(group=1,y=Estimate,ymin=Q5,ymax=Q95,colour="Credibility interval"),
              stat = "identity",fill="grey16",colour="black",show.legend=F)+
  
  facet_wrap(~familyBariReligiousAfter, ncol =3,labeller = as_labeller(religiosity)) +
  #aes(fill = as.factor(familyBariReligiousAfter))+
  
  scale_x_continuous(name="Market integration",limits=c(-2,2),breaks=c(-1,0,1), labels=c("-1","0","1"))+
  
  scale_y_continuous(name="Number of relatives helped with childcare",
                     breaks=c(0,1,2), labels=c("0","1","2"),limits=c(0,3))+
  theme_bw() +
  
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
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="bold")) 

fig_int_rels

setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/Figures")
ggsave("Relatives_you_help_w_childcare_intx_neighbors_rels.pdf", fig_int_rels,width = 24, height = 20, units = "cm")
ggsave("Relatives_you_help_w_childcare_intx_neighbors_rels.png", fig_int_rels,width = 24, height = 20, units = "cm")



##### split panels by women's occupations

#make new models with occupation or use same models but divide by panels

# first link women's occupation back to new data
HHQ <- read_csv("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/HHQpeopleDeID.csv")
library(readr)

HHQ<- HHQ %>% filter(relationToRespondent=="Respondent")
# HHP <- read_csv("C:/Users/robert/Dropbox/PSU postdoc/Access text files Bangladesh/HHQPeopleinNW.csv")
# HHP[is.na(HHP)]<-0
# 
# HHP <- HHP %>% filter(relationship==0)
# 
# HHP <- HHP %>% select (2,)
plyr::count(HHQ$mainOccupation)

# x freq
# 1   0    3
# 2   4    2
# 3   7    5
# 4   8    4
# 5   9    3
# 6  10   19 service
# 7  15    1
# 8  16    1
# 9  18  721  housewife
# 10 19    1
# 11 21    1
# 12 24    2
# 13 NA    1

HHQ$occupation_d <- ifelse(HHQ$mainOccupation==18,0,1)
library(readr)
newdata <- read_csv("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/newdata.csv")

# jobless
# 6 cattle/chicken/duck)
# 12 skilled labor )
# 19 professional (doctor, engineer))
# 1 agriculture (own land))
# 7 handicraft
# 13 remittance 
# 20 student)
# 2 agriculture (share crops)-taken
# 8 tailoring works 
#                                       14 food for work
#                                       21 semi-professional (teacher, nurse)??????)
#                                       3 land mortgage/lease/rent-given
#                                       9 business 
#                                       15 old age allowance/NGD
#                                       22 local government
#                                       4 day labour)
#                                       10 service 
#                                       16 house or shop rent
#                                       23 driver/rickshaw-puller
#                                       5 fishing
#                                       11 pension 
#                                       18 housewife
#                                       24 other (give note) )

#link back to new data

names(newdata)

HHQ <- HHQ %>% select (1,52)

d <- newdata %>% left_join(HHQ, by=c("idwife"="id_Questionaire"))

# the women who work have fewer childcare helpers

d$MI_economic_capital<- as.integer(d$MI_economic_capital)

aggregate(NW_total~MI_economic_capital, data=d, FUN=mean)

# and wealthier people have larger networks with more relatives in them!!!!
                           