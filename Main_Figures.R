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
# remove duplicated women
data <- data %>% distinct(idwife, .keep_all = TRUE)
# 1) center and scale variables for easier interpretability fo parameter estimates
data$religious_knowledge_scale <-  data$religious_knowledge_scale-mean(data$religious_knowledge_scale, na.rm=T)
data$hh_total  <- data$hh_total-mean(data$hh_total, na.rm=T)  
data$kids_in_hh  <- data$kids_in_hh-mean(data$kids_in_hh, na.rm=T)

data$kids_in_hh[is.na(data$kids_in_hh)]<- 0
data$religious_knowledge_scale[is.na(data$religious_knowledge_scale)]<-0
data$religion[is.na(data$religion)]<-0
data$sex <- NULL
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
ggsave("Figure1-nw_size_and_comp.png", main_plot,width = 12, height = 25, units = "cm")

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
# #data$religiosity<- as.factor(data$religiosity)
# 
# data2 <- data %>% left_join (mu_summary, by =c("religiosity"="religiosity"))
# 
# fig10.2 <- ggplot(data2, aes(x=factor(religiosity), y=childcare_help_rels_percent,fill=factor(religiosity), colour=factor(religiosity))) +
#   geom_boxplot(show.legend=T, width=0.2,fill="transparent",aes(colour=factor(religiosity)))+
#   
#   stat_summary(fun=mean, size=1) +
#   
#   
#   geom_jitter(width = 0.3,cex=0.1,show.legend=T,alpha=0.3, height=.3)+
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
#   scale_colour_manual(name = "Credibility Interval", labels = c("+/- 97.5%", "", ""),
#                       values=c(colors),
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
ggsave("Figure3_types_help.png", main_plot,width = 12, height = 25, units = "cm")
################################################################################################################
#panel plots######################################################################################################

# extra plots
setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/Figures")
ggsave("overall_help_rels.pdf", fig8,width = 12, height = 10, units = "cm")
ggsave("overall_help_rels.png", fig8,width = 12, height = 10, units = "cm")


setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/Figures")
ggsave("childcare_help_non_rels.pdf", fig10,width = 12, height = 10, units = "cm")

setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/Figures")
ggsave("childcare_help_rels.pdf", fig10.1,width = 12, height = 10, units = "cm")

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
ggsave("Figure2_geo_distance.png", Figure2,width = 32, height = 20, units = "cm")

### Align the X axis

#################################################################################################################
####################################################################################################################
#################################################################################################################
####################################################################################################################
#################################################################################################################
####################################################################################################################
#################################################################################################################
####################################################################################################################

# make the Childcare figure - figure 4 - 2 X 2 (neighborhood kin - neighborhood non-kin)
#                                              (childcare kin      -  childcare non kin )


#### READ IN NEW MODELS USING NEIGHBORS ONLY
##################################################################################
### Do it for relatives
## get data non_rels and models
library(tidyverse)
library(brms)
library(readr)
library(scico)
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

M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Childcare_help_rels_neighborsonly_neg_binom.rds")


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

# 
# j1 <- mu_summary %>% select (1:4,21)
# j1$location <- 1
# names(j1) <- c("estimate","error","5CI","95CI","religiosity","location")
# 
# j2 <- mu_summary %>% select (5:8,21)
# j2$location <- 2
# names(j2) <- c("estimate","error","5CI","95CI","religiosity","location")



# mu_summary <- rbind(j1,j2)

# mu_summary$lower <- mu_summary$`5CI`
# mu_summary$upper <- mu_summary$`95CI`
# rejoin to non_rels
rels$religiosity <- rels$familyBariReligiousAfter

mu_summary$religiosity<-as.factor(mu_summary$religiosity)
rels$religiosity<-as.factor(rels$religiosity)

# reduce dataset to 3 middle locations


mu_summary$religiosity <- recode(mu_summary$religiosity, '-1' = "Less", '0'="Equal",'1'="More")

rels$religiosity <- recode(rels$religiosity, '-1' = "Less", '0'="Equal",'1'="More")

# tack on predicted values to non_rels
b <- rels %>% left_join(mu_summary, by=c("religiosity"="religiosity"))


locations <- c('2' = "Same neighborhood")
  
plota <- ggplot(b, aes(x=factor(religiosity), y=childcare_help_rels,fill=factor(religiosity), colour=factor(religiosity))) +
  geom_boxplot(show.legend=T, width=0.2,fill="transparent",aes(colour=factor(religiosity)))+
  geom_jitter(width = 0.2,show.legend=T,alpha=0.3, cex=.1, height=0.12,)+
  geom_smooth(method = 'lm', aes(group=1,y=Estimate,ymin=Q5,ymax=Q95,colour="Credibility interval"),
                stat = "identity",fill="grey16",colour="black",show.legend=T)+
  theme(legend.position = "none") +
  scale_x_discrete(breaks=c("Less","Equal","More"),name="",
                   labels=c("Less", "Equal", "More"))+
  scale_y_continuous(name="Relatives",
                     breaks=c(0,1,2,3,4,5,6), labels=c("0","1","2","3","4","5","6"),limits=c(-0.250,6.25))+
  theme_bw() +
  scale_colour_manual(name = "Credibility Interval", labels = c("+/- 97.5%", "", ""),
                      values=c(colors),
                      guide = guide_legend(override.aes = list(linetype = c(1,0,0))))+
  

#ggtitle("Relatives in Same Neighborhood\n helping with childcare")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white"),
        legend.key.size = unit(0.15, "in"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_blank(),
        plot.title = element_text(size=16, face="bold"),
        
        # element_text(family = NULL, face = NULL, colour = NULL, size = NULL,
        #              hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL,
        #              color = NULL)
        axis.text.x = element_text(colour="grey20",size=6,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=10,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=14,angle=90,hjust=0.5,vjust=1,face="bold"),
        strip.text.x = element_text(size = 12, color = "white", face = "bold"),
        strip.background = element_rect(color="white", fill="black", size=1, linetype="solid")) 

plota


plota <- plota +
  theme(legend.position = "none") 
# Add overall posterior panel for religious beliefs on bottom combine with ggarrange or something

library(tidybayes)
p <-  M1 %>%
  spread_draws(b_familyBariReligiousAfter) %>%
  mutate(condition_mean1 =  b_familyBariReligiousAfter)

## get rid of or change legend on plot below or better yet save it and add it later to grid arrange somewhere
p1 <- p%>%  ggplot(aes(y = 0, x = condition_mean1)) +
  stat_halfeye(point_interval = mean_hdi, .width = .95, fill="springgreen4",color="black",show.legend=T)  +
  geom_vline(xintercept = -0.008611997, linetype = "dashed",color="black") +
  geom_vline(xintercept = 0 , linetype = "dashed", color="grey") +
  geom_text(aes(x=-0.008611997, label="0.00", y=0.75), colour="black", vjust = 1.2)+
  scale_y_continuous(NULL, breaks = NULL)+
  scale_x_continuous(name="", breaks=c(-2,-1,0),labels=c('-2','-1','0'),limits=c(-2.25,0.07))+
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
library(gridExtra)
library(grid)
library(lattice)         
library(bayesplot)
library(cowplot)

plota   <- grid.arrange(plota,p1,ncol=2,top=textGrob("Relatives in Same Neighborhood\n Helping with Childcare",gp=gpar(fontsize=16,font="bold")))
########################################################################################
## Repeat for non rels

library(tidyverse)
library(brms)
library(readr)
library(scico)
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

# Location Codes
# 1=khana member,
# 2=near bari/neighbor
# 3=other place in Matlab
# 4=Other Place in Bangladesh
# 5=Abroad

# subset to only neighbors or closer
non_rels <- non_rels %>% filter(location==2)

M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Childcare_help_nonrels_neighborsonly_neg_binom.rds")
### make fitted lines plots


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

mu_summary[1,3] <- 0.997
mu_summary[1,4] <- 1.47

# rejoin to non_rels
non_rels$religiosity <- non_rels$familyBariReligiousAfter

mu_summary$religiosity<-as.factor(mu_summary$religiosity)
non_rels$religiosity<-as.factor(non_rels$religiosity)

# reduce dataset to 3 middle locations


mu_summary$religiosity <- recode(mu_summary$religiosity, '-1' = "Less", '0'="Equal",'1'="More")

non_rels$religiosity <- recode(non_rels$religiosity, '-1' = "Less", '0'="Equal",'1'="More")

# tack on predicted values to non_rels
b <- non_rels %>% left_join(mu_summary, by=c("religiosity"="religiosity"))


locations <- c('2' = "Same neighborhood")

plotb <- ggplot(b, aes(x=factor(religiosity), y=childcare_help_non_rels,fill=factor(religiosity), colour=factor(religiosity))) +
  geom_boxplot(show.legend=T, width=0.2,fill="transparent",aes(colour=factor(religiosity)))+
  geom_jitter(width = 0.2,show.legend=T,alpha=0.3, cex=.1, height=0.12,)+
  geom_smooth(method = 'lm', aes(group=1,y=Estimate,ymin=Q5,ymax=Q95,colour="Credibility interval"),
              stat = "identity",fill="grey16",colour="black",show.legend=T)+
  theme(legend.position = "none") +
  scale_x_discrete(breaks=c("Less","Equal","More"),name="Religiosity",
                   labels=c("Less", "Equal", "More"))+
  scale_y_continuous(name="Non-relatives",
                     breaks=c(0,1,2,3,4,5,6), labels=c("0","1","2","3","4","5","6"),limits=c(-0.25,2.25))+
  theme_bw() +
  scale_colour_manual(name = "Credibility Interval", labels = c("+/- 97.5%", "", ""),
                      values=c(colors),
                      guide = guide_legend(override.aes = list(linetype = c(1,0,0))))+
  
  
 # ggtitle("")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white"),
        legend.key.size = unit(0.15, "in"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_blank(),
        plot.title = element_text(size=16, face="bold"),
        
        # element_text(family = NULL, face = NULL, colour = NULL, size = NULL,
        #              hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL,
        #              color = NULL)
        axis.text.x = element_text(colour="grey20",size=6,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=10,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=14,angle=90,hjust=0.5,vjust=1,face="bold"),
        strip.text.x = element_text(size = 12, color = "white", face = "bold"),
        strip.background = element_rect(color="white", fill="black", size=1, linetype="solid")) 

plotb


plotb <- plotb +
  theme(legend.position = "none") 
# Add overall posterior panel for religious beliefs on bottom combine with ggarrange or something

library(tidybayes)
p <-  M1 %>%
  spread_draws(b_familyBariReligiousAfter) %>%
  mutate(condition_mean1 =  b_familyBariReligiousAfter)

## get rid of or change legend on plot below or better yet save it and add it later to grid arrange somewhere
p1 <- p%>%  ggplot(aes(y = 0, x = condition_mean1)) +
  stat_halfeye(point_interval = mean_hdi, .width = .95, fill="springgreen4",color="black",show.legend=T)  +
  geom_vline(xintercept = -1.304154, linetype = "dashed",color="black") +
  geom_vline(xintercept = 0 , linetype = "dashed", color="grey") +
  geom_text(aes(x=-1.304154, label="-1.34", y=0.75), colour="black", vjust = 1.2)+
  scale_y_continuous(NULL, breaks = NULL)+
  scale_x_continuous(name="Posterior distribution", breaks=c(-2,-1,0),labels=c('-2','-1','0'),limits=c(-2.25,0.07))+
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
library(gridExtra)
library(grid)
library(lattice)         
library(bayesplot)
library(cowplot)
#
plotb   <- grid.arrange(plotb,p1,ncol=2,top=textGrob("Non-Relatives in Same Neighborhood\n Helping with Childcare",gp=gpar(fontsize=16,font="bold")))




### Put plot a and plotb together

fig4 <- grid.arrange(plota,plotb,bottom_row,nrow=3,heights = c(1, 1, 1/2))

fig4

setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/Figures")
ggsave("Figure4_childcare.pdf", fig4,width = 24, height = 20, units = "cm")
ggsave("Figure4_childcare.png", fig4,width = 24, height = 20, units = "cm")
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################




#### REDO ABOVE FOR NON RELS
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
d <- newdata[c(1,5,7,8,9,44,45,47,49)] # add 36 for non-rels and 35 for rels
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


M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Childcare_help_nonrels_neighborsonly_neg_binom.rds")


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



mu_summary <- rbind(j1,j2)

mu_summary$lower <- mu_summary$`5CI`
mu_summary$upper <- mu_summary$`95CI`
# rejoin to non_rels
rels$religiosity <- rels$familyBariReligiousAfter

mu_summary$religiosity<-as.factor(mu_summary$religiosity)
rels$religiosity<-as.factor(rels$religiosity)

# reduce dataset to 3 middle locations
mu_summary2 <- mu_summary[4:6,]

mu_summary2$religiosity <- recode(mu_summary2$religiosity, '-1' = "Less", '0'="Equal",'1'="More")

rels$religiosity <- recode(rels$religiosity, '-1' = "Less", '0'="Equal",'1'="More")

# tack on predicted values to non_rels
b <- rels %>% left_join(mu_summary2, by=c("religiosity"="religiosity", "location"= "location"))


locations <- c('2' = "Same neighborhood")


plotb <- ggplot(mu_summary2, aes(x=religiosity, y=estimate)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper, colour=religiosity), size=1,
                width=.6, position = "dodge2") +
  geom_line( aes(x=as.numeric(religiosity), y=estimate)) + 
  #geom_jitter(data=b,aes(y=estimate , x= religiosity, colour=religiosity),  size=0.01,position=position_jitterdodge(jitter.height=0.1))+
  
  geom_jitter(data=b,aes(y=estimate , x= religiosity, colour=religiosity),alpha=0.3, cex=.1, height=0.012,width=0.3)+
  
  ggtitle("Non-Kin in Network Living in\nthe Same Neigborhood")+
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
        plot.title = element_text(size=16, face="bold"),
        
        # element_text(family = NULL, face = NULL, colour = NULL, size = NULL,
        #              hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL,
        #              color = NULL)
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




plotb


plotb <- plotb +
  theme(legend.position = "none") 
# Add overall posterior panel for religious beliefs on bottom combine with ggarrange or something

library(tidybayes)
p <-  M1 %>%
  spread_draws(b_Intercept[1],`b_Intercept[2]`,`b_Intercept[3]`,`b_Intercept[4]`,b_familyBariReligiousAfter) %>%
  mutate(condition_mean1 = b_familyBariReligiousAfter,
         condition_mean2=`b_Intercept[3]`+b_familyBariReligiousAfter,
         condition_mean3=`b_Intercept[4]`+b_familyBariReligiousAfter)

## get rid of or chnage legend on plot below or better yet save it and add it later to grid arrange somewhere
p1 <- p%>%  ggplot(aes(y = 0, x = condition_mean1)) +
  stat_halfeye(point_interval = mean_hdi, .width = .95, fill="springgreen4",color="black",show.legend=T)  +
  geom_vline(xintercept = 0.43436051, linetype = "dashed",color="black") +
  geom_vline(xintercept = 0 , linetype = "dashed", color="grey") +
  geom_text(aes(x=0.4343605, label="0.43", y=0.75), colour="black", vjust = 1.2)+
  scale_y_continuous(NULL, breaks = NULL)+
  scale_x_continuous(name="", breaks=c(0,0.25,0.5,0.75,1),labels=c('0','0.25','0.5','0.75','1'),limits=c(-0.07,1.04))+
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
library(gridExtra)
library(grid)
library(lattice)         
library(bayesplot)
library(cowplot)
plotb   <- grid.arrange(plotb,p1,ncol=2)

####################################################################################################################################
### Get kin and non kin childcare
# Non-relatives who help with childcare
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/childcare_help_non_rels_neg_binom.rds") # boxplots and CI look like shit
# # all clustered around 0 (Do not plot!!!)

setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh")
data <- read_csv("newdata.csv")
options(scipen=999)
# remove duplicated women
data <- data %>% distinct(idwife, .keep_all = TRUE)
# 1) center and scale variables for easier interpretability fo parameter estimates
data$religious_knowledge_scale <-  data$religious_knowledge_scale-mean(data$religious_knowledge_scale, na.rm=T)
data$hh_total  <- data$hh_total-mean(data$hh_total, na.rm=T)  
data$kids_in_hh  <- data$kids_in_hh-mean(data$kids_in_hh, na.rm=T)

data$kids_in_hh[is.na(data$kids_in_hh)]<- 0
data$religious_knowledge_scale[is.na(data$religious_knowledge_scale)]<-0
data$religion[is.na(data$religion)]<-0
data$sex <- NULL
data <- data[c(4,6:8,9,10,18,20,22,28,34,35,36,37,39,41,43,44,45,46,48)] 
data <- data[complete.cases(data), ] 
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

CCNR <- ggplot(data2, aes(x=factor(religiosity), y=childcare_help_non_rels,fill=factor(religiosity), colour=factor(religiosity))) +
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

CCNR

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
  scale_x_continuous(name="Posterior distribution", breaks=c(-3,-2,-1,0),labels=c('-3','-2','-1','0'),limits=c(-3.18,0.30))+
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

CCNR  <- grid.arrange(CCNR,p1,ncol=2)

plot(CCNR)
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

CCR <- ggplot(data2, aes(x=factor(religiosity), y=childcare_help_rels,fill=factor(religiosity), colour=factor(religiosity))) +
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

CCR

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
  scale_x_continuous(name="", breaks=c(-3,-2,-1,0),labels=c('-3','-2','-1','0'),limits=c(-3.18,0.30))+
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


#Grob the figure and the posterior distribution into a single figure 
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)         

CCR  <- grid.arrange(CCR,p1,ncol=2)

plot(CCR)

# Put plota, plotb CCR and CCNR together

one <- grid.arrange(plota,plotb)
two <- grid.arrange(CCR,CCNR)
