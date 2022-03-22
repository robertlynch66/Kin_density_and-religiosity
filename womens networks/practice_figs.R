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

# tack on predicted values to non_rels
b <- non_rels %>% left_join(mu_summary2, by=c("religiosity"="religiosity", "location"= "location"))


locations <- c('2' = "Neighbor",'3'= "Same Municipality",'4' = "Other Municipality")
library(wesanderson)
plot1 <- ggplot(mu_summary2, aes(x=religiosity, y=estimate)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper, colour=religiosity), size=1,
                 width=.6, position = "dodge2") +
  geom_line( aes(x=as.numeric(religiosity), y=estimate)) + 
  geom_jitter(data=b,aes(y=estimate , x= religiosity, colour=religiosity),  size=0.05,position=position_jitterdodge(jitter.height=0.1))+
  #scale_color_viridis_d()+
  
  
  facet_wrap(~location, nrow = 5, labeller = as_labeller(locations), scales="free")+
  
  #facet_grid(hospital ~ ., labeller = as_labeller(hospital_names))
  guides(fill = FALSE) +
  #scale_y_continuous(name=expression(paste("Estimate ", Beta)))+
  #ylab(name=expression(paste("Estimate ", Beta)))+
  scale_colour_manual(name = "Credibility Interval\n +/- 97.5%", labels = c("Less religious","Equally religious", "More religious"),
                    
                    values=wes_palette(n=4, name="FantasticFox1"))+
  #theme_void()+
  labs(x ="Religiosity", y = expression(paste("Estimate ", Beta)))+
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

#+
  # theme(legend.position="none")+theme(axis.title.x=element_blank(),
  #                                     axis.text.x=element_blank(),
  #                                     axis.ticks.x=element_blank())

                    
   
                  
plot1

facet_bounds <- read.table(header=TRUE,
                           text=                           
                             "location ymin ymax breaks
2     0.7     1.0    3
3     0.0     0.25   4
4     0.0     0.2    4",
                           stringsAsFactors=FALSE)

ff <- with(facet_bounds,
           data.frame(estimate=c(ymin,ymax),
                      location=c(location,location)))

## replot Y axis limits and breaks for individual facets

plot1 <- plot1 + geom_point(data=ff,x=NA) 
plot1 

# suppress and save legend - use for relatives too
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(plot1)
legend 

#remove legend

plot1 <- plot1 +
  theme(legend.position = "none") 
# Add overall posterior panel for religious beliefs on bottom combine with ggarrange or something

##  Add this to plot1
post <- 
  posterior_samples(M1) %>%
  mutate(iter = 1:n())
lp <- post %>% 
  ggplot(aes(x = b_familyBariReligiousAfter, y = 0)) +
  stat_halfeye(point_interval = mode_hdi, .width = .95,fill = sl[4], color = sl[8]) +
  scale_y_continuous(NULL, breaks = NULL)+

library(tidybayes)
library(wesanderson)
  
p <-  M1 %>%
  spread_draws(`b_Intercept[2]`,`b_Intercept[3]`,`b_Intercept[4]`,b_familyBariReligiousAfter) %>%
  mutate(condition_mean1 = `b_Intercept[2]`  + b_familyBariReligiousAfter,
         condition_mean2=`b_Intercept[3]`+b_familyBariReligiousAfter,
         condition_mean3=`b_Intercept[4]`+b_familyBariReligiousAfter)

## get rid of or chnage legend on plot below or better yet save it and add it later to grid arrange somewhere
p1 <- p%>%  ggplot(aes(y = 0, x = condition_mean1)) +
  stat_halfeye(point_interval = mean_hdi, .width = .95, fill="#f9a242ff",color="black",show.legend=T)  +
  geom_vline(xintercept = 1.708, linetype = "dashed",color="yellow") +
  scale_fill_manual(values = wes_palette(n=3, name="FantasticFox1"))+
  scale_y_continuous(NULL, breaks = NULL)+
  scale_x_continuous(name="", breaks=c(1,2,3),labels=c('1','2','3'),limits=c(-1,4.5))+
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

p2 <- p%>%  ggplot(aes(y = 0, x = condition_mean2)) +
  stat_halfeye(point_interval = mean_hdi, .width = .95, fill="#f9a242ff",color="black")  +
  geom_vline(xintercept = 3.1066, linetype = "dashed",color="yellow") +
  scale_fill_manual(values = wes_palette(n=3, name="FantasticFox1"))+
  scale_y_continuous(NULL, breaks = NULL)+
  scale_x_continuous(name="", breaks=c(2,3,4),labels=c('2','3','4'),limits=c(0,6))+
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


p3 <- p%>%  ggplot(aes(y = 0, x = condition_mean3)) +
  stat_halfeye(point_interval = mean_hdi, .width = .95, fill="#f9a242ff",color="black")  +
  geom_vline(xintercept = 4.959, linetype = "dashed", color="yellow") +
  scale_fill_manual(values = wes_palette(n=3, name="FantasticFox1"))+
  scale_y_continuous(NULL, breaks = NULL)+
  scale_x_continuous(name="Mean estimate", breaks=c(4,5,6),labels=c('4','5','6'))+
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


## use grid arrange to put plots in order
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)         

     
x   <- grid.arrange(p1,p2,p3,nrow=3)
finalplot1 <- grid.arrange(plot1,x, ncol=2)   

     
     
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
# tack on predicted values to non_rels
b <- rels %>% left_join(mu_summary2, by=c("religiosity"="religiosity", "location"= "location"))


locations <- c('2' = "Neighbor",'3'= "Same Municipality",'4' = "Other Municipality")



library(wesanderson)
plot2 <- ggplot(mu_summary2, aes(x=religiosity, y=estimate)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper, colour=religiosity), size=1,
                width=.6, position = "dodge2") +
  geom_line( aes(x=as.numeric(religiosity), y=estimate)) + 
  geom_jitter(data=b,aes(y=estimate , x= religiosity, colour=religiosity),  size=0.05,position=position_jitterdodge(jitter.height=0.1))+
  #scale_color_viridis_d()+
  
  
  facet_wrap(~location, nrow = 5, labeller = as_labeller(locations), scales="free")+
  

  guides(fill = FALSE) +

  scale_colour_manual(name = "Credibility Interval\n +/- 97.5%", labels = c("Less religious","Equally religious", "More religious"),
                      
                      values=wes_palette(n=4, name="FantasticFox1"))+

  labs(x ="Religiosity", y = expression(paste("Estimate ", Beta)))+
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

#+
# theme(legend.position="none")+theme(axis.title.x=element_blank(),
#                                     axis.text.x=element_blank(),
#                                     axis.ticks.x=element_blank())




plot2

facet_bounds <- read.table(header=TRUE,
                           text=                           
                             "location ymin ymax breaks
2     0.48     0.56    3
3     0.05     0.15   3
4     0.05     0.15    3",
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
  
library(tidybayes)
library(wesanderson)

p <-  M1 %>%
  spread_draws(b_Intercept[1],`b_Intercept[2]`,`b_Intercept[3]`,`b_Intercept[4]`,b_familyBariReligiousAfter) %>%
  mutate(condition_mean1 = `b_Intercept[2]`  + b_familyBariReligiousAfter,
         condition_mean2=`b_Intercept[3]`+b_familyBariReligiousAfter,
         condition_mean3=`b_Intercept[4]`+b_familyBariReligiousAfter)

## get rid of or chnage legend on plot below or better yet save it and add it later to grid arrange somewhere
p1 <- p%>%  ggplot(aes(y = 0, x = condition_mean1)) +
  stat_halfeye(point_interval = mean_hdi, .width = .95, fill="#f9a242ff",color="black")  + # ,show.legend=T (if you want legend back)
  geom_vline(xintercept = .9501, linetype = "dashed",color="yellow") +
  scale_fill_manual(values = wes_palette(n=3, name="FantasticFox1"))+
  scale_y_continuous(NULL, breaks = NULL)+
  scale_x_continuous(name="", breaks=c(0.5,1,1.5),labels=c('0.5','1','1.5'),limits=c(0,2))+
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

p2 <- p%>%  ggplot(aes(y = 0, x = condition_mean2)) +
  stat_halfeye(point_interval = mean_hdi, .width = .95, fill="#f9a242ff",color="black")  +
  geom_vline(xintercept = 1.6931, linetype = "dashed",color="yellow") +
  scale_fill_manual(values = wes_palette(n=3, name="FantasticFox1"))+
  scale_y_continuous(NULL, breaks = NULL)+
  scale_x_continuous(name="", breaks=c(1.25,1.75,2.25),labels=c('1.25','1.75','2.25'),limits=c(0.85,2.5))+
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

p2

p3 <- p%>%  ggplot(aes(y = 0, x = condition_mean3)) +
  stat_halfeye(point_interval = mean_hdi, .width = .95, fill="#f9a242ff",color="black")  +
  geom_vline(xintercept = 3.032 , linetype = "dashed", color="yellow") +
  scale_fill_manual(values = wes_palette(n=3, name="FantasticFox1"))+
  scale_y_continuous(NULL, breaks = NULL)+
  scale_x_continuous(name="", breaks=c(2.5,3.0,3.5),labels=c('2.5','3.0','3.5'),limits=c(2.25,3.9))+
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
p3

## use grid arrange to put plots in order
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)         


x   <- grid.arrange(p1,p2,p3,nrow=3)
finalplot1 <- grid.arrange(plot1,x, ncol=2)  


# fix the Y axes for plot 1 and plot2 above to reveal the diff between the error bards - use width or reduce size of dots etc...












library(resample)
int <- CI.percentile(p$condition_mean1, probs = c(0.025, 0.975))
int <- quantile(, prob=probs)
p$int <- findInterval(p$condition_mean1,int)
ggplot(df, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + scale_x_continuous(breaks=quantiles) + scale_fill_brewer(guide="none")

M1 %>%
  spread_draws(b_familyBariReligiousAfter) %>%

scale_colour_manual(name = "Credibility Interval\n +/- 97.5%", labels = c("Less religious","Equally religious", "More religious"),
                    
                    values=wes_palette(n=4, name="FantasticFox1"))+
#################################################################

post <- 
  posterior_samples(M1) %>%
  mutate(iter = 1:n())

# visualize the parametrization
post <-
  post %>%
  select(`b_Intercept[2]`) %>%
  mutate(iter = 1:n())


means <-
  post %>% 
  summarise_at(vars(`b_Intercept[2]`), mean) %>% 
  pivot_longer(everything(),
               values_to = "mean")

t1 <- post %>% 
  gather(name, threshold, -iter) %>% 
  group_by(iter) %>% 
  mutate(theta_bar = mean(threshold)) %>% 
  
  ggplot(aes(x = threshold, y = theta_bar, color = name)) +
  geom_vline(data = means,
             aes(xintercept = mean, color = name),
             linetype = 2) +
  geom_point(alpha = 1/10) +
  scale_color_scico_d(palette = "lajolla", begin = .25) +
  ylab("mean threshold") +
  theme(legend.position = "none")

# 2nd parametrization plot
post <- 
  posterior_samples(M1) %>%
  mutate(iter = 1:n())

# visualize the parametrization
post <-
  post %>%
  select(`b_Intercept[3]`) %>%
  mutate(iter = 1:n())




### make final plots and add common legend
plot_grid(fig1, fig2,fig3,fig4, legend, ncol = 1,nrow=5, rel_heights = c(1/5, 1/5, 1/5,1/5,1/5))
main_plot
### make the plots legible with spacing


save_plot("Figure1.pdf", main_plot,
          base_aspect_ratio = 1.3 # make room for figure legend
)
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####Extra shit pasted from Models
### Start here for plotting!!! Here is the website:  https://bookdown.org/content/4857/monsters-and-mixtures.html#ordered-categorical-outcomes


#Here's our brms version of the bottom plot of Figure 23.2.
means <-
  post %>% 
  summarise_at(vars(`b_Intercept[3]`), mean) %>% 
  pivot_longer(everything(),
               values_to = "mean")

t2 <- post %>% 
  gather(name, threshold, -iter) %>% 
  group_by(iter) %>% 
  mutate(theta_bar = mean(threshold)) %>% 
  
  ggplot(aes(x = threshold, y = theta_bar, color = name)) +
  geom_vline(data = means,
             aes(xintercept = mean, color = name),
             linetype = 2) +
  geom_point(alpha = 1/10) +
  scale_color_scico_d(palette = "lajolla", begin = .25) +
  ylab("mean threshold") +
  theme(legend.position = "none")
t2

# 3rd parametrization plot
post <- 
  posterior_samples(M1) %>%
  mutate(iter = 1:n())

# visualize the parametrization
post <-
  post %>%
  select(`b_Intercept[4]`) %>%
  mutate(iter = 1:n())


#Here's our brms version of the bottom plot of Figure 23.2.
means <-
  post %>% 
  summarise_at(vars(`b_Intercept[4]`), mean) %>% 
  pivot_longer(everything(),
               values_to = "mean")

t3 <- post %>% 
  gather(name, threshold, -iter) %>% 
  group_by(iter) %>% 
  mutate(theta_bar = mean(threshold)) %>% 
  
  ggplot(aes(x = threshold, y = theta_bar, color = name)) +
  geom_vline(data = means,
             aes(xintercept = mean, color = name),
             linetype = 2) +
  geom_point(alpha = 1/10) +
  scale_color_scico_d(palette = "lajolla", begin = .25) +
  ylab("mean threshold") +
  theme(legend.position = "none")
t3
labs <- str_c("beta[", 1:8, "]")

library(tidybayes)
library(ggthemes)

posterior_samples(model) %>% 
  select(b_age_wife:b_familyBariReligiousAfter) %>% 
  set_names(labs) %>% 
  pivot_longer(everything()) %>% 
  
  ggplot(aes(x = value, y = name)) +
  geom_vline(xintercept = 0, alpha = 1/5, linetype = 3) +
  stat_gradientinterval(.width = .5, size = 1, point_size = 3/2, shape = 21,
                        point_fill = canva_pal("Green fields")(4)[3], 
                        fill = canva_pal("Green fields")(4)[1], 
                        color = canva_pal("Green fields")(4)[2]) +
  scale_x_continuous("marginal posterior", breaks = -5:0 / 4) +
  scale_y_discrete(NULL, labels = parse(text = labs)) +
  coord_cartesian(xlim = c(-1.2, 0.8))

## then use fitted function and run through new data like before
M1 <- readRDS("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh/results/Geo_distance_non_relatives_ord_cum.rds")

religious_seq <- tibble(religiosity = seq(from = -1, to = 1, by = 1))
attach(non_rels)
newdata <- data.frame(
  kids_in_hh = mean(kids_in_hh),
  age_wife = mean(age_wife),
  religion=mean(religion),
  location=c(1,2,3,4,5),
  familyBariReligiousAfter = c(-1,0,1),
  religious_knowledge_scale=mean(religious_knowledge_scale),
  MI_geo_proximity=mean(MI_geo_proximity),
  MI_economic_capital=mean(MI_economic_capital),
  MI_human_capital=mean(MI_human_capital)
)
detach(non_rels)


z0 <- predict(M1, newdata = newdata) %>% as.data.frame()

z <- predict(M1, newdata = non_rels2) %>% as.data.frame()
colnames(z) <- c("Same_hh","Neighbor","Matlab","Bangladesh","Abroad")
z2 <- predict(M1, newdata = non_rels) %>% as.data.frame()
colnames(z2) <- c("Same_hh","Neighbor","Matlab","Bangladesh","Abroad")


### plot 1

# mu_summary <-
#   fitted(M1, 
#          newdata = newdata, probs=c(0.05,0.95)) %>%
#   as_tibble() %>%
#   # let's tack on the `religiosity` values from `religious_seq` if necessary (here it is not)
#   bind_cols(religious_seq)
# 
# mu_summary



f <-
  fitted(M1)



f %>% str()

# must link location from non_rels to location f

f <-
  rbind(f[, , 1],
        f[, , 2],
        f[, , 3],
        f[, , 4],
        f[, , 5]) %>% 
  data.frame() %>% 
  #set_names(pull(newdata, familyBariReligiousAfter)) %>% 
  mutate(location_pred = rep(1:5, each = n() / 5),
         row_num     = rep(1:796, times = 5)) #  %>% 


# pivot_longer(-c(iter, response),
#              #names_to = c("Low","Medium","High"),
#              #names_sep = "_",
#              values_to = "pk") 

g <-
  rbind(non_rels,
        non_rels,
        non_rels,
        non_rels,
        non_rels) %>% 
  data.frame() 
#set_names(pull(newdata, familyBariReligiousAfter)) %>% 
#mutate(row_num2    = rep(1:796, times = 5))


big <- g %>% left_join(f, by =c("location"="location_pred"))


ggplot(data1, aes(x=group, y=estimate)) + 
  geom_errorbar(aes(ymin=estimate-lower, ymax=estimate+upper), 
                colour="black", width=.1, position=pd) +
  geom_line( aes(x=as.numeric(group), y=estimate)) + 
  geom_point(position=pd, size=4)


plot <- big %>% ggplot(aes(x=factor(religiosity), y=Estimate)) + 
  geom_errorbar(aes(ymin=Estimate-Q5,ymax=Estimate+Q95)) + 
  facet_wrap(~location_pred)



f$religiosity[f$religiosity == "X1"] <- "Low"
f$religiosity[f$religiosity == "X2"] <- "Med"
f$religiosity[f$religiosity == "X3"] <- "High"

# levels <- c("religiosity=low", "religiosity=medium", "religiosity=high")
# 
# library(ggthemes)
# # these next three lines allow us to compute the cumulative probabilities
# p1 <- f %>% 
#   #mutate(facet = factor(str_c("religiosity=", religiosity),
#          #               levels = levels)) %>% 
#   group_by(iter, religiosity) %>% 
#   arrange(iter, religiosity, response) %>% 
#   mutate(probability = cumsum(pk)) %>% 
#   ungroup() %>% 
#   # these next three lines are how we randomly selected 50 posterior draws
#   nest(data = -iter) %>% 
#   slice_sample(n = 50) %>%
#   unnest(data) %>% 
# 
#   ggplot(aes(x = religiosity, y = probability)) +
#   geom_line(aes(group = interaction(iter, response), color = probability),
#             alpha = 1/10) +
#   geom_point(data = non_rels %>%  # wrangle the original data to make the dots
#                group_by(religiosity) %>% 
#                count(location) %>% 
#                mutate(probability = cumsum(n / sum(n)),
#                       facet = factor(str_c("Religiosity=", religiosity),
#                                      levels = levels)) %>% 
#                filter(location < 7),
#              color = canva_pal("Green fields")(4)[2]) +
#   scale_color_gradient(low = canva_pal("Green fields")(4)[4],
#                        high = canva_pal("Green fields")(4)[1]) +
#   scale_x_discrete("Religiosity", breaks = 0:1) +
#   #scale_y_continuous(breaks = c(0, .5, 1), limits = c(0, 1)) +
#   theme(legend.position = "none")  #+ 
#   #facet_wrap(~facet)



#### HERE SHOULd be like f in the 6 column , 168000 row df here:  https://bookdown.org/content/4857/monsters-and-mixtures.html#ordered-categorical-outcomes

# library(HDInterval)
# int_1<- hdi(f$pk[f$religiosity=="Low" & f$response==1], credMass = 0.89)




# tack on pk to non_rels
f$religiosity2[f$religiosity == "Low"] <- -1
f$religiosity2[f$religiosity == "Med"] <- 0
f$religiosity2[f$religiosity == "High"] <- 1

# get the means for pk by religious group and location
new <- f %>% group_by (response,religiosity) %>% summarise(mean=mean(pk))


data <- non_rels %>% left_join(mu_summary2,by=c("location"="location","religiosity"="religiosity"))

data$location[data$location==1|data$location==2] <- "Neighbor"
data$location[data$location==3] <- "Matlab"
data$location[data$location==4|data$location==5] <- "Outside Matlab"
# Location Codes
# 1=khana member,
# 2=near bari/neighbor
# 3=other place in Matlab
# 4=Other Place in Bangladesh
# 5=Abroad


#### FIX THIS BELOW  - Maybe Error bars are better - use lines with CI's
data$location <- factor(data$location, levels=c("Neighbor", "Matlab", "Outside Matlab"), ordered=TRUE)

k1 <- data   %>% ggplot(aes(x = factor(religiosity), y = estimate)) +
  geom_line(position = "dodge", fill="#4682B4",alpha=0.66)+
  geom_boxplot(width=.1,fill='#A4A4A4', col="darkred",position="dodge") +
  #geom_point(width=.2,aes(color="orange"))
  scale_shape_identity() +
  
  
  
  scale_x_discrete(name="Religiosity",breaks=c("Low","Med","High"),labels=c("Less",
                                                                            "Same","More")) +
  
  #scale_y_continuous(breaks=c(0,0.01,0.02,0.03,0.04),limits = c(0,0.05),
  #                 labels=c("0","0.01","0.02","0.03","0.04")) +
  
  facet_grid(rows=vars(location))+
  
  k1

# xlab("") + ylab("Number of Children") + 
# ggtitle("Returned to Karelia") +
theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, "in")) +
  theme(plot.title = element_text(hjust = 0.5, size=7,face="bold"),
        axis.text.x = element_text(colour="grey20",size=5,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=5,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=6,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=6,angle=90,hjust=.5,vjust=.5,face="bold"))  



k1



int_2<- hdi(z$Neighbor, credMass = 0.89)
# yields a range of 2.79 to 3.13
# subset data
data <- data[which(data$lambda >min(int_2) & data$lambda < max(int_2)), ]

mu_summary <- fitted(M1,
                     newdata = newdata, probs=c(0.05,0.95)) %>%
  as_tibble() %>%
  bind_cols(religious_seq)

mu_summary %>% str()

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

mu_summary2 <- rbind(j1,j2,j3,j4,j5)

mu_summary2$lower <- mu_summary2$`5CI`
mu_summary2$upper <- mu_summary2$`95CI`
# rejoin to non_rels
non_rels$religiosity <- non_rels$familyBariReligiousAfter

data2 <- non_rels %>% left_join (mu_summary2, by =c("religiosity"="religiosity", "location"="location"))


## data2 is what we need to plot this stuff now

# library(HDInterval)
# int_2<- hdi(data$lambda, credMass = 0.89)
# # yields a range of 2.79 to 3.13
# # subset data
# data <- data[which(data$lambda >min(int_2) & data$lambda < max(int_2)), ]





library(ggplot2)
library(hrbrthemes)
location2<- data2 %>% filter (location ==2) %>% ggplot(aes(x=factor(religiosity),y=estimate)) +
  geom_point()+
  geom_smooth(method=lm , color="red", se=FALSE,aes(y=estimate,ymin='5CI',ymax='95CI')) +
  theme_ipsum()


# linear trend + confidence interval
p3 <- ggplot(data, aes(x=my_x, y=my_y)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()

plot <- ggplot(data2, aes(x=factor(religiosity),y=factor(location),fill=factor(religiosity),colour=factor(location)))+
  geom_boxplot(show.legend=T, width=0.5, aes(fill=factor(religiosity)))+
  geom_jitter(width = 0.2, aes(fill=factor(religiosity)))+
  geom_line( aes(group=factor(location),y=estimate,ymin='5CI',ymax='95CI',colour=factor(location) ))  #,
#stat = "identity",fill="grey16",show.legend=T)


mu_summary2 %>% filter %>% (religiosity<2) %>% sum(estimate)

a <- mu_summary2 %>% filter (religiosity ==-1) 
b <- mu_summary2 %>% filter (religiosity ==0) 
c <- mu_summary2 %>% filter (religiosity ==1) 
sum(c$estimate)


## Try it exactly as in book
p1 <- mu_summary2 %>% group_by(religiosity, location) %>% 
  #arrange(iter, facet, intention, response) %>% 
  mutate(probability = cumsum(estimate)) %>% 
  ungroup() 

p1 <- p1 %>% select(1,4,14,15,19)

library(effects)
plot(Effect("Infl",model2))

# ggplot(aes(x = religosity, y = location)) +
#   geom_line(aes(group = interaction(iter, response), color = probability),
#             alpha = 1/10) +
#   geom_point(data = d %>%  # wrangle the original data to make the dots
#                group_by(intention, contact, action) %>% 
#                count(response) %>% 
#                mutate(probability = cumsum(n / sum(n)),
#                       facet = factor(str_c("action=", action, ", contact=", contact),
#                                      levels = levels)) %>% 
#                filter(response < 7),
#              color = canva_pal("Green fields")(4)[2]) +
#   scale_color_gradient(low = canva_pal("Green fields")(4)[4],
#                        high = canva_pal("Green fields")(4)[1]) +
#   scale_x_continuous("intention", breaks = 0:1) +
#   scale_y_continuous(breaks = c(0, .5, 1), limits = c(0, 1)) +
#   theme(legend.position = "none") 

# ggplot(data2, aes(x=factor(religiosity), y=NW_total)) + 
#   geom_boxplot(show.legend=T, width=0.2, aes(fill=factor(religiosity)))+
#   geom_jitter(width = 0.2, aes(fill=factor(religiosity)))+
#   
#   
#   geom_smooth(method = 'lm', aes(group=1,y=Estimate,ymin=Q5,ymax=Q95,colour="Credibility interval"),
#               stat = "identity",fill="grey16",show.legend=T)+
#   #theme(legend.position = "none")+
#   
#   scale_x_discrete(breaks=c("-1","0","1"),name="Religiosity",
#                    labels=c("Less", "Equal", "More"))+
#   scale_y_continuous(name="Network total number",
#                      breaks=c(5,10,15,20), labels=c("5","10","15","20"),limits=c(2,23))+
#   theme_bw() 
p1 <-
  f %>% 
  # unnecessary for these plots
  filter(response < 7) %>% 
  # this will help us define the three panels of the triptych
  mutate(facet = factor(str_c("action=", action, ", contact=", contact),
                        levels = levels)) %>% 
  # these next three lines allow us to compute the cumulative probabilities
  group_by(iter, facet, intention) %>% 
  arrange(iter, facet, intention, response) %>% 
  mutate(probability = cumsum(pk)) %>% 
  ungroup() %>% 
  # these next three lines are how we randomly selected 50 posterior draws
  nest(data = -iter) %>% 
  slice_sample(n = 50) %>%
  unnest(data) %>% 
  
  # plot!
  ggplot(aes(x = intention, y = probability)) +
  geom_line(aes(group = interaction(iter, response), color = probability),
            alpha = 1/10) +
  geom_point(data = d %>%  # wrangle the original data to make the dots
               group_by(intention, contact, action) %>% 
               count(response) %>% 
               mutate(probability = cumsum(n / sum(n)),
                      facet = factor(str_c("action=", action, ", contact=", contact),
                                     levels = levels)) %>% 
               filter(response < 7),
             color = canva_pal("Green fields")(4)[2]) +
  scale_color_gradient(low = canva_pal("Green fields")(4)[4],
                       high = canva_pal("Green fields")(4)[1]) +
  scale_x_continuous("intention", breaks = 0:1) +
  scale_y_continuous(breaks = c(0, .5, 1), limits = c(0, 1)) +
  theme(legend.position = "none") +
  facet_wrap(~facet)

### Then 
p <-
  predict(b12.5,
          newdata = nd,
          nsamples = 1000,
          scale = "response",
          summary = F)

p %>% str()

# Then
p2 <-
  p %>% 
  data.frame() %>% 
  set_names(pull(nd, combination)) %>% 
  pivot_longer(everything(),
               names_to = c("action", "contact", "intention"),
               names_sep = "_",
               values_to = "response") %>% 
  mutate(facet = factor(str_c("action=", action, ", contact=", contact),
                        levels = levels)) %>% 
  
  ggplot(aes(x = response, fill = intention)) +
  geom_bar(width = 1/3, position = position_dodge(width = .4)) +
  scale_fill_manual(values = canva_pal("Green fields")(4)[2:1]) +
  scale_x_continuous("response", breaks = 1:7) +
  theme(legend.position = "none") +
  facet_wrap(~facet)

# make the new figure like mcelreaths
(p1 / p2) & theme(panel.background = element_rect(fill = "grey94"))

## an alternative figure
p1 <-
  f %>% 
  
  group_by(iter, facet, intention) %>% 
  summarise(mean_response = sum(pk * response)) %>% 
  ungroup() %>% 
  nest(data = -iter) %>% 
  slice_sample(n = 50) %>%
  unnest(data) %>%
  
  ggplot(aes(x = intention, y = mean_response)) +
  geom_line(aes(group = iter, color = mean_response),
            alpha = 1/10) +
  scale_color_gradient(low = canva_pal("Green fields")(4)[4],
                       high = canva_pal("Green fields")(4)[1]) +
  scale_x_continuous("intention", breaks = 0:1) +
  scale_y_continuous("resopnse", breaks = 1:7, limits = c(1, 7)) +
  theme(legend.position = "none") +
  facet_wrap(~facet)

p2 <-
  f %>% 
  
  
  ggplot(aes(x = response, y = pk, fill = factor(intention))) +
  stat_ccdfinterval(.width = .95, justification = 1, size = 1/4, 
                    shape = 21, point_fill = canva_pal("Green fields")(4)[3], point_size = 1/3,
                    position = "dodge", width = .75) +
  scale_fill_manual(values = canva_pal("Green fields")(4)[2:1]) +
  scale_x_continuous("resopnse", breaks = 1:7) +
  scale_y_continuous("count", breaks = 0:3 / 10, labels = 0:3 * 100, limits = c(0, NA)) +
  theme(legend.position = "none") +
  facet_wrap(~facet)

p1 / p2