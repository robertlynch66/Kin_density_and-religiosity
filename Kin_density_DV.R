setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh")
library(readr)
library(tidyverse)
options(scipen = 999)
# read in the data
data <- read.csv("final_data.csv")

names(data)
### make sure data is okay- IT IS!!!

cor(data$marketIntegrationLargeBazaarTime,data$marketIntegrationTownTime)

# test - should be 0.8197575 if everything worked as planned

# remove "MI_single_factor1"  and "MI_single_factor2" 

data <- data[-c(56,57)]

all_MI_data <- read_csv("all_MI_data.csv")

d <- all_MI_data %>% select (4)

data2 <- d %>% left_join(data,c("idwife"="idwife"))

# HERE!!!!! Check to make sure it's okay - spot check 5 with raw data!!!

#  Check data 2 is good


### get kin density variable (DV) for husbands and wives

# a) percent kin to non-kin (social network and or HH)
# b) geographic proximity to kin (mean)
# c) number of kin,  non kin and total network on list or in HH or social network (control for family size)
# d) contact with kin or importance of kin -- divide between emotional and practical support (q1-q12) - get dimensions for 
# questions q1-q12 to see if they break down into emotional and practicalor simply analyze the network without
# caring about the questions (just use this list)


library(readr)
#  get HH roster
PeopleinHH <- read_csv("HHQPeopleinHH.csv")
names(PeopleinHH)

plyr::count(PeopleinHH$relationToRespondent)
# Other HH residents -- "relationToRespondent" (compare number and % of kin vs non-kin living in HH)
# Here is the list and frequencies:
#   Child 2987
# Daughter in law    1
# G, Son    1
# Husband  761
# Mather in law    1
# Mother    1
# Respondent  764
# <NA>   81  
# Other 1236 
# 
# **Then under 1236 Other's here is the breakdown and frequency:
#  
# Adopt daughter	1
# BABA	6
# Batita thaka	1
# BHAI	6
# Bhoner Chele	1
# BON	4
# Boner cala	1
# Brother	2
# BROTHER	1
# Brother  Daughter	1
# brother in Law	1
# Brother in law	6
# Brother in Law	5
# BROTHER IN LAW	5
# Brother in low	1
# BROTHER IN LOW	3
# Brother Son	2
# Brothers  wife	1
# Brothers daughter	1
# Brothers son	1
# Cacaler baw	6
# Caler bow	2
# CHELER B0OW	16
# CHELER BOW	120
# CHOTO BON	1
# Daborer cala	1
# Daughter-in-law	2
# DAUGHTER-IN-LAW	12
# DAUGHTER  IN LAW	7
# DAUGHTER  IN LOW	3
# DAUGHTER 0F SISTER IN LOW	3
# Daughter in law	69
# Daughter In Law	1
# DAUGHTER IN LAW	43
# Daughter in low	11
# Daughters husband	1
# DEBOER  MAYE	1
# DEBOR	9
# DEBOR'S DAUGHTER	1
# DEBOR'S SON	1
# DEBOR'S WIFE	1
# DEBOR ER CHELA	1
# DEBORER BOW	1
# DEBORER CALA	1
# DEBORER CHELA	1
# DEBORER CHELE	5
# DEBORER MAYE	4
# DEBORER MEYE	1
# DOUGHTIER NATI	1
# Father	3
# FATHER	4
# FATHER-IN-LAW	1
# Father in law	10
# FATHER IN LAW	7
# Father in low	3
# FATHER IN LOW	6
# Foster child	1
# FUPU SHASURI	1
# G . DAUGHTER	1
# G Daughter	7
# G DAUGHTER	29
# G Son	2
# G SON	40
# G, Daughter	9
# G, Son	9
# G,DAUGHTER	5
# G. Daughter	30
# G. Daughter\nG. Son	1
# G. Son	19
# G. SON	4
# G.Daughter	7
# G.Son	7
# Grad daughter	1
# Grand daughter	14
# Grand Daughter	10
# GRAND DAUGHTER	20
# GRAND MOTHER	1
# Grand son	11
# Grand Son	15
# GRAND SON	14
# Grandson	1
# HUSBAND OF DAUGHTER	1
# Ja	1
# JA	10
# JA'S SON	1
# JAER CHELE	1
# JEARE	1
# MA	6
# Mayer jamai	1
# Mayer jamay	1
# Mayer nati	1
# Mayer Nati	2
# Mayer Natin	2
# MEYER BACHCHA	1
# MEYER CHELE	1
# MEYER GHORER NATI	1
# MEYER GHORER NATIN	2
# MEYER JAMAI	3
# MEYER ZAMAI	1
# mother	1
# Mother	4
# MOTHER	3
# MOTHER-IN-LAW	4
# Mother in law	34
# MOTHER IN LAW	15
# Mother in low	5
# Mother In Low	1
# MOTHER IN LOW	8
# NANAD	2
# NANAD ER CHELA	1
# Nanoder cala	1
# Nati	6
# NATI	142
# NATI BOW	1
# Natin	8
# NATIN	134
# NATIR MEYE	1
# NATN	1
# Nephew	2
# NEPHEW	7
# Niece	8
# NIECE	4
# NONOD	1
# NONOD(HUSBAND'S SISTER	1
# NONOSH	1
# NONOSHER CHELE	1
# OTHER	1
# SHASUR	21
# SHASURI	42
# SHOT BON	1
# SHOTIN	1
# SHOTMA	1
# SISTER	2
# Sister in law	4
# SISTER IN LAW	5
# Sister in law, Ja	1
# Sister in low	2
# SISTER IN LOW	8
# Son in law	1
# Son in low	1
# SON OF BROTHER IN LOW	3
# SON OF SISTER IN LOW	5
# Step child	1
# Step Daughter	1
# VAGNI	1
# VAI	2
# VAI ER CHELE	1
# VASHUR	2
# Vashurer maya	1
# VASUR	1
# VASURER BOW	1
# VASURER CHELE	2
# VATIZA	1
# VATIZI	2

# Wait to hear from Mary if it is even reasonable to do this using the above list

plyr::count(PeopleinHH$relationOther) # find and code in Bangla kinship terms (if not on list ask Mary)

# pg 8 general HH survey starts social network questions

# read in pg 8  main HH questionnaire
#pg8 <- read_csv("Main_table_pgs_7-9.csv")

#### NO USEFUL STAND ALONES FOR MEASURING KIN DENSITY!!! 


# get womens and mens newtworks
WifeNW <- read_csv("HHQPeopleinNW.csv")

# get husbands newtwork
HusbandNW <- read_csv("HusbandPeopleinNW.csv")

names(WifeNW)
names(HusbandNW )

# what are the questions q01-q12 (pgs 13-18)

#Type of relationship code: N= Neighbor, C= Colleague, F = Friend, B = Business relation, O = Other
plyr::count(WifeNW$relationshipType)
# difference between a dash and an NA -- ask mary (pull up frequencies)
### clean this up first

## Do not use relationshipType!!!!!!!!!!!

# location:  Location Codes
#1=khana member,(same household)
#2=near bari/neighbor (walking distance), 
#3=other place in Matlab,
#4=Other Place in Bangladesh, 
#5=Abroad, 
plyr::count(WifeNW$location)

# 1 1419
# 2 3706
# 3  745
# 4  582
# 5  230
# NA 1168

plyr::count(HusbandNW$location)
# 1  945
# 2 3472
# 3  753
# 4  821
# 5  194
# NA   31
## Key is to link relationship codes with location to get density


#q1 = Who helps you with childcare or housework when you need it? (poribar, attiyo, friends, neighbors)
plyr::count(WifeNW$q01)

# link to WifeNW 'id_Questionaire' to idwife from data
# get "gender" ,"age","relationship","relationshipType","location" 


#q2= Who do you help with childcare or housework? (poribar, attiyo, friends, neighbors)
plyr::count(WifeNW$q02)

##  we want both q1 and q2 (childcare)
# percent of relatives who help with child care and who you help with child care


#q3 = Who would you ask for a small loan of Tk. 100-1000 if you needed money? (poribar, attiyo, friends, neighbors)
plyr::count(WifeNW$q03)
#q4 = Who would ask you for a small loan of Tk. 100-1000? (poribar, attiyo, friends, neighbors)
plyr::count(WifeNW$q04)
#q5 = Who would you ask for a large loan if you needed money? (poribar, attiyo, friends, neighbors)
plyr::count(WifeNW$q05)
#q6 = Who would ask you for a large loan? (poribar, attiyo, friends, neighbors)
plyr::count(WifeNW$q06)
#q7 = Who do you ask for advice or discuss important matters with? (poribar, attiyo, friends, neighbors)
plyr::count(WifeNW$q07)
#q8 = Who asks you for advice or discusses important matters with you? (poribar, attiyo, friends, neighbors)
plyr::count(WifeNW$q08)
#q9 = Who do you know personally who has local social influence, prestige, or wealth? (poribar, attiyo, friends, neighbors)
plyr::count(WifeNW$q09)
#q10 = Do you serve as a prestigious or wealthy social connection for anyone else? (poribar, attiyo, friends, neighbors)
plyr::count(WifeNW$q10)
#q11 = Who do you spend the most time with? (poribar, attiyo, friends, neighbours; including talking over phone once in a week or more)
plyr::count(WifeNW$q11)
#q12 = If some problem happens, who comes to your aid to help you? (poribar, attiyo, friends, neighbors)
plyr::count(WifeNW$q12)


#This section focuses on the respondent's CURRENT social network-now, not in the past. 
#Please tell us about anyone in your poribar, attiyo, friends, neighbors, or others who you have 
#the following types of relationships with.

# relationship codes: Relation code: 0 = not a relative, 1 = nijer poribar (parents and kids), 2 = babar bari (fathers family- 2nd or 3rd degree relatives),
#3 = mayer bari (mother side - 2nd or 3rd degree relatives), 4 = shoshur bari (in-laws, affinal), 
#5= babar bari (far relative), 6 = mayer bari (far relative), 7 = shoshur bari (far relative) see Koster et. al. Phil Trand 2019 - for how to quantiy density of kin networks
# 2016 Shenk - Current Anthro
plyr::count(WifeNW$relationship)
plyr::count(HusbandNW$relationship)

# convert relationship to numeric
WifeNW$relationship <- as.numeric(WifeNW$relationship)
HusbandNW$relationship <- as.numeric(HusbandNW$relationship)
# count the zeros (e.g. not a relative) (total and as a proportion of network)
### clean this up first (no need)
# wifes realtionshsips
# 0  799
# 1 2734
# 2  217
# 3  266
# 4 1986
# 5   54
# 6   55
# 7  555
# NA 1151  
# weird shit (miscoded etc...)  33 - not a big deal


# husbands relationships
# 0 2404
# 1 1526
# 2 1410
# 3  111
# 4  418
# 5  250
# 6   27
# 7   21
# <NA>   28
# weird shit (miscoded etc...)  20 - not a big deal

# what are the NA's in the wife Nw

### Okay need to calculate percent of kin in network (0's to everyone else per respondent)
library(tidyr)
library(tidyverse)

# first count the number on non relatives and total relatives per wife newtwork 
# replace relationship NA's with 99
WifeNW$relationship[is.na(WifeNW$relationship)]<- 99
# classify NA's as NA's
#WifeNW$relationship[is.na(WifeNW$relationship)]<- 0

######!!!!!!!!MAKE SURE THIS IS OKAY!!!!!!!!!!!!!!!!!!! _ e.g. making all preistigous people non-relatives!!!

WifeNW$location[WifeNW$location == 0] <- NA
WifeNW$location[WifeNW$location >5 ] <- NA

### location codes higher = farther
# Location Codes
# 1=khana member,
# 2=near bari/neighbor
# 3=other place in Matlab
# 4=Other Place in Bangladesh
# 5=Abroad

wnr <-WifeNW %>% group_by(id_Questionaire) %>% summarise(total=n(),non_rels = sum(relationship==0),parents_kids=
                                                           sum(relationship==1),pat_rels=sum(relationship==2),
                                                          mat_rels=sum(relationship==3),
                                                         in_laws=sum(relationship==4),far_rels=sum(relationship==5|
                                                        relationship==6|relationship==7),
                                                        geo_distance_non_rels = mean(location[relationship==0],na.rm=T),
                                                        geo_distance_rels = mean(location[relationship>0 & relationship<8],na.rm=T),
                                                        nonrels_help_childcare=sum(q01==1 & relationship==0,na.rm=T),
                                                        rels_help_childcare=sum(q01==1 & relationship>0 & relationship<8,na.rm=T),
                                                        help_nonrels_childcare=sum(q02==1 & relationship==0,na.rm=T),
                                                        help_rels_childcare=sum(q02==1 & relationship>0 & relationship<8,na.rm=T),
                                                        non_rels_loan=sum((q03==1 | q05==1)&relationship==0,na.rm=T),
                                                        rels_loan=sum((q03==1 | q05==1)&relationship>0 & relationship<8,na.rm=T),
                                                        non_rels_loaning=sum((q04==1 | q06==1)&relationship==0,na.rm=T),
                                                        rels_loaning=sum((q04==1 | q06==1)&relationship>0 & relationship<8,na.rm=T),
                                                        non_rels_ask_advice=sum(q07==1 & relationship==0, na.rm=T),
                                                        rels_ask_advice=sum(q07==1 & relationship>0 & relationship<8, na.rm=T),
                                                        non_rels_asking_advice=sum(q08==1 & relationship==0, na.rm=T),
                                                        rels_asking_advice=sum(q08==1 & relationship>0 & relationship<8, na.rm=T),
                                                        non_rels_prestige=sum((q09==1 |prestigePosition==1)&relationship==0, na.rm=T),
                                                        rels_prestige=sum((q09==1 |prestigePosition==1)& relationship>0 & relationship<8, na.rm=T),
                                                        non_rels_time_spent=sum(q11==1 & relationship==0,na.rm=T),
                                                        rels_time_spent=sum(q11==1 & relationship>0 & relationship<8,na.rm=T),
                                                        non_rels_aid=sum(q12==1 & relationship==0,na.rm=T),
                                                        rels_aid=sum(q12==1 & relationship>0 & relationship<8,na.rm=T))
## what to do with q09 and prestigePosition ???? ask mary

## yes no rels geo distance is correct

HusbandNW$relationship[is.na(HusbandNW$relationship)]<- 0
HusbandNW$location[HusbandNW$location == 0] <- NA
hnr <-  HusbandNW %>% group_by(id_Questionaire) %>% summarise(total_h=n(),non_rels_h = sum(relationship==0),parents_kids_h=
                                                                sum(relationship==1),pat_rels_h=sum(relationship==2),
                                                              mat_rels_h=sum(relationship==3),
                                                              in_laws_h=sum(relationship==4),far_rels_h=sum(relationship==5|
                                                              relationship==6|relationship==7),
                                                              geo_distance_non_rels_h = mean(location[relationship==0],na.rm=T),
                                                              geo_distance_rels_h = mean(location[relationship>0 & relationship<8],na.rm=T),
                                                              nonrels_help_work_h=sum(q01==1 & relationship==0,na.rm=T),
                                                              rels_help_work_h=sum(q01==1 & relationship>0 & relationship<8,na.rm=T),
                                                              help_nonrels_work_h=sum(q02==1 & relationship==0,na.rm=T),
                                                              help_rels_work_h=sum(q02==1 & relationship>0 & relationship<8,na.rm=T),
                                                              non_rels_loan_h=sum((q03==1 | q05==1)&relationship==0,na.rm=T),
                                                              rels_loan_h=sum(q05==1&relationship>0 & relationship<8,na.rm=T),
                                                              non_rels_loaning_h=sum(q06==1&relationship==0,na.rm=T),
                                                              rels_loaning_h=sum((q04==1 | q06==1)&relationship>0 & relationship<8,na.rm=T),
                                                              non_rels_ask_advice_h=sum(q07==1 & relationship==0, na.rm=T),
                                                              rels_ask_advice_h=sum(q07==1 & relationship>0 & relationship<8, na.rm=T),
                                                              non_rels_asking_advice_h=sum(q08==1 & relationship==0, na.rm=T),
                                                              rels_asking_advice_h=sum(q08==1 & relationship>0 & relationship<8, na.rm=T),
                                                              non_rels_prestige_h=sum((q09==1 |prestigePosition==1)&relationship==0, na.rm=T),
                                                              rels_prestige_h=sum((q09==1 |prestigePosition==1)& relationship>0 & relationship<8, na.rm=T),
                                                              non_rels_time_spent_h=sum(q11==1 & relationship==0,na.rm=T),
                                                              rels_time_spent_h=sum(q11==1 & relationship>0 & relationship<8,na.rm=T),
                                                              non_rels_aid_h=sum(q12==1 & relationship==0,na.rm=T),
                                                              rels_aid_h=sum(q12==1 & relationship>0 & relationship<8,na.rm=T))
                                                   

#Got all categories of relatives except distinguishing between far relatives

## Okay now we have mean geo_distance of rels and and non_rels                   


# reduce data2
# first dump all variables in the 3 MI indices 

data3 <- data2[-c(7,9,10,13,16:25,27,29,43,48,49,51)]

#dump  all other superfluous variables
data4 <- data3[c(1:6,28,29,33:35)]

# 1) link husbands (hnr) to data4

data5 <- data4 %>% left_join(wnr,c("idwife"="id_Questionaire"))
  
  

data6 <- data5 %>% left_join(hnr,c("idhusband"="id_Questionaire"))


data7 <- data6[-c(2,3)]
## Start HERE!!!!!! 
names(data7)
# reduce dataset - e.g. make percentages

# maximum 10 DV's - 5 husband and 5 wifes
# rename total nw
names(data7)[names(data7) == "total"] <- "NW_total"
names(data7)[names(data7) == "total_h"] <- "NW_total_h"

#1) total relatives in NW 
data7$rels_in_NW <- data7$NW_total-data7$non_rels
data7$rels_in_NW_h <- data7$NW_total_h-data7$non_rels_h
#2) percent relatives in NW ((parents_kids+pat_rels+mat_rels+in_laws+far_rels)/NW_total))
data7$percent_rels_in_NW <- (data7$parents_kids+data7$pat_rels+data7$mat_rels+data7$in_laws+data7$far_rels)/data7$rels_in_NW
data7$percent_rels_in_NW_h <- (data7$parents_kids_h+data7$pat_rels_h+data7$mat_rels_h+data7$in_laws_h+data7$far_rels_h)/data7$rels_in_NW_h
#3) mean geographic proximity to relatives (mean(geo_distance_rels))
#geo_distance_rels
#geo_distance_rels_h
#4) economic help (number relative loan and borrow) 
data7$rels_econ_help <- data7$rels_loan+data7$rels_loaning
data7$rels_econ_help_h <- data7$rels_loan_h+data7$rels_loaning_h

data7$non_rels_econ_help <- data7$non_rels_loan+data7$non_rels_loaning
data7$non_rels_econ_help_h <- data7$non_rels_loan_h+data7$non_rels_loaning_h

data7$percent_rels_econ_help <- (data7$rels_econ_help)/(data7$rels_econ_help+data7$non_rels_econ_help)
data7$percent_rels_econ_help_h <- (data7$rels_econ_help_h)/(data7$rels_econ_help_h+data7$non_rels_econ_help_h)

#5) emotional support

data7$emot_support_rels <- data7$rels_ask_advice+data7$rels_asking_advice+data7$rels_time_spent
data7$emot_support_rels_h <- data7$rels_ask_advice_h+data7$rels_asking_advice_h+data7$rels_time_spent_h

data7$emot_support_non_rels <- data7$non_rels_ask_advice+data7$non_rels_asking_advice+data7$non_rels_time_spent
data7$emot_support_non_rels_h <-  data7$non_rels_ask_advice_h+data7$non_rels_asking_advice_h+data7$non_rels_time_spent_h
  
  
data7$percent_rels_emot_support <- data7$emot_support_rels/(data7$emot_support_rels+data7$emot_support_non_rels)
data7$percent_rels_emot_support_h <- data7$emot_support_rels_h/(data7$emot_support_rels_h+data7$emot_support_non_rels_h)
                                                        
#7) childcare help 
data7$childcare_help_rels <- data7$rels_help_childcare+data7$help_rels_childcare

data7$childcare_help_non_rels <- data7$help_nonrels_childcare+data7$nonrels_help_childcare

data7$childcare_help_rels_percent <- data7$childcare_help_rels/(data7$childcare_help_rels+data7$childcare_help_non_rels)

#7) overall help 
data7$overall_help_rels <- data7$rels_econ_help + data7$emot_support_rels + data7$childcare_help_rels+data7$rels_aid
data7$overall_help_rels_h <- data7$rels_econ_help_h + data7$emot_support_rels_h + data7$help_rels_work_h+data7$rels_help_work_h+data7$rels_aid_h

data7$overall_help_non_rels <- data7$non_rels_econ_help+data7$emot_support_non_rels+data7$childcare_help_non_rels+
  data7$non_rels_aid

data7$overall_help_non_rels_h <- data7$non_rels_econ_help_h +data7$emot_support_non_rels_h+
  data7$nonrels_help_work_h+data7$help_nonrels_work_h+data7$non_rels_aid_h

data7$percent_overall_help_rels <- data7$overall_help_rels/(data7$overall_help_rels+data7$overall_help_non_rels)


data7$percent_overall_help_rels_h <- data7$overall_help_rels_h/(data7$overall_help_rels_h+data7$overall_help_non_rels_h)

newdata <- data7 %>% select (1:18,64:88)

# get religion variables
religion <- read_csv("Main_table_pgs_1-6.csv")
religion <- religion %>% select (1,2,4:27)
religion2 <- read_csv("Main_table_pgs_7-9.csv")
religion2 <-religion2 %>% select (1,131,132)
# then husbands
h1 <- read_csv("HusbandMain.csv")
h1 <- h1 %>% select(1,2,31:55)


## RELIGION

### relative religiosity
# How religious was/is your family compared to other families?
#"familyBariReligiousBefore"
# Before marriage
# More _____ Average _____ Less _____
plyr::count(religion2$familyBariReligiousBefore)



religion2$familyBariReligiousBefore <- dplyr::recode(religion2$familyBariReligiousBefore,'average'=0,'less'=-1,'more'=1)
religion2$familyBariReligiousBefore[is.na(religion2$familyBariReligiousBefore)]<- 0




# After marriage
# More _____ Average _____ Less _____



religion2$familyBariReligiousAfter <- dplyr::recode(religion2$familyBariReligiousAfter, "average" = 0,"less" = -1, "more"=1, "DK"=0)
religion2$familyBariReligiousAfter[is.na(religion2$familyBariReligiousAfter)]<- 0
plyr::count(religion2$familyBariReligiousAfter)


religion2$religious_change <- religion2$familyBariReligiousAfter-religion2$familyBariReligiousBefore
# link religion2 to newdata
newdata <- newdata %>% left_join(religion2,by= c("idwife"="id"))

# religious belief


religion$religion <- dplyr::recode(religion$religion, "Hindu" = 1,"HINDU" = 1, "ILAM"=0,"Islam"=0,"ISLAM"=0)

religion$religion[is.na(religion$religion)]<- 0
plyr::count(religion$religion)
## religious knowledge - make this a scale
#How did you learn about your religion? (multiple okay)
#  In madrassa/ school --"religiousEducationMuslim" , "religiousEducationFromSchool", "religiousEducationFromTemple"
#plyr::count(religion$religiousEducationMuslim) -- remove this - it is the same as religion (above)

## change al NA's to 0


#religion[, 2:26][is.na(religion[, 2:26])] <- 0


plyr::count(religion$religiousEducationFromSchool) ## 10 people
# 1 point
plyr::count(religion$religiousEducationFromTemple) ## 12 people
# 1 point
# imam/ hujur   -- "religiousEducationFromReligiousLeader"
# 1 point
plyr::count(religion$religiousEducationFromReligiousLeader) # 672 people
#  family -- "religiousEducationFromFamily" 
plyr::count(religion$religiousEducationFromFamily) # 158 people
# no one   -- "religiousEducationFromNoOne" 
plyr::count(religion$religiousEducationFromNoOne)## exclude this one (only one person)
# Relative  -- "religiousEducationFromRelatives"
plyr::count(religion$religiousEducationFromRelatives) # 25 people
# I did not learn -- "religiousEducationNeverLearned"
plyr::count(religion$religiousEducationNeverLearned) ## exclude this one (only one person)
# others -- "religiousEducationFromOthers"
plyr::count(religion$religiousEducationFromOthers) ## exclude this one (only 3 persons)


# Have you learned to read the Quran? -- "religiousEducationReadReligiousText"
# yes  no
plyr::count(religion$religiousEducationReadReligiousText) # 401 yes

# How much can you read the Quran?  -- "religiousEducationReadTextLevel", "religiousEducationReadTextBangla"       
# "religiousEducationReadTextSanskrit" 
plyr::count(religion$religiousEducationReadTextLevel) # 
religion$religiousEducationReadTextLevel <- dplyr::recode(religion$religiousEducationReadTextLevel, "Can" = 1,"Good" = 2, "Very good"=3)
religion$religiousEducationReadTextLevel[is.na(religion$religiousEducationReadTextLevel)]<- 0 # these are the can't read I am guessing

# Can read while seeing
# Good at reading 
# Very good at reading

plyr::count(religion$religiousEducationReadTextBangla) # 28 yes
plyr::count(religion$religiousEducationReadTextSanskrit) # 1 yes


#Did you ever learn to recite from the Qur'an?  -- "religiousEducationReciteText" 
plyr::count(religion$religiousEducationReciteText)# 59 yes

#   If Yes, how much of it did you memorize?? -- "religiousEducationMemorizeTextLevel"
# All   half    A little Nothing
plyr::count(religion$religiousEducationMemorizeTextLevel)
religion$religiousEducationMemorizeTextLevel <- dplyr::recode(religion$religiousEducationMemorizeTextLevel, "Nothing" = 0,"Little" = 1, "Half"=2,
                                                       "All"=3)
religion$religiousEducationMemorizeTextLevel[is.na(religion$religiousEducationMemorizeTextLevel)]<- 0 # these are the can't read I am guessing
# 704 are 0's

# Have you gone on Umrah? Yes No  -- "religiousEducationGoneOnUmrah"
plyr::count(religion$religiousEducationGoneOnUmrah)# 32 yes

#Have you gone on hajj? -- "religiousEducationGoneOnHajj"
plyr::count(religion$religiousEducationGoneOnHajj) # 41 yes

#How many people in your family have gone on Umrah? -- "religiousEducationNumberFamilyUmrah" 
plyr::count(religion$religiousEducationNumberFamilyUmrah) # 548 are 0 and 220 are 1 or more
#On hajj? -- "religiousEducationNumberFamilyHajj" 
plyr::count(religion$religiousEducationNumberFamilyHajj) # 525 are 0 and 200 are 1 or more

## extra questions:
# "religiousEducationReadOtherHolyBooks" 
plyr::count(religion$religiousEducationReadOtherHolyBooks) # 21 yes
#  "religiousEducationGoneOnPilgrimageIndia" 
plyr::count(religion$religiousEducationGoneOnPilgrimageIndia) # 5 yes
"religiousEducationGoneToHolyPlaces"   
plyr::count(religion$religiousEducationGoneToHolyPlaces) # 38 yes
#  "religiousEducationNumberPilgrimageIndia"
plyr::count(religion$religiousEducationNumberPilgrimageIndia) # 724 no - but these are hindus
"religiousEducationNumberHolyPlaces"
plyr::count(religion$religiousEducationNumberHolyPlaces) # 724 no

religion[, 2:26][is.na(religion[, 2:26])] <- 0

#####START HERE!!!!
religion$religious_knowledge_scale <- religion$religiousEducationFromSchool+religion$religiousEducationFromTemple+
  religion$religiousEducationFromReligiousLeader+religion$religiousEducationFromFamily+religion$religiousEducationFromRelatives-
  religion$religiousEducationFromNoOne-religion$religiousEducationNeverLearned+religion$religiousEducationReadReligiousText+
  religion$religiousEducationReadTextLevel+(2*religion$religiousEducationReadTextBangla)+(3*religion$religiousEducationReadTextSanskrit)+
  religion$religiousEducationReciteText+religion$religiousEducationMemorizeTextLevel+religion$religiousEducationGoneOnUmrah+
  religion$religiousEducationGoneOnHajj+(religion$religiousEducationNumberFamilyUmrah*0.5)+(religion$religiousEducationNumberFamilyHajj*0.5)+
  religion$religiousEducationReadOtherHolyBooks+religion$religiousEducationGoneOnPilgrimageIndia+religion$religiousEducationGoneToHolyPlaces+
  religion$religiousEducationNumberPilgrimageIndia+religion$religiousEducationNumberHolyPlaces


plyr::count(religion$religious_knowledge_scale) 

# calculate mens religious knowledge score next maybe (from h1)
# Next step - link to new data
religion <- religion %>% select (1,2,27)

newdata <- newdata %>% select (1:43)

newdata <- newdata %>% left_join (religion,by= c("idwife"="id")) 

# get number of kids and number in HH
hh <- read_csv("HHQPeopleinHH.csv")

hh$relationToRespondent <- dplyr::recode(hh$relationToRespondent, "Child" = 1)
hh$relationToRespondent <- as.numeric(hh$relationToRespondent)
hh$relationToRespondent[is.na(hh$relationToRespondent)]<- 0

d1 <- hh %>% group_by(id_Questionaire) %>% summarise(hh_total=n(), kids_in_hh=sum(relationToRespondent==1))


#Link back to new data
          
newdata <- newdata %>% left_join (d1,by= c("idwife"="id_Questionaire")) 

# link to religion 2
newdata <- newdata %>% left_join (religion2,by= c("idwife"="id")) 


write.csv(newdata,"newdata.csv", row.names = FALSE)



############################################################################
############################################################################
############################################################################
############################################################################HERE!!!!!

  
  
  
  

