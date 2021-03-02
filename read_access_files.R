setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh")
library(readr)
library(tidyverse)
# get women main pages
w <- read.csv("total_wealth.csv")
w1 <- read.csv("Main_table_pgs_1-6_2.csv")
w1 <- w1 %>% select (1,4,7:8,142,156:157)
w2 <-  left_join(w1,w, by=c("id"="hhid"))

###########################################
# add hh income
HHP <- read_csv("HHQPeopleinHH.csv")
# read in cleaned education data
HHC <- read_csv("HHQPeopleEducation_cleaned.csv")

# link cleaned ed data (HHc) to HHP
HHP <- left_join(HHP,HHC, by=c("id"="id_hhmember"))

# add number of labor migrants in HH past plus present

#replace NA's with zeroes
HHP[is.na(HHP)]<-0
library(tidyr)
library(tidyverse)
d <- HHP %>% group_by(id_Questionaire) %>% count(id_Questionaire) 

d1 <- HHP %>% group_by(id_Questionaire) %>% summarise (hh_income=sum(incomeTotalMonthly))

# join and d1 by id_Questionaire- id+questionnaire is repondent id (I think)
d2 <- left_join(d,d1, by="id_Questionaire")

#add remittances, rents, pension, crop income
d1 <- HHP %>% group_by(id_Questionaire) %>% summarise (hh_remit=sum(incomeMonthlyRemittance))

d3 <- left_join(d2,d1, by="id_Questionaire")
#add pension
d1 <- HHP %>% group_by(id_Questionaire) %>% summarise (hh_pension=sum(incomeMonthlyPension))

d4 <- left_join(d3,d1, by="id_Questionaire")
#add crops
d1 <- HHP %>% group_by(id_Questionaire) %>% summarise (hh_crops=sum(incomeYearlyCrops/12))

d5 <- left_join(d4,d1, by="id_Questionaire")

# add rents and investments
d1 <- HHP %>% group_by(id_Questionaire) %>% summarise (hh_rents=sum(incomeMonthlyRentsInvestments))

d6 <- left_join(d5,d1, by="id_Questionaire")


d6$total_income <- d6$hh_income+d6$hh_remit+d6$hh_pension+d6$hh_crops+d6$hh_rents 

# get total number of hh members and their spouses who were labor migrants now or in the past  
HHP[is.na(HHP)]<-0
d1 <- HHP %>% group_by(id_Questionaire) %>% summarise (labor_migrant_hh_total=sum(laborMigrantPast+spouseLaborMigrantNow+spouseLaborMigrantPast+
                                                                                laborMigrantNow))
                                                       
d7 <- left_join(d6,d1, by="id_Questionaire")                                                                           
# write d2 to csv file
write.csv(d7,"Total_HH_income.csv", row.names = FALSE)


w1 <- read.csv("Total_HH_income.csv")

data <- left_join(w2,w1, by=c("id"="id_Questionaire"))
data$WEALTH_TOTAL_VAL <-as.numeric(data$WEALTH_TOTAL_VAL)

rm("d", "d1", "d2","d3", "d4", "d5","d6")
data[is.na(data)]<-0



w3<- read.csv("Main_table_pgs_7-9.csv")
# select mkt int variables
w3 <- w3 %>% select (1:3,10,11,18,19,26,27,34,35,42,43,50,51,58,59,63,64,71,72,87,88,89,90,91,92,111:122,130,139,141:145)

#replace hours with minutes and add to minutes
w3$marketIntegrationPrimarySchoolTime <- w3$marketIntegrationPrimarySchoolHours*60 +w3$marketIntegrationPrimarySchoolMinutes
w3$marketIntegrationHighSchoolTime <- w3$marketIntegrationHighSchoolHours*60 +w3$marketIntegrationCollegeMinutes
w3$marketIntegrationCollegeTime <- w3$marketIntegrationCollegeHours*60 + w3$marketIntegrationCollegeMinutes
w3$marketIntegrationSmallBazaarTime <- w3$marketIntegrationSmallBazaarHours*60 + w3$marketIntegrationSmallBazaarMinutes
w3$marketIntegrationLargeBazaarTime <- w3$marketIntegrationLargeBazaarHours*60 + w3$marketIntegrationLargeBazaarMinutes
w3$marketIntegrationTownTime <- w3$marketIntegrationTownHours*60 + w3$marketIntegrationTownMinutes
w3$marketIntegrationMainRoadTime <- w3$marketIntegrationMainRoadHours*60 + w3$marketIntegrationMainRoadMinutes
w3$marketIntegrationDhakaTime <- w3$marketIntegrationDhakaHours*60 + w3$marketIntegrationDhakaMinutes
w3$marketIntegrationPharmacyTime <- w3$marketIntegrationPharmacyHours*60 + w3$marketIntegrationPharmacyMinutes
w3$marketIntegrationMBBSTime <- w3$marketIntegrationMBBSHours*60 + w3$marketIntegrationMBBSMinutes

w3 <- w3 %>% select (1,22:56)

# join to data
MIdata <- data %>% left_join(w3,by="id")


write.csv(MIdata,"MI_data.csv", row.names = FALSE)
# read in pg 10 _ nothing here is relevant I don't think -except loans maybe
#w4 <- read_csv("Main_table_pg_10.csv")


library(tidyverse)
MIdata <- read.csv("MI_data.csv")

MIdata <- MIdata %>% select (1:5,7,8,9,15,16,17:51)

# count and weight the number of visits people from your natal and marital Bari have travelled to 
MIdata$travel_in_bangladesh <- MIdata$husbandDhakaVisit+MIdata$husbandBangladeshVisit+MIdata$respondentDhakaVisit+MIdata$respondentBangladeshVisit
  
MIdata$travel_abroad <- MIdata$husbandAbroadVisit+MIdata$respondentAbroadVisit

#rename n to no in hh
names(MIdata)[names(MIdata) == "n"] <- "No_in_hh"
MIdata <- MIdata %>% select (1:10,17:46)

# get number of individuals form natal and marital bari who work or live elsewhere
MIdata$labor_migrant_bari_in_bangladesh<-MIdata$natalBariDhakaMale+MIdata$natalBariDhakaFemale+MIdata$maritalBariDhakaMale+MIdata$maritalBariDhakaFemale+
MIdata$natalBariBangladeshMale+MIdata$natalBariBangladeshFemale+MIdata$maritalBariBangladeshMale+MIdata$maritalBariBangladeshFemale

MIdata$labor_migrant_bari_abroad <- MIdata$natalBariAbroadMale+MIdata$natalBariAbroadFemale+MIdata$maritalBariAbroadMale+MIdata$maritalBariAbroadFemale

MIdata <- MIdata %>% select (1:10,23:41)

HHP <- HHP %>% select (1,2,3,4,5,8,9,10,23,34,53:55)
HHP1 <- HHP %>% filter(relationToRespondent=="Respondent")
HHP2 <- HHP %>% filter(relationToRespondent=="Husband")
HHP2 <- HHP2 %>% select(1,9,11,12,13)

HHP2 <- HHP2  %>% 
  rename(husband_occupation=mainOccupation,                                             
         husband_years_education=edulevel_years_of_education_values_0to19__blank_is_dont_know,
         husband_in_school=inschool,                                                  
         husband_ed_medium=educationmedium
  )
### CHECK FOR SPOUSES OCCUPATION HERE
MIdata <- left_join(MIdata, HHP1, by=c("id"="id_Questionaire"))

MIdata <- left_join(MIdata, HHP2, by=c("id"="id_Questionaire"))

MIdata <- MIdata %>% select (1:29,31:34,37:45)



MIdata$watchTVPer <- ifelse(MIdata$watchTVPer=="week",4,ifelse(MIdata$watchTVPer=="day",30,ifelse(MIdata$watchTVPer=="month",1,0)))
MIdata[is.na(MIdata)]<-0

MIdata$watchTV_mins_per_month <- MIdata$watchTVPer*((MIdata$watchTVHours*60)+MIdata$watchTVMinutes)
  
MIdata <- MIdata %>% select (1:13,17:34,36:43)
# codes for use internet and listen radio
#0=  never or almost never, 1 = daily, 2 = weekly, 3 = monthly, 4 = few times/year


rm("d7","data","HHC","HHP","w","w1","w2","w3")

# get relocate for better MI data 0 or 1
w3<- read.csv("Main_table_pgs_7-9.csv")

w3 <- w3 %>% select (1,95:110)
w3[is.na(w3)]<-0
w3$relocate_MI <- w3$relocationJob+w3$relocationFinancialImprovement+w3$relocationBetterEducation+w3$relocationBetterHealthcare+
                        w3$relocationBetterInfrastructure+w3$closerToPrimarySchool+w3$closerToCollege+w3$closerToMarket+w3$closerToTown+
  w3$closerToMainRoad+w3$closerToCity
w3$relocate_MI <- ifelse(w3$relocate_MI>0,1,0)

w3 <- w3 %>% select (1,18)

MIdata <- left_join(MIdata, w3, by="id")
# dump type of skilled work
MIdata <- MIdata %>% select (1:3,5:41)

# add loans bank vs microcredit vs other
w3 <- read_csv("Main_table_pg_10.csv")

w3 <- w3 %>% select (1,93:99)
w3[is.na(w3)]<-0
w3[w3=="microcredit"]<-2
w3[w3=="bank"]<-1
w3 <- w3 %>% select (1,3,5,7)

w3$bank_vs_micro <- ifelse(w3$loanInstitution1Type==1|w3$loanInstitution2Type==1|w3$loanInstitution3Type==1,1,0)
w3$institutionloan_vs_other <- ifelse(w3$loanInstitution1Type==1|w3$loanInstitution2Type==1|w3$loanInstitution3Type==1|w3$loanInstitution1Type==2|w3$loanInstitution2Type==2|w3$loanInstitution3Type==2,1,0)

w3 <- w3 %>% select (1,5,6)
MIdata <- left_join(MIdata, w3, by="id")

# make religion all caps
MIdata$religion <- toupper(MIdata$religion)

write.csv(MIdata,"w_MI_data.csv", row.names = FALSE)


MIdata <- read.csv("MI_data.csv")


h <- read.csv("HusbandMain.csv")
h <- h %>% select (1,2,5,8,9,56:59,61,63:67,97:103)


hp <- read_csv("HusbandPeople.csv")
hp <- hp %>% select (1,2,3,4,9,10,11,12,14)
h <- left_join (h, hp, by=c("idhusband"="id_Questionaire"))

# rename all variables to husband h_ after fixing
h$travel_in_bangladesh_h <- h$respondentDhakaVisit+h$respondentBangladeshVisit

h$travel_abroad_h <- h$respondentAbroadVisit

h$watchTVPer <- ifelse(h$watchTVPer=="week",4,ifelse(h$watchTVPer=="day",30,ifelse(h$watchTVPer=="month",1,0)))
h[is.na(h)]<-0

h$watchTV_mins_per_month_h <- h$watchTVPer*((h$watchTVHours*60)+h$watchTVMinutes)

h$loanInstitution1Type <- as.character(h$loanInstitution1Type)
h$loanInstitution2Type <- as.character(h$loanInstitution2Type)
h$loanInstitution3Type <- as.character(h$loanInstitution3Type)
h[h=="microcredit"]<-2
h[h=="bank"]<-1
h[h==""]<-0

h$bank_vs_micro_h <- ifelse(h$loanInstitution1Type==1|h$loanInstitution2Type==1|h$loanInstitution3Type==1,1,0)
h$institutionloan_vs_other_h <- ifelse(h$loanInstitution1Type==1|h$loanInstitution2Type==1|h$loanInstitution3Type==1|h$loanInstitution1Type==2|h$loanInstitution2Type==2|h$loanInstitution3Type==2,1,0)

h$total_income_h <- h$incomeTotalMonthly+h$incomeYearlyCrops+h$incomeMonthlyRentsInvestments+h$incomeMonthlyPension+h$incomeMonthlyRemittance

h <- h%>% select(1,2,4,5,9,10,11,15,24,25,31:36)

# rename variables
h <- h  %>% 
  rename(
    foodSourceNow_h=foodSourceNow,
    skilledWork_h=skilledWork,                    
    MacArthurLadderPresent_h=MacArthurLadderPresent,
    MacArthurLadderTelephonePresent_h=MacArthurLadderTelephonePresent,
    listenRadio_h=listenRadio,
    useInternet_h=useInternet,                  
    yearOfBirth_h=yearOfBirth,
    mainOccupation_h= mainOccupation)
    
w_MI_data <- read_csv("w_MI_data.csv")
newdata <- left_join (w_MI_data,h, by=c("id"="id.x"))
# look at h




##START HERE####### with MIdata 
# occupations codes: 
#0 jobless, 6 cattle/chicken/duck, 12 skilled labor,
#19 professional (doctor, engineer), 1 agriculture (own land), 7 handicraft,13 remittance, 20 student,
# 2 agriculture (share crops) taken, 8 tailoring works, 14 food for work,21 semi-professional (teacher, nurse),
#3 land mortgage/lease/rent given, 9 business,15 old age allowance/NGD,
#22 local government, 4 day labour,10 service, 16 house or shop rent,
#23 driver/rickshaw-puller, 5 fishing,11 pension, 18 housewife, 24 other (give note)
#- from least to most integarted

## Make occupations into 4 factors - husband plus spouse - figure out how to stack husbands!!
#1) farming and animal husbandry - 6, 2,1,
#2) almost all women are llisted as housewives
#3)
#HERE!!!! STACK husbands and wives with a couple id - make couple id in R then stack in excel
# make a couple id if husband reported information
rm(hp,w3,HHP1,HHP2)



# temp file 
write.csv(newdata,"all_MI_data2.csv", row.names = FALSE)



data <- read_csv("all_MI_data.csv")


data$spouse_id[is.na(data$spouse_id)]<-0

#add couple id
data$couple_id <- with(data, {m1 = ifelse(id < spouse_id, id, spouse_id);
m2 = ifelse(id < spouse_id, spouse_id, id);
return(as.numeric(interaction(m1, m2, drop=TRUE)))})

write.csv(data,"new_MI_data.csv", row.names = FALSE)

####################################################################
####################################################################
####################################################################START HERE!!!!#############
####################################################################START HERE!!!!#############
####################################################################START HERE!!!!#############
####################################################################START HERE!!!!#############
data <- read.csv("new_MI_data.csv")
# re-order columnn
my_data2 <- my_data[, c(5, 4, 1, 2, 3)]


# linked data
data <- read_csv("all_MI_data.csv")


data$spouse_id[is.na(data$spouse_id)]<-0

#add couple id
data$couple_id <- with(data, {m1 = ifelse(id < spouse_id, id, spouse_id);
m2 = ifelse(id < spouse_id, spouse_id, id);
return(as.numeric(interaction(m1, m2, drop=TRUE)))})

write.csv(data,"new_MI_data.csv", row.names = FALSE)


data <- read.csv("new_MI_data.csv")
# re-order columnn
data <- data[, c(1,2,58,3:57)]

data <- data %>% select(1:39, 41:58)

data <- data  %>% 
  rename(watchTV_mins_per_month_h=watchTV_mins_per_month_h_1
  )


data <- read.csv("data.csv")
data <- data %>% select(1:40, 42:57)

data <- data  %>% 
  rename(bank_vs_micro_h=bank_vs_micro_h_1,
         institutionloan_vs_other_w=institutionloan_vs_other
  )

write.csv(data,"data.csv", row.names = FALSE)

# fix spouse id
data$spouse_id <- as.character(data$spouse_id)
data$id <- as.character(data$id)
data$spouse_id[is.na(data$spouse_id)]<-0

data <- subset(data, select = -c(couple_id) )
#add couple id
data$couple_id <- with(data, {m1 = ifelse(id < spouse_id, id, spouse_id);
m2 = ifelse(id < spouse_id, spouse_id, id);
return(as.numeric(interaction(m1, m2, drop=TRUE)))})

data <- data %>% select (1,2,56,4,3,5:55)
write.csv(data,"data.csv", row.names = FALSE)
####################################################################
####################################################################
####################################################################START HERE!!!!#############
####################################################################START HERE!!!!#############
####################################################################START HERE!!!!#############
####################################################################START HERE!!!!#############

