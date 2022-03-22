setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh")
options(scipen=999)
library(readr)
library(tidyverse)
data <- read.csv("new_MI_data.csv")


data2 <- data[!duplicated(data$idwife), ]
# make MI indices
# first categorize occupations
##START HERE####### with MIdata 
# occupations codes: 
#0 jobless, 6 cattle/chicken/duck, 12 skilled labor,
#19 professional (doctor, engineer), 1 agriculture (own land), 7 handicraft,13 remittance, 20 student,
# 2 agriculture (share crops) taken, 8 tailoring works, 14 food for work,21 semi-professional (teacher, nurse),
#3 land mortgage/lease/rent given, 9 business,15 old age allowance/NGD,
#22 local government, 4 day labour,10 service, 16 house or shop rent,
#23 driver/rickshaw-puller, 5 fishing,11 pension, 18 housewife, 24 other (give note)
#7 handicraft, 24 other

# x freq
# 1   0  126
# 2   1  148
# 3   2   55
# 4   3    1
# 5   4   67
# 6   5   12
# 7   6    5
# 8   7    4
# 9   8    6
# 10  9  131
# 11 10  141
# 12 11    3
# 13 12   25
# 14 13    8
# 15 15    1
# 16 19    3
# 17 21    4
# 18 23   21
# 19 24    4
# 20 25    1

#- from least to most integrated
# 2013 PNAS paper
## Make occupations into 4 factors
#1) farming and animal husbandry (ag=1, non-ag=0)
data$occupation_agriculture_dummy <- ifelse(data$husband_occupation==6 | data$husband_occupation==1|data$husband_occupation==2|
                                        data$husband_occupation==5,1,0)


#2) connected to markets (low to high) e.g. dependency on markets
# age 
# 0 lowest fishing and all agriculture husbandry and fishing - (least - feed yourself etc...) sharecroppers
# 1 day labor etc..   anybody selling their labor, not engaged in primary food production, rickshaw drivers
# 2 business , have a small business
# 3 education based jobs - service. professional, semi-professional 

                                                  
attach(data)
data$occupation_mkt_connection[husband_occupation==0|husband_occupation==6 | husband_occupation==1|husband_occupation==2|
                                 husband_occupation==5|husband_occupation==18|husband_occupation==24|husband_occupation==25|
                                 husband_occupation==11|husband_occupation==13|husband_occupation==15||husband_occupation==24|husband_occupation==25] <- 0

data$occupation_mkt_connection[husband_occupation==4| husband_occupation==23|
                                 husband_occupation==12|husband_occupation==8|husband_occupation==7|husband_occupation==3] <- 1


data$occupation_mkt_connection[husband_occupation==9 |husband_occupation==16] <- 2


data$occupation_mkt_connection[husband_occupation==19 |husband_occupation==21|husband_occupation==20|husband_occupation==10] <- 3
detach(data)
                ####                      HERE!!!!!
#3) SES (low to high)
# low SES rickshaw drivers- education based jobs at the top, skilled labor above unskilled labor.
attach(data)
data$occupation_ses[husband_occupation==0| husband_occupation==4|husband_occupation==2|
husband_occupation==3|husband_occupation==14|husband_occupation==23] <- 0

data$occupation_ses[husband_occupation==5| husband_occupation==6|husband_occupation==7|husband_occupation==8 |husband_occupation==1 |
husband_occupation==11|husband_occupation==13|husband_occupation==15|husband_occupation==18|husband_occupation==24|husband_occupation==25] <- 1


data$occupation_ses[husband_occupation==9|husband_occupation==12|
husband_occupation==16 |husband_occupation==20] <- 2


data$occupation_ses[husband_occupation==10 |husband_occupation==19|husband_occupation==21|husband_occupation==22] <- 3
detach(data)


#  215 jobless,  (less integrated)
# 239 agriculture, own land (farming)
#  95 agriculture, share crops taken (farming)
#  2 land mortgage/lease/rent given (unknown)
# 112 day labour (somewhat intergrated)
# 19 fishing (less integrated)
# 10 cattle/chicken/duck (less integrated)
# 7 handicraft
# 10 tailoring works
# 228 business (most intergrated)
# 200 service (more integrated)
#  5 pension
# 39 skilled labor 
# 10 remittance
# 2 old age allowance/NGD
# 2 housewife
# 5 professional (doctor, engineer) (most integrated)
# 8 semi-professional (teacher, nurse) (most integrated)
# 37 driver/rickshaw-puller
#  6 other
data$familyBariEducationAfter <- as.character(data$familyBariEducationAfter)
# fix familyBariEducationAfter
attach(data)
data$familyBariEducationAfter[familyBariEducationAfter=="average"] <- 1
data$familyBariEducationAfter[familyBariEducationAfter=="less"] <- 0
data$familyBariEducationAfter[familyBariEducationAfter=="more"] <- 2
data$familyBariEducationAfter[familyBariEducationAfter=="less\n\n\nmore"] <- NA
data$familyBariEducationAfter[familyBariEducationAfter=="DK"] <- NA
detach(data)

# 
data$familyBariEducationAfter <- as.integer(data$familyBariEducationAfter)

# check these
data$marketIntegrationDhakaTime <- ifelse(data$marketIntegrationDhakaTime==1802,180,
                                          data$marketIntegrationDhakaTime)
data$marketIntegrationMBBSTime <- ifelse(data$marketIntegrationMBBSTime==1800,180,
                                         data$marketIntegrationMBBSTime)
data$marketIntegrationTownTime <- ifelse(data$marketIntegrationTownTime>180,180,
                                         data$marketIntegrationTownTime)
data$marketIntegrationSmallBazaarTime <- ifelse(data$marketIntegrationSmallBazaarTime>60,60,
                                                data$marketIntegrationSmallBazaarTime)


# standardize and invert time to market minutes
# manually invert the times to ... so that all variables are in the same direction
# e.g. 0 = least integrated , 10 = most integrated
data$marketIntegrationPrimarySchoolTime <- max(data$marketIntegrationPrimarySchoolTime)/(data$marketIntegrationPrimarySchoolTime+1)
data$marketIntegrationHighSchoolTime <- max(data$marketIntegrationHighSchoolTime)/(data$marketIntegrationHighSchoolTime+1)
data$marketIntegrationCollegeTime <- max(data$marketIntegrationCollegeTime)/(data$marketIntegrationCollegeTime+1)
data$marketIntegrationSmallBazaarTime <- max(data$marketIntegrationSmallBazaarTime)/(data$marketIntegrationSmallBazaarTime+1)
data$marketIntegrationLargeBazaarTime <- max(data$marketIntegrationLargeBazaarTime)/(data$marketIntegrationLargeBazaarTime+1)
data$marketIntegrationTownTime <- max(data$marketIntegrationTownTime)/(data$marketIntegrationTownTime+1)
data$marketIntegrationMainRoadTime <- max(data$marketIntegrationMainRoadTime)/(data$marketIntegrationMainRoadTime+1)
data$marketIntegrationDhakaTime <- max(data$marketIntegrationDhakaTime)/(data$marketIntegrationDhakaTime+1)
data$marketIntegrationPharmacyTime <- max(data$marketIntegrationPharmacyTime)/(data$marketIntegrationPharmacyTime+1)
data$marketIntegrationMBBSTime <- max(data$marketIntegrationMBBSTime)/(data$marketIntegrationMBBSTime+1)


# run correlations
#data <- data %>% select(4,7:46)

library("Hmisc")

#mydata.rcorr = rcorr(as.matrix(data))
#mydata.rcorr

# fix data$foodSecurity

data$foodSecurity <- recode(data$foodSecurity, '0'=6,'1'= 6, '2' = 5,
                              '3'=4,'4'=3,'5'=2,'6'=1)

data$age_wife<- 2018-data$YOB_wife
data$age_wife <- ifelse(data$age_wife>83,83,
                        data$age_wife)


data$age_h<- 2018-data$yearOfBirth_h

# drop variables
data = subset(data, select = -c(yearOfBirth_h,YOB_wife))




# reverse dummy code of ag vs not
data$occupation_agriculture_dummy <- recode(data$occupation_agriculture_dummy,'0'=1,'1'=0)

#run correlations

#data <- data[complete.cases(data), ]
#x <- data[40]
#y <- data[1:40]
#cor(x, y)



# food source now can't be included in wealth index
# they assort big time on education
# relocate_MI is strange - maybe drop
# institutionloan_vs_other is strange - maybe drop
# bank_vs_micro is strange - maybe drop
# institutionloan_vs_other_h might be useless
# keep agricultural dummy separate from income and wealth
# I might not have occupation_ses correct - check with mary
# age of wife is weirdly connect to MI variables

# get 10-12 assets of particular interest for MI
w1 <- read.csv("Main_table_pgs_1-6.csv")

plyr::count(w1$landOwnedFarmAmount) # see below 


# convert land owned farm amount to standardize
w1$landOwnedFarmAmount <- ifelse(w1$landOwnedUnit==1,w1$landOwnedFarmAmount,
                                 ifelse(w1$landOwnedUnit==2,w1$landOwnedFarmAmount*33,
                                        ifelse(w1$landOwnedUnit==3,w1$landOwnedFarmAmount*6,
                                               ifelse(w1$landOwnedUnit==4,w1$landOwnedFarmAmount*2,
                                                      ifelse(w1$landOwnedUnit==5,w1$landOwnedFarmAmount*1.33,
                                                             0)))))
                                 
                                 
               
                                 
# if using must reverse score it
plyr::count(w1$landOwnedHouseAmount) # see below (ask mary)- less concerned with this one!
plyr::count(w1$landOwnedUnit)
#Amount of land owned):(Tick One): 1 = decimal ; 2 = bigha ; 3 = gonda ; 4 = katha  5 = kora 6 = other 
#Owned House Land (you and your husband) __________ owned Farm Land (you and your husband)________

#Here is the land conversion data I have; let me know if this does not solve the problem.  I would put 
#everything in decimals if you can and use that amount.

#4 Kora = 1 gonda

#20 gonda = 1 kani

#80 kora = 1 kani

#120 decimals = 1 kani
# 1 decimal =1/120 kani


#1 kattha = 2 decimals

#1 kattha = 720 square feet (based on online research) 


plyr::count(w1$assetComputer)
plyr::count(w1$assetSmartphone)
#plyr::count(w1$assetSmartphoneNumber)
plyr::count(w1$assetMobilePhoneNumber)
plyr::count(w1$assetElectricity)
plyr::count(w1$assetElectricityYears)
plyr::count(w1$assetTelevision)

plyr::count(w1$assetSolarPanel)
#Land Inheritance

w <- w1 %>% select (1,112,,110,118,43)
w[is.na(w)]<-0
colnames(w) <- c('id','assetComputer', 'assetSmartphone','landOwnedFarmAmount','assetElectricity')
# remove duplicates
data <- data[!duplicated(data$idwife), ]
# link w to data
data <- data %>% left_join(w, by=c("idwife"="id"))
plyr::count(data$landOwnedFarmAmount)



### save MI_data here
write.csv(data,"all_mi_data.csv", row.names = FALSE)


