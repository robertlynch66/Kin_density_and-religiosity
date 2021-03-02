# first add idwife
data$id<- as.character(data$id)
data$spouse_id<- as.character(data$spouse_id)
data$idwife <- ifelse(data$sex==0,data$id,data$spouse_id)

data <- data %>% select (1,2,3,57,42,4:41,43:56)

write.csv(data,"data.csv", row.names = FALSE)

data <- read.csv("data.csv")


## START Below HERE#######################################
## START Below HERE#######################################
## START Below HERE#######################################
## START Below HERE#######################################
## START Below HERE#######################################


setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh")
library(readr)
library(tidyverse)
data <- read.csv("data.csv")





# add wife report on bank_vs_micro

data <- data %>% left_join(w3, c("idwife"="id"))

data <- data  %>% 
  rename(bank_vs_micro_w=bank_vs_micro
  )

data <- data %>% select (1:58)




cor.test(data$travel_in_bangladesh,data$travel_in_bangladesh_h)
#### HERE (Look for NA's and use wifes info on husband as default)
table(data$skilledWork, useNA = "always") 


# select wife as default but note when the correlations are off

# dump total_income_h but note that correlation with wife is only 0.22 and that husbands reports on average 10k less than wives - check within categories of income
# note total income monthly between men and women r = 0.07, incomeYearlyCrops r =-0.02, incomeMonthlyPension r=0.00,
# incomeMonthlyRentsInvestments =0.37, incomeMonthlyRemittance -0.01, 

# husband yearly crops and husband total income r=0.63

#####It is like they are reporting completely different households!!!


########HERE!!!!!!

# dump main occupation_h (not a 0.64 correl with wife report)
# dump institutionloan_vs_other_h but note correlation with wife is only 0.36 (might be for different person - e.g. wife vs husband, 
#did I take a loan vs did our family take a loan?
# dump bank vs micro husband but note vs wife reports cor is only 0.14 (check pairwise)
# dump MacArthurLadderPresent_h but note correlation with wife is 0.31  (check averages - distance)

# 13% of husbands reported doing skilled work but 38% of wive's did - does this make sense? Yes - DO NOT use this variable for MI
# dump skilled work


# must add travel abroad wife or wifes report on husbands travel both in Bangladesh and abroad???
# husband and wifes report of husbands travel in bangladesh is correlated at 0.36
# husband and wifes report of husbands travel abroad is correlated at 0.56

data2 <- data %>% select(1:6,9,10,11,13,14,15,16,17,18,19:28,29,30,31,35:42,43,44,47,48,49,50,53,54,55,56)
# # must check travel Bangladesh wife or wifes report on husbands travel???

x <- read_csv("Main_table_pgs_7-9.csv")
x <- x %>% select(1,87:92)

data2$id <- as.character(data2$id)

data3 <- data2 %>% left_join(x,c("idwife"="id"))


data3$husbandTravelINBangladesh <- data3$husbandDhakaVisit+data3$husbandBangladeshVisit
data3$wifeTravelINBangladesh <- data3$respondentBangladeshVisit+data3$respondentDhakaVisit
data3$wifeAbroadTravel <- data3$respondentAbroadVisit


data4 <- data3 %>% left_join(h,c("idwife"="id"))

h<- read_csv("HusbandMain.csv")

h <- h %>% select(2,56:58)
h$husbandTravelINBangladesh_h <- h$respondentDhakaVisit+h$respondentBangladeshVisit
h$HusbandAbroadTravel_h <- h$respondentAbroadVisit
h <- h %>% select(1,5:6)

data5 <- data3 %>% left_join(h,c("idhusband"="idhusband"))

data5$husbandAbroadTravel <- data5$husbandAbroadVisit




data5$foodSourceNow <- recode(data5$foodSourceNow, "all from bazar"=100,"25 percent from own work" = 75, "50 percent from own work" = 50,
                              "75 percent from own work"=25,"all from own work"=0)


data5$foodSourceNow_h <- recode(data5$foodSourceNow_h, "all from bazar"=100,"25 percent from own work" = 75, "50 percent from own work" = 50,
                              "75 percent from own work"=25,"75 percent from own work."=25,"all from own work"=0)


# food source now wife and husband good correlation =0.61


data5<- data5 %>% select (1:25,27:37,40:42,44:46,53,54,55,58)

data5 <- data5 %>% rename (
                           education_medium_wife=educationmedium_w,
                           years_of_education_wife=edulevel_years_of_education_values_0to19__blank_is_dont_know_w,
                           YOB_wife=yearOfBirth,
                           years_of_education_husband=husband_years_education_w,
                           education_medium_husband=husband_ed_medium_w,
                           )
data5 <- data5 %>% rename (husband_occupation=husband_occupation_w,
                           husband_in_school=husband_in_school_w,
                           institutionloan_vs_other=institutionloan_vs_other_w)


# dump education medium - not enough variance
data5 <- data5 %>% select (1:28,31,32,35:46)

data5 <- data5 %>% select (1:32,39:42,33:38)
#listenRadio_h,                                                 
                                              
# add bank vs micro wife
data6 <- data5 %>% left_join(w3,c("idwife"="id"))



data6 <- data6 %>% select (1:32,43,33:42)

write.csv(data6)
write.csv(data6,"new_MI_data.csv", row.names = FALSE)

# create data to compare
# w is womens main
w <- read_csv("Main_table_pgs_1-6.csv")
# h is husbands main

#hp is husband people nw
hp <- read_csv("HusbandPeople.csv")
w1 <- w %>% select (1,141,142,143,144,146)

w1[is.na(w1)]<-0
h1 <- hp %>% select (1,2,9,10,11,12,14)

#Comparison dataset (overwrite) then link to h1 and w1
data2 <- data %>% select (1:6,13,57)

data3 <- data2 %>% left_join(w1, c("idwife"="id"))
data3$idhusband<- as.character(data3$idhusband)
# replace NA's with 0's

# data 4 is what we want to compare husbands and wives on
data4 <- data3 %>% left_join(h1, c("idhusband"="id_Questionaire"))

# select variables to correlate
data6 <- data5 %>% select (47:52)
res <- cor(data6, use = "complete.obs")
round(res, 2)

####HERE
#NO MI data here just kin networks stuff mainly (get what you need from social network data)
network_data <- read_csv("HHQPeopleinNW.csv")
# relationship codes: Relation code: 0 = not a relative, 1 = nijer poribar (parents and kids), 2 = babar bari (fathers family- 2nd or 3rd degree relatives),
#3 = mayer bari (mother side - 2nd or 3rd degree relatives), 4 = shoshur bari (in-laws, affinal), 
#5= babar bari (far relative), 6 = mayer bari (far relative), 7 = shoshur bari (far relative) see Koster et. al. Phil Trand 2019 - for how to quantiy density of kin networks
# 2016 Shenk - Current Anthro

# e.g. for counting freqeuncies
#library(plyr)
#count(h, 'MacArthurLadderTelephonePresent_h')

# relationship type: Type of relationship code: N= Neighbor, C= Colleague, F = Friend, B = Business relation, O = Other

# location:  Location Codes
#1=khana member,(same household)
#2=near bari/neighbor (walking distance), 
#3=other place in Matlab,
#4=Other Place in Bangladesh, 
#5=Abroad, 

#q1 = Who helps you with childcare or housework when you need it? (poribar, attiyo, friends, neighbors)
#q2= Who do you help with childcare or housework? (poribar, attiyo, friends, neighbors)
#q3 = Who would you ask for a small loan of Tk. 100-1000 if you needed money? (poribar, attiyo, friends, neighbors)
#q4 = Who would ask you for a small loan of Tk. 100-1000? (poribar, attiyo, friends, neighbors)
#q5 = Who would you ask for a large loan if you needed money? (poribar, attiyo, friends, neighbors)
#q6 = Who would ask you for a large loan? (poribar, attiyo, friends, neighbors)
#q7 = Who do you ask for advice or discuss important matters with? (poribar, attiyo, friends, neighbors)
#q8 = Who asks you for advice or discusses important matters with you? (poribar, attiyo, friends, neighbors)
#q9 = Who do you know personally who has local social influence, prestige, or wealth? (poribar, attiyo, friends, neighbors)
#q10 = Do you serve as a prestigious or wealthy social connection for anyone else? (poribar, attiyo, friends, neighbors)
#q11 = Who do you spend the most time with? (poribar, attiyo, friends, neighbours; including talking over phone once in a week or more)
#q12 = If some problem happens, who comes to your aid to help you? (poribar, attiyo, friends, neighbors)

# mens survey only ten questions

## HERE!!!!#####################################################
w2<- read_csv("Main_table_pgs_7-9.csv")
w3 <- read_csv("Main_table_pg_10.csv")
