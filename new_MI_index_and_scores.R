setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh")
library(readr)
library(tidyverse)
options(scipen = 999)
# read in the data
data <- read.csv("all_MI_data.csv")


# remove duplicates
data2 <- data[!duplicated(data$idwife), ]
data2$rowname <- rownames(data2)
# which variables have substantial missingness (n=282)
#age_h, institutionloan_vs_other_h,useInternet_h, watchTV_mins_per_month_h, bank_vs_micro_h, listenRadio_h
library(dplyr)

positions <- c(4,7:36,42:45,47:51)

complete_data <- data2 %>% select(positions)


############################################################################################################
############################################################################################################
############################################################################################################
############################################################################################################
############################################################################################################
#### EXPLORATORY FACTOR ANALYSIS - measuring the unobserved (latent) variables/ factors (latent constructs)
#EFA models are much more realistic as than PCA because they do not attmept to explain ALL the variance
# in the models

# Reduce dataset
drop <- c("idwife","listenRadio","institutionloan_vs_other","wifeAbroadTravel",
          "occupation_agriculture_dummy","bank_vs_micro","assetMobilePhone","occupation_ses",
          "marketIntegrationDhakaTime","marketIntegrationHighSchoolTime","foodSourceNow","labor_migrant_bari_in_bangladesh","husbandTravelINBangladesh",
          "wifeTravelINBangladesh","husbandAbroadTravel","assetElectricity","useInternet","familyBariEducationAfter",
          "relocate_MI","husband_occupation")
reduced_data <- complete_data[,!(names(complete_data) %in% drop)]


############################################################################################
#additional potential drops?? c("assetComputer","landOwnedFarmAmount" )   
# improve in some areas - but marginally
############################################################################################
# better to impute missing cases

#data_complete <- reduced_data[complete.cases(reduced_data), ] 

#Steps
#1) check for data factorability
# the bartlett sphericity test
library(polycor)
#calculate the correlations
data_hetcor <- hetcor(reduced_data,NA.method="pairwise.complete.obs")


#get the correlation matrix
data_c <- data_hetcor$correlations

# apply the bartlett test

library(psych)
data_factorability <- cortest.bartlett(data_c)

data_factorability

#### But can't use the bartlett sphericity test one with large sample sizes (e.g. sample/ variables >5)

### so we will use the Kaiser-Meyer-Olkin test for sampling adequacy

KMO(data_c)

# this test compares the partial correlation matrix with the pairwise correlation matrix
#The statistic is a measure of how small the partial corealtions are relative to the original zero order correlations
# the partial correaltion for each pair of varibales is comprised of the correlation between those variable after
#partialing
# out the influence of ALL of the other variables used in the factor analysis. If the variables shared 
#common factors then the
# partial correlations should be small and the KMO should be close to 1.  Standard practice is that the KMO should
# at least be in the 60's to be acceptable.

## what is mine?
# Kaiser-Meyer-Olkin factor adequacy
# Call: KMO(r = data_c)
# Overall MSA =  0.87
# MSA for each item = 
#   foodSecurity                   WEALTH_TOTAL_VAL                       total_income 
# 0.82                               0.75                               0.80 
# labor_migrant_hh_total             MacArthurLadderPresent marketIntegrationPrimarySchoolTime 
# 0.62                               0.83                               0.92 
# marketIntegrationCollegeTime   marketIntegrationSmallBazaarTime   marketIntegrationLargeBazaarTime 
# 0.92                               0.91                               0.90 
# marketIntegrationTownTime      marketIntegrationMainRoadTime      marketIntegrationPharmacyTime 
# 0.92                               0.96                               0.90 
# marketIntegrationMBBSTime            years_of_education_wife         years_of_education_husband 
# 0.88                               0.71                               0.75 
# occupation_mkt_connection                           age_wife                    assetSmartphone 
# 0.76                               0.65                               0.81 
# assetComputer                landOwnedFarmAmount 
# 0.78                               0.80 

#2) extract factors
library(psych)
library(GPArotation)
# EFA with 3 factors - the first argument can be a matrix (data_c or reduced_data)
#A correlation or covariance matrix or a raw data matrix. If raw data, the correlation matrix will be found using pairwise deletion.
#If covariances are supplied, they will be converted to correlations unless the covar option is TRUE.

#minres (minimum residual) - minimizes the residula matrix.  In other words it minimizes the differences between the
# correaltion matrix implied by the extracted factors and the original correlation matrix

f_data_minres <- fa(reduced_data, nfactors=3, rotate="none",  missing=TRUE)

# sorted by communality
f_data_minres_common <- sort(f_data_minres$communality, decreasing=TRUE)

# make a df for better overview
data.frame(f_data_minres_common)

# f_data_minres_common
# marketIntegrationLargeBazaarTime              0.8508527
# marketIntegrationTownTime                     0.8151899
# marketIntegrationMBBSTime                     0.7366401
# WEALTH_TOTAL_VAL                              0.6486280
# marketIntegrationCollegeTime                  0.6242082
# marketIntegrationSmallBazaarTime              0.6050392
# years_of_education_wife                       0.5967025
# marketIntegrationPharmacyTime                 0.5928636
# marketIntegrationPrimarySchoolTime            0.5544164
# age_wife                                      0.5360308
# marketIntegrationMainRoadTime                 0.4537930
# total_income                                  0.3800114
# MacArthurLadderPresent                        0.3630067
# years_of_education_husband                    0.3487644
# assetSmartphone                               0.2709403
# labor_migrant_hh_total                        0.2388972
# foodSecurity                                  0.2298665
# occupation_mkt_connection                     0.2215844
# assetComputer                                 0.1091134
# landOwnedFarmAmount                           0.1030974
# # the output above is the estimated commonality (% of variance explained by the
# #(extracted factors - here 3) and uniqueness (mirror image of commonality) vectors
# #variables with a relatively high commonality are being accounted fairly well by the four factors you
# chose to extract. Recall that a variable's commonality represents the percentage of the variable's variance
# that can be explained by the factors, 
# while the unique variance is due to variable's own idiosyncrasy and not to the common factors.


#3) choose the correct number of factors to retain


fa.parallel(reduced_data, fm = "mle", fa = "fa") ## choose this one
# Parallel analysis suggests that the number of factors =  4  and the number of components =  NA 


#4) rotate the factors to reflect the factor structure in a "better" way and this makes them easier to interpret.
# As long as the factor structure is not altered, meaning that the location of any variable
# in the factor space is not changed, the location of the axes is irrelevant. Rotation methods fall
# into two categories: orthogonal and oblique. Orthogonal methods produce uncorrelated factors whose axes
# maintain a 90 degrees angle between them, while oblique methods allow for slightly correlated factors. 
# The rule of thumb for choosing between the different rotation methods is that if the targeted factors 
# are initially assumed to be correlated, then an oblique rotation should be employed. On the other hand,
# if the factors are not related in any meaningful way, then an orthogonal rotation should be used. 

#-- I will use oblique methods

# oblimin and promax are oblique methods

# build a 3 factor model with varimax (othrogonal method) or oblimin (oblique method)
f_data_oblimon <- fa(reduced_data, fm="mle", nfactors=3, rotate = "varimax",  missing=TRUE)
print(f_data_oblimon)

# Output:
# Factor Analysis using method =  ml
# Call: fa(r = reduced_data, nfactors = 3, rotate = "varimax", 
#          missing = TRUE, fm = "mle")
# Standardized loadings (pattern matrix) based upon correlation matrix
#                                     ML1   ML2   ML3   h2   u2 com
# foodSecurity                        0.09  0.46  0.05 0.22 0.78 1.1
# WEALTH_TOTAL_VAL                    0.01  0.80  0.02 0.65 0.35 1.0
# total_income                       -0.03  0.61 -0.15 0.39 0.61 1.1
# labor_migrant_hh_total             -0.01  0.24 -0.42 0.24 0.76 1.6
# MacArthurLadderPresent             -0.05  0.59  0.06 0.35 0.65 1.0
# marketIntegrationPrimarySchoolTime  0.74  0.01 -0.04 0.55 0.45 1.0
# marketIntegrationCollegeTime        0.79  0.04 -0.03 0.63 0.37 1.0
# marketIntegrationSmallBazaarTime    0.78  0.00  0.00 0.60 0.40 1.0
# marketIntegrationLargeBazaarTime    0.92 -0.02 -0.01 0.84 0.16 1.0
# marketIntegrationTownTime           0.90 -0.02  0.03 0.82 0.18 1.0
# marketIntegrationMainRoadTime       0.68  0.00  0.03 0.46 0.54 1.0
# marketIntegrationPharmacyTime       0.76  0.04  0.08 0.59 0.41 1.0
# marketIntegrationMBBSTime           0.86  0.00  0.02 0.75 0.25 1.0
# years_of_education_wife             0.03  0.23  0.74 0.60 0.40 1.2
# years_of_education_husband          0.02  0.35  0.49 0.36 0.64 1.8
# occupation_mkt_connection           0.01  0.07  0.46 0.22 0.78 1.0
# age_wife                            0.03  0.16 -0.69 0.50 0.50 1.1
# assetSmartphone                     0.03  0.53  0.05 0.29 0.71 1.0
# assetComputer                       0.02  0.32  0.15 0.12 0.88 1.4
# landOwnedFarmAmount                -0.04  0.31 -0.07 0.11 0.89 1.1
# 
# ML1  ML2  ML3
# SS loadings           5.24 2.32 1.72
# Proportion Var        0.26 0.12 0.09
# Cumulative Var        0.26 0.38 0.46
# Proportion Explained  0.56 0.25 0.19
# Cumulative Proportion 0.56 0.81 1.00
# 
# Mean item complexity =  1.1
# Test of the hypothesis that 3 factors are sufficient.
# 
# The degrees of freedom for the null model are  190  and the objective function was  9.33 with Chi Square of  7063.81
# The degrees of freedom for the model are 133  and the objective function was  0.96 
# 
# The root mean square of the residuals (RMSR) is  0.03 
# The df corrected root mean square of the residuals is  0.04 
# 
# The harmonic number of observations is  766 with the empirical chi square  335.72  with prob <  0.00000000000000000017 
# The total number of observations was  766  with Likelihood Chi Square =  723.74  with prob <  0.00000000000000000000000000000000000000000000000000000000000000000000000000000000049 
# 
# Tucker Lewis Index of factoring reliability =  0.877
# RMSEA index =  0.076  and the 90 % confidence intervals are  0.071 0.082
# BIC =  -159.54
# Fit based upon off diagonal values = 0.99
# Measures of factor score adequacy             
# ML1  ML2  ML3
# Correlation of (regression) scores with factors   0.98 0.90 0.88
# Multiple R square of scores with factors          0.95 0.81 0.77
# Minimum correlation of possible factor scores     0.90 0.62 0.53

#5) interpret results
# path diagrams
library(psych)
fa.diagram(f_data_oblimon)
# check out the factor loadings
print(f_data_oblimon$loadings, cut=0)

#OUTPUT
# Loadings:
#                                       ML1    ML2    ML3   
# foodSecurity                        0.090  0.456  0.054
# WEALTH_TOTAL_VAL                    0.011  0.805  0.022
# total_income                       -0.028  0.606 -0.147
# labor_migrant_hh_total             -0.009  0.243 -0.420
# MacArthurLadderPresent             -0.051  0.588  0.060
# marketIntegrationPrimarySchoolTime  0.742  0.009 -0.038
# marketIntegrationCollegeTime        0.789  0.042 -0.034
# marketIntegrationSmallBazaarTime    0.776  0.003 -0.002
# marketIntegrationLargeBazaarTime    0.917 -0.016 -0.007
# marketIntegrationTownTime           0.904 -0.022  0.033
# marketIntegrationMainRoadTime       0.678 -0.003  0.028
# marketIntegrationPharmacyTime       0.763  0.043  0.077
# marketIntegrationMBBSTime           0.863  0.002  0.018
# years_of_education_wife             0.032  0.230  0.741
# years_of_education_husband          0.018  0.353  0.486
# occupation_mkt_connection           0.011  0.070  0.462
# age_wife                            0.034  0.164 -0.690
# assetSmartphone                     0.029  0.531  0.054
# assetComputer                       0.023  0.316  0.150
# landOwnedFarmAmount                -0.038  0.314 -0.071
# 
# ML1   ML2   ML3
# SS loadings    5.237 2.321 1.720
# Proportion Var 0.262 0.116 0.086
# Cumulative Var 0.262 0.378 0.464

# ML1 is commuting time to markets

# ML2 is wealth, income, use of interent, computers and smartphones, food security and
# farm land owned

# ML3 occupation, husband and wifes education, number of labor migrants in hh (negative relationship), relocation to 
# gain access to markets, degree of mkt connection



# Step #6   Link scores back to original data -# get id, spouse id, wife id, husband id and couple id back
#fa$scores  #returns a matrix with rownames that you can use to join/merge the data together.

#First, make sure data has rownames. If not, give it dummy names like:

rownames(reduced_data) <- 1:nrow(reduced_data)

#Then run model <- fa(...), and convert fa$scores to a data frame of factor scores. E.g.,

fs <- data.frame(f_data_oblimon$scores)
rownames(fs) <- 1:nrow(fs)
#Then, add a rowname column to both your original data and fs:
  
reduced_data$rowname <- as.numeric(rownames(reduced_data))
fs$rowname   <- as.numeric(rownames(fs))
#Then left join to data (using dplyr package):
  
  library(dplyr)
new_data <- left_join(reduced_data, fs, by = "rowname")

# join new_data to data2
# but first just get factors from new data
new_data <- new_data %>% select (21:24)
new_data$rowname <- as.numeric(new_data$rowname)

# link

data2$rowname <- 1:nrow(data2)
data2$rowname <- as.numeric(data2$rowname)



new_data <- left_join (data2, new_data, by="rowname") 

# rename factors

new_data <- new_data %>% rename(MI_geo_proximity = ML1, MI_economic_capital=ML2, MI_human_capital=ML3) 


### make sure new_data is okay- IT IS!!!

#cor(new_data$marketIntegrationLargeBazaarTime,new_data$marketIntegrationTownTime)
# supposed to be 0.8197575

# factors are
# a) human capital
# b) economic capital
# c) location market proximity

#### 1)REPEAT SIMPLIFIED ABOVE FOR MI INDEX SINGLE FACTOR
library(psych)
library(GPArotation)
# Use Nfactors
# 1) PCA - get factors - put everything in
library(FactoMineR)


# convert df to all numeric
reduced_data[] <- lapply(reduced_data, function(x) {
  if(is.integer(x)) as.numeric(as.character(x)) else x
})
sapply(reduced_data, class)

reduced_data <- reduced_data %>% select(1:20)
## deal with missing data

library(polycor)
#calculate the correlations
data_hetcor <- hetcor(reduced_data,NA.method="pairwise.complete.obs")

#get the correlation matrix
data_c <- data_hetcor$correlations


#### But can't use the bartlett sphericity test one with large sample sizes (e.g. sample/ variables >5)

### so we will use the Kaiser-Meyer-Olkin test for sampling adequacy

KMO(data_c)

# Kaiser-Meyer-Olkin factor adequacy
# Call: KMO(r = data_c)
# Overall MSA =  0.87
# MSA for each item = 
#   foodSecurity                   WEALTH_TOTAL_VAL                       total_income 
# 0.82                               0.75                               0.80 
# labor_migrant_hh_total             MacArthurLadderPresent marketIntegrationPrimarySchoolTime 
# 0.62                               0.83                               0.92 
# marketIntegrationCollegeTime   marketIntegrationSmallBazaarTime   marketIntegrationLargeBazaarTime 
# 0.92                               0.91                               0.90 
# marketIntegrationTownTime      marketIntegrationMainRoadTime      marketIntegrationPharmacyTime 
# 0.92                               0.96                               0.90 
# marketIntegrationMBBSTime            years_of_education_wife         years_of_education_husband 
# 0.88                               0.71                               0.75 
# occupation_mkt_connection                           age_wife                    assetSmartphone 
# 0.76                               0.65                               0.81 
# assetComputer                landOwnedFarmAmount 
# 0.78                               0.80 

### KEEP EVERYTHING IN SINGLE FACTOR ANALYSIS
library(psych)
library(GPArotation)
# EFA with 1 factors - the first argument can be a matrix (data_c or reduced_data)
#A correlation or covariance matrix or a raw data matrix. If raw data, the correlation matrix will be found using pairwise deletion.
#If covariances are supplied, they will be converted to correlations unless the covar option is TRUE.

#minres (minimum residual) - minimizes the residual matrix.  In other words it minimizes the differences between the
# correaltion matrix implied by the extracted factors and the original correlation matrix


# these are the variables with reasonable loadings onto a single factor c("foodSecurity","WEALTH_TOTAL_VAL",
#"total_income","MacArthurLadderPresent","years_of_education_wife",
#"years_of_education_husband","occupation_mkt_connection","assetSmartphone",
#"assetComputer","landOwnedFarmAmount")

reduced_data_2 <- reduced_data %>% select (1:3,5,14,15,16,18,19,20)
  
 
single_factor_pca1 <- fa(reduced_data_2, nfactors=1, rotate="none",  missing=TRUE)

# sorted by communality
single_factor_minres_common <- sort(single_factor_pca1$communality, decreasing=TRUE)

# make a df for better overview
# data.frame(single_factor_minres_common)
# single_factor_minres_common
# WEALTH_TOTAL_VAL                            0.62587383
# MacArthurLadderPresent                      0.36231786
# assetSmartphone                             0.27890475
# total_income                                0.26927369
# foodSecurity                                0.21565967
# years_of_education_husband                  0.19209388
# years_of_education_wife                     0.12342247
# assetComputer                               0.10729412
# landOwnedFarmAmount                         0.07587618
# occupation_mkt_connection                   0.03461785


 print(single_factor_pca1$loadings, cut=0)
# # 
 # Loadings:
 #   MR1  
 # foodSecurity               0.464
 # WEALTH_TOTAL_VAL           0.791
 # total_income               0.519
 # MacArthurLadderPresent     0.602
 # years_of_education_wife    0.351
 # years_of_education_husband 0.438
 # occupation_mkt_connection  0.186
 # assetSmartphone            0.528
 # assetComputer              0.328
 # landOwnedFarmAmount        0.275
 # 
 # MR1
 # SS loadings    2.285
 # Proportion Var 0.229




# combine commute time into a single variable
#reduced_data$avg_commute_time <- reduced_data$marketIntegrationPrimarySchoolTime/mean(reduced_data$marketIntegrationPrimarySchoolTime)+
#  reduced_data$marketIntegrationCollegeTime/mean(reduced_data$marketIntegrationCollegeTime)+
# reduced_data$marketIntegrationSmallBazaarTime/mean(reduced_data$marketIntegrationSmallBazaarTime)+
# reduced_data$marketIntegrationLargeBazaarTime/mean(reduced_data$marketIntegrationLargeBazaarTime)+
#  reduced_data$marketIntegrationLargeBazaarTime/mean(reduced_data$marketIntegrationLargeBazaarTime)+
#  reduced_data$marketIntegrationTownTime/mean(reduced_data$marketIntegrationTownTime)+
#  reduced_data$marketIntegrationMainRoadTime/mean(reduced_data$marketIntegrationMainRoadTime)+
# reduced_data$marketIntegrationPharmacyTime/mean(reduced_data$marketIntegrationPharmacyTime)+
# reduced_data$marketIntegrationMBBSTime/mean(reduced_data$marketIntegrationMBBSTime)


data_s <- reduced_data %>% select(6:13)

#c("marketIntegrationPrimarySchoolTime","marketIntegrationCollegeTime","marketIntegrationMainRoadTime",
#  "marketIntegrationLargeBazaarTime","marketIntegrationTownTime","marketIntegrationMainRoadTime",
#  "marketIntegrationPharmacyTime","marketIntegrationSmallBazaarTime")

library(polycor)
#calculate the correlations
data_hetcor <- hetcor(data_s,NA.method="pairwise.complete.obs")

#get the correlation matrix
data_c <- data_hetcor$correlations


#### But can't use the bartlett sphericity test one with large sample sizes (e.g. sample/ variables >5)

### so we will use the Kaiser-Meyer-Olkin test for sampling adequacy

KMO(data_c)
# Kaiser-Meyer-Olkin factor adequacy
# Call: KMO(r = data_c)
# Overall MSA =  0.92
# MSA for each item = 
#   marketIntegrationPrimarySchoolTime       marketIntegrationCollegeTime   marketIntegrationSmallBazaarTime 
# 0.93                               0.93                               0.91 
# marketIntegrationLargeBazaarTime          marketIntegrationTownTime      marketIntegrationMainRoadTime 
# 0.90                               0.92                               0.97 
# marketIntegrationPharmacyTime          marketIntegrationMBBSTime 
# 0.90                               0.89 

single_factor_pca2 <- fa(data_s,nfactors=1,n.obs = NA, rotate="oblimin", scores=TRUE, 
                        residuals=FALSE, SMC=TRUE, covar=FALSE,missing=TRUE,impute="median",min.err = 0.001,
                        max.iter = 50,symmetric=TRUE,warnings=TRUE,fm="minres",alpha=.1)



fa.diagram(single_factor_pca2)
# check out the factor loadings
print(single_factor_pca2$loadings, cut=0)

# Loadings:
#   MR1  
# marketIntegrationPrimarySchoolTime 0.743
# marketIntegrationCollegeTime       0.787
# marketIntegrationSmallBazaarTime   0.777
# marketIntegrationLargeBazaarTime   0.922
# marketIntegrationTownTime          0.903
# marketIntegrationMainRoadTime      0.673
# marketIntegrationPharmacyTime      0.765
# marketIntegrationMBBSTime          0.859
# 
# MR1
# SS loadings    5.216
# Proportion Var 0.652          
                                                                          

# get scores for PCA 1 and PCA composite 2
#single_factor_pca1 and single_factor_pca2

# Step #6   Link scores back to original data -# get id, spouse id, wife id, husband id and couple id back
#fa$scores  #returns a matrix with rownames that you can use to join/merge the data together.

#First, make sure data has rownames. If not, give it dummy names like:

rownames(data_s) <- 1:nrow(data_s)

#Then run model <- fa(...), and convert fa$scores to a data frame of factor scores. E.g.,

fs1 <- data.frame(single_factor_pca1$scores)
fs2 <- data.frame(single_factor_pca2$scores)
#Then, add a rowname column to both your original data and fs:

data_s$rowname <- rownames(data_s)
fs1$rowname   <- rownames(fs1)
fs2$rowname   <- rownames(fs2)
#Then left join to data (using dplyr package):

library(dplyr)
final_data <- left_join(data_s, fs1, by = "rowname")
final_data <- left_join(final_data, fs2, by = "rowname")
# join new_data to data2
# but first just get factors from new data
final_data <- final_data %>% select (10:11)
# link

final_data$rowname <- rownames(final_data)
final_data$rowname <- as.numeric(final_data$rowname)
new_data <- left_join (new_data, final_data, by="rowname") 

# rename factors

new_data <- new_data %>% rename(MI_single_factor1 = MR1.x, MI_single_factor2=MR1.y) 

# fix 

new_data$labor_migrant_hh_total <- max(new_data$labor_migrant_hh_total)/(new_data$labor_migrant_hh_total+1)
write.csv(new_data,"final_data.csv", row.names = FALSE)

#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
###3) Get a cronbachs alpha
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
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

######HERE########################################################
######HERE########################################################
######HERE########################################################
######HERE########################################################
######HERE########################################################
######HERE########################################################


# these are the 3 objects
#single_factor_pca2
#single_factor_pca1
#summary(f_data_oblimon)

print(f_data_oblimon, cut = 0.3, digits = 3)


# Factor Analysis using method =  ml
# Call: fa(r = reduced_data, nfactors = 3, rotate = "varimax", 
#          missing = TRUE, fm = "mle")
# Standardized loadings (pattern matrix) based upon correlation matrix
# ML1    ML2    ML3    h2    u2  com
# foodSecurity                               0.456        0.219 0.781 1.11
# WEALTH_TOTAL_VAL                           0.805        0.648 0.352 1.00
# total_income                               0.606        0.390 0.610 1.12
# labor_migrant_hh_total                           -0.420 0.235 0.765 1.60
# MacArthurLadderPresent                     0.588        0.351 0.649 1.04
# marketIntegrationPrimarySchoolTime  0.742               0.552 0.448 1.01
# marketIntegrationCollegeTime        0.789               0.626 0.374 1.01
# marketIntegrationSmallBazaarTime    0.776               0.602 0.398 1.00
# marketIntegrationLargeBazaarTime    0.917               0.841 0.159 1.00
# marketIntegrationTownTime           0.904               0.818 0.182 1.00
# marketIntegrationMainRoadTime       0.678               0.461 0.539 1.00
# marketIntegrationPharmacyTime       0.763               0.589 0.411 1.03
# marketIntegrationMBBSTime           0.863               0.745 0.255 1.00
# years_of_education_wife                           0.741 0.603 0.397 1.19
# years_of_education_husband                 0.353  0.486 0.361 0.639 1.83
# occupation_mkt_connection                         0.462 0.218 0.782 1.05
# age_wife                                         -0.690 0.504 0.496 1.12
# assetSmartphone                            0.531        0.286 0.714 1.03
# assetComputer                              0.316        0.123 0.877 1.44
# landOwnedFarmAmount                        0.314        0.105 0.895 1.13
# 
# ML1   ML2   ML3
# SS loadings           5.237 2.321 1.720
# Proportion Var        0.262 0.116 0.086
# Cumulative Var        0.262 0.378 0.464
# Proportion Explained  0.564 0.250 0.185
# Cumulative Proportion 0.564 0.815 1.000
# 
# Mean item complexity =  1.1
# Test of the hypothesis that 3 factors are sufficient.
# 
# The degrees of freedom for the null model are  190  and the objective function was  9.325 with Chi Square of  7063.807
# The degrees of freedom for the model are 133  and the objective function was  0.958 
# 
# The root mean square of the residuals (RMSR) is  0.034 
# The df corrected root mean square of the residuals is  0.041 
# 
# The harmonic number of observations is  766 with the empirical chi square  335.723  with prob <  0.000000000000000000166 
# The total number of observations was  766  with Likelihood Chi Square =  723.741  with prob <  0.000000000000000000000000000000000000000000000000000000000000000000000000000000000488 
# 
# Tucker Lewis Index of factoring reliability =  0.8769
# RMSEA index =  0.0761  and the 90 % confidence intervals are  0.0708 0.0817
# BIC =  -159.536
# Fit based upon off diagonal values = 0.986
# Measures of factor score adequacy             
# ML1   ML2   ML3
# Correlation of (regression) scores with factors   0.975 0.901 0.876
# Multiple R square of scores with factors          0.951 0.811 0.767
# Minimum correlation of possible factor scores     0.902 0.622 0.533
## Get alpha

names(reduced_data)

PA1 = c("marketIntegrationCollegeTime", "marketIntegrationSmallBazaarTime", "marketIntegrationLargeBazaarTime",
        "marketIntegrationTownTime", "marketIntegrationPrimarySchoolTime", "marketIntegrationMainRoadTime",
        "marketIntegrationPharmacyTime","marketIntegrationMBBSTime")
alpha.pa1 = alpha(data[PA1],check.keys=TRUE)
print(alpha.pa1, digits = 3)

# OUTPUT
# Reliability analysis   
# Call: alpha(x = reduced_data[PA1])
# 
# raw_alpha std.alpha G6(smc) average_r  S/N     ase mean   sd median_r
# 0.835     0.936   0.939     0.645 14.5 0.00494 12.7 16.2    0.635
# 
# lower alpha upper     95% confidence boundaries
# 0.825 0.835 0.845 
# 
# Reliability if an item is dropped:
#   raw_alpha std.alpha G6(smc) average_r  S/N alpha se   var.r med.r
# marketIntegrationCollegeTime           0.820     0.928   0.930     0.648 12.9  0.00526 0.01037 0.643
# marketIntegrationSmallBazaarTime       0.832     0.929   0.930     0.650 13.0  0.00519 0.01118 0.646
# marketIntegrationLargeBazaarTime       0.811     0.919   0.920     0.618 11.3  0.00558 0.00838 0.622
# marketIntegrationTownTime              0.788     0.920   0.922     0.622 11.5  0.00623 0.00901 0.613
# marketIntegrationPrimarySchoolTime     0.832     0.931   0.933     0.659 13.5  0.00513 0.00963 0.643
# marketIntegrationMainRoadTime          0.803     0.936   0.939     0.676 14.6  0.00535 0.00771 0.653
# marketIntegrationPharmacyTime          0.796     0.930   0.930     0.654 13.2  0.00611 0.01006 0.643
# marketIntegrationMBBSTime              0.859     0.923   0.922     0.632 12.0  0.00622 0.00923 0.643
# 
# Item statistics 
# n raw.r std.r r.cor r.drop  mean    sd
# marketIntegrationCollegeTime       766 0.750 0.820 0.790  0.710  6.15 10.81
# marketIntegrationSmallBazaarTime   766 0.736 0.812 0.780  0.712  6.97  6.48
# marketIntegrationLargeBazaarTime   766 0.873 0.915 0.915  0.850  7.17 10.99
# marketIntegrationTownTime          766 0.906 0.902 0.898  0.876  7.49 17.49
# marketIntegrationPrimarySchoolTime 766 0.696 0.786 0.746  0.666  7.62  6.99
# marketIntegrationMainRoadTime      766 0.773 0.733 0.671  0.657 18.19 28.67
# marketIntegrationPharmacyTime      766 0.826 0.803 0.772  0.770 18.27 18.75
# marketIntegrationMBBSTime          766 0.938 0.871 0.863  0.839 29.95 52.27

PA2 = c("foodSecurity", "WEALTH_TOTAL_VAL", "total_income","MacArthurLadderPresent","years_of_education_husband",
        "assetSmartphone","assetComputer","landOwnedFarmAmount")

alpha.pa2 = alpha(data[PA2],check.keys=TRUE)
print(alpha.pa2, digits = 3)

#OUTPUT

# Reliability analysis   
# Call: alpha(x = reduced_data[PA2])
# 
# raw_alpha std.alpha G6(smc) average_r S/N    ase  mean    sd median_r
# 0.232     0.714    0.71     0.238 2.5 0.0111 34283 20950    0.238
# 
# lower alpha upper     95% confidence boundaries
# 0.21 0.232 0.254 
# 
# Reliability if an item is dropped:
#   raw_alpha std.alpha G6(smc) average_r  S/N alpha se   var.r med.r
# foodSecurity                0.236858     0.689   0.682     0.241 2.22 0.011345 0.01654 0.248
# WEALTH_TOTAL_VAL            0.001059     0.626   0.605     0.193 1.67 0.000137 0.00809 0.171
# total_income                0.000316     0.673   0.663     0.227 2.06 0.000037 0.01509 0.194
# MacArthurLadderPresent      0.236851     0.662   0.655     0.219 1.96 0.011345 0.01506 0.194
# years_of_education_husband  0.236848     0.710   0.701     0.259 2.45 0.011345 0.01657 0.260
# assetSmartphone             0.236858     0.678   0.670     0.231 2.11 0.011345 0.01559 0.227
# assetComputer               0.236860     0.718   0.707     0.267 2.55 0.011345 0.01433 0.261
# landOwnedFarmAmount         0.236656     0.719   0.709     0.268 2.56 0.011346 0.01397 0.264
# 
# Item statistics 
# n raw.r std.r r.cor r.drop        mean         sd
# foodSecurity               766 0.351 0.565 0.462  0.351      5.5509      0.760
# WEALTH_TOTAL_VAL           766 0.979 0.783 0.797  0.503 240927.4661 144379.508
# total_income               766 0.667 0.627 0.560  0.502  33295.1633  39269.151
# MacArthurLadderPresent     766 0.481 0.666 0.608  0.481      4.5235      1.886
# years_of_education_husband 766 0.258 0.482 0.343  0.258      4.3081      4.442
# assetSmartphone            766 0.474 0.607 0.528  0.474      0.5261      0.500
# assetComputer              766 0.302 0.446 0.299  0.302      0.0496      0.217
# landOwnedFarmAmount        766 0.283 0.442 0.291  0.282     24.4792     65.307


# #PA3 = c("labor_migrant_hh_total",
#         "years_of_education_wife",
#         "husband_occupation", 
#         "occupation_mkt_connection",                       
#         "age_wife")

 
PA3 = c("years_of_education_wife",                      
        "occupation_mkt_connection",                       
        "age_wife","years_of_education_husband")


keys <- c(1,1,-1,1)
alpha.pa3 =alpha(reduced_data[PA3],keys)
print(alpha.pa3, digits = 3)

# Reliability analysis   
# Call: alpha(x = reduced_data[PA3], keys = keys)
# 
# raw_alpha std.alpha G6(smc) average_r  S/N    ase mean   sd median_r
# 0.459     0.693   0.654      0.36 2.25 0.0212 9.92 4.34    0.333
# 
# lower alpha upper     95% confidence boundaries
# 0.418 0.459 0.501 
# 
# Reliability if an item is dropped:
#   raw_alpha std.alpha G6(smc) average_r  S/N alpha se   var.r med.r
# years_of_education_wife        0.250     0.539   0.444     0.281 1.17   0.0257 0.00388 0.313
# occupation_mkt_connection      0.464     0.661   0.603     0.394 1.95   0.0244 0.02649 0.460
# age_wife-                      0.603     0.660   0.577     0.393 1.94   0.0176 0.01115 0.345
# years_of_education_husband     0.373     0.640   0.550     0.372 1.78   0.0173 0.00596 0.345
# 
# Item statistics 
# n raw.r std.r r.cor r.drop  mean    sd
# years_of_education_wife    766 0.719 0.804 0.739  0.577  3.61  3.96
# occupation_mkt_connection  766 0.458 0.686 0.496  0.401  1.08  1.19
# age_wife-                  766 0.910 0.687 0.525  0.398 30.69 12.69
# years_of_education_husband 766 0.548 0.708 0.573  0.329  4.31  4.44
# 
# Non missing response frequency for each item
# 0 1 2 3  miss
# [1,] 1 0 0 0 0.999

fa.diagram(single_factor_pca1)
fa.diagram(single_factor_pca2)
# # check out the factor loadings
print(single_factor_pca2$loadings, cut=0)
# Loadings:
#   MR1  
# marketIntegrationPrimarySchoolTime 0.743
# marketIntegrationCollegeTime       0.787
# marketIntegrationSmallBazaarTime   0.777
# marketIntegrationLargeBazaarTime   0.922
# marketIntegrationTownTime          0.903
# marketIntegrationMainRoadTime      0.673
# marketIntegrationPharmacyTime      0.765
# marketIntegrationMBBSTime          0.859
# 
# MR1
# SS loadings    5.216
# Proportion Var 0.652

PA_all1 = c("marketIntegrationPrimarySchoolTime","marketIntegrationCollegeTime","marketIntegrationMainRoadTime",
            "marketIntegrationLargeBazaarTime","marketIntegrationTownTime","marketIntegrationMainRoadTime",
            "marketIntegrationPharmacyTime","marketIntegrationMBBSTime")
alpha.pa_all1 = alpha(reduced_data[PA_all1])
print(alpha.pa_all1, digits = 3)

# Reliability analysis   
# Call: alpha(x = reduced_data[PA_all1])
# 
# raw_alpha std.alpha G6(smc) average_r  S/N     ase mean   sd median_r
# 0.867     0.935   0.913     0.644 14.4 0.00456 14.1 18.6    0.635
# 
# lower alpha upper     95% confidence boundaries
# 0.858 0.867 0.876 
# 
# Reliability if an item is dropped:
#   raw_alpha std.alpha G6(smc) average_r  S/N alpha se  var.r med.r
# marketIntegrationPrimarySchoolTime     0.870     0.933   0.900     0.664 13.9  0.00478 0.0156 0.643
# marketIntegrationCollegeTime           0.861     0.929   0.897     0.652 13.1  0.00489 0.0172 0.643
# marketIntegrationMainRoadTime          0.832     0.929   0.930     0.650 13.0  0.00519 0.0112 0.646
# marketIntegrationLargeBazaarTime       0.854     0.921   0.889     0.624 11.6  0.00512 0.0172 0.607
# marketIntegrationTownTime              0.837     0.920   0.888     0.621 11.5  0.00557 0.0168 0.607
# marketIntegrationMainRoadTime.1        0.832     0.929   0.930     0.650 13.0  0.00519 0.0112 0.646
# marketIntegrationPharmacyTime          0.844     0.930   0.898     0.656 13.3  0.00542 0.0174 0.643
# marketIntegrationMBBSTime              0.883     0.923   0.888     0.631 12.0  0.00509 0.0171 0.643
# 
# Item statistics 
# n raw.r std.r r.cor r.drop  mean    sd
# marketIntegrationPrimarySchoolTime 766 0.670 0.764 0.733  0.643  7.62  6.99
# marketIntegrationCollegeTime       766 0.726 0.802 0.782  0.689  6.15 10.81
# marketIntegrationMainRoadTime      766 0.847 0.808 0.688  0.776 18.19 28.67
# marketIntegrationLargeBazaarTime   766 0.847 0.892 0.898  0.824  7.17 10.99
# marketIntegrationTownTime          766 0.888 0.902 0.910  0.858  7.49 17.49
# marketIntegrationMainRoadTime.1    766 0.847 0.808 0.688  0.776 18.19 28.67
# marketIntegrationPharmacyTime      766 0.799 0.791 0.767  0.746 18.27 18.75
# marketIntegrationMBBSTime          766 0.910 0.871 0.875  0.803 29.95 52.27

print(single_factor_pca1$loadings, cut=0)
# Loadings:
#   MR1  
# foodSecurity               0.464
# WEALTH_TOTAL_VAL           0.791
# total_income               0.519
# MacArthurLadderPresent     0.602
# years_of_education_wife    0.351
# years_of_education_husband 0.438
# occupation_mkt_connection  0.186
# assetSmartphone            0.528
# assetComputer              0.328
# landOwnedFarmAmount        0.275
# 
# MR1
# SS loadings    2.285
# Proportion Var 0.229


PA_all2 = c ("foodSecurity","WEALTH_TOTAL_VAL","total_income","MacArthurLadderPresent","years_of_education_wife",
             "years_of_education_husband","occupation_mkt_connection","assetSmartphone",
            "assetComputer","landOwnedFarmAmount")


alpha.pa_all2 = alpha(reduced_data[PA_all2])
print(alpha.pa_all2, digits = 3)

# Reliability analysis   
# Call: alpha(x = reduced_data[PA_all2])
# 
# raw_alpha std.alpha G6(smc) average_r S/N    ase  mean    sd median_r
# 0.226     0.715   0.735       0.2 2.5 0.0108 27427 16760    0.171
# 
# lower alpha upper     95% confidence boundaries
# 0.204 0.226 0.247 
# 
# Reliability if an item is dropped:
#   raw_alpha std.alpha G6(smc) average_r  S/N  alpha se  var.r med.r
# foodSecurity                0.228406     0.692   0.715     0.200 2.25 0.0109397 0.0214 0.170
# WEALTH_TOTAL_VAL            0.001027     0.651   0.662     0.171 1.86 0.0001328 0.0145 0.157
# total_income                0.000318     0.688   0.705     0.197 2.20 0.0000357 0.0187 0.166
# MacArthurLadderPresent      0.228399     0.674   0.697     0.187 2.07 0.0109397 0.0195 0.164
# years_of_education_wife     0.228402     0.695   0.704     0.202 2.28 0.0109399 0.0187 0.178
# years_of_education_husband  0.228397     0.683   0.697     0.193 2.15 0.0109399 0.0198 0.170
# occupation_mkt_connection   0.228409     0.722   0.735     0.224 2.60 0.0109397 0.0169 0.195
# assetSmartphone             0.228406     0.684   0.706     0.194 2.17 0.0109397 0.0205 0.178
# assetComputer               0.228408     0.708   0.729     0.212 2.43 0.0109397 0.0219 0.195
# landOwnedFarmAmount         0.228212     0.720   0.738     0.223 2.58 0.0109402 0.0188 0.195
# 
# Item statistics 
# n  raw.r std.r r.cor r.drop        mean         sd
# foodSecurity               766 0.3512 0.532 0.442 0.3512      5.5509      0.760
# WEALTH_TOTAL_VAL           766 0.9793 0.725 0.733 0.5026 240927.4661 144379.508
# total_income               766 0.6671 0.555 0.491 0.5024  33295.1633  39269.151
# MacArthurLadderPresent     766 0.4809 0.621 0.569 0.4809      4.5235      1.886
# years_of_education_wife    766 0.1734 0.518 0.456 0.1733      3.6123      3.961
# years_of_education_husband 766 0.2577 0.578 0.528 0.2577      4.3081      4.442
# occupation_mkt_connection  766 0.0279 0.369 0.247 0.0279      1.0836      1.189
# assetSmartphone            766 0.4743 0.571 0.502 0.4743      0.5261      0.500
# assetComputer              766 0.3015 0.447 0.328 0.3015      0.0496      0.217
# landOwnedFarmAmount        766 0.2826 0.378 0.243 0.2822     24.4792     65.307


# STILL TO DO
# 
# Check how to weight loadings for new scale


library(fmsb)

data_10<- data%>% select(7:51)

omega_data = omega(data_10,fm="mle")
omega_data
# Alpha:                 0.63 
# G.6:                   0.78 
# Omega Hierarchical:    0.36 
# Omega H asymptotic:    0.5 
# Omega Total            0.71 

# The first two pieces of info are as in alpha, the next regard ?? specifically. ??
# is based on the squared factor loadings. ??hierarchical regards just the loadings of 
# the general factor. The asymptotic is the same for a 'test of infinite items', and so
# can be seen as an upper bound. ??total is based on all the general and specific factor loadings.
# I personally like to think of the ratio of ??hier??total,
# which if very high, say .9 or so, may suggest unidimensionality.

omega_data2 = omega(data[PA_all1],fm="mle")
omega_data2

omega_data3 = omega(data[PA_all2],fm="mle")
omega_data3
# Alpha:                 0.71 
# G.6:                   0.74 
# Omega Hierarchical:    0.54 
# Omega H asymptotic:    0.7 
# Omega Total            0.78

omega_data4 = omega(data[PA1],fm="mle")
omega_data4
# Alpha:                 0.94 
# G.6:                   0.94 
# Omega Hierarchical:    0.84 
# Omega H asymptotic:    0.88 
# Omega Total            0.96

omega_data5 = omega(data[PA2],fm="mle")
omega_data5

# Alpha:                 0.71 
# G.6:                   0.71 
# Omega Hierarchical:    0.65 
# Omega H asymptotic:    0.86 
# Omega Total            0.75

omega_data6 = omega(data[PA3],fm="mle")
omega_data6

# Alpha:                 0.69 
# G.6:                   0.65 
# Omega Hierarchical:    0.53 
# Omega H asymptotic:    0.68 
# Omega Total            0.77



# The difference is mainly that omega_t gives an reliability estimate of the overall variance
# in the data that is due to a general factor and also specific factors.
# The omega hierarchical is a reliability estimate for the variance that is due to the general factor only (of which omega h 
#asymptotic is the maximum explained varinace - at the limit.

