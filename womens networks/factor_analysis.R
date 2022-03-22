setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh")
library(readr)
library(tidyverse)
options(scipen = 999)
# read in the data
data <- read.csv("all_MI_data.csv")

# remove duplicates
data2 <- data[!duplicated(data$idwife), ]

# which variables have substantial missingness (n=282)
#age_h, institutionloan_vs_other_h,useInternet_h, watchTV_mins_per_month_h, bank_vs_micro_h, listenRadio_h
library(dplyr)

positions <- c(4,7:36,42:45,47:51)

complete_data <- data2 %>% select(positions)

#Steps (make directions all same) - use ordered facators with levels
# Use Nfactors
# 1) PCA - get factors - put everything in
library(FactoMineR)
new_data <- complete_data %>% select(1:40)

# convert df to all numeric
new_data[] <- lapply(new_data, function(x) {
  if(is.integer(x)) as.numeric(as.character(x)) else x
})
sapply(new_data, class)
data_pca <- PCA(new_data)

# NOTE - Missing values are imputed by the mean of the variable: you should use the impute PCA function of the missMDA package


# before you start to make MI dimension based on theory
#1) Human capital - education/ occupation
#2) geographic proximity to markets (time to variables)
#3) access to information -  smart phone, computer, radio, tv, education etc...
#4) reliance on markets (e.g. food produced, proximity, bank loans, agriculture)


#Mary's suggested MI domains: Dependence on supply chains
#1) Total wealth: assets (Nolin may have some R code for this), income, Macarthur ladder ranking
#2) Occupational integration: Education, occupation (e.g. agricultural vs non agricultural vs skilled labor...)
#3) Contact domain: (access to information about the world): tech access, geographic proximity to markets (i.e. cities), education, GIS data
#4) Agricultural vs non-agricultural: (complicated)
#5) Urbanization: GIS data, geographic proximity to markets
#ALL - dump all variable into PCA and see what domains comes out


# Run a PCA for the 10  numeric variables of data.  ncp tells how many dimesnsion to extract (I think)
data_pca <- PCA(data[,9:19], ncp = 4, graph = FALSE)

# Get the summary of the first 100 rows
summary(data_pca, nbelements = 100)
# Get the variance of the first 4 new dimensions.
data_pca$eig[,2][1:4]


# Get the cumulative variance of the first 20 dimensions.
data_pca$eig[,3][1:20]

data_pca <- PCA(data2, graph = FALSE)
data_pca$eig
#look at percentage of variance explained by each component

data_pca$var$cos2
# the closer this value is to 1 the better the quality

data_pca$var$contrib

dimdesc(data_pca)


data2 <- data[complete.cases(data), ]
# Run a PCA with active and supplementary variables
#data_pca <- PCA(data2, quanti.sup = 1:8, quali.sup = 20:21, graph = FALSE)
#data_pca

data_pca <- PCA(data2, ncp = 1, graph = FALSE)
# Get the most correlated variables
dimdesc(data_pca, axes = 1:4)
#
# Run a PCA on the first 100 rows and the middle 20 variables
#pca_output_hundred <- PCA(data2, quanti.sup = 1:2, quali.sup = 43:44, ind.sup=101:nrow(data2), graph = FALSE)

# get the contributions of the varibales
data_pca$var$contrib




# now try PCA with the ade4 package
library(ade4)

pca_dudi <- dudi.pca(data2,scannf=FALSE, nf=4)

summary(pca_dudi)

# use factoextra to visualize factoMiner objects
library(factoextra)
# a key number to unserstand in PCA models is cos2 which shows the accurate representation fo your varibales on the 
# PC plane

data_pca <- PCA(data2, ncp=2,graph = FALSE)
#data_pca <- PCA(data2, ncp = 4, graph = FALSE)
# Create a factor map for the variables with cos2 > 0.7

fviz_pca_var(data_pca, select.var = list(cos2 = 0.5), repel = TRUE)

# Modify the code to create a factor map for the individuals.
fviz_pca_ind(data_pca, select.ind = list(cos2 = 0.7), repel = TRUE)


#### GOOD ONE TO KNOW AND USE!!
# Create a barplot for the variables with the highest cos2 in the 1st PC.
fviz_cos2(data_pca, choice = "var",axes = 1,  top = 10)


# Create a factor map for the top 5 variables with the highest contributions.
fviz_pca_var(data_pca, select.var = list(contrib = 4), repel = TRUE)


# Create a factor map for the top 5 individuals with the highest contributions.
fviz_pca_ind(data_pca, select.ind = list(contrib = 5), repel = TRUE)


# Create a barplot for the variables with the highest contributions to the 1st PC.
fviz_contrib(data_pca, choice = "var", axes = 3, top = 15)

# Create a barplot for the variables with the highest contributions to the 2nd PC.
fviz_contrib(data_pca, choice = "var", axes = 2, top = 10)

# Create a barplot for the variables with the highest contributions to the 2nd PC.
fviz_contrib(data_pca, choice = "var", axes = 3, top = 10)

## biplots and ellipsoids
# Create a biplot with no labels for all individuals with the geom argument.
fviz_pca_biplot(data_pca)

###### habillage doesn't work 
data_pca <- PCA(data2)

library(FactoMineR) 
library(factoextra)

data(iris)

res.pca<-PCA(iris , scale.unit=TRUE, ncp=4, quali.sup=c(5), graph =  FALSE)

fviz_pca_biplot(res.pca, label="var", habillage=5,
                addEllipses=TRUE) + theme_minimal()

### use my data
fviz_pca_ind(res.pca2)

# Change title and axis labels
fviz_pca_ind(res.pca2) +
  labs(title ="PCA", x = "PC1", y = "PC2")

# Change axis limits by specifying the min and max
fviz_pca_ind(res.pca2) +
  xlim(-4, 4) + ylim (-4, 4)

# Use text only
fviz_pca_ind(res.pca, geom="text")


res.pca2<-PCA(data2 , scale.unit=TRUE, ncp=4, quali.sup=c(5), graph =  FALSE)


# default plot
fviz_pca_ind(res.pca2)


fviz_pca_biplot(res.pca2, habillage=5,
                addEllipses=TRUE) + theme_minimal()


# Create ellipsoids with habillage for wheeltype
fviz_pca_ind(data_pca, habillage = as.factor(data$assetSmartphone), addEllipses = TRUE)

fviz_pca_biplot(data_pca, habillage = as.factor(data$foodSourceNow), addEllipses = TRUE, alpha.var = "cos2")






#### DATA camp course ---  NOTES
#### DATA camp course ---  NOTES
#### DATA camp course ---  NOTES
#### DATA camp course ---  NOTES
#### DATA camp course ---  NOTES

### stART again with PCA!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

data_pca <- PCA(new_data)
# get the scree plot to see how many dimesnions to use (ncp - no. dimensions you want to check)
fviz_screeplot(data_pca, ncp=15)


# it looks to me like PCA is saying there are 3 dimensions!!!
## keep the ones to the left of the 'elbow' on the screee plot (e.g. where it levels off)
Or 
# use the Kaiser-Gutmman rule which is to retain components with an eigen value >1

summary(data_pca)
data_pca$eig
get_eigenvalue(data_pca)

## 14 dimensions have an eigen value >1 (TOO MANY!!))
## eigen values of less than 1 explain less of the total variance than a single original variable does on average

## parallel analysis (seen to be superior to eigen value rule and scree test)
# here we compare a randomly generated datset with the same number of obsrevations and variables to our own
library(paran)
data_pca_ret <- paran(new_data, graph=TRUE)

data_pca_ret$Retained

## using this method we are supposed to retain 10 factors or dimensions
# use the psych package function fa.parallel to do parallel analysis with missing data

library(psych)
data_parallel <- fa.parallel(data2)
# Parallel analysis suggests that the number of factors =  11  and the number of components =  9 
data_parallel 

# missing values with PCA
library(missMDA)

nPCs <- estim_ncpPCA(new_data)

nPCs$ncp

completed_data <- imputePCA(new_data, ncp=nPCs$ncp,scale=TRUE)

pca_data <- PCA(completed_data$completeObs)

##
# Check out the number of cells with missing values.
sum(is.na(new_data[,1:39]))

# Estimate the optimal number of dimensions for imputation.
data_ncp <- estim_ncpPCA(data[,1:39])

# Do the actual data imputation.
complete_data <- imputePCA(data[,1:39], ncp = data_ncp$ncp, scale = TRUE)

############################################################################################################
############################################################################################################
############################################################################################################
############################################################################################################
############################################################################################################
#### EXPLORATORY FACTOR ANALYSIS - measuring the unobserved (latent) variables/ factors (latent constructs)
#EFA models are much more realistic as than PCA because they do not attempt to explain ALL the variance
# in the models

# Reduce dataset
drop <- c("idwife","listenRadio","institutionloan_vs_other","wifeAbroadTravel",
          "occupation_agriculture_dummy","bank_vs_micro","assetMobilePhone","occupation_ses",
          "marketIntegrationDhakaTime","marketIntegrationHighSchoolTime")
reduced_data <- new_data[,!(names(new_data) %in% drop)]
#Steps
#1) check for data factorability
# the bartlett sphericity test
library(polycor)
#calculate the correlations
data_hetcor <- hetcor(reduced_data,NA.method="pairwise.complete.obs")


#get the correlation matrix
data_c <- data_hetcor$correlations

# apply the bartlett test
data_factorability <- cortest.bartlett(data_c)

data_factorability

#$chisq
#[1] 1062.91

#$p.value
#[1] 0.00000000000006567376

#$df
#[1] 741


#### But can't use the bartlett sphericity test one with large sample sizes (e.g. sample/ variables >5)

### so we will use the Kaiser-Meyer-Olkin test for sampling adequacy

library(psych)
KMO(data_c)

# this test compares the partial correlation matrix with the pairwise correlation matrix
#The statistic is a measure of how small the patrial corealtions are relative to the original zero order correlations
# the partial correaltion for each pair of varibales is comprised of the correlation between those variable after
#partialing
# out the influence of ALL of the other variables used in the factor analysis. If the variables shared 
#common factors then the
# partial correlations should be small and the KMO should be close to 1.  Standard practice is that the KMO should
# at least be in the 60's to be acceptable.

## what is mine?
# Kaiser-Meyer-Olkin factor adequacy
# Call: KMO(r = data_c)
# Overall MSA =  0.76
# MSA for each item = 
#   foodSecurity                      foodSourceNow                   WEALTH_TOTAL_VAL 
# 0.84                               0.68                               0.80 
# total_income             labor_migrant_hh_total           familyBariEducationAfter 
# 0.82                               0.63                               0.71 
# MacArthurLadderPresent                        listenRadio                        useInternet 
# 0.83                               0.60                               0.72 
# marketIntegrationPrimarySchoolTime    marketIntegrationHighSchoolTime       marketIntegrationCollegeTime 
# 0.86                               0.57                               0.87 
# marketIntegrationSmallBazaarTime   marketIntegrationLargeBazaarTime          marketIntegrationTownTime 
# 0.85                               0.84                               0.86 
# marketIntegrationMainRoadTime         marketIntegrationDhakaTime      marketIntegrationPharmacyTime 
# 0.92                               0.57                               0.84 
# marketIntegrationMBBSTime   labor_migrant_bari_in_bangladesh            years_of_education_wife 
# 0.82                               0.67                               0.79 
# husband_occupation         years_of_education_husband                        relocate_MI 
# 0.89                               0.84                               0.79 
# institutionloan_vs_other                      bank_vs_micro          husbandTravelINBangladesh 
# 0.51                               0.48                               0.71 
# wifeTravelINBangladesh                   wifeAbroadTravel                husbandAbroadTravel 
# 0.71                               0.53                               0.68 
# occupation_agriculture_dummy          occupation_mkt_connection                     occupation_ses 
# 0.53                               0.62                               0.58 
# age_wife                   assetMobilePhone                    assetSmartphone 
# 0.70                               0.59                               0.83 
# assetComputer                   assetElectricity                landOwnedFarmAmount 
# 0.76                               0.71                               0.71 


#2) extract factors
library(psych)
library(GPArotation)
# EFA with 3 factors - the first argument can be a matrix (data_c or reduced_data)
#A correlation or covariance matrix or a raw data matrix. If raw data, the correlation matrix will be found using pairwise deletion.
#If covariances are supplied, they will be converted to correlations unless the covar option is TRUE.

#minres (minimum residual) - minimizes the residual matrix.  In other words it minimizes the differences between the
# correaltion matrix implied by the extracted factors and the original correlation matrix
f_data_minres <- fa(reduced_data, nfactors=1, rotate="none")

# sorted by communality
f_data_minres_common <- sort(f_data_minres$communality, decreasing=TRUE)

# make a df for better overview
data.frame(f_data_minres_common)

############################################################################
#mle (using the maximum likelihood method) 
f_data_mle <- fa(reduced_data,nfactors=1,fm="mle")

# sorted by communality
f_data_mle_common <- sort(f_data_mle$communality,decreasing=TRUE)

# make a df for better overview
data.frame(f_data_mle_common)

# use the PAF method
# Use PAF on hsq_polychoric.
#data_correl_pa <- fa(data_c, nfactors = 1, fm = "pa") 

# Sort the communalities of hsq_correl_pa.
#sort(data_correl_pa$communality, decreasing = TRUE) 

# Sort the uniqueness of hsq_correl_pa.
#sort(data_correl_pa$uniqueness, decreasing = TRUE)

# # the output here is the estimated commonality (% of variance explained by the
# #(extracted factors - here 3) and uniqueness (mirror image of commonality) vectors
# #variables with a relatively high communality are being accounted fairly well by the four factors you
# chose to extract. Recall that a variable's communality represents the percentage of the variable's variance
# that can be explained by the factors, 
# while the unique variance is due to variable's own idiosyncrasy and not to the common factors.


#3) choose the correct number of factors to retain

fa.parallel(reduced_data,  fa="fa", fm="mle")


# Parallel analysis suggests that the number of factors =  8  and the number of components =  NA 
# but looking at the plot - I think it should be three!!!

# Check out the scree test and the Kaiser-Guttman criterion.
scree(data_c)

# Use parallel analysis for estimation with the minres extraction method.
fa.parallel(data_c, n.obs = 766, fm = "minres", fa = "fa")

# Use parallel analysis for estimation with the mle extraction method.
fa.parallel(data_c, n.obs = 766, fm = "mle", fa = "fa")

# or 
parallel <- fa.parallel(reduced_data, fm = "mle", fa = "fa", n.iter=50, SMC=TRUE,quant=0.95) ## choose this one

#Create data frame &amp;amp;amp;amp;amp;quot;obs&amp;amp;amp;amp;amp;quot; from observed eigenvalue data
obs = data.frame(parallel$fa.values)
obs$type = c('Observed Data')
obs$num = c(row.names(obs))
obs$num = as.numeric(obs$num)
colnames(obs) = c('eigenvalue', 'type', 'num')

#Calculate quantiles for eigenvalues, but only store those from simulated CF model in percentile1
percentile = apply(parallel$values,2,function(x) quantile(x,.95))
min = as.numeric(nrow(obs))
min = (4*min) - (min-1)
max = as.numeric(nrow(obs))
max = 4*max
percentile1 = percentile[min:max]

#Create data frame called &amp;amp;amp;amp;amp;quot;sim&amp;amp;amp;amp;amp;quot; with simulated eigenvalue data
sim = data.frame(percentile1)
sim$type = c('Simulated Data (95th percentile)')
sim$num = c(row.names(obs))
sim$num = as.numeric(sim$num)
colnames(sim) = c('eigenvalue', 'type', 'num')

#Merge the two data frames (obs and sim) together into data frame called eigendat
eigendat = rbind(obs,sim)

apatheme=theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text=element_text(family='Arial'),
        legend.title=element_blank(),
        legend.position=c(.7,.8),
        axis.line.x = element_line(color='black'),
        axis.line.y = element_line(color='black'))

#Use data from eigendat. Map number of factors to x-axis, eigenvalue to y-axis, and give different data point shapes depending on whether eigenvalue is observed or simulated
p = ggplot(eigendat, aes(x=num, y=eigenvalue, shape=type)) +
  #Add lines connecting data points
  geom_line()+
  #Add the data points.
  geom_point(size=4)+
  #Label the y-axis 'Eigenvalue'
  scale_y_continuous(name='Eigenvalue')+
  #Label the x-axis 'Factor Number', and ensure that it ranges from 1-max # of factors, increasing by one with each 'tick' mark.
  scale_x_continuous(name='Factor Number', breaks=min(eigendat$num):max(eigendat$num))+
  #Manually specify the different shapes to use for actual and simulated data, in this case, white and black circles.
  scale_shape_manual(values=c(16,1)) +
  #Add vertical line indicating parallel analysis suggested max # of factors to retain
  geom_vline(xintercept = parallel$nfact, linetype = 'dashed')+
  #Apply our apa-formatting theme
  apatheme
#Call the plot. Looks pretty!
p

ggsave('scree_plot_parallel_analysis(2).png', width=6, height=6, unit='in', dpi=300)


### make a nice scree plot

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
 f_data_oblimon <- fa(reduced_data, fm="mle", nfactors=3, rotate = "oblimin")
 print(f_data_oblimon)
# OUTPUT:
 # Factor Analysis using method =  ml
 # Call: fa(r = data_c, nfactors = 3, rotate = "oblimin", fm = "mle")
 # Standardized loadings (pattern matrix) based upon correlation matrix
 # ML2   ML1   ML3     h2    u2 com
 # foodSecurity                        0.06  0.04  0.46 0.2213 0.779 1.0
 # foodSourceNow                       0.06  0.24 -0.12 0.0691 0.931 1.6
 # WEALTH_TOTAL_VAL                   -0.02 -0.04  0.79 0.6208 0.379 1.0
 # total_income                       -0.05 -0.03  0.58 0.3289 0.671 1.0
 # labor_migrant_hh_total             -0.02 -0.15  0.21 0.0608 0.939 1.8
 # familyBariEducationAfter           -0.03  0.06  0.41 0.1778 0.822 1.1
 # MacArthurLadderPresent              0.04 -0.01  0.62 0.3851 0.615 1.0
 # listenRadio                         0.01  0.06  0.10 0.0146 0.985 1.7
 # useInternet                        -0.03  0.02  0.30 0.0936 0.906 1.0
 # marketIntegrationPrimarySchoolTime  0.63 -0.06  0.02 0.3932 0.607 1.0
 # marketIntegrationHighSchoolTime     0.03  0.02 -0.13 0.0162 0.984 1.2
 # marketIntegrationCollegeTime        0.60  0.02  0.06 0.3705 0.630 1.0
 # marketIntegrationSmallBazaarTime    0.67  0.01  0.00 0.4562 0.544 1.0
 # marketIntegrationLargeBazaarTime    0.85 -0.01 -0.02 0.7172 0.283 1.0
 # marketIntegrationTownTime           0.85  0.00 -0.04 0.7161 0.284 1.0
 # marketIntegrationMainRoadTime       0.60 -0.05  0.00 0.3617 0.638 1.0
 # marketIntegrationDhakaTime          0.14 -0.02 -0.09 0.0282 0.972 1.8
 # marketIntegrationPharmacyTime       0.64  0.04  0.05 0.4213 0.579 1.0
 # marketIntegrationMBBSTime           0.76  0.02  0.01 0.5747 0.425 1.0
 # labor_migrant_bari_in_bangladesh   -0.02 -0.01  0.22 0.0460 0.954 1.0
 # years_of_education_wife             0.08  0.33  0.23 0.1890 0.811 1.9
 # husband_occupation                 -0.02  0.66 -0.03 0.4310 0.569 1.0
 # years_of_education_husband          0.06  0.29  0.35 0.2421 0.758 2.0
 # relocate_MI                         0.05  0.21 -0.03 0.0462 0.954 1.1
 # institutionloan_vs_other           -0.01  0.00 -0.16 0.0267 0.973 1.0
 # bank_vs_micro                      -0.01  0.05  0.01 0.0024 0.998 1.2
 # husbandTravelINBangladesh           0.01  0.19  0.14 0.0620 0.938 1.8
 # wifeTravelINBangladesh              0.05  0.06  0.24 0.0707 0.929 1.2
 # wifeAbroadTravel                    0.00  0.03  0.09 0.0091 0.991 1.2
 # husbandAbroadTravel                -0.02  0.11  0.17 0.0457 0.954 1.8
 # occupation_agriculture_dummy        0.00  0.59 -0.03 0.3452 0.655 1.0
 # occupation_mkt_connection           0.00  1.00 -0.04 0.9950 0.005 1.0
 # occupation_ses                     -0.01  0.86  0.07 0.7594 0.241 1.0
 # age_wife                           -0.04 -0.33  0.13 0.1191 0.881 1.3
 # assetMobilePhone                   -0.07  0.04  0.05 0.0083 0.992 2.5
 # assetSmartphone                     0.03  0.04  0.53 0.2932 0.707 1.0
 # assetComputer                       0.03  0.08  0.31 0.1079 0.892 1.2
 # assetElectricity                    0.03  0.00  0.18 0.0343 0.966 1.1
 # landOwnedFarmAmount                -0.05 -0.10  0.35 0.1226 0.877 1.2
 # 
 # ML2  ML1  ML3
 # SS loadings           4.06 3.07 2.85
 # Proportion Var        0.10 0.08 0.07
 # Cumulative Var        0.10 0.18 0.26
 # Proportion Explained  0.41 0.31 0.29
 # Cumulative Proportion 0.41 0.71 1.00
 # 
 # With factor correlations of 
 # ML2  ML1  ML3
 # ML2 1.00 0.08 0.06
 # ML1 0.08 1.00 0.12
 # ML3 0.06 0.12 1.00
 # 
 # Mean item complexity =  1.3
 # Test of the hypothesis that 3 factors are sufficient.
 # 
 # The degrees of freedom for the null model are  741  and the objective function was  12.48
 # The degrees of freedom for the model are 627  and the objective function was  4.09 
 # 
 # The root mean square of the residuals (RMSR) is  0.06 
 # The df corrected root mean square of the residuals is  0.06 
 # 
 # Fit based upon off diagonal values = 0.86
 # Measures of factor score adequacy             
 # ML2  ML1  ML3
 # Correlation of (regression) scores with factors   0.95 1.00 0.91
 # Multiple R square of scores with factors          0.91 0.99 0.82
 # Minimum correlation of possible factor scores     0.82 0.99 0.65
 

#5) interpret results
# path diagrams
 library(psych)
 fa.diagram(f_data_oblimon)
 # check out the factor loadings
 print(f_data_oblimon$loadings, cut=0)
 # get rid of variables that do not correlate with extracted factors
 
# M1 is commuting time to markets
 
# m2 is wealth, income, use of interent, computers and smartphones, food security and
# farm land owned
 
# M3 occupation, husband and wifes education, number of migrant in hh (negative relationship), relocation to 
 # gain access to markets, degree of mkt connection of occupation and husbands travels
 
 
 fa.parallel(reduced_data,  fa="fa", fm="mle")
 
 
 # Parallel analysis suggests that the number of factors =  6  and the number of components =  NA 
 # but looking at the plot - I think it shoud be three!!!
 
 # Check out the scree test and the Kaiser-Guttman criterion.
 scree(data_c)
 
 # Use parallel analysis for estimation with the minres extraction method.
 fa.parallel(data_c, n.obs = 766, fm = "minres", fa = "fa")
 #calculate the correlations
 data_hetcor2 <- hetcor(reduced_data,NA.method="pairwise.complete.obs")
 
 
 #get the correlation matrix
 data_c2 <- data_hetcor2$correlations
 
 f_data_oblimon <- fa(data_c, fm="mle", nfactors=3, rotate = "oblimin")
 f_data_oblimon
 
 fa.diagram(f_data_oblimon)
 # check out the factor loadings
 print(f_data_oblimon$loadings, cut=0)
 
 library(psych)
 library(data.table)
 library(ggplot2)
 library(reshape2)
 
 options(width=240)
 
 # df = read.csv('precities.csv')
 # df2 = cor(df, use="pairwise.complete.obs")
 # 
 # scree plot, find #factors
 #scree(df)
 #fa.parallel(df)
 #fa.parallel(df2)
 
 #loadings
 load = fa(data_c,9,rotate='oblimin',fm='mle', nfactors=3)

 load = load$loadings
 load = load[]
 load = data.frame(load)
 setDT(load,keep.rownames=TRUE)[]
 colnames(load)[1] <- "Indicators"
 
 colnames(load)[2:4] <- c("Geographic proximity","Economic capital","Human capital")
 load[1,1] <- "Food security"
 load[2,1] <- "Food source"
 load[3,1] <- "Total wealth"
 load[4,1] <- "Total income"
 load[5,1] <- "Labor migrants in hh"
 load[6,1] <- "Relative education"
 load[7,1] <- "Perceived wealth"
 load[8,1] <- "Internet use"
 load[9,1] <- "Time to nearest Primary School"
 load[10,1] <- "Time to nearest College"
 load[11,1] <- "Time to nearest Small Bazaar"
 load[12,1] <- "Time to nearest Large Bazaar"
 load[13,1] <- "Time to nearest Town"
 load[14,1] <- "Time to nearest Main Road"
 load[15,1] <- "Time to nearest Pharmacy"
 load[16,1] <- "Time to  Hospital (MBBS site)"
 load[17,1] <- "Labor migrants in village"
 load[18,1] <- "Education wife"
 
 load <- load[-c(19),] 

 load[19,1] <- "Education husband"
 load[20,1] <- "Relocated to improve access to markets"
 
 load <- load[-c(21),] 
 load <- load[-c(22),]
 
 load[21,1] <- "Husband's Travel"
 load[22,1] <- "Occupation market connection"
 load[23,1] <- "Age wife"
 load[24,1] <- "Own smartphone "
 load[25,1] <- "Own computer"
 load[26,1] <- "Have electricity"
 load[27,1] <- "Land owned "
 
 load.m <- melt(load, id="Indicators", variable.name="Factors", value.name="Loading", measure = colnames(load)[2:4])
 

 loadPlot <- ggplot(load.m, aes(Indicators, abs(Loading), fill=Loading)) + 
   facet_wrap(~ Factors, nrow=1) + geom_bar(stat="identity")+ coord_flip() +
   scale_fill_gradient2(name="Loading",high="blue",mid="white",low="red",midpoint=0,guide=F)+
   ylab("Loading")+theme_bw(base_size=10)
 
 ### save path analysis figure to Figures 
 ggsave("C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures/path diagram_MI_loadings.png")
 ggsave("C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/SM figures/path diagram_MI_loadings.pdf")

 # View the first few lines of  factor scores
 head(f_data_oblimon$scores)

str(f_data_oblimon)
# output: Use this list to delete variables that do not load well
## these are the correlations between the variables and the 3 factors we presumed
#   Loadings:
#   ML2    ML1    ML3   
# foodSecurity                        0.059  0.036  0.457
# foodSourceNow                       0.058  0.238 -0.120
# WEALTH_TOTAL_VAL                   -0.020 -0.038  0.792
# total_income                       -0.052 -0.025  0.577
# labor_migrant_hh_total             -0.017 -0.150  0.213
# familyBariEducationAfter           -0.030  0.058  0.412
# MacArthurLadderPresent              0.044 -0.008  0.617
# listenRadio                         0.013  0.057  0.098
# useInternet                        -0.032  0.020  0.304
# marketIntegrationPrimarySchoolTime  0.628 -0.063  0.022
# marketIntegrationHighSchoolTime     0.026  0.024 -0.126
# marketIntegrationCollegeTime        0.601  0.016  0.056
# marketIntegrationSmallBazaarTime    0.675  0.006  0.001
# marketIntegrationLargeBazaarTime    0.848 -0.006 -0.023
# marketIntegrationTownTime           0.848  0.001 -0.036
# marketIntegrationMainRoadTime       0.603 -0.046  0.002
# marketIntegrationDhakaTime          0.145 -0.024 -0.091
# marketIntegrationPharmacyTime       0.640  0.035  0.052
# marketIntegrationMBBSTime           0.756  0.021  0.006
# labor_migrant_bari_in_bangladesh   -0.022 -0.015  0.216
# years_of_education_wife             0.081  0.329  0.225
# husband_occupation                 -0.021  0.661 -0.030
# years_of_education_husband          0.057  0.290  0.355
# relocate_MI                         0.045  0.208 -0.026
# institutionloan_vs_other           -0.013  0.002 -0.162
# bank_vs_micro                      -0.013  0.048  0.006
# husbandTravelINBangladesh           0.005  0.189  0.140
# wifeTravelINBangladesh              0.053  0.060  0.243
# wifeAbroadTravel                    0.001  0.029  0.088
# husbandAbroadTravel                -0.017  0.113  0.170
# occupation_agriculture_dummy        0.001  0.590 -0.033
# occupation_mkt_connection           0.002  1.001 -0.035
# occupation_ses                     -0.009  0.862  0.067
# age_wife                           -0.044 -0.330  0.128
# assetMobilePhone                   -0.069  0.042  0.046
# assetSmartphone                     0.029  0.042  0.532
# assetComputer                       0.026  0.082  0.305
# assetElectricity                    0.033  0.002  0.180
# landOwnedFarmAmount                -0.046 -0.103  0.345
# 
# ML2   ML1   ML3
# SS loadings    4.056 3.049 2.834
# Proportion Var 0.104 0.078 0.073
# Cumulative Var 0.104 0.182 0.255

# cumulative variance shows us how much the factors account for the data


head(f_data_oblimon$scores)

summary(f_data_oblimon$scores)


alpha(reduced_data,check.keys=TRUE)


# Getting the correlation matrix of the dataset.


#Factor analysis explicitly assumes the existence of latent factors underlying the observed data.
#PCA instead seeks to identify variables that are composites of the observed variables.



#http://www.sthda.com/english/wiki/fviz-pca-quick-principal-component-analysis-data-visualization-r-software-and-data-mining

#factor scores - look for eigen values > 1
# varimax (orthogonal
# oblimon (correlated factors)
# making an index





