## draw a Dag for our model
library(rethinking)
library(dagitty)
Kin_dag <-dagitty("dag{
Religiosity -> Kin_density
Religion -> Kin_density
Kin_density -> Religiosity
Unobserved -> Religiosity
Unobserved-> Kin_density
}")
coordinates( Kin_dag)<-list(x=c(Religiosity=0,Kin_density=2,Unobserved=1.5,Religion=1),
                            y=c(Religiosity=1,"Kin_density"=2,Unobserved=3,Religion=4) )
drawdag( Kin_dag)



### Do a DAG for our models to get confounding variables
library(dagitty)
kin_density_dag <-dagitty("dag{
U [unobserved]
religiosity ->kin_density
kids_in_hh -> kin_density
religion -> kin_density
age_wife -> kin_density
religiosity <-U<-neighborhood->house->kin_density
religion <- religious knowledge -> religiosity
MI_economic_capital<- U<- MI_human_capital -> MI_geo_proximity

religiosity <-U<-MI_human_capital->religion->kin_density
age_wife ->U<-kin_density
}")
adjustmentSets( kin_density_dag,exposure="religiosity",outcome="kin_density")

## example

library(dagitty)
dag_6.1 <-dagitty("dag{
U [unobserved]
religiosity ->kin_density
religiosity <-U<-relgion->market_intergration->kin_density
U ->siblings<-kids
}")
adjustmentSets( dag_6.1,exposure="religiosity",outcome="kin_density")

#### DAG Examples from Chapter 6

library(ggdag)

dag_coords <-
  tibble(name = c("L", "D", "F", "K"),
         x    = c(1, 2, 3, 2),
         y    = c(2, 2, 2, 1))

dagify(L ~ D,
       F ~ D,
       K ~ L + F,
       coords = dag_coords) %>%
  
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_point(aes(color = name == "D"),
                 alpha = 1/2, size = 6.5, show.legend = F) +
  geom_point(x = 2, y = 2, 
             size = 6.5, shape = 1, stroke = 1, color = "orange") +
  geom_dag_text(color = "black") +
  geom_dag_edges() +
  scale_color_manual(values = c("steelblue", "orange")) +
  scale_x_continuous(NULL, breaks = NULL, expand = c(.1, .1)) +
  scale_y_continuous(NULL, breaks = NULL, expand = c(.1, .1))


# Number 2
# define our coordinates
dag_coords <-
  tibble(name = c("H0", "T", "F", "H1"),
         x    = c(1, 5, 4, 3),
         y    = c(2, 2, 1.5, 1))

# save our DAG
dag <-
  dagify(F ~ T,
         H1 ~ H0 + F,
         coords = dag_coords)

# plot 
dag %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_point(color = "steelblue", alpha = 1/2, size = 6.5) +
  geom_dag_text(color = "black") +
  geom_dag_edges() + 
  theme_dag()


## function to help with DAG's

gg_simple_dag <- function(d) {
  
  d %>% 
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_point(color = "steelblue", alpha = 1/2, size = 6.5) +
    geom_dag_text(color = "black") +
    geom_dag_edges() + 
    theme_dag()
  
}

## And
gg_fancy_dag <- function(d, x = 1, y = 1, circle = "U") {
  
  d %>% 
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_point(aes(color = name == circle),
                   alpha = 1/2, size = 6.5, show.legend = F) +
    geom_point(x = x, y = y, 
               size = 6.5, shape = 1, stroke = 1, color = "orange") +
    geom_dag_text(color = "black") +
    geom_dag_edges() + 
    scale_color_manual(values = c("steelblue", "orange")) +
    theme_dag()
  
}

###THIS ONE IS A GOOD MODEL TO USE
#### make DAG using siblings
dag_coords <-
  tibble(name = c("MI","Religiosity", "Siblings", "Unobserved", "NW_Size"),
         x    = c(1.5,1,2,2.5,2),
         y    = c(4,3,3,2,1))

DAG1 <- dagify(Religiosity~MI,
       Siblings~ Unobserved+Religiosity+MI,
       NW_Size ~ Unobserved+Religiosity+Siblings,
       coords = dag_coords) %>%
  gg_fancy_dag(x = 1, y = 3, circle = "NW_Size") 

ggsave(DAG1, filename = "C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/DAG1.pdf",width = 18, height = 10)
ggsave(DAG1, filename = "C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/DAG1.png",width = 18, height = 10)

#  gg_fancy_dag(x = 1, y = 3,circle = "Unobserved2")
#The unmeasured U makes Siblings a collider,and conditioning on Siblings produces collider bias.So
#what can we do about this? You have to measure U.

# 1) This is an example of Simpsons paradox.  By including siblings we we can reverse the direct association between religiosity and 
# network size - adding a variable can CAUSE confounding!!

# shutting the back door
# 1) the unobserved variable here is called a fork
# or 2) post treatment bias (e.g. the pipe) - if we condition on sibs, we block the path from religiosity to NW size
#3) Not a collider unless conditioning on siblings opens the path between Religiosity and NW size
#4) The descendant is a variable that is influenced by
# another variable.Therefore conditioning on the desecdant will partly condition on it's parent thereby opening the door 
# between a predictor and an outcome (e.g. if network size and religiosity are both affected number of siblings - a collider --
# and some other variable like market integration affects ...
# 

# paths from religosity to NW size
# 1) religiosity to MI to Siblings to NW size
#2) Religiosity to siblings to NW size
#3) Religiosity to siblings to Unobserved to NW size
#4) Religiosity to NW size

# Let R figure out the backdoors

library(dagitty)
dag_1 <-dagitty("dag{
U [unobserved]
Religiosity ->NW_Size
Religiosity -> Siblings->U->NW_Size
Religiosity -> Siblings->NW_Size
Religiosity <-MI->Siblings->U->NW_Size
Religiosity <-MI->Siblings->NW_Size
}")
adjustmentSets( dag_1,exposure="Religiosity",outcome="NW_Size")

# this says that conditioning on MI is important to close the backdoor


# to find out what are independent
impliedConditionalIndependencies(dag_1)
# MI _||_ NW_S | Rlgs, Sbln

# translation of above MI should be independent of NW size conditioning on religiosity and siblings


### Do the big DAG with all the covariates
# kids_in_hh+
#   age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
#   MI_geo_proximity+R_NUM_SIBS+
#   MI_economic_capital+
#   MI_human_capital
# collinearity between kids_in_hh, age_wife, MI_human_capital

dag_coords <-
  tibble(name = c("MI_G","MI_EC","MI_HC","Religiosity", "Siblings", "Unobserved", "NW_Size","religion","rel_knol","age_wife","kids_in_hh"),
         x    = c(1.5,1,2,2.5,2),
         y    = c(4,3,3,2,1))

DAG2 <- dagify(Religiosity~MI,
               Siblings~ Unobserved+Religiosity+MI,
               NW_Size ~ Unobserved+Religiosity+Siblings+religion+rel_knol+MI,
               coords = dag_coords) %>%
  gg_fancy_dag(x = 1, y = 3, circle = "NW_Size") 

ggsave(DAG2, filename = "C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/DAG1.pdf",width = 18, height = 10)
ggsave(DAG2, filename = "C:/Users/robert/Dropbox/PSU postdoc/Effect of religiosity on kin network density/Figures/DAG1.png",width = 18, height = 10)


### Run models with siblings - model 1
library(tidyverse)
library(brms)
library(readr)
setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh")
newdata <- read_csv("newdata.csv")
options(scipen=999)

# number of siblings is column 51
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################

# Correlations
# cor(d)
#                             age_wife   MI_geo_proximity  MI_economic_capital MI_human_capital childcare_help_non_rels
# age_wife                   1.00000000      -0.04250186          0.17347068      -0.78685110            -0.025472605
# MI_geo_proximity          -0.04250186       1.00000000          0.04303967       0.09168829             0.017495569
# MI_economic_capital        0.17347068       0.04303967          1.00000000       0.02947770             0.014130015
# MI_human_capital          -0.78685110       0.09168829          0.02947770       1.00000000             0.045989545
# childcare_help_non_rels   -0.02547260       0.01749557          0.01413002       0.04598954             1.000000000
# childcare_help_rels       -0.08249364      -0.10101146          0.01114938      -0.01958970            -0.117644361
# religion                  -0.03054555       0.01223926         -0.12422657       0.02140739            -0.009003088
# religious_knowledge_scale -0.13865374       0.04825318          0.17221460       0.28453509             0.044879087
# kids_in_hh                 0.61436925      -0.08545547          0.13393911      -0.58719766             0.012325238
# familyBariReligiousAfter   0.10815010      -0.02886306          0.31693343       0.01747384            -0.116255418
# R_NUM_SIBS                 0.09056518       0.03799168          0.07132299      -0.02225237            -0.013111341
#                                childcare_help_relsreligion    religious_knowledge_scale  kids_in_hh familyBariReligiousAfter
# age_wife                          -0.08249364 -0.0305455476               -0.13865374  0.61436925               0.10815010
# MI_geo_proximity                  -0.10101146  0.0122392635                0.04825318 -0.08545547              -0.02886306
# MI_economic_capital                0.01114938 -0.1242265724                0.17221460  0.13393911               0.31693343
# MI_human_capital                  -0.01958970  0.0214073855                0.28453509 -0.58719766               0.01747384
# childcare_help_non_rels           -0.11764436 -0.0090030885                0.04487909  0.01232524              -0.11625542
# childcare_help_rels                1.00000000 -0.0277076764               -0.02168981 -0.00263945              -0.00267780
# religion                          -0.02770768  1.0000000000                0.43220573 -0.11786592              -0.03894147
# religious_knowledge_scale         -0.02168981  0.4322057318                1.00000000 -0.18030958               0.11197358
# kids_in_hh                        -0.00263945 -0.1178659216               -0.18030958  1.00000000               0.03570450
# familyBariReligiousAfter          -0.00267780 -0.0389414721                0.11197358  0.03570450               1.00000000
# R_NUM_SIBS                         0.04581969  0.0002368424                0.10234189  0.05641608               0.09112263
#                            R_NUM_SIBS
# age_wife                   0.0905651844
# MI_geo_proximity           0.0379916835
# MI_economic_capital        0.0713229894
# MI_human_capital          -0.0222523708
# childcare_help_non_rels   -0.0131113406
# childcare_help_rels        0.0458196938
# religion                   0.0002368424
# religious_knowledge_scale  0.1023418885
# kids_in_hh                 0.0564160766
# familyBariReligiousAfter   0.0911226280
# R_NUM_SIBS                 1.0000000000


## Brms models - Model 1 NW total
library(brms)
# 51 is number of siblings aka 'R_NUM_SIBS'
d <- newdata[c(10,47,5,7,8,9,44,45,49,51)] 

d <- d[complete.cases(d), ]  


d$NW_total <- as.numeric(d$NW_total)
### try model
model1 <- brm(NW_total ~ kids_in_hh+
                age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
                MI_geo_proximity+R_NUM_SIBS+
                MI_economic_capital+
                MI_human_capital, data=d, 
              family = lognormal(),
              prior = c(set_prior("normal(0,2)", class = "b"),
                        set_prior("normal(0,10)", class="b",coef="age_wife")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model1, prob=0.95,priors=TRUE), digits = 6)

# Population-Level Effects: 
#   Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                  2.600481  0.097226  2.409831  2.790745 0.999977    16685    13553
# kids_in_hh                 0.005471  0.008545 -0.011344  0.022112 1.000360    17352    11820
# age_wife                  -0.007200  0.001819 -0.010757 -0.003623 0.999888    16307    13869
# religion                  -0.099841  0.047444 -0.192845 -0.005957 1.000427    13884    12790
# familyBariReligiousAfter   0.125435  0.021490  0.083110  0.166880 1.000081    17005    12838
# religious_knowledge_scale  0.004869  0.004568 -0.004102  0.013950 1.000242    15739    13218
# MI_geo_proximity          -0.015489  0.018414 -0.051166  0.020549 1.000040    17326    12255
# R_NUM_SIBS                 0.005180  0.005858 -0.006352  0.016730 1.000131    21756    11370
# MI_economic_capital        0.030323  0.016281 -0.001690  0.062049 1.000075    13392    11754
# MI_human_capital          -0.049204  0.026392 -0.100399  0.002179 0.999951    12938    12179

# take home - number of siblings does not effect the path to NW_size - try a few more models especially  geo distance and
#childcare models

# religiosity and number of siblings is positively correlated r=0.1 but not so much to fuck up the effects
library(tidyverse)
library(brms)
library(readr)

d <- newdata[c(21,47,5,7,8,9,44,45,49,51)] 
d <- d[complete.cases(d), ] 

d$percent_rels_in_NW<- d$percent_rels_in_NW+0.01
## run as log normal  
model2 <- brm(percent_rels_in_NW ~ kids_in_hh+
                age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
                MI_geo_proximity+R_NUM_SIBS+
                MI_economic_capital+
                MI_human_capital, data=d, 
              family = lognormal(),
              prior = c(set_prior("normal(0,2)", class = "b"),
                        set_prior("normal(0,10)", class="b",coef="age_wife")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))



print(summary(model2, prob=0.95,priors=TRUE), digits = 6)

# Population-Level Effects: 
#                             Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                 -0.156478  0.063065 -0.281226 -0.032045 1.000028    17660    13067
# kids_in_hh                 0.011014  0.005533  0.000096  0.021949 0.999992    20461    11972
# age_wife                  -0.001252  0.001173 -0.003565  0.001034 1.000005    17299    12281
# religion                   0.016267  0.030673 -0.043600  0.076810 1.000219    15895    11539
# familyBariReligiousAfter   0.029167  0.013761  0.002419  0.056507 1.000441    16475    11642
# religious_knowledge_scale -0.002280  0.002945 -0.008037  0.003462 1.000296    19018    14009
# MI_geo_proximity           0.009084  0.011731 -0.013716  0.031928 1.000011    17717    11189
# R_NUM_SIBS                 0.006203  0.003797 -0.001258  0.013656 0.999936    22526    11550
# MI_economic_capital        0.022549  0.010558  0.001795  0.043381 1.000433    16495    12444
# MI_human_capital          -0.008863  0.017068 -0.042085  0.024235 1.000387    14979    11406

## take home: same results with our without sibs

# Model 2.1 Relatives in Network

library(tidyverse)
library(brms)
library(readr)


d <- newdata[c(19,47,4,5,7,8,9,44,45,49,51)] 
d <- d[complete.cases(d), ] 


d$rels_in_NW<- d$rels_in_NW+0.01
## run as log normal  
model2.1 <- brm(rels_in_NW ~ kids_in_hh+
                  age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
                  MI_geo_proximity+R_NUM_SIBS+
                  MI_economic_capital+
                  MI_economic_capital+
                  MI_human_capital, data=d, 
                family = lognormal(),
                prior = c(set_prior("normal(0,2)", class = "b"),
                          set_prior("normal(0,10)", class="b",coef="age_wife")),
                warmup = 1000, iter = 5000, chains = 4,
                control = list(adapt_delta = 0.95))

print(summary(model2.1, prob=0.95,priors=TRUE), digits = 6)
#                               Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                  2.441651  0.114337  2.217421  2.669011 1.000336    15144    12607
# kids_in_hh                 0.010411  0.009922 -0.008922  0.030257 1.000154    15844    12230
# age_wife                  -0.006878  0.002135 -0.011101 -0.002697 1.000396    14678    12543
# religion                  -0.098752  0.055712 -0.205959  0.011124 1.000153    13166    12186
# familyBariReligiousAfter   0.143273  0.025386  0.092878  0.192750 1.000373    17365    11691
# religious_knowledge_scale -0.000720  0.005384 -0.011158  0.009806 1.000184    15161    12705
# MI_geo_proximity          -0.021306  0.021424 -0.063852  0.020610 1.000042    17621    12347
# R_NUM_SIBS                 0.008270  0.006857 -0.005293  0.021841 1.000048    19808    12113
# MI_economic_capital        0.038459  0.019298  0.000635  0.075629 1.000068    14027    12162
# MI_human_capital          -0.047210  0.030815 -0.107885  0.012577 1.000515    11981    11224


# Ditto adding sibs makes no difference 


# Model 2.2 Non-relatives in Network
library(tidyverse)
library(brms)
library(readr)
d <- newdata[c(11,47,4,5,7,8,9,44,45,49,51)] 

d <- d[complete.cases(d), ] 


## run as Negative bionomial
model2.2 <- brm(non_rels ~ kids_in_hh+
                  age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
                  MI_geo_proximity+R_NUM_SIBS+
                  MI_economic_capital+
                  MI_economic_capital+
                  MI_human_capital, data=d, 
                family = negbinomial(link = "log", link_shape = "identity"),
                prior = c(set_prior("normal(0,2)", class = "b"),
                          set_prior("normal(0,10)", class="b",coef="age_wife")),
                warmup = 1000, iter = 5000, chains = 4,
                control = list(adapt_delta = 0.95))

print(summary(model2.2, prob=0.95,priors=TRUE), digits = 6)

#         Population-Level Effects: 
#                             Estimate  Est.Error  l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                  0.411307  0.363076 -0.294880 1.123899 1.000029    16636    12979
# kids_in_hh                -0.026826  0.034261 -0.094199 0.041490 1.000164    22861    11041
# age_wife                  -0.007893  0.006772 -0.021131 0.005362 1.000012    16250    12465
# religion                   0.011992  0.171823 -0.329519 0.348707 1.000201    18529    12602
# familyBariReligiousAfter   0.009020  0.077301 -0.142944 0.157835 1.000463    26949    12077
# religious_knowledge_scale  0.014877  0.015225 -0.014501 0.045475 1.000178    17697    13207
# MI_geo_proximity           0.078859  0.079976 -0.072722 0.244261 0.999870    23402    10669
# R_NUM_SIBS                 0.006369  0.022426 -0.037784 0.050630 1.000890    24160    11322
# MI_economic_capital       -0.019047  0.060539 -0.138598 0.097955 0.999958    20089    13095
# MI_human_capital          -0.024372  0.094997 -0.211284 0.161217 1.000287    14867    11289


# Ditto adding sibs makes no difference 

#### Next do Geo distance models and childcare models


# in rethinking and brms
setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh")
newdata <- read_csv("newdata.csv")
options(scipen=999)
# remove duplicated women
newdata <- newdata %>% distinct(idwife, .keep_all = TRUE)
  
d <- newdata[c(1,5,7,8,9,36,35,44,45,47,49,51)] # add 36 for non-rels and 35 for rels
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
# 2=near bari/neighbor or 1=khana member,
# 3=other place in Matlab
# 4=Other Place in Bangladesh or 5=Abroad
 

model3.1c <- brm(data = non_rels, 
      family = cumulative("logit"),
      location ~ 1 + age_wife +  MI_geo_proximity + MI_economic_capital + MI_human_capital +R_NUM_SIBS+
        religion+ religious_knowledge_scale + kids_in_hh + familyBariReligiousAfter,
      prior = c(prior(normal(0, 1.5), class = Intercept),
                prior(normal(0, 0.5), class = b)),
      iter = 5000, warmup = 1000, cores = 4, chains = 4)

print(model3.1c)

#                              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept[1]                 -6.45      0.91    -8.29    -4.74 1.00    10818    10560
# Intercept[2]                  1.24      0.69    -0.13     2.57 1.00    12879    12168
# Intercept[3]                  2.65      0.70     1.27     4.02 1.00    13032    13021
# age_wife                     -0.01      0.01    -0.03     0.02 1.00    12201    12761
# MI_geo_proximity              0.23      0.15    -0.07     0.52 1.00    18693    10889
# MI_economic_capital           0.17      0.11    -0.05     0.39 1.00    17390    11854
# MI_human_capital             -0.02      0.16    -0.35     0.30 1.00    12246    11434
# R_NUM_SIBS                   -0.03      0.04    -0.12     0.06 1.00    21266    12099
# religion                     -0.52      0.30    -1.11     0.05 1.00    18463    11910
# religious_knowledge_scale    -0.03      0.03    -0.09     0.02 1.00    16139    12358
# kids_in_hh                   -0.10      0.07    -0.25     0.04 1.00    17764    11740
# familyBariReligiousAfter      0.53      0.15     0.24     0.82 1.00    19967    11975


## old value parameter
# familyBariReligiousAfter      0.48      0.14     0.20     0.76 1.00    19739    11664

# same interpretations with and without sibs

### do relatives next
rels <- r %>% left_join (d, by=c("id_Questionaire"="idwife"))
rels <- rels[complete.cases(rels),]


# Make model for rels

model3.2c <- 
  brm(data = rels, 
      family = cumulative("logit"),
      location ~ 1 + age_wife +  MI_geo_proximity + MI_economic_capital + MI_human_capital +R_NUM_SIBS+
        religion+ religious_knowledge_scale + kids_in_hh + familyBariReligiousAfter,
      prior = c(prior(normal(0, 1.5), class = Intercept),
                prior(normal(0, 0.5), class = b)),
      iter = 5000, warmup = 1000, cores = 4, chains = 4)

print(model3.2c)

#                             Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept[1]                 -1.52      0.19    -1.90    -1.15 1.00    18345    12942
# Intercept[2]                  0.79      0.19     0.42     1.17 1.00    18914    13569
# Intercept[3]                  1.53      0.19     1.15     1.91 1.00    18949    13319
# Intercept[4]                  2.89      0.20     2.49     3.28 1.00    19833    13135
# age_wife                     -0.01      0.00    -0.01     0.00 1.00    18265    12347
# MI_geo_proximity              0.06      0.04    -0.01     0.14 1.00    26868    11732
# MI_economic_capital          -0.09      0.03    -0.15    -0.03 1.00    19820    13110
# MI_human_capital             -0.03      0.05    -0.13     0.07 1.00    18464    11852
# R_NUM_SIBS                    0.00      0.01    -0.02     0.03 1.00    25860    11797
# religion                     -0.49      0.09    -0.68    -0.31 1.00    20337    12675
# religious_knowledge_scale     0.04      0.01     0.02     0.06 1.00    19521    12211
# kids_in_hh                    0.03      0.02    -0.01     0.06 1.00    23787    12633
# familyBariReligiousAfter      0.14      0.04     0.06     0.22 1.00    21921    11562


# old value of parameter
# familyBariReligiousAfter      0.15      0.04     0.07     0.23 1.00    21774    12218

# Now childcare 

# 6) childcare_help_rels_percent: B=0.08 p=0.08 (religious knowledge however is a very significant negative predictor)

d <- newdata[c(37,11,47,4,5,7,8,9,44,45,49,51)] 

d <- d[complete.cases(d), ] 

# transform beta to be between 0 and 1
d$childcare_help_rels_percent <- (d$childcare_help_rels_percent * (783) + 0.5) / 784

model6<-brm(childcare_help_rels_percent ~ kids_in_hh+
              age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
              MI_geo_proximity+R_NUM_SIBS+
              MI_economic_capital+
              MI_human_capital, data=d, family = "beta",
            prior = c(set_prior("normal(0,2)", class = "b"),
                      set_prior("normal(0,10)", class="b",coef="age_wife")),
            warmup = 1000, iter = 5000, chains = 4,
            control = list(adapt_delta = 0.95))

print(summary(model6, prob=0.95,priors=TRUE), digits = 6)

# Population-Level Effects: 
#                             Estimate Est.Error  l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                  2.453727  0.302700  1.857617 3.044239 1.000060    14657    12314
# kids_in_hh                 0.009171  0.026059 -0.041164 0.060296 0.999922    18789    11823
# age_wife                  -0.005853  0.005542 -0.016722 0.005051 1.000092    14566    12239
# religion                   0.058719  0.157855 -0.252112 0.364572 1.000378    14165    12020
# familyBariReligiousAfter   0.058582  0.065599 -0.069703 0.189254 1.000250    20345    12357
# religious_knowledge_scale -0.024675  0.019405 -0.057075 0.018286 1.000186    13402    10787
# MI_geo_proximity          -0.030275  0.058981 -0.139288 0.092696 1.000504    20624    11071
# R_NUM_SIBS                 0.008723  0.018165 -0.026870 0.044059 1.000727    23968    11340
# MI_economic_capital        0.003443  0.050801 -0.094200 0.103440 0.999930    18770    13018
# MI_human_capital          -0.073099  0.081432 -0.232641 0.087169 1.000198    14054    12331

#Before it was
# familyBariReligiousAfter   0.086154  0.065696 -0.041044  0.215616 1.001234    19161    12394

# maybe a bit better when sibs are included

# 6.1) childcare_help_non_rels
library(brms)
library(readr)

d <- newdata[c(36,11,47,4,5,7,8,9,44,45,49,51)] 

d <- d[complete.cases(d), ] 

# make an integer again
d$childcare_help_non_rels <-d$childcare_help_non_rels-0.001
d$childcare_help_non_rels <- as.integer(d$childcare_help_non_rels)

model6.1<-brm(childcare_help_non_rels ~ kids_in_hh+
                age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
                MI_geo_proximity+R_NUM_SIBS+
                MI_economic_capital+
                MI_human_capital, data=d, family = negbinomial(link = "log", link_shape = "identity"),
              prior = c(set_prior("normal(0,2)", class = "b"),
                        set_prior("normal(0,10)", class="b",coef="age_wife")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model6.1, prob=0.95,priors=TRUE), digits = 6)

#                             Estimate Est.Error   l-95% CI u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                 -2.053570  5.180181 -11.850771 8.791770 1.000584    10904     9019
# kids_in_hh                 0.744118  0.445974  -0.068952 1.699024 1.000119    11927     9229
# age_wife                  -0.019569  0.098097  -0.224110 0.169934 1.000572    10411     8969
# religion                  -0.446059  1.552334  -3.430350 2.695512 1.000289    14404    12311
# familyBariReligiousAfter  -1.509357  0.956479  -3.456111 0.328614 1.000868    12861    10502
# religious_knowledge_scale  0.128183  0.316594  -0.476911 0.798038 1.000708     9951     7795
# MI_geo_proximity           1.168356  1.416095  -1.518738 4.003066 1.000103    13868    11599
# R_NUM_SIBS                 0.099159  0.337494  -0.584409 0.770454 1.000041    11575     9406
# MI_economic_capital       -0.189724  0.783335  -1.764125 1.366582 1.000671     9812     9350
# MI_human_capital           0.991605  1.059321  -1.098316 3.080480 1.000315    11098    10639

# old parameter
## familyBariReligiousAfter  -1.367604  0.380401 -2.158242 -0.657826 1.000209    14342    11246

# take home - adding sibs makes the childcare non-rels not significant******

# 6.2) childcare_help_rels
library(brms)
library(readr)

d <- newdata[c(35,11,47,4,5,7,8,9,44,45,49,51)] 

d <- d[complete.cases(d), ] 
# make an integer again
d$childcare_help_rels <-d$childcare_help_rels-0.001
d$childcare_help_rels <- as.integer(d$childcare_help_rels)

model6.2<-brm(childcare_help_rels ~ kids_in_hh+
                age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
                MI_geo_proximity+R_NUM_SIBS+
                MI_economic_capital+
                MI_human_capital, data=d, family = negbinomial(link = "log", link_shape = "identity"),
              prior = c(set_prior("normal(0,2)", class = "b"),
                        set_prior("normal(0,10)", class="b",coef="age_wife")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model6.2, prob=0.95,priors=TRUE), digits = 6)
#                           Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                  1.834402  0.182225  1.476647  2.192452 1.000081    17899    13055
# kids_in_hh                 0.002668  0.015976 -0.028583  0.034506 0.999970    22538    12242
# age_wife                  -0.014537  0.003425 -0.021208 -0.007768 0.999976    17472    12516
# religion                  -0.050494  0.089119 -0.225930  0.124143 1.000132    18527    12166
# familyBariReligiousAfter   0.005953  0.039019 -0.069899  0.081622 1.000326    22408    12742
# religious_knowledge_scale  0.002146  0.008836 -0.015504  0.019143 1.000068    17622    12701
# MI_geo_proximity          -0.145412  0.050304 -0.251703 -0.052492 1.000029    23078    11317
# R_NUM_SIBS                 0.019903  0.010947 -0.001429  0.041378 1.000604    24687    12081
# MI_economic_capital        0.048043  0.029496 -0.009930  0.105618 0.999964    18334    12539
# MI_human_capital          -0.170931  0.049136 -0.267256 -0.074523 1.000008    16001    12294

# without sibs
## familyBariReligiousAfter   0.003640  0.034681 -0.065048  0.070895 1.000056    18307    12292

# take home of adding sibs - makes no difference

### Limit the childcare helpers to neighbors ONLY
setwd("C:/Users/robert/Dropbox/Github/Kin_networks_Bangladesh")
newdata <- read_csv("newdata.csv")

d <- newdata[c(1,5,7,8,9,36,35,44,45,47,49,51)] # add 36 for non-rels and 35 for rels
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


# subset to only neighbors or closer
non_rels <- non_rels %>% filter(location==2)

d$childcare_help_non_rels <-d$childcare_help_non_rels-0.001
non_rels$childcare_help_non_rels<- as.integer(non_rels$childcare_help_non_rels)
## Run kin and non-kin living in same neighborhood only
model8<-brm(childcare_help_non_rels ~ kids_in_hh+
              age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
              MI_geo_proximity+R_NUM_SIBS+
              MI_economic_capital+
              MI_human_capital, data=non_rels, family = negbinomial(link = "log", link_shape = "identity"),
            prior = c(set_prior("normal(0,2)", class = "b"),
                      set_prior("normal(0,10)", class="b",coef="age_wife")),
            warmup = 1000, iter = 5000, chains = 4,
            control = list(adapt_delta = 0.95))

print(summary(model8, prob=0.95,priors=TRUE), digits = 6)

#                            Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                 -1.473112  0.901511 -3.235451  0.287086 1.000053    12505    11619
# kids_in_hh                 0.449557  0.081686  0.291044  0.615302 1.000120    13483    10758
# age_wife                   0.010738  0.016961 -0.022635  0.043492 1.000008    12634    11466
# religion                   0.000817  0.369251 -0.731828  0.721307 1.000124    14168    11424
# familyBariReligiousAfter  -1.288030  0.207114 -1.704727 -0.895208 0.999958    13458    11753
# religious_knowledge_scale  0.028066  0.021941 -0.013427  0.073312 0.999988    14411    11935
# MI_geo_proximity           0.916792  0.394643  0.189856  1.743647 1.000068    12270    10734
# R_NUM_SIBS                -0.051545  0.053252 -0.155921  0.055614 0.999922    16501    11110
# MI_economic_capital        0.053647  0.130828 -0.203023  0.314774 1.000062    15459    12077
# MI_human_capital           0.874340  0.224798  0.442708  1.322791 1.000281    12101    11603

# prior to adding sibs
## familyBariReligiousAfter  -1.304154  0.205370 -1.723399 -0.916847 1.000107    12493    11190

# identical results before and after

d <- newdata[c(1,5,7,8,9,36,35,44,45,47,49,51)] # add 36 for non-rels and 35 for rels
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

# subset to only neighbors or closer
rels <- rels %>% filter(location==2)


d$childcare_help_rels <-d$childcare_help_rels-0.001
rels$childcare_help_rels<- as.integer(rels$childcare_help_rels)
## Run kin and non-kin living in same neighborhood only
model8.1<-brm(childcare_help_rels ~ kids_in_hh+
                age_wife+religion+familyBariReligiousAfter+religious_knowledge_scale+
                MI_geo_proximity+R_NUM_SIBS+
                MI_economic_capital+
                MI_human_capital, data=rels, family = negbinomial(link = "log", link_shape = "identity"),
              prior = c(set_prior("normal(0,2)", class = "b"),
                        set_prior("normal(0,10)", class="b",coef="age_wife")),
              warmup = 1000, iter = 5000, chains = 4,
              control = list(adapt_delta = 0.95))

print(summary(model8.1, prob=0.95,priors=TRUE), digits = 6)

# #                          Estimate Est.Error  l-95% CI  u-95% CI     Rhat Bulk_ESS Tail_ESS
# Intercept                  1.863035  0.063781  1.739145  1.987812 1.000018    16673    12092
# kids_in_hh                 0.010460  0.005847 -0.001005  0.021868 1.000164    18926    11455
# age_wife                  -0.012254  0.001205 -0.014605 -0.009895 1.000035    16032    12869
# religion                  -0.061522  0.033096 -0.126697  0.003064 1.000142    12904    11595
# familyBariReligiousAfter  -0.003554  0.014526 -0.032045  0.024721 1.000046    15377    11890
# religious_knowledge_scale  0.008797  0.003703  0.001438  0.016018 1.000077    15116    12209
# MI_geo_proximity          -0.108085  0.017842 -0.143332 -0.073865 1.000305    16470    11509
# R_NUM_SIBS                 0.011287  0.004058  0.003369  0.019404 1.000343    19843    12117
# MI_economic_capital        0.024127  0.010400  0.003763  0.044559 1.000391    14092    11194
# MI_human_capital          -0.143555  0.017671 -0.178863 -0.108806 1.000097    13024    10683


# without sibs
# familyBariReligiousAfter  -0.008612  0.017758 -0.043330  0.026442 1.000289    13741    10984

# No difference really