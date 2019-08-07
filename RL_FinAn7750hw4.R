#################################################
# Name: RL
# ABA 
#################################################
# data wrangling
install.packages("tidyverse")
install.packages("caret")
install.packages("leaps")
install.packages("rlang")
library(tidyverse);library(ggplot2);library(tibble);library('rpart')
library('rpart.plot');library('car');library('e1071');library(glmnet) 
library(MASS);library(ISLR); library(psych); library(cluster)
library(tidyverse)  ## tidyverse for easy data manipulation and visualization
library(caret)      ## caret for easy machine learning workflow
library(leaps)      ## leaps, for computing stepwise regression
library(rlang)
################################################
# stepAIC() [MASS package], which choose the best model by AIC.
# It has an option named direction, which can take the following values:
# i) "both" (for stepwise regression, both forward and backward
# selection); "backward" (for backward selection) and "forward" (for forward selection). 
# It return the best final model.
############################################################################
library(readr)
Ins_Claims <- read_csv("C:/Users/brrsl/Desktop/Spring2019/MV/Insurance_Claims.csv") #(Home)
#Ins_Claims <- read_csv("U:/ABAMS/MV_2019/Insurance_Claims.csv") #

#View(Ins_Claims)
############################################################################
###############################################################################
##  Question # 1

# How much coverage should insurance provide to each client based on background 
# information provided by the client? 
################################################################################

names(Ins_Claims)  # #list of names of the variables
#[1] "claimid"        "incident_date"  "claim_type"     "uninhabit"      "claim_amount"  
#[6] "fraudulent"     "policy_id"      "policy_date"    "coverage"       "deductible"    
#[11] "townsize"       "gender"         "dob"            "ed_cat"         "job_start"     
#[16] "retire"         "income"         "marital"        "reside"         "occypancy_date"
#[21] "primary_res"  


testdata<-Ins_Claims[c("claimid","incident_date","claim_type","uninhabit","claim_amount",  
  "fraudulent","policy_id","policy_date","coverage","deductible", "townsize","gender","dob",
  "ed_cat","job_start", "retire","income","marital","reside","occypancy_date","primary_res")]

dim(Ins_Claims)  # #how many variables and how many subjects? [1] 4415   21
dim(testdata)    # [1] 4415   21

str(testdata)  # structure of the data

summary(Ins_Claims)

DataIns = Ins_Claims[,c("coverage","claim_type","fraudulent","deductible","income",
                        "townsize","gender","ed_cat","retire","marital","reside",
                        "primary_res","uninhabit","claim_amount")]


DataIns$claim_type
DataIns$claim_type[DataIns$claim_type==1] <- "wind_hail"
DataIns$claim_type[DataIns$claim_type==2] <- "water_damage"
DataIns$claim_type[DataIns$claim_type==3] <- "fire_smoke"
DataIns$claim_type[DataIns$claim_type==5] <- "Theft_vand"


DataIns$claim_type<-as.factor(DataIns$claim_type)

fraudt<-lm(fraudulent~claim_type, data = DataIns)
summary(fraudt)

table(DataIns$claim_type)
#
# Fit the full model 
full.model <- lm(coverage ~., data = DataIns)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)


step.model$total <- A + DataIns$income + data$cond2
data

step.model(intercept)

A<-(coef(step.model))

table(claim_type)
attach(DataIns)
DataIns$claim_type
DataIns$claim_type[DataIns$claim_type=="?"] <- NA

DataIns$claim_type[DataIns$claim_type==1] <- "wind_hail"
DataIns$claim_type[DataIns$claim_type==2] <- "water_damage"
DataIns$claim_type[DataIns$claim_type==3] <- "fire_smoke"
#DataIns$claim_type[DataIns$claim_type==4] <- "contamination"
DataIns$claim_type[DataIns$claim_type==5] <- "Theft_vand"

DataIns$claim_type<-as.factor(DataIns$claim_type)

fraudt<-lm(fraudulent~claim_type, data = DataIns)
summary(fraudt)

table(DataIns$claim_type)

###################################################################


DataIns$townsize[DataIns$townsize ==1]<- "250000"
DataIns$townsize[DataIns$townsize ==2]<- "50000 & 249999"
DataIns$townsize[DataIns$townsize ==3]<- "10000 & 49999"
DataIns$townsize[DataIns$townsize ==4]<- "2500 & 9999"

#DataIns$townsize<-as.integer(DataIns$townsize)
DataIns$townsize<-as.factor(DataIns$townsize)

table(DataIns$townsize)

library(car)


######################################################################
#
datainsurane7t = Ins_Claims[c("coverage","uninhabit","claim_amount","deductible",
                              "retire","income","reside","primary_res")]

datainsurane7 = Ins_Claims[c("coverage","claim_type","uninhabit","claim_amount","fraudulent","deductible",
                             "townsize","gender","ed_cat","retire","income","marital","reside",
                             "primary_res")]

#
attach(Ins_Claims)

datainsurane7a<-lm(coverage~deductible+income+retire+reside+primary_res +
                     uninhabit+claim_amount,data=DataIns)

summary(datainsurane7a)
##

# Fit the full model 
full.model <- lm(Ins_Claims$coverage ~ yrs_inpolicy2 + yrs_job2 + yrs_house_occup2 +  age2 +
                   Ins_Claims$claim_type + Ins_Claims$primary_res + Ins_Claims$retire + 
                   Ins_Claims$deductible + Ins_Claims$income + Ins_Claims$claim_amount +
                   Ins_Claims$uninhabit +  Ins_Claims$ed_cat +
                   Ins_Claims$reside + Ins_Claims$fraudulent + Ins_Claims$gender+  
                   Ins_Claims$townsize +Ins_Claims$marital)

# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)

step.model$anova # display results


# All Subsets Regression
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(step.model)


###############################################
lmMod <- lm(coverage ~ . , data = DataIns)
selectedMod <- step(lmMod)
summary(selectedMod)

# In statistics, the variance inflation factor (VIF) is the ratio of variance in a model with
# multiple terms, divided by the variance of a model with one term alone. It quantifies the severity
# of multicollinearity in an ordinary least squares regression analysis.
all_vifs <- car::vif(selectedMod)
print(all_vifs)

library(car)
#vif(lm(coverage ~ . , data = DataIns))
vif(selectedMod)
############################################################################################
myInsClaims <- data.frame(Ins_Claims$coverage, yrs_inpolicy2, yrs_job2, yrs_house_occup2, age2,
                          Ins_Claims$claim_type, Ins_Claims$primary_res, Ins_Claims$retire, 
                          Ins_Claims$deductible, Ins_Claims$income, Ins_Claims$claim_amount,
                          Ins_Claims$uninhabit,Ins_Claims$ed_cat,
                          Ins_Claims$reside, Ins_Claims$fraudulent, Ins_Claims$gender,  
                          Ins_Claims$townsize, Ins_Claims$marital)


#############################################################################################
library(caret)
library(leaps)

# Other useful functions 
coefficients(step.model) # model coefficients
confint(step.model, level=0.95) # CIs for model parameters 
#fitted(step.model) # predicted values
#residuals(step.model) # residuals
anova(step.model) # anova table 
#vcov(step.model) # covariance matrix for model parameters 
#influence(step.model) # regression diagnostics
                                          
#########################################################################################
datainsurane7pva <-lm(coverage~ uninhabit + claim_amount + deductible +
                      income + primary_res)

summary(datainsurane7pva)


datainsurane7pval<-lm(coverage~uninhabit + claim_amount + deductible +
                       income + primary_res + yrs_house_occup2 + retire)

summary(datainsurane7pval)


corrgood<-data.frame(coverage,primary_res,retire,deductible,
            income,claim_amount,uninhabit,yrs_job2,yrs_house_occup2)

cor(coverage,primary_res,yrs_job2,yrs_house_occup2,retire,deductible,
    income,claim_amount,uninhabit)

cor(coverage,primary_res,yrs_job2,yrs_house_occup2,retire,deductible,
    income,claim_amount,uninhabit,yrs_job2,yrs_house_occup2)



cor(corrgood)

#R-squared is the "percent of variance explained" by the model.  That is, R-squared is the 
#fraction by which the variance of the errors is less than the variance of the dependent variable

str(DataIns)    # str() function gives the structure of the data 
########################################################################################
# converting incident_date and dob to numeric factor to get the actual age of the client
Inddob<-as.Date(Ins_Claims$dob, format = "%m / %d / %Y")
Inc_date<-as.Date(Ins_Claims$incident_date, format = "%m / %d / %Y")
dob_old<-Inc_date - Inddob
age<-ceiling(dob_old/(365))
age2<- as.numeric(age)

# converting policy_date to numeric factor to get the the client's in_policy (years)
poldate<-as.Date(Ins_Claims$policy_date, format = "%m / %d / %Y")
Inpolicy<-Inc_date - poldate
yrs_inpolicy<-ceiling(Inpolicy/(365))
yrs_inpolicy2<- as.numeric(yrs_inpolicy)

# converting occypancy_date to numeric factor to get the client's date of occupancy
occupdate<-as.Date(Ins_Claims$occypancy_date, format = "%m / %d / %Y")
house_occup<-Inc_date - occupdate;
yrs_house_occup<-ceiling(house_occup/(365))
yrs_house_occup2<-as.numeric(yrs_house_occup)

# converting job_start to numeric factor to get the years in the workforce of the client
jobdate<-as.Date(Ins_Claims$job_start, format = "%m / %d / %Y")
jobdate2<-Inc_date - jobdate
yrs_job<-ceiling(jobdate2/(365))
yrs_job2<-as.numeric(yrs_job)

#### These are the new re-named vaiables that were converted above

other_dat<-data.frame(age2,yrs_house_occup2,yrs_job2,yrs_inpolicy2)

########################################################################################
########################################################################################
str(other_dat)  # Data of the clients in terms of years, Age, occupancy, workforce and 
######################################################################################
#
###################### CORRELATION ############################ 
cor(datainsurane7)

cor(other_dat)  # Given separate

 cor(DataIns)    # Given separate
###############################################################
########################################################################################

# plotting the data to see if they have some correlation among each other
plot(DataIns)     # Given separate in pdf format

plot(other_dat)   # Given separate

plot(selectedMod)
## simple linear regression ## Multiple Linear Regression Assumptions
#1- Linearity- relationship between X and Y is linear
#2- Homoscedasticity- equal variance, the variance of residual value is same for any value of X
#3- Independence- observations are independent of each other
#4- Normality- Y is normally ydistributed for any value of X
#5- existence of Outliers
#6- leverage points- any value of X far away from mean of X
#7- influential observations- any observation that changes the slope of the line

DataInsurance<-lm(Ins_Claims$coverage ~ yrs_inpolicy2 + yrs_job2 + yrs_house_occup2 +  age2 +
                    Ins_Claims$claim_type + Ins_Claims$primary_res + Ins_Claims$retire + 
                    Ins_Claims$deductible + Ins_Claims$income + Ins_Claims$claim_amount +
                    Ins_Claims$uninhabit +  Ins_Claims$ed_cat +
                    Ins_Claims$reside + Ins_Claims$fraudulent + Ins_Claims$gender+  
                    Ins_Claims$townsize +Ins_Claims$marital)

summary(DataInsurance)
##

########################################################################
########################################################################
#        y =    b0 + b1*x1   + b2*x2 + b3*x3 + b3*x4 +.......+ bn*xn
# coverage = 43.81 - 2.223(yrs_inpolicy2) + 1.20(yrs_job2) + 3.047(yrs_house_occup2) + 
#            - 0.65(age2) - 2.417(Ins_Claims$claim_type) + 0.0808(deductible) + 1.389(income) 
#           + 1.008(claim_amount) - 29.42(primary_res) -43.10(retire) - 189.50(uninhabit)

# A unit increase in yrs_house_occup will increase the coverage by 3.047 thousands dollars, 
# while a client whose retire might decrease his coverage by 43.10 thousands dollars
#(statement confirmed as long as the p-value are small p-value: < 2.2e-16).
# Also, yrs_house_occup2, deductible,income2, claim_amount, primary_res, retire,
# uninhabit are significant,
# 
# R-square = 0.5032 indicates that about 50.32% of the variation in coverage is explained by
# the DataInsurance model.

# Conclusion: A multiple linear regression is originally modeled using several numerical
# and nominal features such as yrs_house_occup2, deductible,income, claim_amount, 
# primary_res, retire, uninhabit.

# All are related and are strongly affecting
# Dependent variable: coverage
# Independent variable: yrs_house_occup2, deductible,income, claim_amount, 
# primary_res, retire, and uninhabit

# The results provides the intercept and estimated value and this in turn shows
# that all the p values of independent variables, as yrs_house_occup2, deductible,income2,
# claim_amount, primary_res, retire, uninhabit are highly significant and are
#  making an impact on the insurace coverage. 


##################################################################################

##################################################################################
newDatInsRevised <- lm(Ins_Claims$coverage ~ yrs_job2 + yrs_house_occup2 +  
     Ins_Claims$primary_res + Ins_Claims$retire + Ins_Claims$deductible +
       Ins_Claims$income + Ins_Claims$claim_amount +
     Ins_Claims$uninhabit +  Ins_Claims$ed_cat)

summary(newDatInsRevised)

#Call:
# lm(formula = Ins_Claims$coverage ~ yrs_job2 + yrs_house_occup2 + 
#     Ins_Claims$primary_res + Ins_Claims$retire + Ins_Claims$deductible + 
#     Ins_Claims$income + Ins_Claims$claim_amount + Ins_Claims$uninhabit + 
#     Ins_Claims$ed_cat)

# Residuals:
#  Min       1Q   Median       3Q      Max 
# -1885.82  -120.92   -35.99    55.55  2324.62 

# Coefficients:
#                            Estimate  Std. Error  t value  Pr(>|t|)    
#  (Intercept)              1.340e+01  1.714e+01    0.782   0.43435    
#  yrs_job2                 1.072e+00  5.421e-01    1.977   0.04815 *  
#  yrs_house_occup2         7.400e-01  3.652e-01    2.026   0.04282 *  
#  Ins_Claims$primary_res  -2.944e+01  1.303e+01   -2.260   0.02386 *  
#  Ins_Claims$retire       -4.922e+01  1.373e+01   -3.585   0.00034 ***
#  Ins_Claims$deductible    8.815e-02  3.914e-03   22.522   < 2e-16 ***
#  Ins_Claims$income        1.386e+00  6.637e-02   20.885   < 2e-16 ***
#  Ins_Claims$claim_amount  1.007e+00  2.883e-02   34.922   < 2e-16 ***
#  Ins_Claims$uninhabit    -1.896e+02  1.108e+01  -17.111   < 2e-16 ***
#  Ins_Claims$ed_cat        5.612e+00  3.109e+00    1.805   0.07114 .  
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 229.9 on 4405 degrees of freedom
# Multiple R-squared:  0.5044,	Adjusted R-squared:  0.5034 
# F-statistic: 498.2 on 9 and 4405 DF,  p-value: < 2.2e-16

#################################################################################

#################################################################################
clamount2<-(Ins_Claims$claim_amount^2)

income2<-ifelse(Ins_Claims$income >= 65, 1, 0)


###################################################################################
#        y =    b0 + b1*x1   + b2*x2 + b3*x3 + b3*x4 +.......+ bn*xn
# coverage = 25.43 + 0.6918(yrs_house_occup2) + 0.0834(deductible) + 1.283(income) + 34(income2)
#                 - 0.000511(clamount) + 1.46(claim_amount) - 30.52(primary_res) -24.81(retire)
#                 - 213.90(uninhabit)

# The estimated mean home insurance coverage that the company should provide to their clients
# should be estimated $25.35 thousands dollars

# A unit increase in deductible will increase the coverage by 0.083 thousands dollars, 
# while a client whose retire might decrease his coverage by 24.81 thousands dollars
#(statement confirmed as long as the p-value are small p-value: < 2.2e-16).
# Also, yrs_house_occup2, deductible,income2, claim_amount, primary_res, retire,
# uninhabit are significant,
# while the rest of the variables in the previous DataInsurance model were NOT which they
# were dropped for newDatInsRevised modelimprovement.

# R-square = 0.5142 indicates that about 51.42% of the variation in coverage is explained by
#the newDatInsRevised model.

# Conclusion: A multiple linear regression is originally modeled using several numerical
# and nominal features such as yrs_house_occup2, deductible,income2, claim_amount, 
# primary_res, retire, uninhabit.

# All are related and are strongly affecting
# Dependent variable: coverage
# Independent variable: yrs_house_occup2, deductible,income2, claim_amount, 
# primary_res, retire, and uninhabit

# The results provides the intercept and estimated value and this in turn shows
# that all the p values of independent variables, as yrs_house_occup2, deductible,income2,
# claim_amount, primary_res, retire, uninhabit are highly significant and are
#  making an impact on the insurace coverage. 


##################################################################################
#cross checked with correlation

myInsClaims <- data.frame(Ins_Claims$coverage, yrs_inpolicy2, yrs_job2, yrs_house_occup2, age2,
                    Ins_Claims$claim_type, Ins_Claims$primary_res, Ins_Claims$retire, 
                    Ins_Claims$deductible, Ins_Claims$income, Ins_Claims$claim_amount,
                    Ins_Claims$uninhabit,Ins_Claims$ed_cat,
                    Ins_Claims$reside, Ins_Claims$fraudulent, Ins_Claims$gender,  
                    Ins_Claims$townsize, Ins_Claims$marital)

cor(myInsClaims)

##################################################################################

#################################################################################
# Question # 2

#How can insurance company identify and/or predict a fraudulent claim? Specifically:
# a) What types of clients tend to file fraudulent claims?
# b) What are the conditions (or specific insurance claim characteristics) that increase
#    the chance of receiving a fraudulent claim? (Location, type, customer type, etc)

###################################################################################
str(myInsClaims)
###################################################################################
logistic1<-glm(Ins_Claims$fraudulent ~ yrs_inpolicy2 + yrs_job2 + yrs_house_occup2 +  
                 Ins_Claims$claim_type + Ins_Claims$primary_res + Ins_Claims$retire + 
                 Ins_Claims$deductible + Ins_Claims$income + Ins_Claims$claim_amount +
                 Ins_Claims$uninhabit +  Ins_Claims$ed_cat + age2 +
                 Ins_Claims$reside + Ins_Claims$coverage + Ins_Claims$gender+  
                 Ins_Claims$townsize +Ins_Claims$marital, family = "binomial")

summary(logistic1)

#
# Deviance Residuals: 
#   Min       1Q     Median    3Q      Max  
# -0.7901  -0.5066  -0.4469  -0.3838   2.4912  

#Coefficients:
#                          Estimate Std. Error z value Pr(>|z|)    
# (Intercept)             -1.957e+00  3.572e-01  -5.477 4.33e-08 ***
# yrs_inpolicy2            1.152e-02  2.225e-02   0.518 0.604586    
# yrs_job2                -1.283e-02  8.939e-03  -1.435 0.151347    
# yrs_house_occup2        -1.716e-02  2.032e-02  -0.845 0.398177    
# age2                    -4.914e-03  6.827e-03  -0.720 0.471656    
# Ins_Claims$claim_type    1.228e-01  3.241e-02   3.789 0.000151 ***
#  Ins_Claims$primary_res -2.058e-01  1.760e-01  -1.169 0.242257    
# Ins_Claims$retire        1.504e-01  2.200e-01   0.684 0.494208    
# Ins_Claims$deductible    1.127e-06  5.907e-05   0.019 0.984786    
# Ins_Claims$income       -9.217e-04  1.224e-03  -0.753 0.451473    
# Ins_Claims$claim_amount  2.896e-04  5.026e-04   0.576 0.564493    
# Ins_Claims$uninhabit    -3.012e-01  1.757e-01  -1.714 0.086552 .  
# Ins_Claims$ed_cat        4.581e-02  4.464e-02   1.026 0.304808    
# Ins_Claims$reside       -8.900e-02  4.875e-02  -1.826 0.067914 .  
# Ins_Claims$coverage     -1.125e-04  2.336e-04  -0.482 0.629946    
# Ins_Claims$gender        1.651e-01  9.940e-02   1.661 0.096791 .  
# Ins_Claims$townsize      5.110e-02  3.642e-02   1.403 0.160578    
# Ins_Claims$marital       1.299e-01  1.334e-01   0.973 0.330323    
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# (Dispersion parameter for binomial family taken to be 1)

# Null deviance: 2963.8  on 4414  degrees of freedom
# Residual deviance: 2910.5  on 4397  degrees of freedom
# AIC: 2946.5

# Number of Fisher Scoring iterations: 5
##################################################################



###############################################################
# answer to Q2: (A) Claim Type seems to file fraudulent claims. It is a good predictor.
# the RESIDUAL DEVIANCE is 2910.8, and the AIC is 2946.
# fraudulent = -1.957 + 1.228e-01*(0)  this reduces fraudulent = -1.957 
# the log(odds) that a client file a claim type is -1.957
# if we are predicting claim_type filing a fraudulent, we get: 
# fraudulent = -1.957 + 1.228e-01*(1). This reduces the fraudulent = -1.925 + 1.2737
# since the first term is the log(odds) of no fraudulent ... the second term is 
# indicates the increase in the log(odds) that the client has filee a claim type.
# both p-values are well below 0.05, the log(odds) and the log(odds ratio) are both
# significantly significant.
# The coefficient associated with Ins_Claims$claim_type is positive,(1.228e-01) 
# and the associated p-value is statistically significant (0.000151). This indicates 
# that CLAIM TYPE tend to have higher FRAUDULENT probabilities than non-claim types.
# In fact, this model suggests that claim type has nearly twice the odds of
# fraudulent than non-claim types. 

#The rest of the variables are not useful predictors because they have a large p-values
#


plot(fitted(logistic1),residuals(logistic1))
###############################################

# Calculating the McFadden's Pseudo R^2, we pull the log.likelihood of the null model
# out of the logistic1 variable by getting the value for the NULL DEVIANCE and /(-2)

# Also, we pull the log.likelihood of the null model
# out of the logistic1 variable by getting the value for the RESIDUAL DEVIANCE and /(-2)

#################################################
ll.null <- logistic1$null.deviance/(-2)

ll.proposed <- logistic1$deviance/(-2)

(ll.null - ll.proposed)/ll.null
# [1] 0.01799977   # this can be interpreted as the OVERALL effect size


## Use the same log.likelihood, to calculate a p-value for that R^2 using chi^2 distribution
1-pchisq(2*(ll.proposed - ll.null), df=(length(logistic1$coefficients)-1))

# [1] 1.258103e-05  ### p-value is very small

#####################################################################################
# To draw the graph, we start crerating a new data.frame that contains the
# probabilities of having a Fraudulent along with the actual fraudulent status

predicted.myInsClaims <- data.frame(probability.of.Ins_Claims.fraudulent = logistic1$fitted.values,
                                    Ins_Claims.fraudulent = Ins_Claims$fraudulent)

# then we sort the data.frame from low probabilities to higher probabilities
predicted.myInsClaims <- predicted.myInsClaims[
  order(predicted.myInsClaims$probability.of.Ins_Claims.fraudulent, 
                                           decreasing = FALSE),]

# Adding a new column to the data.frame that has RANK of each sample, from low
# probability to high probability
predicted.myInsClaims$rank <- 1:nrow(predicted.myInsClaims)

library(ggplot2)
library(cowplot)

ggplot(data = predicted.myInsClaims, aes(x = rank, y = probability.of.Ins_Claims.fraudulent)) + 
  geom_point(aes(color = Ins_Claims.fraudulent),alpha = 1, shape = 4, stroke = 2) + 
  xlab("Index") + ylab("predicted probability of getting a fraudulent client")

##################################################################################
# 
##################################################################################
# b) What are the conditions (or specific insurance claim characteristics) that increase
#    the chance of receiving a fraudulent claim? (Location, type, customer type, etc)

######################################################################
logisticB<-glm(Ins_Claims$fraudulent ~ Ins_Claims$claim_type, family = "binomial")

summary(logisticB)
##############################
#Call:
glm(formula = Ins_Claims$fraudulent ~ Ins_Claims$claim_type, 
    family = "binomial")

# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -0.5235  -0.5235  -0.4660  -0.4142   2.2353  

# Coefficients:
#                        Estimate  Std. Error  z value Pr(>|z|)    
# (Intercept)            -2.53610    0.11717  -21.644  < 2e-16 ***
#  Ins_Claims$claim_type  0.12355    0.03237    3.817  0.000135 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# (Dispersion parameter for binomial family taken to be 1)

# Null deviance: 2963.8  on 4414  degrees of freedom
# Residual deviance: 2949.1  on 4413  degrees of freedom
# AIC: 2953.1

# Number of Fisher Scoring iterations: 5
############################################################

#answer to Q2:(B):- 
# fraudulent = -2.53610 + 0.2355*(0)  this reduces fraudulent = -2.53610 
# the log(odds) that a client file a claim type is -1.957
# if we are predicting claim_type filing a fraudulent, we get: 
# fraudulent = -2.53610 + 0.2355*(1). This reduces the fraudulent= -2.53610 + 0.2355
# since the first term is the log(odds) of no fraudulent ... the second term is 
# indicates the increase in the log(odds) that the client has filee a claim type.
# both p-values are well below 0.05, the log(odds) and the log(odds ratio) are both
# significantly significant.
###################################################################################

# Calculating the McFadden's Pseudo R^2, we pull the log.likelihood of the null model
# out of the logistic1 variable by getting the value for the NULL DEVIANCE and /(-2)

#########################################################################
ll.null <- logisticB$null.deviance/(-2)

# Also, we pull the log.likelihood of the null model
# out of the logistic1 variable by getting the value for the RESIDUAL DEVIANCE and /(-2)

ll.proposed <- logisticB$deviance/(-2)
  
(ll.null - ll.proposed)/ll.null
# [1] 0.004970638  # this can be interpreted as the OVERALL effect size


## Use the same log.likelihood, to calculate a p-value for that R^2 using chi^2 distribution
1-pchisq(2*(ll.proposed - ll.null), df=(length(logisticB$coefficients)-1))

# [1] 0.0001239213  


# To draw the graph, we start crerating a new data.frame that contains the
# probabilities of having a Fraudulent along with the actual fraudulent status

predicted.myInsClaims <-data.frame(probability.of.Ins_Claims.fraudulent = logisticB$fitted.values,
                                    Ins_Claims.fraudulent = Ins_Claims$fraudulent)

# then we sort the data.frame from low probabilities to higher probabilities
predicted.myInsClaims <- predicted.myInsClaims[
  order(predicted.myInsClaims$probability.of.Ins_Claims.fraudulent, 
        decreasing = FALSE),]

# Adding a new column to the data.frame that has RANK of each sample, from low
# probability to high probability
predicted.myInsClaims$rank <- 1:nrow(predicted.myInsClaims)

## using ggplot for fraudulent and claim_type

ggplot(data = predicted.myInsClaims, aes(x = rank, y = probability.of.Ins_Claims.fraudulent)) + 
  geom_point(aes(color = Ins_Claims.fraudulent),alpha = 1, shape = 4, stroke = 2) + 
  xlab("Index") + ylab("predicted probability of getting a fraudulent client")
###################################################################

anova(logisticB, test="Chisq")

exp(coef(logisticB))


plot(logisticB, which = 4, id.n = 5)
#######################
library(psych)
library(gclus)
attach(Ins_Claims)
plot(fraudulent,claim_type)

qqplot(fraudulent,claim_type)

pairs.panels(myInsClaims[c(6,15)])


###************************************************************************
###################################################################################

# Question # 3
# What type or claims are the most costly for the company?
##################################################################################
claim_amount3<-lm(Ins_Claims$claim_amount ~ yrs_inpolicy2 + yrs_job2 + yrs_house_occup2 +
                    age2 +Ins_Claims$claim_type + Ins_Claims$primary_res +
                     Ins_Claims$retire + Ins_Claims$deductible + Ins_Claims$income +
                    Ins_Claims$uninhabit +  Ins_Claims$ed_cat + Ins_Claims$fraudulent+
                    Ins_Claims$reside + Ins_Claims$coverage + Ins_Claims$gender +  
                    Ins_Claims$townsize + Ins_Claims$marital)

summary(claim_amount3)


# Call:
# Residuals:
#  Min      1Q      Median  3Q     Max 
# -477.82  -45.74    7.52   35.11  1089.22 

#  Coefficients:
#                         Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            -62.074453   9.995283  -6.210 5.77e-10 ***
# yrs_inpolicy2            0.375467   0.591142   0.635 0.525360    
# yrs_job2                 0.088493   0.225890   0.392 0.695262    
# yrs_house_occup2        -0.460419   0.539930  -0.853 0.393851    
# age2                     0.091073   0.183917   0.495 0.620494    
# Ins_Claims$claim_type2  16.427521   4.590596   3.579 0.000349 ***
# Ins_Claims$claim_type3 125.443634   4.095581  30.629  < 2e-16 ***
# Ins_Claims$claim_type4 138.572308   5.523413  25.088  < 2e-16 ***
# Ins_Claims$claim_type5  -0.038056   3.777451  -0.010 0.991962    
# Ins_Claims$primary_res  -4.795093   5.150241  -0.931 0.351883    
# Ins_Claims$retire       -3.574995   5.634107  -0.635 0.525770    
# Ins_Claims$deductible    0.002191   0.001633   1.341 0.179949    
# Ins_Claims$income        0.065953   0.027488   2.399 0.016467 *  
# Ins_Claims$uninhabit   121.571518   4.150395  29.292  < 2e-16 ***
# Ins_Claims$ed_cat       -0.913364   1.231478  -0.742 0.458321    
# Ins_Claims$fraudulent1   4.112778   4.492308   0.916 0.359971    
# Ins_Claims$reside        1.047160   1.334904   0.784 0.432821    
# Ins_Claims$coverage      0.216302   0.005267  41.066  < 2e-16 ***
# Ins_Claims$gender       -3.218596   2.741316  -1.174 0.240416    
# Ins_Claims$townsize      1.119472   1.014846   1.103 0.270047    
# Ins_Claims$marital       0.226432   3.646929   0.062 0.950495    
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 90.77 on 4394 degrees of freedom
# Multiple R-squared:  0.6067,	Adjusted R-squared:  0.6049 
# F-statistic: 338.9 on 20 and 4394 DF,  p-value: < 2.2e-16
####################################################################
# Stepwise regression model

claimAmtModel <- lm(claim_amount ~., data = DataIns)
# Stepwise regression model
stepclaimAmtModel <- stepAIC(claimAmtModel, direction = "both", 
                      trace = FALSE)

summary(stepclaimAmtModel)

stepclaimAmtModel$anova # display results

vif(stepclaimAmtModel)

################################################################################
# The adjusted R-squared:  0.6049. So 60.49% of the variance that dependent variable
# is explained by the independent variables. p-values are very small for
# claim_type, uninhabit, income, and coverage. Statistically significant impact on these 
# outcome variables. 
# The rest are not significantly, so we dropped them.
# This tell us claim_amount decreases, the claim_type increases by ONE for every one unit
# of change for claim_type, we are going to see 16.427521 change in the claimAmount
# variable. So 1 point on the claimtype, 1 additional point is associated with 16.48 th$
# increase on the dependent variable
#
#
#####################################################################################
claim_amount3b<-lm(Ins_Claims$claim_amount ~ Ins_Claims$claim_type+ 
                     Ins_Claims$income + Ins_Claims$uninhabit + Ins_Claims$coverage )

summary(claim_amount3b)
######################## OUTPUT
# Residuals:
#  Min      1Q  Median      3Q     Max 
# -518.39  -36.09    0.53   30.28 1124.45 

# Coefficients:
#                         Estimate   Std. Error t value Pr(>|t|)    
#  (Intercept)           -34.252506   4.011115  -8.539  < 2e-16 ***
#  Ins_Claims$claim_type   2.768556   1.043317   2.654  0.00799 ** 
#  Ins_Claims$income       0.082453   0.027413   3.008  0.00265 ** 
#  Ins_Claims$uninhabit  180.110113   4.539109  39.680  < 2e-16 ***
#  Ins_Claims$coverage     0.219984   0.005675  38.762  < 2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 106.3 on 4410 degrees of freedom
# Multiple R-squared:  0.4589,	Adjusted R-squared:  0.4584 
# F-statistic: 934.9 on 4 and 4410 DF,  p-value: < 2.2e-16
####################################################################################
install.packages("dummies")
library(dummies)
cbind(dummy(DataIns$claim_type, sep = "_"))

table(DataIns$claim_type)
#    1    2    3    4    5 
# 1054  627 1039  404 1291 

library(plyr)

DataIns$claim_type <- revalue(DataIns$claim_type,
                              c("Windhail"=1, "WaterDamage"=2,"Firesmoke"=3,
                                "Contamination"=4,"TheftVand"=5))
DataIns$claim_type <- as.factor(DataIns$claim_type)

table(DataIns$claim_type)



DataIns$claim_type[,1] <- ifelse(DataIns$claim_type[,1] == 1, "Windhail",
                                 ifelse(DataIns$claim_type[,1] == 2, "WaterDamage",
                                        ifelse(DataIns$claim_type[,1] == 3, "Firesmoke",
                                               ifelse(DataIns$claim_type[,1] == 4, "Contamination",
                                                      ifelse(DataIns$claim_type[,1] == 5, "TheftVand", 99)))))

#####################################################################################
install.packages("cluster")
library(gclus) 
library(psych)
###### Graphs
hist(claim_amount, main="Cost of Claim in thousands $", 
     xlab="Claim Amount ", ylab=" Cost Claim in 1000s #",
     pch=15, col=c("red", "blue", "green", "orange"))
##
hist(coverage, main="clients coverage", 
     xlab="Amount in coverage ", ylab="client coverage ",
     pch=15, col=c("red", "blue", "green", "orange"))
##
pairs.panels(DataIns)  #shows the scatter plots and correlations of all the variables
##
pairs.panels(other_dat)
##
pairs.panels(myInsClaims)
##
pairs.panels(myInsClaims[c(4,7,9,10,11,12)])
# claim amount, claim type, income, uninhabit, coverage
pairs.panels(myInsClaims[c(1,6,10,11,12)])

pairs.panels(DataIns[,-5], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE) # show correlation ellipses
########################################################################################
##### visualizing the correlations for each set of data found in the previous data above
InsCoverage <- DataIns[c(2,3,5,6,11,14)] # get data 
InsCoverage.r <- abs(cor(InsCoverage)) # get correlations
InsCoverage.col <- dmat.color(InsCoverage.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
InsCoverage.o <- order.single(InsCoverage.r) 
cpairs(InsCoverage, InsCoverage.o, panel.colors=InsCoverage.col, gap=.5,
       main="Variables Ordered and Colored by Correlation")
########################################################################################
# load the library
library(caret)
##########################
x <- DataIns[,1:10]
y <- DataIns[,5]
# boxplot for each attribute on one image
par(mfrow=c(3,4))
for(i in 1:10) {
  boxplot(x[,i], main=names(DataIns)[i])
}
##
# load the data
x=myInsClaims3[,1:8]
y=myInsClaims3[,7]
# box and whisker plots for each attribute by class value
par(mfrow=c(2,4))
for(i in 1:8) {
  boxplot(x[,i], main=names(myInsClaims3)[i])
}  

#################################################################
plot(Ins_Claims$income,Ins_Claims$deductible,pch = 15,
     col = c("red", "blue", "green", "orange"))

plot(coverage,claim_amount,pch = 15,
     col = c("red", "blue", "green", "orange"))
##########################################################

#################################################################
plot(coverage, income, pch = 15, col = c("red", "blue", "green", "orange"),
     main="Relationship Between Ins.Coverage and Income", 
     xlab="Coverage ", ylab="Income")

# Add fit lines
abline(lm(income~coverage), col="red", lw=3) # regression line (y~x) 
lines(lowess(coverage,income), col="black", lw=3) # lowess line (x,y)
######################################

#######################################
##  Regression Line
ggplot(DataIns, aes(income, coverage,col="red")) +
  geom_point() + stat_smooth(method = lm, col="orange")

ggplot(DataIns, aes(claim_amount, claim_type,col="red")) +
  geom_point() + stat_smooth(method = lm, col="orange")

###########################################################################
InsCover <- DataIns[c(1,3,5,6)] # get data 
InsCover.r <- abs(cor(InsCover)) # get correlations
InsCover.col <- dmat.color(InsCover.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
InsCover.o <- order.single(InsCover.r) 
cpairs(InsCover, InsCover.o, panel.colors=InsCover.col, gap=.5,
       main="Variables Ordered and Colored by Correlation")


####################################################
hist(Ins_Claims$coverage, main="Histogram for Insurance Claims", 
     xlab="Coverage", 
     border="blue", 
     col="orange",
     xlim=c(1,3000),
     las=1, 
     breaks=30)


cor(Ins_Claims[c("coverage","townsize", "reside", "income")])

pairs(Ins_Claims[c("coverage","townsize", "reside", "income")])
####
###########################################################################

###################################################################
install.packages("gclus")
library(gclus)

######################################################
library(biglm)
attach(DataIns)
lm.cov<-lm(coverage~yrs_house_occup2)
lm.cov2<-lm(coverage~yrs_house_occup2 - 1) # ommiting intercept 

anova(lm.cov)
summary(lm.cov2)

opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm.cov, las = 1)      # Residuals, Fitted, ...
par(opar)

# Must hit return to run these graphs
plot(lm.cov, which = 1:6)
plot(lm.cov2, which = 1:6)
#plot(lm.cov2, which = 1:6)
# Hit <Return> to see next plot: 
#  Hit <Return> to see next plot: 
#  > plot(lm.cov, which = 1:6)
#Hit <Return> to see next plot:

hist(coverage, freq = F,
     main="Histogram for Coverage", 
     xlab="coverage", ylab="clients", 
     border="black", 
     col="orange", 
     xlim=c(1,3000),
     las=1,
     breaks=30, 
     prob = TRUE)
lines(density(coverage),col="red")
##############################
boxplot(coverage ~ age2, ylab = "coverage")
points(coverage ~ age2, predictions, col = "red")

boxplot(coverage)
boxplot(coverage ~ income, data = DataIns,col = "lightgray", varwidth = TRUE, 
        main = "Coverage vs Income",
        ylab = "Coverage",xlab = "Income")

boxplot(DataIns)
boxplot(age2,yrs_house_occup2,yrs_inpolicy2,yrs_job2,col = "lightgray", varwidth = TRUE, 
        main = "Age, Household, policy in effect and Years Employed",
        ylab = "Claim's Age",xlab = "Age   Household   Policy_effect   Years Employed")

########################################################