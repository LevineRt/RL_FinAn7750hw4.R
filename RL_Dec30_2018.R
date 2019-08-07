###########################################################
# Final Project: 201840-BAN7010-70
# Professor: Dr. G, M.
# RL
###########################################################
library(readr)

vendor1b <- read.csv("C:/Users/brrsl/Desktop/data700/vendor_1_demand.csv")
vendor2e <- read.csv("C:/Users/brrsl/Desktop/data700/vendor_2_demand.csv")

names(vendor1b)
names(vendor2e)

v1d<-data.matrix(vendor1b$demand)
v1s<-data.frame(vendor1b$supply)
v1se<-data.frame(vendor1b$sales_effort)
v1a<-data.frame(vendor1b$advertising)
v1db<-data.frame(vendor1b$demand_b)
max(v1db)
min(v1db)

v2d<-data.matrix(vendor2e$demand)
v2s<-data.frame(vendor2e$supply)
v2se<-data.frame(vendor2e$sales_effort)
v2a<-data.frame(vendor2e$advertising)
v2db<-data.frame(vendor2e$demand_b)
max(v2db)
min(v2db)


###########################################
# Lagging a Variable - Vendor 1 - demand B
###########################################
test_data <-c(vendor1b$demand)
# Lag the data by 1:
# Remove the last ( most recent ) element :
test_data_before <- test_data [-length(test_data)]
# Remove the first ( oldest ) element :
test_data_current <- test_data[-1]

# Create the data frame :
test_data_lagged <-data.frame(test_data_before, test_data_current)

test_data <-c(vendor1b$demand)
# Lag the data by 2:

# Remove the last two ( most recent ) elements :
test_data_before2 <- test_data[-c(length(test_data)-1, length(test_data))]

# Remove the first and last elements :
test_data_before <- test_data[-c(1, length(test_data))]

# Remove the first two ( oldest ) elements :
test_data_current <- test_data[-c(1,2)]

# Create the data frame :
test_data_lagged <-data.frame(test_data_before2, test_data_before, test_data_current)
########################################################################
# Vendor 2 -demand E
########################################################################
test_data2 <-c(vendor2e$demand_b)
# Lag the data by 1:
# Remove the last ( most recent ) element :
test_data_before2 <- test_data2 [-length(test_data2)]
# Remove the first ( oldest ) element :
test_data_current2 <- test_data2[-1]

# Create the data frame :
test_data_lagged2 <-data.frame(test_data_before2, test_data_current2)

test_dataE <-c(vendor2e$demand_b)
# Lag the data by 2:

# Remove the last two ( most recent ) elements :
test_data_before3 <- test_dataE[-c(length(test_dataE)-1, length(test_dataE))]

# Remove the first and last elements :
test_data_before2 <- test_dataE[-c(1, length(test_dataE))]

# Remove the first two ( oldest ) elements :
test_data_current2 <- test_dataE[-c(1,2)]

# Create the data frame :
test_data_laggedE <-data.frame(test_data_before3, test_data_before2, test_data_current2)

#########
max(test_data_current)
min(test_data_current)
####################################################################### 
## Linear Regression: - Fitting the Model and Testing Specification
### for vendor1 - demnad B

new_demand <- sample(278:3244,10, replace=TRUE)
n_demand_b <- sample(8300:10595,10, replace=TRUE, prob = c())
error <- sapply(1:10, function(x) {
  rnorm (1 ,0 , new_demand [ x ]* (50000 /3244))
})
sales_ef <- 20000+1000 *new_demand +2000 * n_demand_b + error
the_data <-data.frame(new_demand, n_demand_b, sales_ef)
###

model1 <-lm(sales_ef~new_demand)
model2 <-lm(sales_ef~n_demand_b)
model3 <-lm(sales_ef~new_demand + n_demand_b)

min(vendor1b$sales_effort)
max(vendor1b$sales_effort)
##################################################
####################################################################### 
## Linear Regression: - Fitting the Model and Testing Specification
### for vendor 2 - demnad E

demandV2e <- sample(278:3244,5, replace=TRUE)
demand_E <- sample(8300:10595,5, replace=TRUE, prob = c())
error <- sapply(1:5, function(x) {
  rnorm (1 ,0 , new_demand [ x ]* (50000 /3244))
})
sales_ef2 <- 20000+1000 *demandV2e +2000 * demand_E + error
the_data <-data.frame(demandV2e, demand_E, sales_ef2)
###

#model4 <-lm(sales_ef2~demandV2e)

model5 <-lm(sales_ef2~demand_E)
model6 <-lm(sales_ef2~demandV2e + demand_E)

model7 <-lm(sales_ef2~demand_E + demandV2e)
##########
#Call:
#lm(formula = sales_ef2 ~ demandV2e + demand_E)

#Coefficients:
#  (Intercept)    demandV2e     demand_E  
#  159125.3        996.5       1986.0  

min(vendor2e$sales_effort)
max(vendor2e$sales_effort)
##################################################
##So which model is better? Well, the lmtest package provides a function that allows
# us to test which of two models is "better", so to speak

library (lmtest)
lrtest (model1, model3)
# Likelihood ratio test
# Model 1: sales_ef ~ new_demand
# Model 2: sales_ef ~ new_demand + n_demand_b
#Df  LogLik Df  Chisq Pr(>Chisq)    
# 1   3 -153.09                           
# 2   4 -118.44  1 69.305  < 2.2e-16 ***

# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
####################### Now, compare model 2 to model 3:
lrtest(model2, model3)
# Likelihood ratio test

# Model 1: sales_ef ~ n_demand_b
# Model 2: sales_ef ~ new_demand + n_demand_b
#   Df  LogLik Df  Chisq Pr(>Chisq)    
#1   3 -150.63                            
#2   4 -118.44  1 64.39  1.021e-15 ***

# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# The alternative hypothesis is that the model that contained more variables (model1 and model3)
# is a "better fit" than the other ones (model2 and model3). Hence, the combine model1 and model3 is what we
# will go with.


#################################
lrtest (model4, model6)
#Likelihood ratio test

#Model 1: sales_ef2 ~ demandV2e
#Model 2: sales_ef2 ~ demandV2e + demand_E
# Df  LogLik Df  Chisq Pr(>Chisq)    
#1   3 -78.671                         
#2   4 -57.065  1 43.212  4.912e-11 ***
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  
lrtest(model5, model6)  ### Now, compare model 5 to model 6:
#Likelihood ratio test

#Model 1: sales_ef2 ~ demand_E
#Model 2: sales_ef2 ~ demandV2e + demand_E
# Df  LogLik Df  Chisq Pr(>Chisq)    
#1   3 -70.532                         
#2   4 -57.065  1 26.933  2.106e-07 ***
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

######################################
# Testing and Fixed Heteroscedasticity ################# We constructed this example 
# with heteroscedasiticty build in. Just look at how the error terms were generated.

bptest(model3)
# 	studentized Breusch-Pagan test
# data:  model3
# BP = 1.8288, df = 2, p-value = 0.4008

vcovHC(model3)
#            (Intercept)   new_demand    n_demand_b
#(Intercept) 61188049423 2432812.8395 -6637215.5523
#new_demand      2432813     202.6453     -277.5092
#n_demand_b     -6637216    -277.5092      722.0423

#install.packages("sandwich")
library(sandwich)

#sandwich::vcovHC(model3,"HC1")

coeftest(model3, vcov=vcovHC(model3, "HC1"))
#t test of coefficients:

#            Estimate    Std. Error   t value   Pr(>|t|)    
#(Intercept) 1.6008e+04  1.7222e+05    0.0929     0.9285    
#new_demand  9.8961e+02  9.3437e+00  105.9121  1.763e-12 ***
#n_demand_b  2.0017e+03  1.8602e+01  107.6053  1.578e-12 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

summary(model3)
#Call:
#lm(formula = sales_ef ~ new_demand + n_demand_b)

#Residuals:
#  Min     1Q Median     3Q    Max 
#-43907  -6535   -693   6011  45934 

#Coefficients:
#              Estimate Std. Error  t value  Pr(>|t|)    
#(Intercept)  1.601e+04  1.216e+05    0.132    0.899    
#new_demand   9.896e+02  9.038e+00  109.499  1.40e-12 ***
# n_demand_b  2.002e+03  1.303e+01  153.593  1.31e-13 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 27620 on 7 degrees of freedom
#Multiple R-squared:  0.9998,	Adjusted R-squared:  0.9998 
#F-statistic: 2.208e+04 on 2 and 7 DF,  p-value: 5.014e-14
########################################################################
model46<-c(model4, model6)
bptest(model46)
#     studentized Breusch-Pagan test
# data:  model46
# BP = 0.015366, df = 1, p-value = 0.9013
bptest(model6)
#   data:  model6
# BP = 3.7948, df = 2, p-value = 0.15

coeftest(model6, vcov=vcovHC(model6, "HC1"))

# t test of coefficients:
  
#               Estimate    Std. Error  t value    Pr(>|t|)    
# (Intercept) 1.5913e+05    1.1653e+05   1.3656    0.3054    
# demandV2e   9.9652e+02    9.7241e+00   102.4796  9.521e-05 ***
#  demand_E   1.9860e+03    1.1134e+01   178.3775  3.143e-05 ***
#########################################################################
summary(model6)

#Call:
#lm(formula = sales_ef2 ~ demandV2e + demand_E)
#Residuals:
#  1      2      3      4      5 
# -1234   9239  21422 -21086  -8340 

# Coefficients:
#             Estimate   Std. Error   t value   Pr(>|t|)    
# (Intercept) 159125.35   134597.76     1.182   0.358626    
# demandV2e      996.52       10.49    95.029   0.000111 ***
#  demand_E     1985.98       13.72   144.732   4.77e-05 ***

#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 23020 on 2 degrees of freedom
# Multiple R-squared:  0.9999,	Adjusted R-squared:  0.9999 
# F-statistic: 1.453e+04 on 2 and 2 DF,  p-value: 6.881e-05
#########################################################################
#########################################################################
#########################################################################

####  Writing a Linear Program in R
# 
install.packages("lpSolve")
library(lpSolve)
####  Writing a Linear Program in R
# solving the following problem in R:
#  max 10*0.4x1 + 5x2*0.6
# subject to:
#           - 20x5  ??? 100
#  4x1 + 5x4 - 10x5 ??? 45
#  7x1 - 4x5         ??? 25
#  20x3 - 25x5       ??? 330
#
# In order to put this into R, we need a few things. First, we need to create
# the vector that contains the coefficients in the objective function:

#obj<-c(10*.4,5*.6)
obj<-c(15*0.4,45*0.6)
A <- matrix(c(0, 0, 0, 20,
              4, 0, 5, 10,
              7, 0, 0, 4,
              0, 20, 0, 25), nrow=4, byrow=TRUE)

# Right hand side of Constraints
b <- c(100, 45, 220, 330)

# Signs
constranints_direction <- c ("<=", "<=", "<=", "<=")

solution <- lp(direction ="max",
               objective.in = obj,
               const.mat = A,
               const.dir = constranints_direction,
               const.rhs = b,
               all.int = T,
               compute.sens = TRUE)
solution
#  Success: the objective function is 114  
solution$solution
#[1] 1 4

#### Conclusion: From the above output, we can see that the compnay should produce 1 one 
### unit of product A and 4 units of product F to get the sales of $ 114

###########################################
# #########################################
###########################################

max(v1db)
min(v1db)
# Load quarterly data and convert to time series for demand
db1 <- ts(v1d, start=c(250,1), frequency=20)
# Plot the data, adding a main title and a suitable label for the y-axis
plot(db1, main="Stationary quarterly data", ylab="Demand")

hist(db1, main="Stationary quarterly data", ylab="Demand")
boxplot(db1)


# Load monthly data and convert to time series for supply
db2 <- ts(v1s, start=c(250,1), frequency=20)
# Plot the data, adding a main title and a suitable label for the y-axis
plot(db2, main="Stationary quarterly data", ylab="Supply")


db3 <- ts(v1se, start=c(2006,1), frequency=12)
plot(db3, main="Trend and seasonal monthly data", ylab="Sales Effort")

db4 <- ts(v1a, start=c(2011,1), frequency=12)
plot(db4, main="Intermittent monthly data", ylab="Advertising")

db5 <- ts(v1db, start=c(2011,1), frequency=12)
plot(db5, main="Intermittent monthly data", ylab="Demand_b")

##########################################################
##########################################################

#load the necessary libraries
library(forecast)
install.packages("TStools")
library(TStools)
#install.packages("devtools")
#devtools::install_github("trnnick/TStools")
db5 <- ts(v1db, start=c(2001,1), frequency=12)
# Let us store the time series to be explored in variable 'y' so that we can
# repeat the analysis easily with new data if needed.
y <- db5
# First we plot the series to get a general impression
plot(y)

# Let us look for trend in the data, by calculating the Centred Moving
# Average
#source("cmav.R")
cma <- cmav(y, outplot=1)
print(cma)

# CMA without back- and forecasting of missing values
cmav(y, outplot=1, fill=FALSE)

# Cox-Stuart on the estimated CMA to test for significant trend
coxstuart(cma)


# We can test for seasonality visually by producing a seasonal plot
seasplot(y)
# This functions removes the trend automatically but we can control this.
# It also provides other interesting visualisations of the seasonal component
seasplot(y,outplot=2)
seasplot(y,outplot=3)
seasplot(y,outplot=4)

# The equivalent function in the forecast package is:
seasonplot(y)

summary(seasonplot(y))

summary(y)
##########################################################
##########################################################
# ----- Decomposition ----
# We can perform classical decomposition using the decomp function

decomp(y,outplot=1)
# This again comes with various options how to estimate the seasonal component or to test for
# the presence of trend and so on. For example we can use a pure seasonal smoothing model instead
# of the mean and ask it to predict future seasonal values as well. 
y.dc <- decomp(y,outplot=1,type="pure.seasonal",h=12)

# We can also visualise the residuals for any extraordinary behaviour
# Control chart of residuals
#source("residout.R")
residout(y.dc$irregular)


# Calculate and plot ACF and PACF
acf(y,lag.max=36)
pacf(y,lag.max=36)

# 1st difference
plot(diff(y,1))
acf(diff(y,1),lag.max=36)

# Seasonal difference

plot(diff(y,12))
acf(diff(y,12),lag.max=36)
# 1st & seasonal differences
plot(diff(diff(y,12),1))
acf(diff(diff(y,12),1),lag.max=36)

##########################################################
##########################################################
######### Forecasting for fast demand ########

# Load the necessary libraries
#library(forecast)
#library(MAPA)
#library(TStools)
# Load demand and demand_b as before
db1 <- ts(v1d, start=c(2000,1), frequency=12)
# 
db5 <- ts(v1db, start=c(2001,1), frequency=12)

# In order to evaluate our forecasts let us load some test data as well.
db1.test <- ts(v1d, start=c(2016,1), frequency=20)
db5.test <- ts(v1db, start=c(2016,1), frequency=12)

# Let us first model db5
y <- db5
y.test <- db5.test
# Set horizon
h <- length(y.test)

# Naive
# Replicate the last value h times
f.naive <- rep(tail(y,1),h)
# If we transform this to a ts object it makes plotting the series and the
# forecast together very easy.
# Otherwise we need to be careful with the x coordinates of the plot
f.naive <- ts(f.naive,start=start(y.test),frequency=frequency(y.test))
# Now we can plot all elements
ts.plot(y,y.test,f.naive,col=c("blue","orange","red"))


# Seasonal Naive
f.snaive <- rep(tail(y,frequency(y)),ceiling(frequency(y)/h))
f.snaive <- ts(f.snaive,start=start(y.test),frequency=frequency(y.test))
ts.plot(y,y.test,f.snaive,col=c("blue","orange","red"))


# Exponential smoothing
# First we need to fit the appropriate model, which is selected automatically via AICc
fit.ets <- ets(y)
print(fit.ets)
# And then we use this to forecast
f.ets <- forecast(fit.ets,h=h)
# Notice that since now we have a model we can produce analytical prediction intervals
print(f.ets)
# Plot the resulting forecast and prediction intervals
plot(f.ets)
# Let us add the true test values to the plot


lines(y.test,col="red")
# Note that we easily can force a specific model
ets(y,model="AAA") # Additive errors, Additive trend, Additive seasonality

#The console output provides the fitted model and the forecast with the prediction intervals
#and the test set actuals, which were added to the default plot, are given in figure 4.3

print(fit.ets)

#The output provides information about the selected model, parameters, initial states, as
#well as various fit statistics. We can see in detail what elements are contained in fit.ets
#by using the function names as follows:

names(fit.ets)

fit.ets$method

plot(fit.ets)

#Trigonometric Exponential Smoothing (TBATS)

# Similar to ETS, TBATS also offers a decomposition of the series
plot(fit.tbats)

#Multiple Aggregation Prediction Algorithm (MAPA)

tsaggr(y, c(3,12))
tsaggr(y, c(3,12), outplot=1)

# MAPA
# The forecasts can be produced in two steps, first mapaest is used to
# estimate the models at each aggregation level and mapafor is used to
# produce the forecasts and combine them by time series component.
fit.mapa <- mapaest(y)
f.mapa <- mapafor(y, fit.mapa, fh = h)

# The estimation and forecasting can be done with a single function
mapa(y, fh=h, ifh=h, conf.lvl=c(0.8,0.95))


# Forecasting using decomposition
# STLF
f.stlf <- stlf(y,method="arima",h=h)
plot(f.stlf)
lines(y.test,col="red")

y.comp <- decomp(y,h=h,type="pure.seasonal")
y.dc <- y/y.comp$season
# Plot the series without the seasonal component
plot(y.dc)
# Forecast it with exponential smoothing and add seasonality
f.decomp <- forecast(ets(y.dc,model="ZZN"),h=h)$mean * y.comp$f.season
ts.plot(y,y.test,f.decomp,col=c("blue","orange","red"))

#Forecast evaluation

#It is trivial to repeat the analysis for the first time series by simply updating the 
#values of y and y.test.
y
y.test


##########################################################
############################################################################################
#Forecasting for intermittent demand###############

# Load the necessary library
install.packages("tsintermittent")
library(tsintermittent)
# Load the third time series
y <- ts(db5, start=c(2011,1), frequency=12)
y.test <- ts(db1, start=c(2016,1), frequency=12)
# Set the forecats horizon to be equal to the test set
h <- length(y.test)

# Croston's method is the most widely used forecasting method for intermittent demand time series. 
f.crost <- crost(y,h=h,outplot=1)
# The output contains various results which are documented in the function help.
print(f.crost)
# $frc.out is the out-of-sample forecast so we will retain only this
f.crost <- f.crost$frc.out

# SBA
f.sba <- crost(y,h=h,type="sba")$frc.out
#This provides the non-zero demand and the inter-demand intervals for the given series.
# Croston Decomposition
crost.decomp(y)

#Using this decomposition we can easily replace the exponential smoothing method used to


#Forecasting with causal methods

# Scan the sales and advertising data
sales <- v1se
advertising <- v1a
# Combine the two set of data in a data frame
data <- data.frame(sales, advertising)
# Plot the data as a scatterplot
plot(data, xlab="Advertising", ylab="Sales")


#########################################################################

##########################################
#A modern take on regression: LASSO
##########################################
#install.packages("glmnet")
library(glmnet)
# Let us get some data from the causal modelling examples

sales<-data.frame(vendor1b$sales_effort)
advertising<-data.frame(vendor1b$advertising)
# Let us load some additional potential explanatory variables
X <- read.csv("C:/Users/brrsl/Desktop/data700/vendor_1_demand.csv")
# Combine all data sets in a data frame
data <- data.frame(sales, advertising,X)

#Let us check the correlation between the target sales variable and all other.
round(cor(data)[1,],2)
# Fit LASSO regression
fit <- glmnet(x=as.matrix(data[,2:5]),y=data[,1])
# This gives us the optimal set of coefficients for each lambda. We can plot this
plot(fit,xvar="lambda")

# Cross-validated LASSO fit
fit.cv <- cv.glmnet(x=as.matrix(data[,2:5]),y=data[,1],grouped=FALSE)
# Let us get the coefficients from this fit
beta.lasso <- coef(fit.cv)

print(beta.lasso)

# OLS fit

fit.cv$lambda.1se

# very important package!!!! ###########################
#if (!require("devtools")){install.packages("devtools")}
#devtools::install_github("trnnick/TStools")