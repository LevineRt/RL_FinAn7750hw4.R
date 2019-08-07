##########################
# SmartPLS
# RL
#########################
library(readr)
nomissing100 <- read.csv("~/ABAMS/MV_2019/Database_1_nomissing100.csv")
#View(Database_1_nomissing100)

names(nomissing100)

missing100<-nomissing100[c("MO1", "MO2", "MO3", "MO4", "MO5", "MO6",      
                           "MO7", "MO8", "MO9", "MO10", "EO1", "EO2",      
                           "EO3", "EO4", "EO5", "EO6", "EO7", "EO8",      
                           "EO9", "Turb1", "Turb2", "Turb3", "Turb4",
                           "PNewCust1", "PNewCust2", "PNewCust3", "PNewCust4",
                           "PNewCust5", "PNewCust6", "Speed1","Speed2", "Speed3",   
                          "Speed4", "Perf1", "Perf2", "Perf3", "Perf4", "Perf5", "Perf6")]    
                           

datanomiss<-lm(MO9~.,missing100)

summary(datanomiss)

#################################################
library(MASS)
library(car)

# Fit the full model 
missingmodel <- lm(MO9 ~., data = missing100)
# Stepwise regression model
step.missingmodel <- stepAIC(missingmodel, direction = "both", 
                      trace = FALSE)
summary(step.missingmodel)

###################################
# Fit the full model 
mo1model <- lm(MO1 ~., data = missing100)
# Stepwise regression model
step.mo1model <- stepAIC(mo1model, direction = "both", 
                             trace = FALSE)
summary(step.mo1model)

#################################################
# Fit the full model 
mo2model <- lm(MO2 ~., data = missing100)
# Stepwise regression model
step.mo2model <- stepAIC(mo2model, direction = "both", 
                         trace = FALSE)
summary(step.mo2model)

#################################################
# Fit the full model 
mo3model <- lm(MO3 ~., data = missing100)
# Stepwise regression model
step.mo3model <- stepAIC(mo3model, direction = "both", 
                         trace = FALSE)
summary(step.mo3model)

#################################################

# Fit the full model 
mo4model <- lm(MO4 ~., data = missing100)
# Stepwise regression model
step.mo4model <- stepAIC(mo4model, direction = "both", 
                         trace = FALSE)
summary(step.mo4model)

#################################################
####  Multiple R-squared:  0.8434,	Adjusted R-squared:  0.8027 
#### F-statistic: 20.73 on 20 and 77 DF,  p-value: < 2.2e-16

mo5model <- lm(MO5 ~., data = missing100)
# Stepwise regression model
step.mo5model <- stepAIC(mo5model, direction = "both", 
                         trace = FALSE)
summary(step.mo5model)

#################################################

mo10model <- lm(MO10 ~., data = missing100)
# Stepwise regression model
step.mo10model <- stepAIC(mo10model, direction = "both", 
                         trace = FALSE)
summary(step.mo10model)

###################################################
###################################################
###################################################

EO1model <- lm(EO1 ~., data = missing100)
# Stepwise regression model
step.EO1model <- stepAIC(EO1model, direction = "both", 
                          trace = FALSE)
summary(step.EO1model)

#################################################

EO2model <- lm(EO2 ~., data = missing100)
# Stepwise regression model
step.EO2model <- stepAIC(EO2model, direction = "both", 
                          trace = FALSE)
summary(step.EO2model)

#################################################

EO3model <- lm(EO3 ~., data = missing100)
# Stepwise regression model
step.EO3model <- stepAIC(EO3model, direction = "both", 
                         trace = FALSE)
summary(step.EO3model)

#################################################

EO4model <- lm(EO4 ~., data = missing100)
# Stepwise regression model
step.EO4model <- stepAIC(EO4model, direction = "both", 
                         trace = FALSE)
summary(step.EO4model)

#################################################

EO5model <- lm(EO5 ~., data = missing100)
# Stepwise regression model
step.EO5model <- stepAIC(EO5model, direction = "both", 
                         trace = FALSE)
summary(step.EO5model)

#################################################

EO6model <- lm(EO6 ~., data = missing100)
# Stepwise regression model
step.EO6model <- stepAIC(EO6model, direction = "both", 
                         trace = FALSE)
summary(step.EO6model)

#################################################

EO7model <- lm(EO7 ~., data = missing100)
# Stepwise regression model
step.EO7model <- stepAIC(EO7model, direction = "both", 
                         trace = FALSE)
summary(step.EO7model)

#################################################

EO8model <- lm(EO8 ~., data = missing100)
# Stepwise regression model
step.EO8model <- stepAIC(EO8model, direction = "both", 
                         trace = FALSE)
summary(step.EO8model)

#################################################

EO9model <- lm(EO9 ~., data = missing100)
# Stepwise regression model
step.EO9model <- stepAIC(EO9model, direction = "both", 
                         trace = FALSE)
summary(step.EO9model)

#################################################
################################################################
################################################################

missingEO<-nomissing100[c("EO1", "EO2",      
                           "EO3", "EO4", "EO5", "EO6", "EO7", "EO8",      
                           "EO9", "Turb1", "Turb2", "Turb3", "Turb4",
                           "PNewCust1", "PNewCust2", "PNewCust3", "PNewCust4",
                           "PNewCust5", "PNewCust6", "Speed1","Speed2", "Speed3",   
                           "Speed4", "Perf1", "Perf2", "Perf3", "Perf4", "Perf5", "Perf6")]    

missingEON<-nomissing100[c("EO1", "EO2","EO3", "EO4", "EO5", "EO6", "EO7", "EO8",  
                           "EO9","PNewCust1", "PNewCust2", "PNewCust3", "PNewCust4",
                           "PNewCust5", "PNewCust6", "Speed1","Speed2", "Speed3",   
                           "Speed4", "Perf1", "Perf2", "Perf3", "Perf4", "Perf5", "Perf6")]    

missingMON<-nomissing100[c("MO1", "MO2", "MO3", "MO4", "MO5", "MO6",      
                           "MO7", "MO8", "MO9", "MO10", "Turb1", "Turb2", "Turb3", "Turb4",
                           "PNewCust1", "PNewCust2", "PNewCust3", "PNewCust4",
                           "PNewCust5", "PNewCust6", "Speed1","Speed2", "Speed3",   
                           "Speed4", "Perf1", "Perf2", "Perf3", "Perf4", "Perf5", "Perf6")]    

missingMOEO<-nomissing100[c("MO1", "MO2", "MO3", "MO4", "MO5", "MO6","MO7", "MO8", "MO9", "MO10",      
                           "EO1", "EO2","EO3", "EO4", "EO5", "EO6", "EO7", "EO8", "EO9", 
                           
                           "PNewCust1", "PNewCust2", "PNewCust3", "PNewCust4",
                           "PNewCust5", "PNewCust6", "Speed1","Speed2", "Speed3",   
                           "Speed4", "Perf1", "Perf2", "Perf3", "Perf4", "Perf5", "Perf6")]    

################################################################
################################################################
################################################################
# Fit the full model 
missingmodel <- lm(MO9 ~., data = missingMON)
# Stepwise regression model
step.missingmodel <- stepAIC(missingmodel, direction = "both", 
                             trace = FALSE)
summary(step.missingmodel)

###################################
# Fit the full model 
mo1model <- lm(MO1 ~., data = missingMON)
# Stepwise regression model
step.mo1model <- stepAIC(mo1model, direction = "both", 
                         trace = FALSE)
summary(step.mo1model)

#################################################
# Fit the full model 
mo2model <- lm(MO2 ~., data = missingMON)
# Stepwise regression model
step.mo2model <- stepAIC(mo2model, direction = "both", 
                         trace = FALSE)
summary(step.mo2model)

#################################################
# Fit the full model 
mo3model <- lm(MO3 ~., data = missingMON)
# Stepwise regression model
step.mo3model <- stepAIC(mo3model, direction = "both", 
                         trace = FALSE)
summary(step.mo3model)

#################################################

# Fit the full model 
mo4model <- lm(MO4 ~., data = missingMON)
# Stepwise regression model
step.mo4model <- stepAIC(mo4model, direction = "both", 
                         trace = FALSE)
summary(step.mo4model)

#################################################

mo5model <- lm(MO5 ~., data = missingMON)
# Stepwise regression model
step.mo5model <- stepAIC(mo5model, direction = "both", 
                         trace = FALSE)
summary(step.mo5model)

#################################################

mo10model <- lm(MO10 ~., data = missingMON)
# Stepwise regression model
step.mo10model <- stepAIC(mo10model, direction = "both", 
                          trace = FALSE)
summary(step.mo10model)

###################################################
###################################################
###################################################

EO1model <- lm(EO1 ~., data = missingEON)
# Stepwise regression model
step.EO1model <- stepAIC(EO1model, direction = "both", 
                         trace = FALSE)
summary(step.EO1model)

#################################################

EO2model <- lm(EO2 ~., data = missingEON)
# Stepwise regression model
step.EO2model <- stepAIC(EO2model, direction = "both", 
                         trace = FALSE)
summary(step.EO2model)

#################################################

EO3model <- lm(EO3 ~., data = missingEON)
# Stepwise regression model
step.EO3model <- stepAIC(EO3model, direction = "both", 
                         trace = FALSE)
summary(step.EO3model)

#################################################

EO4model <- lm(EO4 ~., data = missingEON)
# Stepwise regression model
step.EO4model <- stepAIC(EO4model, direction = "both", 
                         trace = FALSE)
summary(step.EO4model)

#################################################

EO5model <- lm(EO5 ~., data = missingEON)
# Stepwise regression model
step.EO5model <- stepAIC(EO5model, direction = "both", 
                         trace = FALSE)
summary(step.EO5model)

#################################################

EO6model <- lm(EO6 ~., data = missingEON)
# Stepwise regression model
step.EO6model <- stepAIC(EO6model, direction = "both", 
                         trace = FALSE)
summary(step.EO6model)

#################################################

EO7model <- lm(EO7 ~., data = missingEON)
# Stepwise regression model
step.EO7model <- stepAIC(EO7model, direction = "both", 
                         trace = FALSE)
summary(step.EO7model)

#################################################

EO8model <- lm(EO8 ~., data = missingEON)
# Stepwise regression model
step.EO8model <- stepAIC(EO8model, direction = "both", 
                         trace = FALSE)
summary(step.EO8model)

#################################################

EO9model <- lm(EO9 ~., data = missingEON)
# Stepwise regression model
step.EO9model <- stepAIC(EO9model, direction = "both", 
                         trace = FALSE)
summary(step.EO9model)



#################################################
###################################################
###################################################
###################################################

EO1model <- lm(EO1 ~., data = missingMOEO)
# Stepwise regression model
step.EO1model <- stepAIC(EO1model, direction = "both", 
                         trace = FALSE)
summary(step.EO1model)

#################################################

EO2model <- lm(EO2 ~., data = missingMOEO)
# Stepwise regression model
step.EO2model <- stepAIC(EO2model, direction = "both", 
                         trace = FALSE)
summary(step.EO2model)

#################################################

EO3model <- lm(EO3 ~., data = missingMOEO)
# Stepwise regression model
step.EO3model <- stepAIC(EO3model, direction = "both", 
                         trace = FALSE)
summary(step.EO3model)

#################################################

EO4model <- lm(EO4 ~., data = missingMOEO)
# Stepwise regression model
step.EO4model <- stepAIC(EO4model, direction = "both", 
                         trace = FALSE)
summary(step.EO4model)

#################################################

EO5model <- lm(EO5 ~., data = missingMOEO)
# Stepwise regression model
step.EO5model <- stepAIC(EO5model, direction = "both", 
                         trace = FALSE)
summary(step.EO5model)

#################################################

EO6model <- lm(EO6 ~., data = missingMOEO)
# Stepwise regression model
step.EO6model <- stepAIC(EO6model, direction = "both", 
                         trace = FALSE)
summary(step.EO6model)

#################################################

EO7model <- lm(EO7 ~., data = missingMOEO)
# Stepwise regression model
step.EO7model <- stepAIC(EO7model, direction = "both", 
                         trace = FALSE)
summary(step.EO7model)

#################################################

EO8model <- lm(EO8 ~., data = missingMOEO)
# Stepwise regression model
step.EO8model <- stepAIC(EO8model, direction = "both", 
                         trace = FALSE)
summary(step.EO8model)

#################################################

EO9model <- lm(EO9 ~., data = missingMOEO)
# Stepwise regression model
step.EO9model <- stepAIC(EO9model, direction = "both", 
                         trace = FALSE)
summary(step.EO9model)

################################################################
################################################################
# Fit the full model 
mo10gmodel <- lm(MO10 ~., data = missingMOEO)
# Stepwise regression model
step.mo10gmodel <- stepAIC(mo10gmodel, direction = "both", 
                             trace = FALSE)
summary(step.mo10gmodel)

###################################
# Fit the full model 
mo1model <- lm(MO1 ~., data = missingMOEO)
# Stepwise regression model
step.mo1model <- stepAIC(mo1model, direction = "both", 
                         trace = FALSE)
summary(step.mo1model)

#################################################
# Fit the full model 
mo2model <- lm(MO2 ~., data = missingMOEO)
# Stepwise regression model
step.mo2model <- stepAIC(mo2model, direction = "both", 
                         trace = FALSE)
summary(step.mo2model)

#################################################
# Fit the full model 
mo3model <- lm(MO3 ~., data = missingMOEO)
# Stepwise regression model
step.mo3model <- stepAIC(mo3model, direction = "both", 
                         trace = FALSE)
summary(step.mo3model)

#################################################

# Fit the full model 
mo4model <- lm(MO4 ~., data = missingMOEO)
# Stepwise regression model
step.mo4model <- stepAIC(mo4model, direction = "both", 
                         trace = FALSE)
summary(step.mo4model)

#################################################

mo5model <- lm(MO5 ~., data = missingMOEO)
# Stepwise regression model
step.mo5model <- stepAIC(mo5model, direction = "both", 
                         trace = FALSE)
summary(step.mo5model)

#################################################

mo6model <- lm(MO6 ~., data = missingMOEO)
# Stepwise regression model
step.mo10model <- stepAIC(mo6model, direction = "both", 
                          trace = FALSE)
summary(step.mo6model)
#################################################

mo7model <- lm(MO7 ~., data = missingMOEO)
# Stepwise regression model
step.mo7model <- stepAIC(mo7model, direction = "both", 
                          trace = FALSE)
summary(step.mo7model)

#################################################

mo8model <- lm(MO8 ~., data = missingMOEO)
# Stepwise regression model
step.mo8model <- stepAIC(mo8model, direction = "both", 
                         trace = FALSE)
summary(step.mo8model)

#################################################

mo9model <- lm(MO9 ~., data = missingMOEO)
# Stepwise regression model
step.mo9model <- stepAIC(mo9model, direction = "both", 
                         trace = FALSE)
summary(step.mo9model)

#######################################################
#######################################################
######################################################

chisq.test(missing100$MO5, missing100$Perf6)

# Pearson's Chi-squared test: data:  missing100$MO5 and missing100$Perf6 
# X-squared = 57.959, df = 36, p-value = 0.01162

###############################################
# 3 rows 3 col
par(mfrow=c(3,3))
plot(missing100$MO7,  main="Market Orientation 7",
     xlab ="MO7", ylab= "density")

plot(missing100$PNewCust2, main="New Customer 2",
     xlab ="y", ylab= "density")
plot(missing100$MO2, main="Market Orientation 2",
     xlab ="MO2", ylab= "density")
plot(missing100$MO3, main="Market Orientation 3",
     xlab ="MO3", ylab= "density")
plot(missing100$MO5, main="Market Orientation 5",
     xlab ="MO5", ylab= "density")
plot(missing100$MO10, main="Market Orientation 10",
     xlab ="MO10", ylab= "density")
plot(missing100$EO1, main="Entrepreneur Orientation 1",
     xlab ="EO1", ylab= "density")
plot(missing100$EO4, main="Entrepreneur Orientation 4",
     xlab ="EO4", ylab= "density")
plot(missing100$Perf4, main="Performance",
     xlab ="Perf4", ylab= "density")

########################################
library(ggplot2)
library(cowplot)

par(mfrow=c(1,2))
hist(missing100$Perf4, main="Performance 4", 
     xlab="Perf4 ", ylab=" performance",
     pch=15, col=c("red", "blue", "green", "orange"))
##
hist(missing100$MO4, main="Market Orientation 4", 
     xlab="MO4 ", ylab="Orientation ",
     pch=15, col=c("red", "blue", "green", "yellow"))
##
pairs.panels(missing100)  #shows the scatter plots and correlations of all the variables

##
pairs.panels(missing100[c(4,7,9,10,11,12)])
# Market Orientations, Entrepenurial Orientaions, and Turbulance 
pairs.panels(missing100[c(20,21,22,23,24,25)])

pairs.panels(missingEON[,-15], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE) # show correlation ellipses
###################################################

boxplot(missing100$MO5 ~ missing100$PNewCust1, ylab = "New Customer")
