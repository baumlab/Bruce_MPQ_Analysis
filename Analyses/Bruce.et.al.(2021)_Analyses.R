# Structural complexity change resulting from prolonged heatwave-induced mass coral mortality: Influence of underlying disturbance and structural morphology

# Authors: Kevin A. Bruce[1], John H.R. Burns[2], and Julia K. Baum[1]
# Institution: [1] Department of Biology, University of Victoria, Victoria, British Columbia, V8P 5C2, Canada
#              [2] Marine Science Department, College of Natural and Health Sciences, University of Hawai‘i at Hilo, 200 W. Kawili Street, Hilo, Hawai‘i, 96720, USA

# Corresponding Author: Julia K. Baum, Email: baum@uvic.ca


# Script to fit linear mixed-effects models of 3D structural complexity throughout the marine heatwave 

##############################


## load packages 
library(dplyr)
library(lme4)
library(MuMIn)
library(qpcR)
library(emmeans)
library(randomForest)


# library(dplyr)
# library(ggplot2)
# library(stats)
# library(Rmisc)
# library(here)
# library(ggpmisc)
# library(knitr)
# library(magick)
# library(gridExtra)
# library(car)
# library(tidyr)
# library(readxl)
# library(vegan)
# library(tidyverse)
# library(ade4)
# library(MASS)
# library(ellipse)
# library(FactoMineR)
# library(arm)
# library(ape)
# library(ggrepel)
# library(FactoMineR)
# library(ggpubr)
# library(corrplot)
# library(Matrix)
# library(lme4)
# library(TMB)
# library(glmmTMB)
# library(MuMIn)
# library(vegan)
# library(nlme)
# library(randomForest)
# library(qpcR)
# library(emmeans)
# library(sjPlot)
# library(lmtest) #Durbin-Watson Test


## Set your working directory
# Make sure that this contains the "raw_coral_2007to2019.Rdata" file
setwd("C:/Users/...") # If on a PC
setwd("/Users/...") # If on a Mac

#Load the data
load("StructuralComplexityData.Rdata") #Habitat metric dataset for linear models
load("Perc.Cover.Metrics.RData") #Habitat metric/Structure coverage dataset for random forest regressions

#########################

## Models
### 1. VRM
#Models
global.vrm <- lmer(vrm~ year * dist + exposure + (1|site), data = mpq.structure)
vrm1 <- lm(vrm ~ year, data = mpq.structure) 
vrm2 <- lm(vrm ~ year + dist, data = mpq.structure)
vrm3 <- lm(vrm ~ year * dist, data = mpq.structure)
vrm4 <- lm(vrm ~ dist, data = mpq.structure)
vrm5 <- lm(vrm ~ year + dist + exposure, data = mpq.structure) 
vrm6 <- lm(vrm ~ year * dist + exposure, data = mpq.structure)
vrm7 <- lm(vrm~ year + exposure, data = mpq.structure)
vrm8 <- lmer(vrm ~ year + dist + exposure + (1|site), data = mpq.structure)
vrm9 <- lmer(vrm ~ year * dist + exposure + (1|site), data = mpq.structure)
vrm10 <- lmer(vrm ~ year + dist + (1|site), data = mpq.structure)
vrm11 <- lmer(vrm ~ year * dist + (1|site), data = mpq.structure)
vrm12 <- lmer(vrm ~ year + (1|site), data = mpq.structure)
vrm13 <- lmer(vrm ~ year + (1|site), data = mpq.structure)

## AIC model selection
AIC_vrm.table <- model.sel(vrm1,vrm2,vrm3,vrm4,vrm5,vrm6,vrm7,vrm8,vrm9,vrm10,vrm11,vrm12, vrm13, global.vrm)

## AIC summary table 
names(AIC_vrm.table) <- c("(Int)", "Year", "Human Disturbance", "Disturbance*Year", "exposure", "Family", "Model.Class","df", "LL", "AICc", "∆AICc", "weight")
AIC_vrm.table$weight <- c(akaike.weights(AIC_vrm.table$AICc)$weights)

#Top Models within 4AIC
summary(vrm6)
summary(vrm5)

#Simplified model printout of top model
anova(vrm6)

#a. Investigate interaction term
emtrends(vrm6, pairwise ~ year, var="dist") #2017 slope significantly different than rest

#b. Examine site exposure effect
emmeans(vrm6, pairwise ~ exposure)


# < ------------------- >

### 2. Surface Complexity
#Models
global.sc <- lmer(sc~ year * dist + exposure + (1|site), data = mpq.structure)
sc1 <- lm(sc ~ year, data = mpq.structure) 
sc2 <- lm(sc ~ year + dist, data = mpq.structure)
sc3 <- lm(sc ~ year * dist, data = mpq.structure)
sc4 <- lm(sc ~ dist, data = mpq.structure)
sc5 <- lm(sc ~ year + dist + exposure, data = mpq.structure) 
sc6 <- lm(sc ~ year * dist + exposure, data = mpq.structure)
sc7 <- lm(sc~ year + exposure, data = mpq.structure)
sc8 <- lmer(sc ~ year + dist + exposure + (1|site), data = mpq.structure)
sc9 <- lmer(sc ~ year * dist + exposure + (1|site), data = mpq.structure)
sc10 <- lmer(sc ~ year + dist + (1|site), data = mpq.structure)
sc11 <- lmer(sc ~ year * dist + (1|site), data = mpq.structure)
sc12 <- lmer(sc ~ year + (1|site), data = mpq.structure)
sc13 <- lmer(sc ~ year + (1|site), data = mpq.structure)

## AIC model selection
AIC_sc.table <- model.sel(sc1,sc2,sc3,sc4,sc5,sc6,sc7,sc8,sc9,sc10,sc11,sc12, sc13, global.sc)

## AIC summary table 
names(AIC_sc.table) <- c("(Int)", "Year", "Human Disturbance", "Disturbance*Year", "exposure", "Family", "Model.Class","df", "LL", "AICc", "∆AICc", "weight")
AIC_sc.table$weight <- c(akaike.weights(AIC_sc.table$AICc)$weights)

#Top Models within 4AIC
summary(sc5)
summary(sc6)

#Simplified model printout of top model
anova(sc5)

#a. Investigate yearly differences 
emmeans(sc5, pairwise ~ year)

#b. Examine site exposure effect
emmeans(sc5, pairwise ~ exposure)


# < ------------------- >

### 3. Fractal Dimension
#Models
global.fractal.dimension <- lmer(fractal.dimension~ year * dist + exposure + (1|site), data = mpq.structure)
fractal.dimension1 <- lm(fractal.dimension ~ year, data = mpq.structure) 
fractal.dimension2 <- lm(fractal.dimension ~ year + dist, data = mpq.structure)
fractal.dimension3 <- lm(fractal.dimension ~ year * dist, data = mpq.structure)
fractal.dimension4 <- lm(fractal.dimension ~ dist, data = mpq.structure)
fractal.dimension5 <- lm(fractal.dimension ~ year + dist + exposure, data = mpq.structure) 
fractal.dimension6 <- lm(fractal.dimension ~ year * dist + exposure, data = mpq.structure)
fractal.dimension7 <- lm(fractal.dimension~ year + exposure, data = mpq.structure)
fractal.dimension8 <- lmer(fractal.dimension ~ year + dist + exposure + (1|site), data = mpq.structure)
fractal.dimension9 <- lmer(fractal.dimension ~ year * dist + exposure + (1|site), data = mpq.structure)
fractal.dimension10 <- lmer(fractal.dimension ~ year + dist + (1|site), data = mpq.structure)
fractal.dimension11 <- lmer(fractal.dimension ~ year * dist + (1|site), data = mpq.structure)
fractal.dimension12 <- lmer(fractal.dimension ~ year + (1|site), data = mpq.structure)
fractal.dimension13 <- lmer(fractal.dimension ~ year + (1|site), data = mpq.structure)

## AIC model selection
AIC_fractal.dimension.table <- model.sel(fractal.dimension1,fractal.dimension2,fractal.dimension3,fractal.dimension4,fractal.dimension5,fractal.dimension6,fractal.dimension7,fractal.dimension8,fractal.dimension9,fractal.dimension10,fractal.dimension11,fractal.dimension12, fractal.dimension13, global.fractal.dimension)

## AIC summary table 
names(AIC_fractal.dimension.table) <- c("(Int)", "Year", "Human Disturbance", "Disturbance*Year", "exposure", "Family", "Model.Class","df", "LL", "AICc", "∆AICc", "weight")
AIC_fractal.dimension.table$weight <- c(akaike.weights(AIC_fractal.dimension.table$AICc)$weights)

#Top Models within 4AIC
summary(fractal.dimension5)
summary(fractal.dimension6)

#Simplified model printout of top model
anova(fractal.dimension5)

#a. Investigate yearly differences 
emmeans(fractal.dimension5, pairwise ~ year)

#b. Examine site exposure effect
emmeans(fractal.dimension5, pairwise ~ exposure)


# < ------------------- >

### 4. Profile Curvature Range
#Models
global.pro.range <- lmer(pro.range~ year * dist + exposure + (1|site), data = mpq.structure)
pro.range1 <- lm(pro.range ~ year, data = mpq.structure) 
pro.range2 <- lm(pro.range ~ year + dist, data = mpq.structure)
pro.range3 <- lm(pro.range ~ year * dist, data = mpq.structure)
pro.range4 <- lm(pro.range ~ dist, data = mpq.structure)
pro.range5 <- lm(pro.range ~ year + dist + exposure, data = mpq.structure) 
pro.range6 <- lm(pro.range ~ year * dist + exposure, data = mpq.structure)
pro.range7 <- lm(pro.range~ year + exposure, data = mpq.structure)
pro.range8 <- lmer(pro.range ~ year + dist + exposure + (1|site), data = mpq.structure)
pro.range9 <- lmer(pro.range ~ year * dist + exposure + (1|site), data = mpq.structure)
pro.range10 <- lmer(pro.range ~ year + dist + (1|site), data = mpq.structure)
pro.range11 <- lmer(pro.range ~ year * dist + (1|site), data = mpq.structure)
pro.range12 <- lmer(pro.range ~ year + (1|site), data = mpq.structure)


## AIC model selection
AIC_pro.range.table <- model.sel(pro.range1,pro.range2,pro.range3,pro.range4,pro.range5,pro.range6,pro.range7,pro.range8,pro.range9,pro.range10,pro.range11,pro.range12, global.pro.range)

## AIC summary table 
names(AIC_pro.range.table) <- c("(Int)", "Year", "Human Disturbance", "Disturbance*Year", "exposure", "Family", "Model.Class","df", "LL", "AICc", "∆AICc", "weight")
AIC_pro.range.table$weight <- c(akaike.weights(AIC_pro.range.table$AICc)$weights)

#Top Models within 4AIC
summary(pro.range5)
summary(pro.range4)
summary(pro.range2)

#Simplified model printout of top model
anova(pro.range5)

#a. Investigate yearly differences 
emmeans(pro.range5, pairwise ~ year) #Confirmation no significant effect

#b. Examine site exposure effect
emmeans(pro.range5, pairwise ~ exposure)


# < ------------------- >

### 5. Planform Curvature Range
#Models
global.plan.range <- lmer(plan.range~ year * dist + exposure + (1|site), data = mpq.structure)
plan.range1 <- lm(plan.range ~ year, data = mpq.structure) 
plan.range2 <- lm(plan.range ~ year + dist, data = mpq.structure)
plan.range3 <- lm(plan.range ~ year * dist, data = mpq.structure)
plan.range4 <- lm(plan.range ~ dist, data = mpq.structure)
plan.range5 <- lm(plan.range ~ year + dist + exposure, data = mpq.structure) 
plan.range6 <- lm(plan.range ~ year * dist + exposure, data = mpq.structure)
plan.range7 <- lm(plan.range~ year + exposure, data = mpq.structure)
plan.range8 <- lmer(plan.range ~ year + dist + exposure + (1|site), data = mpq.structure)
plan.range9 <- lmer(plan.range ~ year * dist + exposure + (1|site), data = mpq.structure)
plan.range10 <- lmer(plan.range ~ year + dist + (1|site), data = mpq.structure)
plan.range11 <- lmer(plan.range ~ year * dist + (1|site), data = mpq.structure)
plan.range12 <- lmer(plan.range ~ year + (1|site), data = mpq.structure)

## AIC model selection
AIC_plan.range.table <- model.sel(plan.range1,plan.range2,plan.range3,plan.range4,plan.range5,plan.range6,plan.range7,plan.range8,plan.range9,plan.range10,plan.range11,plan.range12, global.plan.range)

## AIC summary table 
names(AIC_plan.range.table) <- c("(Int)", "Year", "Human Disturbance", "Disturbance*Year", "exposure", "Family", "Model.Class","df", "LL", "AICc", "∆AICc", "weight")
AIC_plan.range.table$weight <- c(akaike.weights(AIC_plan.range.table$AICc)$weights)

#Top Models within 4AIC
summary(plan.range2)
summary(plan.range5)

#Simplified model printout of top model
anova(plan.range2)

#a. Investigate yearly differences 
emmeans(plan.range2, pairwise ~ year)


# < ------------------- >

# Random Forest Regressions

#Create matrix's for each habitat metric
vrm.rf <- metric.rf[c(2,7:28)]
sc.rf <- metric.rf[c(3,7:28)]
frac.rf <- metric.rf[c(4,7:28)]
pro.rf <- metric.rf[c(5,7:28)]
plan.rf <- metric.rf[c(6,7:28)]


# < ------------------- >

## 1. VRM
#Create training data
set.seed(2)
nrow(vrm.rf) #26 rows total
train.vrm = sample(1:nrow(vrm.rf),53) #53 rows = 66% of the rows in full dataset

#a. Run Random Forest
vrm.rf.model <- randomForest(vrm ~., data = vrm.rf, subset = train.vrm, ntree = 500, importance=TRUE)
print(vrm.rf.model) #57.18% variance explained

#b. Ensure # of trees returns low levels of error
plot(vrm.rf.model)

#c. Determine variable importance
varImpPlot(vrm.rf.model) 


# < ------------------- >


## 2. Surface Complexity
#Create training data
set.seed(2)
nrow(sc.rf) #26 rows total
train.sc = sample(1:nrow(sc.rf),53) #53 rows = 66% of the rows in full dataset

#a. Run Random Forest
sc.rf.model <- randomForest(sc ~., data = sc.rf, subset = train.sc, ntree = 500, importance=TRUE)
print(sc.rf.model) #49.9% variance explained

#b. Ensure # of trees returns low levels of error
plot(sc.rf.model)

#c. Determine variable importance
varImpPlot(sc.rf.model) 


# < ------------------- >


## 3. Fractal Dimension
#Create training data
set.seed(2)
nrow(frac.rf) #26 rows total
train.frac = sample(1:nrow(frac.rf),53) #53 rows = 66% of the rows in full dataset

#a. Run Random Forest
frac.rf.model <- randomForest(fractal.dimension ~., data = frac.rf, subset = train.frac, ntree = 500, importance=TRUE)
print(frac.rf.model) #53.38% variance explained

#b. Ensure # of trees returns low levels of error
plot(frac.rf.model)

#c. Determine variable importance
varImpPlot(frac.rf.model) 


# < ------------------- >


## 4. Profile Curvature Range 
#Create training data
set.seed(2)
nrow(pro.rf) #26 rows total
train.pro = sample(1:nrow(pro.rf),53) #53 rows = 66% of the rows in full dataset

#a. Run Random Forest
pro.rf.model <- randomForest(pro.range ~., data = pro.rf, subset = train.pro, ntree = 500, importance=TRUE)
print(pro.rf.model) #9.83% variance explained

#b. Ensure # of trees returns low levels of error
plot(pro.rf.model)

#c. Determine variable importance
varImpPlot(pro.rf.model) 


# < ------------------- >


## 5. Planform Curvature Range 
#Create training data
set.seed(2)
nrow(plan.rf) #26 rows total
train.plan = sample(1:nrow(plan.rf),53) #53 rows = 66% of the rows in full dataset

#a. Run Random Forest
plan.rf.model <- randomForest(plan.range ~., data = plan.rf, subset = train.plan, ntree = 500, importance=TRUE)
print(plan.rf.model) #10.4% variance explained

#b. Ensure # of trees returns low levels of error
plot(plan.rf.model)

#c. Determine variable importance
varImpPlot(plan.rf.model) 


