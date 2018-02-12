# ------------------------------------------------------------------------------
# This R code is a demonstration for multivaraite missing value imputation using
# 'Hmisc'
# ------------------------------------------------------------------------------

setwd(dir = "D:/DA/PGDBA/cummins_internship/project/datasets")

# Clear the Workspace
rm(list=ls())

# Load libraries
library(data.table)
library(tidyverse)
library(lubridate)
library(xts)
library(leaflet)
library(ggfortify)
library(ggplot2)
library(tidyquant)
library(magrittr)
library(dplyr)
library(tidyverse)
library(tidyr)
library(readr)
library(moments)
library(chron)
library(caret)
library(foreach) # for parallel processing
library(doParallel) # for parallel processing

# Load dataset
# UCI - Appliances energy prediction Data Set 
# https://archive.ics.uci.edu/ml/datasets/Appliances+energy+prediction

appErg.data <- fread(input = "energydata_complete.csv")

# Summary of data
str(appErg.data)
dim(appErg.data)

#Look for missing data
sum(is.na(appErg.data)) # None found

sum(appErg.data==0) # number of zeros in dataset are 15434
# replace zeroes with NAs, to facilitate calculation of MAPE
appErg.data <- appErg.data %>% as.data.frame() # convert into data.frame
for(i in 1:ncol(appErg.data)){
    appErg.data[appErg.data[, i] == 0, i] <- mean(appErg.data[,i], na.rm = TRUE)
}
sum(appErg.data==0) # number of zeros in dataset are zero now.
sum(is.na(appErg.data)) # no NAs too
appErg.data <- as.data.table(appErg.data) # convert back to data.table


# observation: values in column "rv1" are identical to the ones in "rv2"


# Splitting into train and test sets - 70:30 in order
set.seed(123)
trainIndex <- floor(x = 0.7*nrow(appErg.data))
appErg.data.Train <- appErg.data[1:trainIndex, ] # 13,814 observations in train set
appErg.data.Test <- appErg.data[(trainIndex+1):nrow(appErg.data), ] # 5,921 observations in test set



# Create 10% missing data in each of the columns except "date"
library(missForest)
library(magrittr)
set.seed(123)
appErg.data.Train.mis.10perc <- prodNA(appErg.data.Train[ ,-1], noNA = 0.1) %>% 
  cbind(appErg.data.Train[ ,1], .)

#fwrite(x = appErg.data.Train.mis.10perc, file = "appErg_data_Train_mis_10perc.csv")
#summary(appErg.data.Train.mis.10perc)


# Create 20% missing data in each of the columns except "date"
appErg.data.Train.mis.20perc <- prodNA(appErg.data.Train[ ,-1], noNA = 0.2) %>% 
  cbind(appErg.data.Train[ ,1], .)
#fwrite(x = appErg.data.Train.mis.20perc, file = "appErg_data_Train_mis_20perc.csv")


# Create 30% missing data in each of the columns except "date"
appErg.data.Train.mis.30perc <- prodNA(appErg.data.Train[ ,-1], noNA = 0.3) %>% 
  cbind(appErg.data.Train[ ,1], .) 
#fwrite(x = appErg.data.Train.mis.30perc, file = "appErg_data_Train_mis_30perc.csv")


###############################################
# Imputing missing values using "Hmisc" package
###############################################

#install.packages("Hmisc")
appErg.data.mis <- appErg.data.Train.mis.10perc # load dataset
library(Hmisc)

# impute with mean value
Hmisc.RMSE.mean <- lapply(4:28, function(i) {
  impute(appErg.data.mis[[i]], fun = mean) %>% 
    as.data.frame() %>% 
    caret::RMSE(pred = ., obs = appErg.data.Train[[i]])
  }) %>% 
  as.data.frame() %>% 
  `colnames<-`(colnames(appErg.data.Train[ ,4:28]))

Hmisc.MAPE.mean <- lapply(4:28, function(i) {
  impute(appErg.data.mis[[i]], fun = mean) %>% 
    MLmetrics::MAPE(., appErg.data.Train[[i]])
}) %>% 
  as.data.frame() %>% 
  `colnames<-`(colnames(appErg.data.Train[ ,4:28]))

Hmisc.SMAPE.mean <- lapply(4:28, function(i) {
  impute(appErg.data.mis[[i]], fun = mean) %>% 
    Metrics::smape(., appErg.data.Train[[i]])
}) %>% 
  as.data.frame() %>% 
  `colnames<-`(colnames(appErg.data.Train[ ,4:28]))


# impute with median value
Hmisc.RMSE.median <- lapply(4:28, function(i) {
  impute(appErg.data.mis[[i]], fun = median) %>% 
    as.data.frame() %>% 
    caret::RMSE(pred = ., obs = appErg.data.Train[[i]])
}) %>% 
  as.data.frame() %>% 
  `colnames<-`(colnames(appErg.data.Train[ ,4:28]))

Hmisc.MAPE.median <- lapply(4:28, function(i) {
  impute(appErg.data.mis[[i]], fun = median) %>% 
    MLmetrics::MAPE(., appErg.data.Train[[i]])
}) %>% 
  as.data.frame() %>% 
  `colnames<-`(colnames(appErg.data.Train[ ,4:28]))

Hmisc.SMAPE.median <- lapply(4:28, function(i) {
  impute(appErg.data.mis[[i]], fun = median) %>% 
    Metrics::smape(., appErg.data.Train[[i]])
}) %>% 
  as.data.frame() %>% 
  `colnames<-`(colnames(appErg.data.Train[ ,4:28]))


# impute with minimum value
Hmisc.RMSE.min <- lapply(4:28, function(i) {
  impute(appErg.data.mis[[i]], fun = min) %>% 
    as.data.frame() %>% 
    caret::RMSE(pred = ., obs = appErg.data.Train[[i]])
}) %>% 
  as.data.frame() %>% 
  `colnames<-`(colnames(appErg.data.Train[ ,4:28]))

Hmisc.MAPE.min <- lapply(4:28, function(i) {
  impute(appErg.data.mis[[i]], fun = min) %>% 
    MLmetrics::MAPE(., appErg.data.Train[[i]])
}) %>% 
  as.data.frame() %>% 
  `colnames<-`(colnames(appErg.data.Train[ ,4:28]))

Hmisc.SMAPE.min <- lapply(4:28, function(i) {
  impute(appErg.data.mis[[i]], fun = min) %>% 
    Metrics::smape(., appErg.data.Train[[i]])
}) %>% 
  as.data.frame() %>% 
  `colnames<-`(colnames(appErg.data.Train[ ,4:28]))


# impute with maximum value
Hmisc.RMSE.max <- lapply(4:28, function(i) {
  impute(appErg.data.mis[[i]], fun = max) %>% 
    as.data.frame() %>% 
    caret::RMSE(pred = ., obs = appErg.data.Train[[i]])
}) %>% 
  as.data.frame() %>% 
  `colnames<-`(colnames(appErg.data.Train[ ,4:28]))

Hmisc.MAPE.max <- lapply(4:28, function(i) {
  impute(appErg.data.mis[[i]], fun = max) %>% 
    MLmetrics::MAPE(., appErg.data.Train[[i]])
}) %>% 
  as.data.frame() %>% 
  `colnames<-`(colnames(appErg.data.Train[ ,4:28]))

Hmisc.SMAPE.max <- lapply(4:28, function(i) {
  impute(appErg.data.mis[[i]], fun = max) %>% 
    Metrics::smape(., appErg.data.Train[[i]])
}) %>% 
  as.data.frame() %>% 
  `colnames<-`(colnames(appErg.data.Train[ ,4:28]))


# impute with random value
Hmisc.RMSE.random <- lapply(4:28, function(i) {
  impute(appErg.data.mis[[i]], fun = random) %>% 
  as.data.frame() %>% 
  caret::RMSE(pred = ., obs = appErg.data.Train[[i]])
}) %>% 
  as.data.frame() %>% 
  `colnames<-`(colnames(appErg.data.Train[ ,4:28]))

Hmisc.MAPE.random <- lapply(4:28, function(i) {
  impute(appErg.data.mis[[i]], fun = random) %>% 
    MLmetrics::MAPE(., appErg.data.Train[[i]])
}) %>% 
  as.data.frame() %>% 
  `colnames<-`(colnames(appErg.data.Train[ ,4:28]))

Hmisc.SMAPE.random <- lapply(4:28, function(i) {
  impute(appErg.data.mis[[i]], fun = random) %>% 
    Metrics::smape(., appErg.data.Train[[i]])
}) %>% 
  as.data.frame() %>% 
  `colnames<-`(colnames(appErg.data.Train[ ,4:28]))

# imputation using aregImpute

data.mis.list <- lapply(4:28, function(i) 
  appErg.data.Train %>% filter(appErg.data.mis[[i]] %>% is.na()) %>% select(i))

# List of packages to be used in for loop
pack.list <- c("Hmisc", "data.table", "MLmetrics", "Metrics", "caret", 
               "foreach", "doParallel", "magrittr")
# formula used for imputation
imp.form = ~ T1 + RH_1 + T2 + RH_2 + T3 + RH_3 + T4 + RH_4 + T5 + RH_5 + T6 + RH_6 + T7 + 
  RH_7 + T8 + RH_8 + T9 + RH_9 + T_out + Press_mm_hg + RH_out + Windspeed + Visibility + 
  Tdewpoint + rv1

# run for loops in parallel
cores = detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

# control 'i' to vary #iterations
PM.Hmisc.aregImpute <- foreach(i = 1:1, .combine = rbind, .packages = pack.list) %dopar% {
  print(i)
  imputed.arg <- aregImpute(formula = imp.form, data = appErg.data.mis, n.impute = 5)
  
  RMSE.aregI.hmisc <- lapply(1:25, function(j) {
    caret::RMSE(imputed.arg$imputed[[j]] %>% rowMeans(),
                data.mis.list[[j]] %>% data.matrix())
    }
                            ) %>% as.data.frame() %>% 
    `colnames<-`(colnames(appErg.data.mis[, 4:28]))
  
  MAPE.aregI.hmisc <- lapply(1:25, function(j) {
    MLmetrics::MAPE(imputed.arg$imputed[[j]] %>% rowMeans(),
                data.mis.list[[j]] %>% data.matrix())
  }
                            ) %>% as.data.frame() %>% 
    `colnames<-`(colnames(appErg.data.mis[, 4:28]))
  
  SMAPE.aregI.hmisc <- lapply(1:25, function(j) {
    Metrics::smape(imputed.arg$imputed[[j]] %>% rowMeans(),
                    data.mis.list[[j]] %>% data.matrix())
  }
                              ) %>% as.data.frame() %>% 
    `colnames<-`(colnames(appErg.data.mis[, 4:28]))
  
  cbind(RMSE.aregI.hmisc, MAPE.aregI.hmisc, SMAPE.aregI.hmisc)
}

stopCluster(cl)
PM.Hmisc.aregImpute
fwrite(x = PM.Hmisc.aregImpute %>% as.data.frame(), file = "PM_Hmisc_aregImpute.csv")

#extract performance metrics

#RMSE
PM.Hmisc.aregImpute.RMSE <- data.frame(PM.Hmisc.aregImpute[, 1:25])
PM.Hmisc.aregImpute.RMSE$mean <- rowMeans(PM.Hmisc.aregImpute.RMSE, na.rm = T)
fwrite(x = PM.Hmisc.aregImpute.RMSE , file = "PM_Hmisc_aregImpute_RMSE.csv")

#MAPE
PM.Hmisc.aregImpute.MAPE <- data.frame(PM.Hmisc.aregImpute[, 26:50])
PM.Hmisc.aregImpute.MAPE$mean <- rowMeans(PM.Hmisc.aregImpute.MAPE, na.rm = T)
fwrite(x = PM.Hmisc.aregImpute.MAPE , file = "PM_Hmisc_aregImpute_MAPE.csv")

#SMAPE
PM.Hmisc.aregImpute.SMAPE <- data.frame(PM.Hmisc.aregImpute[, 51:75])
PM.Hmisc.aregImpute.SMAPE$mean <- rowMeans(PM.Hmisc.aregImpute.SMAPE, na.rm = T)
fwrite(x = PM.Hmisc.aregImpute.SMAPE , file = "PM_Hmisc_aregImpute_SMAPE.csv")


