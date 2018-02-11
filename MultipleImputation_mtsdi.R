# ------------------------------------------------------------------------------
# This R code is a demonstration for multivaraite missing value imputation using
# mtsdi package
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
# Imputing missing values using "mtsdi" package
###############################################

#install.packages("mtsdi")
appErg.data.mis <- appErg.data.Train.mis.10perc # load dataset
# possible imputation methods - spline, ARIMA and gam

# Imputation using "spline" method
# List of packages to be used in for loop
pack.list <- c("mtsdi", "data.table", "MLmetrics", "Metrics", "caret", 
               "foreach", "doParallel", "magrittr")
# formula used for imputation
imp.form = ~ T1 + RH_1 + T2 + RH_2 + T3 + RH_3 + T4 + RH_4 + T5 + RH_5 + T6 + RH_6 + T7 + 
  RH_7 + T8 + RH_8 + T9 + RH_9 + T_out + Press_mm_hg + RH_out + Windspeed + Visibility + 
  Tdewpoint + rv1

# run for loops in parallel
cores = detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

# control 'i' for #iterations
PM.mtsdi.spline <- foreach(i = 1:1, .combine = rbind, .packages = pack.list) %dopar% {
  print(i)
  appErg.data.mtsdi <- mnimput(formula = imp.form, dataset = appErg.data.mis, 
                               maxit = 1E2, ts = TRUE, method = "spline")
  
  appErg.data.mtsdi.spline <- appErg.data.mtsdi$filled.dataset # Imputed data
  
  RMSE.mtsdi.spline <- lapply(1:25, function(i) caret::RMSE(appErg.data.mtsdi.spline[[i]],
                                                            appErg.data.Train[[i+3]])) %>% 
    as.data.frame() %>% 
    `colnames<-`(colnames(appErg.data.mtsdi.spline))
  MAPE.mtsdi.spline <- lapply(1:25, function(i) MLmetrics::MAPE(appErg.data.mtsdi.spline[[i]],
                                                                appErg.data.Train[[i+3]])) %>% 
    as.data.frame() %>% 
    `colnames<-`(colnames(appErg.data.mtsdi.spline))
  SMAPE.mtsdi.spline <- lapply(1:25, function(i) Metrics::smape(appErg.data.mtsdi.spline[[i]],
                                                                appErg.data.Train[[i+3]])) %>% 
    as.data.frame() %>% 
    `colnames<-`(colnames(appErg.data.mtsdi.spline))
  cbind(RMSE.mtsdi.spline, MAPE.mtsdi.spline, SMAPE.mtsdi.spline)
  
}

stopCluster(cl)
PM.mtsdi.spline
fwrite(x = PM.mtsdi.spline %>% as.data.frame(), file = "PM_mtsdi_spline.csv")
gc()

#extract performance metrics

#RMSE
PM.mtsdi.spline.RMSE <- data.frame(PM.mtsdi.spline[, 1:25])
PM.mtsdi.spline.RMSE$mean <- rowMeans(PM.mtsdi.spline.RMSE, na.rm = T)
fwrite(x = PM.mtsdi.spline.RMSE , file = "PM_mtsdi_spline_RMSE.csv")

#MAPE
PM.mtsdi.spline.MAPE <- data.frame(PM.mtsdi.spline[, 26:50])
PM.mtsdi.spline.MAPE$mean <- rowMeans(PM.mtsdi.spline.MAPE, na.rm = T)
fwrite(x = PM.mtsdi.spline.MAPE , file = "PM_mtsdi_spline_MAPE.csv")

#SMAPE
PM.mtsdi.spline.SMAPE <- data.frame(PM.mtsdi.spline[, 51:75])
PM.mtsdi.spline.SMAPE$mean <- rowMeans(PM.mtsdi.spline.SMAPE, na.rm = T)
fwrite(x = PM.mtsdi.spline.SMAPE , file = "PM_mtsdi_spline_SMAPE.csv")


# Imputation using "ARIMA" method
# run for loops in parallel
cores = detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

PM.mtsdi.ARIMA <- foreach(i = 1:1, .combine = rbind, .packages = pack.list) %dopar% {
  print(i)
  appErg.data.mtsdi <- mnimput(formula = imp.form, dataset = appErg.data.mis, 
                               maxit = 1E2, ts = TRUE, method = "ARIMA")
  
  appErg.data.mtsdi.spline <- appErg.data.mtsdi$filled.dataset # Imputed data
  
  RMSE.mtsdi.spline <- lapply(1:25, function(i) caret::RMSE(appErg.data.mtsdi.spline[[i]],
                                                            appErg.data.Train[[i+3]])) %>% 
    as.data.frame() %>% 
    `colnames<-`(colnames(appErg.data.mtsdi.spline))
  MAPE.mtsdi.spline <- lapply(1:25, function(i) MLmetrics::MAPE(appErg.data.mtsdi.spline[[i]],
                                                                appErg.data.Train[[i+3]])) %>% 
    as.data.frame() %>% 
    `colnames<-`(colnames(appErg.data.mtsdi.spline))
  SMAPE.mtsdi.spline <- lapply(1:25, function(i) Metrics::smape(appErg.data.mtsdi.spline[[i]],
                                                                appErg.data.Train[[i+3]])) %>% 
    as.data.frame() %>% 
    `colnames<-`(colnames(appErg.data.mtsdi.spline))
  cbind(RMSE.mtsdi.spline, MAPE.mtsdi.spline, SMAPE.mtsdi.spline)
  
}

stopCluster(cl)

