# ----------------------------------------------------------------------------------
# This R code is a demonstration for multivaraite missing value imputation packages
# mice, Amelia, missForest, Hmisc, mtsdi, imputeR and VIM for cross-sectional data.
# ----------------------------------------------------------------------------------

setwd(dir = "D:/DA/PGDBA/cummins_internship/project/datasets")

# Clear the Workspace
rm(list=ls())

# Load libraries
library(data.table)
library(tidyverse)
library(magrittr)
library(dplyr)
library(tidyverse)
library(tidyr)
library(foreach) # for parallel processing
library(doParallel) # for parallel processing

# Load all existing missing value imputation packages
library(mice)
library(missForest)


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



# Missing values imputation using "mice"
#install.packages("mice")

appErg.data.mis <- appErg.data.Train.mis.10perc # load dataset

# run for loops in parallel
cores = detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

pack.list <- c("mice", "data.table", "MLmetrics", "caret", "Metrics")
#4:(ncol(appErg.data.mis)-1)
PM.mice <- foreach(i = 4:4, .combine = cbind, .packages = c("foreach")) %dopar% {
  
  PM.variable <- foreach(j = 1:1, .combine = rbind, .packages = pack.list) %dopar% {
    
    imputed.data <- mice(data = appErg.data.mis, m = 5, maxit = 5, method = 'pmm')
    complete.data <- rbindlist(list(complete(imputed.data, 1), 
                                    complete(imputed.data, 2), 
                                    complete(imputed.data, 3), 
                                    complete(imputed.data, 4), 
                                    complete(imputed.data, 5)))[,lapply(.SD, mean), list(date)]
    RMSE.temp <- caret::RMSE(complete.data[[i]], appErg.data.Train[[i]])
    MAPE.temp <- MLmetrics::MAPE(complete.data[[i]], appErg.data.Train[[i]])
    SMAPE.temp <- Metrics::smape(complete.data[[i]], appErg.data.Train[[i]])
    cbind(RMSE.temp, MAPE.temp, SMAPE.temp)
    
  }
  PM.variable
  
}

stopCluster(cl) #stop cluster
PM.mice
fwrite(x = PM.mice %>% as.data.frame(), file = "PM_mice.csv")
gc()
