# ----------------------------------------------------------------------------------
# This R code is a demonstration for multivaraite missing value imputation using 
# missForest 
# ----------------------------------------------------------------------------------

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



####################################################
# Imputing missing values using "missForest" package
####################################################

#install.packages("missForest")
appErg.data.mis <- appErg.data.Train.mis.10perc # load dataset

# run for loops in parallel
cores = detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)
appErg.data.mis <- appErg.data.mis[ ,-29] # remove column 'rv2'(29th), as it same as 'rv1'
# now, ncols in "appErg.data.mis" are 28

pack.list <- c("missForest", "data.table", "MLmetrics", "Metrics", "caret", "foreach", 
               "doParallel", "magrittr")
#1:(ncol(appErg.data.mis)-3)
PM.missForest <- foreach(i = 1:1, .combine = cbind, .packages = c("foreach", "doParallel")) %dopar% {
  
  PM.variable <- foreach(j = 1:5, .combine = rbind, .packages = pack.list) %dopar% {
    
    cores = detectCores()
    cl <- makeCluster(cores[1]-1) #not to overload your computer
    registerDoParallel(cl)
    appErg.data.imp <- missForest(xmis = appErg.data.mis[ , 4:28], maxiter = 5, ntree = 50, 
                                  verbose = F, parallelize = 'forests')
    RMSE.temp <- caret::RMSE(pred = appErg.data.imp$ximp[[i]], obs = appErg.data.Train[[i+3]])
    MAPE.temp <- MLmetrics::MAPE(appErg.data.imp$ximp[[i]], appErg.data.Train[[i+3]])
    SMAPE.temp <- Metrics::smape(appErg.data.imp$ximp[[i]], appErg.data.Train[[i+3]])
    cbind(RMSE.temp, MAPE.temp, SMAPE.temp)
    
  }
  PM.variable
  
}

stopCluster(cl) #stop cluster
PM.missForest
fwrite(x = PM.missForest %>% as.data.frame(), file = "PM_missForest_T1.csv")
gc()

#extract performance metrics

#RMSE
PM.missForest.RMSE <- data.frame(PM.missForest[, seq(1,75, 3)])
PM.missForest.RMSE$mean <- rowMeans(PM.missForest.RMSE, na.rm = T)
fwrite(x = PM.missForest.RMSE , file = "PM_Amelia_RMSE.csv")

#MAPE
PM.missForest.MAPE <- data.frame(PM.missForest[, seq(2,75, 3)])
PM.missForest.MAPE$mean <- rowMeans(PM.missForest.MAPE, na.rm = T)
fwrite(x = PM.missForest.MAPE, file = "PM_Amelia_MAPE.csv")

#SMAPE
PM.missForest.SMAPE <- data.frame(PM.missForest[, seq(3,75, 3)])
PM.missForest.SMAPE$mean <- rowMeans(PM.missForest.SMAPE, na.rm = T)
fwrite(x = PM.missForest.SMAPE, file = "PM_Amelia_SMAPE.csv")

