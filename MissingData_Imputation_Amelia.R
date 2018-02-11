# -------------------------------------------------------------------------------------
# This R code is a demonstration for multivaraite missing value imputation using Amelia
# -------------------------------------------------------------------------------------

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



##########################################
# Missing values imputation using "Amelia"
##########################################

#install.packages("Amelia")
appErg.data.mis <- appErg.data.Train.mis.10perc # load dataset

cores = detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

pack.list <- c("Amelia", "data.table", "MLmetrics", "Metrics", "caret", "doParallel", 
               "magrittr")
#4:(ncol(appErg.data.mis)-1)
PM.Amelia <- foreach(i = 4:(ncol(appErg.data.mis)-1), .combine = cbind, .packages = c("foreach")) %dopar% {
  print(i)
  
  PM.variable <- foreach(j = 1:30, .combine = rbind, .packages = pack.list) %dopar% {
    
    print(j)
    amelia.fit <- amelia(appErg.data.mis, m = 5, parallel = "snow", idvars = c("date", "rv2"))
    # "rv2" is also idvar because the value in this column are same as "rv1"
    complete.data <- rbindlist(list(amelia.fit$imputations[[1]], 
                                    amelia.fit$imputations[[2]], 
                                    amelia.fit$imputations[[3]], 
                                    amelia.fit$imputations[[4]], 
                                    amelia.fit$imputations[[3]]))[,lapply(.SD, mean), list(date)]
    # Calculate "RMSE"
    RMSE.temp <- caret::RMSE(complete.data[[i]], appErg.data.Train[[i]])
    MAPE.temp <- MLmetrics::MAPE(complete.data[[i]], appErg.data.Train[[i]])
    SMAPE.temp <- Metrics::smape(complete.data[[i]], appErg.data.Train[[i]])
    cbind(RMSE.temp, MAPE.temp, SMAPE.temp)
    
  }
  PM.variable  
  
}

stopCluster(cl) #stop cluster
#PM.Amelia
#fwrite(x = PM.Amelia %>% as.data.frame(), file = "PM_Amelia.csv")

#extract performance metrics

#RMSE
PM.Amelia.RMSE <- data.frame(PM.Amelia[, seq(1,75, 3)])
PM.Amelia.RMSE$mean <- rowMeans(PM.Amelia.RMSE, na.rm = T)
fwrite(x = PM.Amelia.RMSE , file = "PM_Amelia_RMSE.csv")

#MAPE
PM.Amelia.MAPE <- data.frame(PM.Amelia[, seq(2,75, 3)])
PM.Amelia.MAPE$mean <- rowMeans(PM.Amelia.MAPE, na.rm = T)
fwrite(x = PM.Amelia.MAPE, file = "PM_Amelia_MAPE.csv")

#SMAPE
PM.Amelia.SMAPE <- data.frame(PM.Amelia[, seq(3,75, 3)])
PM.Amelia.SMAPE$mean <- rowMeans(PM.Amelia.SMAPE, na.rm = T)
fwrite(x = PM.Amelia.SMAPE, file = "PM_Amelia_SMAPE.csv")




##############################################################################
# Imputing missing values using "Amelia" considering data in timeseries format
##############################################################################

#install.packages("Amelia")
appErg.data.mis <- appErg.data.Train.mis.10perc # load dataset
# convert "date" into "POSIXct" format
appErg.data.mis$date <- as.POSIXct(x = appErg.data.mis$date)

# run for loops in parallel
cores = detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

pack.list <- c("Amelia", "data.table", "MLmetrics", "Metrics","caret", 
               "doParallel", "magrittr")
#4:(ncol(appErg.data.mis)-1)
PM.Amelia.ts <- foreach(i = 4:(ncol(appErg.data.mis)-1), .combine = cbind, .packages = c("foreach")) %dopar% {
  print()
  PM.variable <- foreach(j = 1:30, .combine = rbind, .packages = pack.list) %dopar% {
    
    amelia.fit.ts <- amelia(x = appErg.data.mis, m = 5, ts = "date", parallel = "snow")
    # "rv2" is also idvar because the value in this column are same as "rv1"
    complete.data <- rbindlist(list(amelia.fit.ts$imputations[[1]],
                                    amelia.fit.ts$imputations[[2]],
                                    amelia.fit.ts$imputations[[3]],
                                    amelia.fit.ts$imputations[[4]],
                                    amelia.fit.ts$imputations[[3]]))[,lapply(.SD, mean), list(date)]
    RMSE.temp <- caret::RMSE(complete.data[[i]], appErg.data.Train[[i]])
    MAPE.temp <- MLmetrics::MAPE(complete.data[[i]], appErg.data.Train[[i]])
    SMAPE.temp <- Metrics::smape(complete.data[[i]], appErg.data.Train[[i]])
    cbind(RMSE.temp, MAPE.temp, SMAPE.temp)
    
  }
  PM.variable
  
}

stopCluster(cl) #stop cluster
PM.Amelia.ts
#fwrite(x = PM.Amelia.ts %>% as.data.frame(), file = "PM_Amelia_ts.csv")
gc()
