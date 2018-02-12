# ----------------------------------------------------------------------------------
# This R code is a demonstration for multivaraite missing value imputation packages
# mice, Amelia, missForest, Hmisc, mtsdi and imputeR
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

# Load all existing missing value imputation packages
library(mice)
library(Hmisc)
library(rpart)
library(Amelia)
library(mi)
library(missForest)
library(DMwR)
library(VIM)

# Packages to impute missing values for Time series data
library(mtsdi)
library(imputeTS)
library(zoo)
library(forecast)
library(spacetime)
library(timeSeries)
library(xts)




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
# Missing values imputation using "mice"
##########################################

#install.packages("mice")
appErg.data.mis <- appErg.data.Train.mis.10perc # load dataset

# run for loops in parallel
cores = detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

pack.list <- c("mice", "data.table", "MLmetrics", "caret", "Metrics")
#4:(ncol(appErg.data.mis)-1)
PM.mice <- foreach(i = 4:(ncol(appErg.data.mis)-1), .combine = cbind, .packages = c("foreach")) %dopar% {
  print(i)
  
  PM.variable <- foreach(j = 1:30, .combine = rbind, .packages = pack.list) %dopar% {
    
    print(j)
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
#PM.mice
#fwrite(x = PM.mice %>% as.data.frame(), file = "PM_mice.csv")

#extract performance metrics

#RMSE
PM.mice.RMSE <- data.frame(PM.mice[, seq(1,75, 3)])
PM.mice.RMSE$mean <- rowMeans(PM.mice.RMSE, na.rm = T)
fwrite(x = PM.mice.RMSE , file = "PM_mice_RMSE.csv")

#MAPE
PM.mice.MAPE <- data.frame(PM.mice[, seq(2,75, 3)])
PM.mice.MAPE$mean <- rowMeans(PM.mice.MAPE, na.rm = T)
fwrite(x = PM.mice.MAPE, file = "PM_mice_MAPE.csv")

#SMAPE
PM.mice.SMAPE <- data.frame(PM.mice[, seq(3,75, 3)])
PM.mice.SMAPE$mean <- rowMeans(PM.mice.SMAPE, na.rm = T)
fwrite(x = PM.mice.SMAPE, file = "PM_mice_SMAPE.csv")





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




# Boxplots
i = 26
RMSE.summary <- cbind(mice = PM.mice.RMSE[i], 
                         Amelia = PM.Amelia.RMSE[i]) %>% 
  `colnames<-`(c("mice", "amelia")) %>% 
  as.matrix()

boxplot.matrix(x = RMSE.summary, use.cols = T, 
               main = "RMSE for Appliances data and 10% missing data", 
               ylab = "RMSE") # boxplot for RMSEs




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

RMSE.summary <- cbind(PM.mice[[1]], PM.Amelia[[1]], PM.Amelia.ts[[1]]) %>% 
  `colnames<-`(c("mice", "amelia", "amelia.ts"))
MAPE.summary <- cbind(PM.mice[[2]], PM.Amelia[[2]], PM.Amelia.ts[[2]]) %>% 
  `colnames<-`(c("mice", "amelia", "amelia.ts"))

boxplot.matrix(x = RMSE.summary, use.cols = T, 
               main = "RMSE for Appliances data and 10% missing data", 
               ylab = "RMSE") # boxplot for RMSEs
boxplot.matrix(x = MAPE.summary, use.cols = T, 
               main = "MAPE for Appliances data and 10% missing data", 
               ylab = "MAPE") # boxplot for MAPEs
# convert "RMSE_all" from matrix to data.frame and save it
#as.data.frame(RMSE.summary) %>% fwrite(file = "RMSE_Summary10perc.csv")
#as.data.frame(MAPE.summary) %>% fwrite(file = "MAPE_Summary10perc.csv")




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





############################################
# Imputing missing values using "mi" package
############################################


# appErg.data.mis <- appErg.data.Train.mis.10perc # load dataset
# #install.packages("mi")
# library(mi)
# 
# 
# #imputing missing value with mi
# appErg.data.imp <- mi(appErg.data.mis, seed = 123)




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
  
  RMSE.mtsdi.spline <- lapply(1:25, function(j) caret::RMSE(appErg.data.mtsdi.spline[[j]],
                                                            appErg.data.Train[[j+3]])) %>% 
    as.data.frame() %>% 
    `colnames<-`(colnames(appErg.data.mtsdi.spline))
  MAPE.mtsdi.spline <- lapply(1:25, function(j) MLmetrics::MAPE(appErg.data.mtsdi.spline[[j]],
                                                                appErg.data.Train[[j+3]])) %>% 
    as.data.frame() %>% 
    `colnames<-`(colnames(appErg.data.mtsdi.spline))
  SMAPE.mtsdi.spline <- lapply(1:25, function(j) Metrics::smape(appErg.data.mtsdi.spline[[j]],
                                                                appErg.data.Train[[j+3]])) %>% 
    as.data.frame() %>% 
    `colnames<-`(colnames(appErg.data.mtsdi.spline))
  cbind(RMSE.mtsdi.spline, MAPE.mtsdi.spline, SMAPE.mtsdi.spline)
  
}

stopCluster(cl)
PM.mtsdi.spline
fwrite(x = PM.mtsdi.spline %>% as.data.frame(), file = "PM_mtsdi_spline.csv")

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
  
  RMSE.mtsdi.spline <- lapply(1:25, function(j) caret::RMSE(appErg.data.mtsdi.spline[[j]],
                                                            appErg.data.Train[[j+3]])) %>% 
    as.data.frame() %>% 
    `colnames<-`(colnames(appErg.data.mtsdi.spline))
  MAPE.mtsdi.spline <- lapply(1:25, function(j) MLmetrics::MAPE(appErg.data.mtsdi.spline[[j]],
                                                                appErg.data.Train[[j+3]])) %>% 
    as.data.frame() %>% 
    `colnames<-`(colnames(appErg.data.mtsdi.spline))
  SMAPE.mtsdi.spline <- lapply(1:25, function(j) Metrics::smape(appErg.data.mtsdi.spline[[j]],
                                                                appErg.data.Train[[j+3]])) %>% 
    as.data.frame() %>% 
    `colnames<-`(colnames(appErg.data.mtsdi.spline))
  cbind(RMSE.mtsdi.spline, MAPE.mtsdi.spline, SMAPE.mtsdi.spline)
  
}

stopCluster(cl)




###############################################
# Imputing missing values using "VIM" package
###############################################

#install.packages("VIM")
library(VIM)

### Hot Deck Imputation using "VIM"
appErg.data.mis <- appErg.data.Train.mis.10perc # load dataset

# run for loops in parallel
cores = detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

# Number of iterations can be changed using variable "i" in the code below
T1.VIM.hotdeck <- foreach(i = 1:10, .combine = rbind, 
                          .packages = c("VIM", "magrittr", "caret", "MLmetrics", "Metrics")) %dopar% {
  
  appErg.data.imp <- VIM::hotdeck(data = appErg.data.mis)
  RMSE.T1.temp <- caret::RMSE(pred = appErg.data.imp$T1 %>% as.data.frame(), 
                              obs = appErg.data$T1)
  MAPE.T1.temp <- MLmetrics::MAPE(appErg.data.imp$T1, appErg.data.Train$T1)
  SMAPE.T1.temp <- Metrics::smape(appErg.data.imp$T1, appErg.data.Train$T1)
  c(RMSE.T1.temp, MAPE.T1.temp, SMAPE.T1.temp)
  
}

stopCluster(cl) #stop cluster
boxplot(T1.VIM.hotdeck[ ,1]) #boxplot for RMSE
boxplot(T1.VIM.hotdeck[ ,2]) #boxplot for MAPE


### k-Nearest neighbour imputation using "VIM"
appErg.data.mis <- appErg.data.Train.mis.10perc # load dataset

# run for loops in parallel
cores = detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

appErg.data.imp <- VIM::kNN(data = appErg.data.mis, k = 5, numFun = median, 
                            dist_var = colnames(appErg.data.imp)[-1])
RMSE.T1.VIM.kNN <- caret::RMSE(pred = appErg.data.imp$T1 %>% as.data.frame(), 
                                   obs = appErg.data.Train$T1)
RMSE.T1.VIM.kNN # 0.1889839

MAPE.T1.VIM.kNN <- MLmetrics::MAPE(appErg.data.imp$T1, appErg.data.Train$T1)
MAPE.T1.VIM.kNN

SMAPE.T1.VIM.kNN <- Metrics::smape(appErg.data.imp$T1, appErg.data.Train$T1)
SMAPE.T1.VIM.kNN


stopCluster(cl) #stop cluster
boxplot(RMSE.T1.VIM.kNN)
boxplot(MAPE.T1.VIM.kNN)


### Iterative robust model-based imputation using "VIM"
appErg.data.mis <- appErg.data.Train.mis.10perc # load dataset

# run for loops in parallel
cores = detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

appErg.data.imp <- irmi(x = appErg.data.mis, maxit = 5, mi = 1, 
                        init.method = "median", robust = T, step = T, 
                        robMethod = "MM", 
                        modelFormulas = list(T1 = colnames(appErg.data.mis[-4])))

stopCluster(cl) #stop cluster

RMSE.T1.VIM.irmi <- caret::RMSE(pred = appErg.data.imp$T1 %>% as.data.frame(), 
                               obs = appErg.data.Train$T1)
RMSE.T1.VIM.kNN # 0.1889839

MAPE.T1.VIM.kNN <- MLmetrics::MAPE(appErg.data.imp$T1, appErg.data.Train$T1)
MAPE.T1.VIM.kNN

SMAPE.T1.VIM.kNN <- Metrics::smape(appErg.data.imp$T1, appErg.data.Train$T1)
SMAPE.T1.VIM.kNN


### "Individual Regression Imputation" using "VIM"
appErg.data.mis <- appErg.data.Train.mis.10perc # load dataset

# run for loops in parallel
cores = detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

form.T1 <- T1 ~ T2 + T3 + T4 + T6 + T6 + T7 + T8 + T9 +
  RH_1 + RH_2 + RH_3 + RH_4 + RH_5 + RH_6 + RH_7 + RH_8 + RH_9 +
  T_out + Press_mm_hg + RH_out + Windspeed + Visibility + Tdewpoint + rv1

stopCluster(cl) #stop cluster

appErg.data.imp$T1 <- regressionImp(formula = form.T1, data = appErg.data.mis)
RMSE.T1.VIM.IRI <- caret::RMSE(pred = appErg.data.imp$T1 %>% as.data.frame(), 
                               obs = appErg.data.Train$T1)
RMSE.T1.VIM.IRI

MAPE.T1.VIM.IRI <- MLmetrics::MAPE(appErg.data.imp$T1, appErg.data.Train$T1)
MAPE.T1.VIM.IRI

SMAPE.T1.VIM.IRI <- Metrics::smape(appErg.data.imp$T1, appErg.data.Train$T1)
SMAPE.T1.VIM.IRI
