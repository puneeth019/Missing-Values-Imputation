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



# Missing values imputation using "mice"
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

pack.list <- c("Amelia", "data.table", "MLmetrics", "Metrics", "caret", "doParallel", "magrittr")
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

pack.list <- c("Amelia", "data.table", "MLmetrics", "Metrics","caret", "doParallel")
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




# Missing values imputation using "missForest"
#install.packages("missForest")

appErg.data.mis <- appErg.data.Train.mis.10perc # load dataset

# run for loops in parallel
cores = detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)
appErg.data.mis <- appErg.data.mis[ ,-29] # remove column 'rv2'
# ncols in "appErg.data.mis" are 28

pack.list <- c("missForest", "data.table", "MLmetrics", "Metrics", "caret", "foreach", "doParallel")
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








# Missing values imputation using "Hmisc"
appErg.data.mis <- appErg.data.Train.mis.10perc # load dataset
#install.packages("Hmisc")
library(Hmisc)

# run for loops in parallel
cores = detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

# impute with mean value
RMSE.T1.mean <- impute(appErg.data.mis$T1, fun = mean) %>% as.data.frame() %>% 
  caret::RMSE(pred = ., obs = appErg.data.Train$T1)
RMSE.T1.mean # 0.3837814



PM.missForest <- foreach(i = 1:2, .combine = cbind, .packages = c("foreach")) %dopar% {
  
  PM.variable <- foreach(j = 1:2, .combine = rbind, .packages = pack.list) %dopar% {
    
    appErg.data.imp <- missForest(xmis = appErg.data.mis[ , 4:6], maxiter = 5, ntree = 50)
    RMSE.temp <- caret::RMSE(pred = appErg.data.imp$ximp[[i]], obs = appErg.data.Train[[i+3]])
    MAPE.temp <- MLmetrics::MAPE(appErg.data.imp$ximp[[i]], appErg.data.Train[[i+3]])
    SMAPE.temp <- Metrics::smape(appErg.data.imp$ximp[[i]], appErg.data.Train[[i+3]])
    c(RMSE.temp, MAPE.temp, SMAPE.temp)
    
  }
  PM.variable
  
}



MAPE.T1.mean <- impute(appErg.data.mis$T1, fun = mean) %>% 
  MLmetrics::MAPE(., appErg.data.Train$T1)
MAPE.T1.mean # 0.004550931

SMAPE.T1.mean <- impute(appErg.data.mis$T1, fun = mean) %>% 
  Metrics::smape(., appErg.data.Train$T1)
SMAPE.T1.mean # 0.004550931


# impute with median value
RMSE.T1.median <- impute(appErg.data.mis$T1, fun = median) %>% as.data.frame() %>% 
  caret::RMSE(pred = ., obs = appErg.data.Train$T1)
RMSE.T1.median # 0.3856154

MAPE.T1.median <- impute(appErg.data.mis$T1, fun = median) %>% 
  MLmetrics::MAPE(., appErg.data.Train$T1)
MAPE.T1.median # 0.004548751

SMAPE.T1.median <- impute(appErg.data.mis$T1, fun = median) %>% 
  Metrics::smape(., appErg.data.Train$T1)
SMAPE.T1.median # 0.004548751


# impute with minimum value
RMSE.T1.min <- impute(appErg.data.mis$T1, fun = min) %>% as.data.frame() %>% 
  caret::RMSE(pred = ., obs = appErg.data.Train$T1)
RMSE.T1.min # 1.390827

MAPE.T1.min <- impute(appErg.data.mis$T1, fun = min) %>% 
  MLmetrics::MAPE(., appErg.data.Train$T1)
MAPE.T1.min # 0.0508717

SMAPE.T1.min <- impute(appErg.data.mis$T1, fun = min) %>% 
  Metrics::smape(., appErg.data.Train$T1)
SMAPE.T1.min # 0.0508717


# impute with maximum value
RMSE.T1.max <- impute(appErg.data.mis$T1, fun = max) %>% as.data.frame() %>% 
  caret::RMSE(pred = ., obs = appErg.data.Train$T1)
RMSE.T1.max # 1.016792

SMAPE.T1.max <- impute(appErg.data.mis$T1, fun = max) %>% 
  Metrics::smape(., appErg.data.Train$T1)
SMAPE.T1.max # 0.04154599


# impute with random value
RMSE.T1.random <- impute(appErg.data.mis$T1, fun = 'random') %>% as.data.frame() %>% 
  caret::RMSE(pred = ., obs = appErg.data.Train$T1)
RMSE.T1.random

MAPE.T1.random <- impute(appErg.data.mis$T1, fun = 'random') %>% 
  MLmetrics::MAPE(., appErg.data.Train$T1)
MAPE.T1.random # 0.03690613

SMAPE.T1.random <- impute(appErg.data.mis$T1, fun = 'random') %>% 
  Metrics::smape(., appErg.data.Train$T1)
SMAPE.T1.random # 0.03690613


#using argImpute
imputed.arg <- aregImpute(~ T1 + T2 + T3 + T4 + T6 + T6 + T7 + T8 + T9 +
                            RH_1 + RH_2 + RH_3 + RH_4 + RH_5 + RH_6 + RH_7 + RH_8 + RH_9 +
                            T_out + Press_mm_hg + RH_out + Windspeed + Visibility + Tdewpoint +
                            rv1, data = appErg.data.mis, n.impute = 5)
imputed.arg
RMSE.T1.aregI.hmisc <- caret::RMSE(pred = imputed.arg$imputed$T1 %>% as.data.frame(), 
                       obs = appErg.data.Train$T1)
RMSE.T1.aregI.hmisc # 1.977089

MAPE.T1.aregI.hmisc <- MLmetrics::MAPE(imputed.arg$imputed$T1, appErg.data.Train$T1)
MAPE.T1.aregI.hmisc # 1.977089

SMAPE.T1.aregI.hmisc <- Metrics::smape(imputed.arg$imputed$T1, appErg.data.Train$T1)
SMAPE.T1.aregI.hmisc # 1.977089



# # Missing values imputation using "mi"
# appErg.data.mis <- appErg.data.Train.mis.10perc # load dataset
# #install.packages("mi")
# library(mi)
# 
# 
# #imputing missing value with mi
# appErg.data.imp <- mi(appErg.data.mis, seed = 123)



#imputing missing value with "mtsdi"
#install.packages("mtsdi")
library(mtsdi)
# Calculate RMSE
appErg.data.mis <- appErg.data.Train.mis.10perc # load dataset

# Multivariate Normal Imputation
appErg.data.mtsdi <- mnimput(formula = ~ T1 + T2 + T3 + T4 + T6 + T6 + T7 + T8 + T9 +
                               RH_1 + RH_2 + RH_3 + RH_4 + RH_5 + RH_6 + RH_7 + RH_8 + RH_9 +
                               T_out + Press_mm_hg + RH_out + Windspeed + Visibility + Tdewpoint +
                               rv1, dataset = appErg.data.mis, maxit = 1E2, ts = TRUE, 
                             method = "spline")

appErg.data.imp <- appErg.data.mtsdi$filled.dataset # Imputed dataset
RMSE.T1.mtsdi <- caret::RMSE(pred = appErg.data.imp$T1 %>% as.data.frame(), 
                             obs = appErg.data.Train$T1)
RMSE.T1.mtsdi # 0.07661691

SMAPE.T1.mtsdi <- Metrics::smape(appErg.data.imp$T1, appErg.data.Train$T1)
SMAPE.T1.mtsdi # 0.03326653




#imputing missing value with "VIM"
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
