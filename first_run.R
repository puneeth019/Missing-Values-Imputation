# ----------------------------------------------------------------------------------
# This R code is a demonstration for multivaraite missing value imputation packages
# mice, Amelia, missForest, Hmisc, mtsdi, imputeR and VIM for cross-sectional data.
# ----------------------------------------------------------------------------------


setwd(dir = "D:/DA/PGDBA/cummins_internship/project/datasets")

# Code to impute missing values using various existing imputation pacakges

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
library(tidyr)
library(readr)
library(moments)
library(chron)
library(caret)
library(foreach)
library(doParallel)

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

url.data <- c("https://archive.ics.uci.edu/ml/machine-learning-databases/00374/energydata_complete.csv")
#appErg.data <- fread(input = url.data)
appErg.data <- fread(input = "energydata_complete.csv")
# Can use colClasses in the above line


# Exploratory Data Analysis (EDA)

# Summary of data
str(appErg.data)

#Look for missing data
sum(is.na(appErg.data)) # None found




# convert classes to "numeric" wherever required
appErg.data$Appliances <- as.numeric(appErg.data$Appliances)
appErg.data$lights <- as.numeric(appErg.data$lights)
appErg.data$T1 <- as.numeric(appErg.data$T1)
appErg.data$RH_1 <- as.numeric(appErg.data$RH_1)
appErg.data$T2 <- as.numeric(appErg.data$T2)
appErg.data$RH_2 <- as.numeric(appErg.data$RH_2)
appErg.data$T3 <- as.numeric(appErg.data$T3)
appErg.data$RH_3 <- as.numeric(appErg.data$RH_3)
appErg.data$T4 <- as.numeric(appErg.data$T4)
appErg.data$RH_4 <- as.numeric(appErg.data$RH_4)
appErg.data$T5 <- as.numeric(appErg.data$T5)
appErg.data$RH_5 <- as.numeric(appErg.data$RH_5)
appErg.data$T6 <- as.numeric(appErg.data$T6)
appErg.data$RH_6 <- as.numeric(appErg.data$RH_6)
appErg.data$T7 <- as.numeric(appErg.data$T7)
appErg.data$RH_7 <- as.numeric(appErg.data$RH_7)
appErg.data$T8 <- as.numeric(appErg.data$T8)
appErg.data$RH_8 <- as.numeric(appErg.data$RH_8)
appErg.data$T9 <- as.numeric(appErg.data$T9)
appErg.data$RH_9 <- as.numeric(appErg.data$RH_9)

appErg.data$T_out <- as.numeric(appErg.data$T_out)
appErg.data$RH_out <- as.numeric(appErg.data$RH_out)
appErg.data$Press_mm_hg <- as.numeric(appErg.data$Press_mm_hg)

appErg.data$Windspeed <- as.numeric(appErg.data$Windspeed)
appErg.data$Visibility <- as.numeric(appErg.data$Visibility)
appErg.data$Tdewpoint <- as.numeric(appErg.data$Tdewpoint)

appErg.data$rv1 <- as.numeric(appErg.data$rv1)
appErg.data$rv2 <- as.numeric(appErg.data$rv2)


# Summary of dataset
summary(appErg.data)


# Check distribution of attributes

# hist(appErg.data$T1, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(appErg.data$T1), col = "blue", lwd = 2)
# lines(density(appErg.data$T1, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(appErg.data$T1)
# 
# 
# hist(appErg.data$RH_1, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(appErg.data$RH_1), col = "blue", lwd = 2)
# lines(density(appErg.data$RH_1, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(appErg.data$RH_1)
# 
# 
# hist(appErg.data$T2, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(appErg.data$T2), col = "blue", lwd = 2)
# lines(density(appErg.data$T2, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(appErg.data$T2)
# 
# 
# hist(appErg.data$RH_2, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(appErg.data$RH_2), col = "blue", lwd = 2)
# lines(density(appErg.data$RH_2, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(appErg.data$RH_2)
# 
# 
# hist(appErg.data$T3, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(appErg.data$T3), col = "blue", lwd = 2)
# lines(density(appErg.data$T3, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(appErg.data$T3)
# 
# 
# hist(appErg.data$RH_3, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(appErg.data$RH_3), col = "blue", lwd = 2)
# lines(density(appErg.data$RH_3, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(appErg.data$RH_3)
# 
# 
# hist(appErg.data$T4, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(appErg.data$T4), col = "blue", lwd = 2)
# lines(density(appErg.data$T4, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(appErg.data$T4)
# 
# 
# hist(appErg.data$RH_4, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(appErg.data$RH_4), col = "blue", lwd = 2)
# lines(density(appErg.data$RH_4, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(appErg.data$RH_4)
# 
# 
# hist(appErg.data$T5, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(appErg.data$T5), col = "blue", lwd = 2)
# lines(density(appErg.data$T5, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(appErg.data$T5)
# 
# 
# hist(appErg.data$RH_5, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(appErg.data$RH_5), col = "blue", lwd = 2)
# lines(density(appErg.data$RH_5, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(appErg.data$RH_5)
# 
# 
# hist(appErg.data$T6, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(appErg.data$T6), col = "blue", lwd = 2)
# lines(density(appErg.data$T6, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(appErg.data$T6)
# 
# 
# hist(appErg.data$RH_6, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(appErg.data$RH_6), col = "blue", lwd = 2)
# lines(density(appErg.data$RH_6, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(appErg.data$RH_6)
# 
# 
# hist(appErg.data$T7, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(appErg.data$T7), col = "blue", lwd = 2)
# lines(density(appErg.data$T7, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(appErg.data$T7)
# 
# 
# hist(appErg.data$RH_7, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(appErg.data$RH_7), col = "blue", lwd = 2)
# lines(density(appErg.data$RH_7, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(appErg.data$RH_7)
# 
# 
# hist(appErg.data$T8, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(appErg.data$T8), col = "blue", lwd = 2)
# lines(density(appErg.data$T8, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(appErg.data$T8)
# 
# 
# hist(appErg.data$RH_8, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(appErg.data$RH_8), col = "blue", lwd = 2)
# lines(density(appErg.data$RH_8, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(appErg.data$RH_8)
# 
# 
# hist(appErg.data$T9, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(appErg.data$T9), col = "blue", lwd = 2)
# lines(density(appErg.data$T9, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(appErg.data$T9)
# 
# 
# hist(appErg.data$RH_9, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(appErg.data$RH_9), col = "blue", lwd = 2)
# lines(density(appErg.data$RH_9, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(appErg.data$RH_9)
# 
# 
# hist(appErg.data$T_out, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(appErg.data$T_out), col = "blue", lwd = 2)
# lines(density(appErg.data$T_out, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(appErg.data$T_out)
# 
# 
# hist(appErg.data$Press_mm_hg, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(appErg.data$Press_mm_hg), col = "blue", lwd = 2)
# lines(density(appErg.data$Press_mm_hg, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(appErg.data$Press_mm_hg)
# 
# 
# hist(appErg.data$RH_out, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(appErg.data$RH_out), col = "blue", lwd = 2)
# lines(density(appErg.data$RH_out, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(appErg.data$RH_out)
# 
# 
# hist(appErg.data$Windspeed, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(appErg.data$Windspeed), col = "blue", lwd = 2)
# lines(density(appErg.data$Windspeed, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(appErg.data$Windspeed)
# 
# 
# hist(appErg.data$Visibility, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(appErg.data$Visibility), col = "blue", lwd = 2)
# lines(density(appErg.data$Visibility, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(appErg.data$Visibility)
# 
# 
# hist(appErg.data$Tdewpoint, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(appErg.data$Tdewpoint), col = "blue", lwd = 2)
# lines(density(appErg.data$Tdewpoint, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(appErg.data$Tdewpoint)

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



# # Visualize missing values using "VIM" package
# install.packages("VIM")
# library(VIM)
# appErg.data.mis <- appErg.data.Train.mis.10perc # load dataset
# vim_plot <- aggr(appErg.data.mis, col = c('navyblue','yellow'),
#                     numbers = TRUE, sortVars = TRUE,
#                     labels = names(appErg.data.mis), cex.axis = .7,
#                     gap = 3, ylab = c("Missing data","Pattern"))
# 
# 
# # Visualise missing values using "mice" package
# install.packages("mice")
# library(mice)
# appErg.data.mis <- appErg.data.Train.mis.10perc # load dataset
# md.pattern(appErg.data.mis)
#
#
# # Use "mice" to impute missing values
# appErg.data.mis <- appErg.data.Train.mis.10perc # load dataset
# imputed.data <- mice(data = appErg.data.mis, m = 5, maxit = 1, method = 'pmm', 
#                      seed = 123, diagnostics = T)
# summary(imputed.data)
# 
# # check imputed values
# imputed.data$imp$T1
# 
# # get complete data
# complete.data.1 <- mice::complete(imputed.data, 1)
# complete.data.2 <- mice::complete(imputed.data, 2)
# complete.data.3 <- mice::complete(imputed.data, 3)
# complete.data.4 <- mice::complete(imputed.data, 4)
# complete.data.5 <- mice::complete(imputed.data, 5)
# 
# # write complete data into files, if required
# fwrite(x = complete.data.1, file = "completeData1.csv")
# fwrite(x = complete.data.2, file = "completeData2.csv")
# fwrite(x = complete.data.3, file = "completeData3.csv")
# fwrite(x = complete.data.4, file = "completeData4.csv")
# fwrite(x = complete.data.5, file = "completeData5.csv")
# 
# 
# # summary of completed datasets
# summary(complete.data.1)
# summary(complete.data.2)
# summary(complete.data.3)
# summary(complete.data.4)
# summary(complete.data.5)
# 
# 
# # calculate average of these five "Complete" datasets
# complete.data <- rbindlist(list(complete.data.1, complete.data.2, complete.data.3, complete.data.4, complete.data.5))[,lapply(.SD, mean), list(date, Appliances, lights)]
# 
# # write data, if required
# fwrite(x = complete.data, file = "completeData.csv")



# Missing values imputation using "mice"
#install.packages("mice")
library(mice)
# Calculate RMSE
appErg.data.mis <- appErg.data.Train.mis.10perc # load dataset

# run for loops in parallel
cores = detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

RMSE.mice <- foreach(i = 1:1, .combine = rbind, .packages = c("mice", "data.table", "caret")) %dopar% {
  
  imputed.data <- mice(data = appErg.data.mis, m = 5, maxit = 1, method = 'pmm')
  complete.data <- rbindlist(list(complete(imputed.data, 1), 
                                  complete(imputed.data, 2), 
                                  complete(imputed.data, 3), 
                                  complete(imputed.data, 4), 
                                  complete(imputed.data, 5)))[,lapply(.SD, mean), list(date, Appliances, lights)]
  RMSE.T1.temp <- caret::RMSE(appErg.data$T1,complete.data$T1)
  RMSE.T1.temp
  
}

stopCluster(cl) #stop cluster
#boxplot(RMSE.mice)




# Missing values imputation using "Amelia"
#install.packages("Amelia")
library(Amelia)

# Calculate RMSE
appErg.data.mis <- appErg.data.Train.mis.10perc # load dataset

# run for loops in parallel
cores = detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

# Number of iterations can be changed using variable "i" in the code below 
RMSE.T1.Amelia <- foreach(i = 1:1, .combine = rbind, .packages = c("Amelia", "data.table", "caret")) %dopar% {
  
  amelia.fit <- amelia(appErg.data.mis, m = 5, parallel = "snow", 
                       idvars = c("date", "rv2"))
  # "rv2" is also idvar because the value in this column are same as "rv1"
  complete.data <- rbindlist(list(amelia.fit$imputations[[1]], 
                                  amelia.fit$imputations[[2]], 
                                  amelia.fit$imputations[[3]], 
                                  amelia.fit$imputations[[4]], 
                                  amelia.fit$imputations[[3]]))[,lapply(.SD, mean), list(date, Appliances, lights)]
  RMSE.T1.temp <- caret::RMSE(appErg.data$T1,complete.data$T1)
  RMSE.T1.temp
  
}

stopCluster(cl) #stop cluster
#boxplot(RMSE.T1.Amelia)

# export the outputs to csv files, if required
#write.amelia(amelia.fit, file.stem = "ameliaImputedData")




# Imputing missing values using "Amelia" considering data in timeseries format
#install.packages("Amelia")
library(Amelia)
appErg.data.mis <- appErg.data.Train.mis.10perc # load dataset

# convert "date" into "POSIXct" format
appErg.data.mis$date <- as.POSIXct(x = appErg.data.mis$date)

# run for loops in parallel
cores = detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

# Number of iterations can be changed using variable "i" in the code below
RMSE.T1.Amelia.ts <- foreach(i = 1:1, .combine = rbind, .packages = c("Amelia", "data.table", "caret")) %dopar% {
  
  amelia.fit.ts <- amelia(x = appErg.data.mis, m = 5,
                       ts = "date", parallel = "snow")
  # "rv2" is also idvar because the value in this column are same as "rv1"
  complete.data <- rbindlist(list(amelia.fit.ts$imputations[[1]],
                                  amelia.fit.ts$imputations[[2]],
                                  amelia.fit.ts$imputations[[3]],
                                  amelia.fit.ts$imputations[[4]],
                                  amelia.fit.ts$imputations[[3]]))[,lapply(.SD, mean), list(date, Appliances, lights)]
  RMSE.T1.ts.temp <- caret::RMSE(appErg.data$T1, complete.data$T1)
  RMSE.T1.ts.temp
 
}

stopCluster(cl) #stop cluster
#boxplot(RMSE.T1.Amelia.ts)


RMSE.summary <- cbind(RMSE.mice, RMSE.T1.Amelia, RMSE.T1.Amelia.ts) %>% `colnames<-`(c("mice", "amelia", "amelia.ts"))
boxplot.matrix(x = RMSE.summary, use.cols = T, main = "RMSE for Appliances data and 10% missing data", 
               ylab = "RMSE") # boxplot
# convert "RMSE_all" from matrix to data.frame and save it
#as.data.frame(RMSE.summary) %>% fwrite(file = "RMSE_Summary10perc.csv")




# Missing values imputation using "missForest"
#install.packages("missForest")
library(missForest)

appErg.data.mis <- appErg.data.Train.mis.10perc # load dataset

# Calculate RMSE
# run for loops in parallel
cores = detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

appErg.data.imp <- missForest(xmis = appErg.data.mis[ ,-1], maxiter = 2, 
                              ntree = 50, mtry = 6)

stopCluster(cl) #stop cluster

#check imputed values
appErg.data.imp$ximp

#check imputation error
appErg.data.imp$OOBerror

#comparing actual data accuracy
appErg.data.err <- mixError(appErg.data.imp$ximp, appErg.data.mis, appErg.data)
appErg.data.err

RMSE.T1.missF <- caret::RMSE(pred = appErg.data.imp$ximp$T1 %>% as.data.frame(), 
                                   obs = appErg.data$T1)
RMSE.T1.aregI.missF # 1.977089


# Missing values imputation using "Hmisc"
appErg.data.mis <- appErg.data.Train.mis.10perc # load dataset
#install.packages("Hmisc")
library(Hmisc)

# impute with mean value
RMSE.T1.mean <- impute(appErg.data.mis$T1, fun = mean) %>% as.data.frame() %>% 
  caret::RMSE(pred = ., obs = appErg.data$T1)
RMSE.T1.mean # 0.3837814

# impute with median value
RMSE.T1.median <- impute(appErg.data.mis$T1, fun = median) %>% as.data.frame() %>% 
  caret::RMSE(pred = ., obs = appErg.data$T1)
RMSE.T1.median # 0.3856154

# impute with minimum value
RMSE.T1.min <- impute(appErg.data.mis$T1, fun = min) %>% as.data.frame() %>% 
  caret::RMSE(pred = ., obs = appErg.data$T1)
RMSE.T1.min # 1.390827

# impute with maximum value
RMSE.T1.max <- impute(appErg.data.mis$T1, fun = max) %>% as.data.frame() %>% 
  caret::RMSE(pred = ., obs = appErg.data$T1)
RMSE.T1.max # 1.016792

# impute with random value
RMSE.T1.median <- impute(appErg.data.mis$T1, fun = 'random') %>% as.data.frame() %>% 
  caret::RMSE(pred = ., obs = appErg.data$T1)
RMSE.T1.median

#using argImpute
imputed.arg <- aregImpute(~ T1 + T2 + T3 + T4 + T6 + T6 + T7 + T8 + T9 +
                            RH_1 + RH_2 + RH_3 + RH_4 + RH_5 + RH_6 + RH_7 + RH_8 + RH_9 +
                            T_out + Press_mm_hg + RH_out + Windspeed + Visibility + Tdewpoint +
                            rv1, data = appErg.data.mis, n.impute = 5)
imputed.arg
RMSE.T1.aregI.hmisc <- caret::RMSE(pred = imputed.arg$imputed$T1 %>% as.data.frame(), 
                       obs = appErg.data$T1)
RMSE.T1.aregI.hmisc # 1.977089




# # Missing values imputation using "mi"
# appErg.data.mis <- appErg.data.Train.mis.10perc # load dataset
# #install.packages("mi")
# library(mi)
# 
# 
# #imputing missing value with mi
# appErg.data.imp <- mi(appErg.data.mis, seed = 123)
# 
# 
# # MAPE (Performance Metric)
# # Mean Absolute Percentage Error
# #rowMeans(abs((actual-predicted)/actual) * 100)
# #install.packages("MLmetrics")
# #MLmetrics::MAPE(appErg.data$T1, appErg.data$T1)  


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
                             obs = appErg.data$T1)
RMSE.T1.mtsdi # 0.07661691




#imputing missing value with "VIM"
#install.packages("VIM")
library(VIM)
# Calculate RMSE
appErg.data.mis <- appErg.data.Train.mis.10perc # load dataset

### Hot Deck Imputation using "VIM"

# run for loops in parallel
cores = detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

# Number of iterations can be changed using variable "i" in the code below
RMSE.T1.VIM.hotdeck <- foreach(i = 1:10, .combine = rbind, .packages = c("VIM", "magrittr", "caret")) %dopar% {
  
  appErg.data.imp <- VIM::hotdeck(data = appErg.data.mis)
  RMSE.T1.temp <- caret::RMSE(pred = appErg.data.imp$T1 %>% as.data.frame(), 
                              obs = appErg.data$T1)
  RMSE.T1.temp
  
}

stopCluster(cl) #stop cluster
boxplot(RMSE.T1.VIM.hotdeck)


### k-Nearest neighbour imputation using "VIM"
# run for loops in parallel
cores = detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

appErg.data.imp <- VIM::kNN(data = appErg.data.mis, k = 5, numFun = median, 
                            dist_var = colnames(appErg.data.imp)[-1])
RMSE.T1.VIM.kNN <- caret::RMSE(pred = appErg.data.imp$T1 %>% as.data.frame(), 
                                   obs = appErg.data$T1)
RMSE.T1.VIM.kNN # 0.1889839

stopCluster(cl) #stop cluster
boxplot(RMSE.T1.VIM.kNN)


# run for loops in parallel
cores = detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

appErg.data.imp <- irmi(x = appErg.data.mis, maxit = 5, mi = 1, 
                        init.method = "median", robust = T, step = T, 
                        robMethod = "MM", 
                        modelFormulas = list(T1 = colnames(appErg.data.mis[-4])))

stopCluster(cl) #stop cluster


### Imputation using "Individual Regression Imputation" in "VIM"
# run for loops in parallel
cores = detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

form.T1 <- T1 ~ T2 + T3 + T4 + T6 + T6 + T7 + T8 + T9 +
  RH_1 + RH_2 + RH_3 + RH_4 + RH_5 + RH_6 + RH_7 + RH_8 + RH_9 +
  T_out + Press_mm_hg + RH_out + Windspeed + Visibility + Tdewpoint + rv1
appErg.data.imp$T1 <- regressionImp(formula = form.T1, data = appErg.data.mis)
RMSE.T1.VIM.IRI <- caret::RMSE(pred = appErg.data.imp$T1 %>% as.data.frame(), 
                               obs = appErg.data$T1)
RMSE.T1.VIM.IRI

stopCluster(cl) #stop cluster

