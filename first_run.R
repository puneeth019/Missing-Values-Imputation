#setwd(dir = "D:/DA/PGDBA/cummins_internship/project/datasets")

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
library(imputeTS)
library(zoo)
library(forecast)
library(spacetime)
library(timeSeries)
library(xts)




# Load dataset
# UCI - Appliances energy prediction Data Set 
# https://archive.ics.uci.edu/ml/datasets/Appliances+energy+prediction

url <- c("https://archive.ics.uci.edu/ml/machine-learning-databases/00374/energydata_complete.csv")
apperg_data <- fread(input = url)

# Exploratory Data Analysis (EDA)

# Summary of data
str(apperg_data)

#Look for missing data
sum(is.na(apperg_data)) # None found

# convert classes to "numeric" wherever required
apperg_data$Appliances <- as.numeric(apperg_data$Appliances)
apperg_data$lights <- as.numeric(apperg_data$lights)
apperg_data$T1 <- as.numeric(apperg_data$T1)
apperg_data$RH_1 <- as.numeric(apperg_data$RH_1)
apperg_data$T2 <- as.numeric(apperg_data$T2)
apperg_data$RH_2 <- as.numeric(apperg_data$RH_2)
apperg_data$T3 <- as.numeric(apperg_data$T3)
apperg_data$RH_3 <- as.numeric(apperg_data$RH_3)
apperg_data$T4 <- as.numeric(apperg_data$T4)
apperg_data$RH_4 <- as.numeric(apperg_data$RH_4)
apperg_data$T5 <- as.numeric(apperg_data$T5)
apperg_data$RH_5 <- as.numeric(apperg_data$RH_5)
apperg_data$T6 <- as.numeric(apperg_data$T6)
apperg_data$RH_6 <- as.numeric(apperg_data$RH_6)
apperg_data$T7 <- as.numeric(apperg_data$T7)
apperg_data$RH_7 <- as.numeric(apperg_data$RH_7)
apperg_data$T8 <- as.numeric(apperg_data$T8)
apperg_data$RH_8 <- as.numeric(apperg_data$RH_8)
apperg_data$T9 <- as.numeric(apperg_data$T9)
apperg_data$RH_9 <- as.numeric(apperg_data$RH_9)

apperg_data$T_out <- as.numeric(apperg_data$T_out)
apperg_data$RH_out <- as.numeric(apperg_data$RH_out)
apperg_data$Press_mm_hg <- as.numeric(apperg_data$Press_mm_hg)

apperg_data$Windspeed <- as.numeric(apperg_data$Windspeed)
apperg_data$Visibility <- as.numeric(apperg_data$Visibility)
apperg_data$Tdewpoint <- as.numeric(apperg_data$Tdewpoint)

apperg_data$rv1 <- as.numeric(apperg_data$rv1)
apperg_data$rv2 <- as.numeric(apperg_data$rv2)


# Summary of dataset
summary(apperg_data)


# Check distribution of attributes

# hist(apperg_data$T1, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(apperg_data$T1), col = "blue", lwd = 2)
# lines(density(apperg_data$T1, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(apperg_data$T1)
# 
# 
# hist(apperg_data$RH_1, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(apperg_data$RH_1), col = "blue", lwd = 2)
# lines(density(apperg_data$RH_1, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(apperg_data$RH_1)
# 
# 
# hist(apperg_data$T2, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(apperg_data$T2), col = "blue", lwd = 2)
# lines(density(apperg_data$T2, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(apperg_data$T2)
# 
# 
# hist(apperg_data$RH_2, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(apperg_data$RH_2), col = "blue", lwd = 2)
# lines(density(apperg_data$RH_2, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(apperg_data$RH_2)
# 
# 
# hist(apperg_data$T3, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(apperg_data$T3), col = "blue", lwd = 2)
# lines(density(apperg_data$T3, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(apperg_data$T3)
# 
# 
# hist(apperg_data$RH_3, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(apperg_data$RH_3), col = "blue", lwd = 2)
# lines(density(apperg_data$RH_3, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(apperg_data$RH_3)
# 
# 
# hist(apperg_data$T4, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(apperg_data$T4), col = "blue", lwd = 2)
# lines(density(apperg_data$T4, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(apperg_data$T4)
# 
# 
# hist(apperg_data$RH_4, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(apperg_data$RH_4), col = "blue", lwd = 2)
# lines(density(apperg_data$RH_4, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(apperg_data$RH_4)
# 
# 
# hist(apperg_data$T5, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(apperg_data$T5), col = "blue", lwd = 2)
# lines(density(apperg_data$T5, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(apperg_data$T5)
# 
# 
# hist(apperg_data$RH_5, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(apperg_data$RH_5), col = "blue", lwd = 2)
# lines(density(apperg_data$RH_5, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(apperg_data$RH_5)
# 
# 
# hist(apperg_data$T6, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(apperg_data$T6), col = "blue", lwd = 2)
# lines(density(apperg_data$T6, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(apperg_data$T6)
# 
# 
# hist(apperg_data$RH_6, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(apperg_data$RH_6), col = "blue", lwd = 2)
# lines(density(apperg_data$RH_6, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(apperg_data$RH_6)
# 
# 
# hist(apperg_data$T7, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(apperg_data$T7), col = "blue", lwd = 2)
# lines(density(apperg_data$T7, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(apperg_data$T7)
# 
# 
# hist(apperg_data$RH_7, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(apperg_data$RH_7), col = "blue", lwd = 2)
# lines(density(apperg_data$RH_7, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(apperg_data$RH_7)
# 
# 
# hist(apperg_data$T8, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(apperg_data$T8), col = "blue", lwd = 2)
# lines(density(apperg_data$T8, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(apperg_data$T8)
# 
# 
# hist(apperg_data$RH_8, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(apperg_data$RH_8), col = "blue", lwd = 2)
# lines(density(apperg_data$RH_8, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(apperg_data$RH_8)
# 
# 
# hist(apperg_data$T9, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(apperg_data$T9), col = "blue", lwd = 2)
# lines(density(apperg_data$T9, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(apperg_data$T9)
# 
# 
# hist(apperg_data$RH_9, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(apperg_data$RH_9), col = "blue", lwd = 2)
# lines(density(apperg_data$RH_9, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(apperg_data$RH_9)
# 
# 
# hist(apperg_data$T_out, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(apperg_data$T_out), col = "blue", lwd = 2)
# lines(density(apperg_data$T_out, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(apperg_data$T_out)
# 
# 
# hist(apperg_data$Press_mm_hg, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(apperg_data$Press_mm_hg), col = "blue", lwd = 2)
# lines(density(apperg_data$Press_mm_hg, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(apperg_data$Press_mm_hg)
# 
# 
# hist(apperg_data$RH_out, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(apperg_data$RH_out), col = "blue", lwd = 2)
# lines(density(apperg_data$RH_out, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(apperg_data$RH_out)
# 
# 
# hist(apperg_data$Windspeed, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(apperg_data$Windspeed), col = "blue", lwd = 2)
# lines(density(apperg_data$Windspeed, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(apperg_data$Windspeed)
# 
# 
# hist(apperg_data$Visibility, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(apperg_data$Visibility), col = "blue", lwd = 2)
# lines(density(apperg_data$Visibility, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(apperg_data$Visibility)
# 
# 
# hist(apperg_data$Tdewpoint, breaks = 20, prob = TRUE, col = "grey") 
# lines(density(apperg_data$Tdewpoint), col = "blue", lwd = 2)
# lines(density(apperg_data$Tdewpoint, adjust = 2), lty = "dotted", col = "red", lwd = 2)
# skewness(apperg_data$Tdewpoint)

# observation: values in column "rv1" are identical to the ones in "rv2"


# Create 10% missing data in each of the columns from "T1" to "rv2"
library(missForest)
library(magrittr)
set.seed(123)
apperg_data.mis_10perc <- prodNA(apperg_data[,-(1:3)], noNA = 0.1) %>% 
  cbind(apperg_data[,1:3], .)
#fwrite(x = apperg_data.mis_10perc, file = "apperg_data_missing_10perc.csv")
#summary(apperg_data.mis)


# Create 20% missing data in each of the columns from "T1" to "rv2"
apperg_data.mis_20perc <- prodNA(apperg_data[,-(1:3)], noNA = 0.2) %>% 
  cbind(apperg_data[,1:3], .)
#fwrite(x = apperg_data.mis_20perc, file = "apperg_data_missing_20perc.csv")


# Create 30% missing data in each of the columns from "T1" to "rv2"
apperg_data.mis_30perc <- prodNA(apperg_data[,-(1:3)], noNA = 0.3) %>% 
  cbind(apperg_data[,1:3], .) 
#fwrite(x = apperg_data.mis_30perc, file = "apperg_data_missing_30perc.csv")



# Visualize missing values using "VIM"
#install.packages("VIM")
#library(VIM)
#vim_plot <- aggr(apperg_data.mis, col = c('navyblue','yellow'),
#                    numbers = TRUE, sortVars = TRUE,
#                    labels = names(apperg_data.mis), cex.axis = .7,
#                    gap = 3, ylab = c("Missing data","Pattern"))

# Visualise missing values using "mice"
#install.packages("mice")
#library(mice)
#md.pattern(apperg_data.mis)


# Use "mice" to impute missing values
#apperg_data.mis <- apperg_data.mis_10perc
#imputed_Data <- mice(data = apperg_data.mis, m = 1, maxit = 1, method = 'pmm', 
#                     seed = 123, diagnostics = T)
#summary(imputed_Data)

# check imputed values
#imputed_Data$imp$T1

#get complete data
#complete_Data_1 <- complete(imputed_Data, 1)
#complete_Data_2 <- complete(imputed_Data, 2)
#complete_Data_3 <- complete(imputed_Data, 3)
#complete_Data_4 <- complete(imputed_Data, 4)
#complete_Data_5 <- complete(imputed_Data, 5)

# write complete data into files, if required
#fwrite(x = complete_Data_1, file = "complete_Data_1.csv")
#fwrite(x = complete_Data_2, file = "complete_Data_2.csv")
#fwrite(x = complete_Data_3, file = "complete_Data_3.csv")
#fwrite(x = complete_Data_4, file = "complete_Data_4.csv")
#fwrite(x = complete_Data_5, file = "complete_Data_5.csv")


# summary of completed datasets
#summary(complete_Data_1)
#summary(complete_Data_2)
#summary(complete_Data_3)
#summary(complete_Data_4)
#summary(complete_Data_5)


# calculate average of these five "Complete" datasets
#complete_Data <- rbindlist(list(complete_Data_1, complete_Data_2, complete_Data_3, complete_Data_4, complete_Data_5))[,lapply(.SD, mean), list(date, Appliances, lights)]

# write data, if required
#fwrite(x = complete_Data, file = "complete_Data.csv")

# Calculate RMSE(performance metric) using "mice"
# load dataset
apperg_data.mis <- apperg_data.mis_20perc
# run for loops in parallel
cores = detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

RMSE_mice <- foreach(i = 1:50, .combine = rbind, .packages = c("mice", "data.table", "caret")) %dopar% {
  
  imputed_Data <- mice(data = apperg_data.mis, m = 5, maxit = 1, method = 'pmm')
  complete_Data <- rbindlist(list(complete(imputed_Data, 1), 
                                  complete(imputed_Data, 2), 
                                  complete(imputed_Data, 3), 
                                  complete(imputed_Data, 4), 
                                  complete(imputed_Data, 5)))[,lapply(.SD, mean), list(date, Appliances, lights)]
  temp_RMSE <- caret::RMSE(apperg_data$T1,complete_Data$T1)
  temp_RMSE
  
}

stopCluster(cl) #stop cluster
#boxplot(RMSE_mice)


# Missing values imputation using "Amelia"
#install.packages("Amelia")

# Calculate RMSE
# run for loops in parallel
cores = detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

RMSE_amelia <- foreach(i = 1:50, .combine = rbind, .packages = c("Amelia", "data.table", "caret")) %dopar% {
  
  amelia_fit <- amelia(apperg_data.mis, m = 5, parallel = "multicore", 
                       idvars = c("date", "Appliances", "lights", "rv2"))
  complete_Data <- rbindlist(list(amelia_fit$imputations[[1]], 
                                  amelia_fit$imputations[[2]], 
                                  amelia_fit$imputations[[3]], 
                                  amelia_fit$imputations[[4]], 
                                  amelia_fit$imputations[[3]]))[,lapply(.SD, mean), list(date, Appliances, lights)]
  temp_RMSE <- caret::RMSE(apperg_data$T1,complete_Data$T1)
  temp_RMSE
  
}

stopCluster(cl) #stop cluster
#boxplot(RMSE_amelia)

# export the outputs to csv files, if required
#write.amelia(amelia_fit, file.stem = "amelia_imputed_data_set")





RMSE_all <- cbind(RMSE_amelia, RMSE_mice) %>% `colnames<-`(c("amelia", "mice"))
boxplot.matrix(x = RMSE_all, use.cols = T, main = "RMSE for Appliances data and 20% missing data", 
               ylab = "RMSE") # boxplot
# convert "RMSE_all" from matrix to data.frame and save it
as.data.frame(RMSE_all) %>% fwrite(file = "RMSE_all_20perc.csv")




# Missing values imputation using "missForest"
#install.packages("missForest")
library(missForest)
# Calculate RMSE
# run for loops in parallel
cores = detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

apperg_data.imp <- missForest(xmis = apperg_data.mis[,-(1:3)])

#check imputed values
apperg_data.imp$ximp

#check imputation error
apperg_data.imp$OOBerror

#comparing actual data accuracy
apperg_data.err <- mixError(apperg_data.imp$ximp, apperg_data.mis, apperg_data)
apperg_data.err




# Missing values imputation using "Hmisc"
#install.packages("Hmisc")
library(Hmisc)

# load dataset
apperg_data.mis <- apperg_data.mis_10perc

# impute with mean value
imputed_T1_mean <- impute(apperg_data.mis$T1, fun = mean)
RMSE_mean <- caret::RMSE(pred = imputed_T1_mean, obs = apperg_data$T1)
RMSE_mean # 0.4980051

# impute with median value
imputed_T1_median <- impute(apperg_data.mis$T1, fun = median)
RMSE_median <- caret::RMSE(pred = imputed_T1_median, obs = apperg_data$T1)
RMSE_median # 0.4986006

# impute with minimum value
imputed_T1_min <- impute(apperg_data.mis$T1, fun = min)
RMSE_min <- caret::RMSE(pred = imputed_T1_min, obs = apperg_data$T1)
RMSE_min # 1.63321

# impute with maximum value
imputed_T1_max <- impute(apperg_data.mis$T1, fun = max)
RMSE_max <- caret::RMSE(pred = imputed_T1_max, obs = apperg_data$T1)
RMSE_max # 1.541201

# impute with random value
imputed_T1_random <- impute(apperg_data.mis$T1, fun = 'random')
RMSE_median <- caret::RMSE(pred = imputed_T1_random, obs = apperg_data$T1)
RMSE_median

#using argImpute
imputed_arg <- aregImpute(~ T1 + T2 + T3 + T4 + T6 + T6 + T7 + T8 + T9 +
                            RH_1 + RH_2 + RH_3 + RH_4 + RH_5 + RH_6 + RH_7 + RH_8 + RH_9 +
                            T_out + Press_mm_hg + RH_out + Windspeed + Visibility + Tdewpoint +
                            rv1, data = apperg_data.mis, n.impute = 5)
imputed_arg

imputed_arg$imputed$T1 <- impute_arg$imputed$T1 %>% as.data.frame()
RMSE <- caret::RMSE(pred = imputed_arg$imputed$T1, obs = apperg_data$T1)
RMSE # 2.279437



# Missing values imputation using "Hmisc"
#install.packages("mi")
library(mi)

#imputing missing value with mi
apperg_data.imp <- mi(apperg_data.mis[,-(1:3)], seed = 123)
