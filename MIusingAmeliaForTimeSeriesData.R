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
library(Amelia)
library(missForest)


# Load dataset
# UCI - Appliances energy prediction Data Set 
# https://archive.ics.uci.edu/ml/datasets/Appliances+energy+prediction

url.data <- c("https://archive.ics.uci.edu/ml/machine-learning-databases/00374/energydata_complete.csv")
appErg.data <- fread(input = url.data)



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


# observation: values in column "rv1" are identical to the ones in "rv2"


# Create 10% missing data in each of the columns from "T1" to "rv2"
library(missForest)
library(magrittr)
set.seed(123)
appErg.data.mis.10perc <- prodNA(appErg.data[,-(1:3)], noNA = 0.1) %>% 
  cbind(appErg.data[,1:3], .)
#fwrite(x = appErg.data.mis.10perc, file = "appErg.data_missing_10perc.csv")
#summary(appErg.data.mis)



# Imputing missing values using "Amelia" considering data in timeseries format
appErg.data.mis <- appErg.data.mis.10perc # load dataset
# convert "date" into "POSIXct" format
appErg.data.mis$date <- as.POSIXct(x = appErg.data.mis$date)

# run for loops in parallel
cores = detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

# Number of iterations can be changed using variable "i" in the code below
amelia.fit <- amelia(x = appErg.data.mis, m = 5, idvars = c("Appliances", "lights", "rv2"),
                     ts = "date", cs = "Appliances", parallel = "snow")
complete.data <- rbindlist(list(amelia.fit$imputations[[1]],
                                amelia.fit$imputations[[2]],
                                amelia.fit$imputations[[3]],
                                amelia.fit$imputations[[4]],
                                amelia.fit$imputations[[3]]))[,lapply(.SD, mean), list(date, Appliances, lights)]
RMSE.T1.temp <- caret::RMSE(appErg.data$T1, complete.data$T1)
RMSE.T1.temp

stopCluster(cl) #stop cluster
