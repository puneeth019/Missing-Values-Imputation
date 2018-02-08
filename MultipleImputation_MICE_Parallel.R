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


# Load dataset
# UCI - Appliances energy prediction Data Set 
# https://archive.ics.uci.edu/ml/datasets/Appliances+energy+prediction

appErg.data <- fread(input = "energydata_complete.csv")

# Summary of data
str(appErg.data)

#Look for missing data
sum(is.na(appErg.data)) # None found

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


# Missing values imputation using "mice"
#install.packages("mice")
library(mice)
library(parallel) # Load pacakge "parallel" to run mice in parallel
appErg.data.mis <- appErg.data.Train.mis.10perc # load dataset



# run for loops in parallel
cores = detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

pack.list <- c("mice", "data.table", "MLmetrics", "caret")
#4:(ncol(appErg.data.mis)-1)
PM.mice <- foreach(i = 12:12, .combine = cbind, .packages = c("foreach")) %dopar% {
  
  PM.variable <- foreach(j = 1:30, .combine = rbind, .packages = pack.list) %dopar% {
    
    imputed.data <- mice(data = appErg.data.mis, m = 5, maxit = 5, method = 'pmm')
    complete.data <- rbindlist(list(complete(imputed.data, 1), 
                                    complete(imputed.data, 2), 
                                    complete(imputed.data, 3), 
                                    complete(imputed.data, 4), 
                                    complete(imputed.data, 5)))[,lapply(.SD, mean), list(date)]
    RMSE.temp <- caret::RMSE(complete.data[[i]], appErg.data.Train[[i]])
    MAPE.temp <- MLmetrics::MAPE(complete.data[[i]], appErg.data.Train[[i]])
    cbind(RMSE.temp, MAPE.temp)
    
  }
  PM.variable
  
}

stopCluster(cl) #stop cluster
PM.mice
fwrite(x = PM.mice %>% as.data.frame(), file = "PM_mice_parallel.csv")
gc()

# wrapper function for mice
parlMICE <- function(data, n.core = detectCores() - 1, n.imp.core = 2,  
                     seed = NULL, m = NULL, ...){
  suppressMessages(require(parallel))
  cl <- makeCluster(n.core, ...)
  clusterExport(cl, varlist = "data", envir = environment())
  clusterEvalQ(cl, library(mice))
  if (!is.null(seed)) {
    clusterSetRNGStream(cl, seed)
  }
  if (!is.null(m)) {
    n.imp.core <- ceiling(m / n.core)
  }
  imps <- parLapply(cl = cl, X = 1:n.core, fun = function(i){
    mice(data, print = FALSE, m = n.imp.core, ...)
  })
  stopCluster(cl)
  imp <- imps[[1]]
  if (length(imps) > 1) {
    for (i in 2:length(imps)) {
      imp <- ibind(imp, imps[[i]])
    }
  }
  return(imp)
}



# run for loops in parallel
cores = detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)


temp <- parlMICE(data = appErg.data.mis, n.core = 7, n.imp.core = 1, method = 'pmm')

aa <- temp$call[[2]]
stopCluster(cl) #stop cluster
PM.mice
fwrite(x = PM.mice %>% as.data.frame(), file = "PM_mice_parallel.csv")
gc()




