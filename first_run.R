
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
library(chron) # To check if a day is weekend or not

# Load dataset
# Appliances energy prediction Data Set 
# https://archive.ics.uci.edu/ml/datasets/Appliances+energy+prediction

apperg_data <- fread(input = "D:/DA/PGDBA/cummins_internship/project/datasets/energydata_complete.csv")

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

hist(apperg_data$T1, breaks = 20, prob = TRUE, col = "grey") 
lines(density(apperg_data$T1), col = "blue", lwd = 2)
lines(density(apperg_data$T1, adjust = 2), lty = "dotted", col = "red", lwd = 2)
skewness(apperg_data$T1)


hist(apperg_data$RH_1, breaks = 20, prob = TRUE, col = "grey") 
lines(density(apperg_data$RH_1), col = "blue", lwd = 2)
lines(density(apperg_data$RH_1, adjust = 2), lty = "dotted", col = "red", lwd = 2)
skewness(apperg_data$RH_1)


hist(apperg_data$T2, breaks = 20, prob = TRUE, col = "grey") 
lines(density(apperg_data$T2), col = "blue", lwd = 2)
lines(density(apperg_data$T2, adjust = 2), lty = "dotted", col = "red", lwd = 2)
skewness(apperg_data$T2)


hist(apperg_data$RH_2, breaks = 20, prob = TRUE, col = "grey") 
lines(density(apperg_data$RH_2), col = "blue", lwd = 2)
lines(density(apperg_data$RH_2, adjust = 2), lty = "dotted", col = "red", lwd = 2)
skewness(apperg_data$RH_2)


hist(apperg_data$T3, breaks = 20, prob = TRUE, col = "grey") 
lines(density(apperg_data$T3), col = "blue", lwd = 2)
lines(density(apperg_data$T3, adjust = 2), lty = "dotted", col = "red", lwd = 2)
skewness(apperg_data$T3)


hist(apperg_data$RH_3, breaks = 20, prob = TRUE, col = "grey") 
lines(density(apperg_data$RH_3), col = "blue", lwd = 2)
lines(density(apperg_data$RH_3, adjust = 2), lty = "dotted", col = "red", lwd = 2)
skewness(apperg_data$RH_3)


hist(apperg_data$T4, breaks = 20, prob = TRUE, col = "grey") 
lines(density(apperg_data$T4), col = "blue", lwd = 2)
lines(density(apperg_data$T4, adjust = 2), lty = "dotted", col = "red", lwd = 2)
skewness(apperg_data$T4)


hist(apperg_data$RH_4, breaks = 20, prob = TRUE, col = "grey") 
lines(density(apperg_data$RH_4), col = "blue", lwd = 2)
lines(density(apperg_data$RH_4, adjust = 2), lty = "dotted", col = "red", lwd = 2)
skewness(apperg_data$RH_4)


hist(apperg_data$T5, breaks = 20, prob = TRUE, col = "grey") 
lines(density(apperg_data$T5), col = "blue", lwd = 2)
lines(density(apperg_data$T5, adjust = 2), lty = "dotted", col = "red", lwd = 2)
skewness(apperg_data$T5)


hist(apperg_data$RH_5, breaks = 20, prob = TRUE, col = "grey") 
lines(density(apperg_data$RH_5), col = "blue", lwd = 2)
lines(density(apperg_data$RH_5, adjust = 2), lty = "dotted", col = "red", lwd = 2)
skewness(apperg_data$RH_5)


hist(apperg_data$T6, breaks = 20, prob = TRUE, col = "grey") 
lines(density(apperg_data$T6), col = "blue", lwd = 2)
lines(density(apperg_data$T6, adjust = 2), lty = "dotted", col = "red", lwd = 2)
skewness(apperg_data$T6)


hist(apperg_data$RH_6, breaks = 20, prob = TRUE, col = "grey") 
lines(density(apperg_data$RH_6), col = "blue", lwd = 2)
lines(density(apperg_data$RH_6, adjust = 2), lty = "dotted", col = "red", lwd = 2)
skewness(apperg_data$RH_6)


hist(apperg_data$T7, breaks = 20, prob = TRUE, col = "grey") 
lines(density(apperg_data$T7), col = "blue", lwd = 2)
lines(density(apperg_data$T7, adjust = 2), lty = "dotted", col = "red", lwd = 2)
skewness(apperg_data$T7)


hist(apperg_data$RH_7, breaks = 20, prob = TRUE, col = "grey") 
lines(density(apperg_data$RH_7), col = "blue", lwd = 2)
lines(density(apperg_data$RH_7, adjust = 2), lty = "dotted", col = "red", lwd = 2)
skewness(apperg_data$RH_7)


hist(apperg_data$T8, breaks = 20, prob = TRUE, col = "grey") 
lines(density(apperg_data$T8), col = "blue", lwd = 2)
lines(density(apperg_data$T8, adjust = 2), lty = "dotted", col = "red", lwd = 2)
skewness(apperg_data$T8)


hist(apperg_data$RH_8, breaks = 20, prob = TRUE, col = "grey") 
lines(density(apperg_data$RH_8), col = "blue", lwd = 2)
lines(density(apperg_data$RH_8, adjust = 2), lty = "dotted", col = "red", lwd = 2)
skewness(apperg_data$RH_8)


hist(apperg_data$T9, breaks = 20, prob = TRUE, col = "grey") 
lines(density(apperg_data$T9), col = "blue", lwd = 2)
lines(density(apperg_data$T9, adjust = 2), lty = "dotted", col = "red", lwd = 2)
skewness(apperg_data$T9)


hist(apperg_data$RH_9, breaks = 20, prob = TRUE, col = "grey") 
lines(density(apperg_data$RH_9), col = "blue", lwd = 2)
lines(density(apperg_data$RH_9, adjust = 2), lty = "dotted", col = "red", lwd = 2)
skewness(apperg_data$RH_9)


hist(apperg_data$T_out, breaks = 20, prob = TRUE, col = "grey") 
lines(density(apperg_data$T_out), col = "blue", lwd = 2)
lines(density(apperg_data$T_out, adjust = 2), lty = "dotted", col = "red", lwd = 2)
skewness(apperg_data$T_out)


hist(apperg_data$Press_mm_hg, breaks = 20, prob = TRUE, col = "grey") 
lines(density(apperg_data$Press_mm_hg), col = "blue", lwd = 2)
lines(density(apperg_data$Press_mm_hg, adjust = 2), lty = "dotted", col = "red", lwd = 2)
skewness(apperg_data$Press_mm_hg)


hist(apperg_data$RH_out, breaks = 20, prob = TRUE, col = "grey") 
lines(density(apperg_data$RH_out), col = "blue", lwd = 2)
lines(density(apperg_data$RH_out, adjust = 2), lty = "dotted", col = "red", lwd = 2)
skewness(apperg_data$RH_out)


hist(apperg_data$Windspeed, breaks = 20, prob = TRUE, col = "grey") 
lines(density(apperg_data$Windspeed), col = "blue", lwd = 2)
lines(density(apperg_data$Windspeed, adjust = 2), lty = "dotted", col = "red", lwd = 2)
skewness(apperg_data$Windspeed)


hist(apperg_data$Visibility, breaks = 20, prob = TRUE, col = "grey") 
lines(density(apperg_data$Visibility), col = "blue", lwd = 2)
lines(density(apperg_data$Visibility, adjust = 2), lty = "dotted", col = "red", lwd = 2)
skewness(apperg_data$Visibility)


hist(apperg_data$Tdewpoint, breaks = 20, prob = TRUE, col = "grey") 
lines(density(apperg_data$Tdewpoint), col = "blue", lwd = 2)
lines(density(apperg_data$Tdewpoint, adjust = 2), lty = "dotted", col = "red", lwd = 2)
skewness(apperg_data$Tdewpoint)


# Create 10% missing data in each of the columns from "T1" to "rv2"
library(missForest)
apperg_data.mis <- prodNA(apperg_data[,-(1:3)], noNA = 0.1) %>% 
  cbind(apperg_data[,1:3], .)
summary(apperg_data.mis)


#install.packages("mice")
library(mice)

# Visualise missing values using "mice"
md.pattern(apperg_data.mis)

# Visualize missing values using "VIM"
#install.packages("VIM")
library(VIM)
vim_plot <- aggr(apperg_data.mis, col = c('navyblue','yellow'),
                    numbers = TRUE, sortVars = TRUE,
                    labels = names(apperg_data.mis), cex.axis=.7,
                    gap=3, ylab=c("Missing data","Pattern"))


# Use "mice" to impute missing values 
imputed_Data <- mice(data = apperg_data.mis, m = 5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Data)
class(imputed_Data)

# check imputed values
imputed_Data$imp$T1

#get complete data (2nd out of 5)
completeData <- complete(imputed_Data, 2)


