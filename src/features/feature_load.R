#load libraries in
library(data.table)
library(dplyr)
library(Metrics)
library(caret)
library(xgboost)

#read in libraries
train <- fread("./XGboost/volume/data/raw/Stat_380_train.csv")
test <- fread("./XGboost/volume/data/raw/Stat_380_test.csv")
covar <- fread("./XGboost/volume/data/raw/covar_data.csv")

#prep for data to come together
train <- train[order(age, days_sinceDose3, ic50_Omicron)]
test$ic50_Omicron <- 0
test$train <- 1
train$train <- 0

#combine data
master <- rbind(train, test)
master$sort_col <- 1:nrow(master)

#utilize covar data
id <- covar$sample_id
newcovar <- subset(covar, select = -c(sample_id))
total <- data.table(rowMeans(newcovar)) #use rowMeans to get average of each row
total$sample_id <- id

#add covar data into master
master <- merge(x = master, y = total, by = 'sample_id')

#add new column
master$days_sinceDose1 <- master$days_sinceDose2 + master$days_dose12interval

master <- master[order(sort_col)]
master$sort_col <- NULL

#separate into train and test
train <- master[train == 0]
test <- master[train == 1]

#remove the columns adding them together
train <- subset(train, select = -c(train, sample_id))
test <- subset(test, select = -c(train, sample_id))

fwrite(train, "./XGboost/volume/data/interim/train.csv")
fwrite(test, "./XGboost/volume/data/interim/test.csv")
