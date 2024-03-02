#load libraries
library(data.table)
library(dplyr)
library(Metrics)
library(caret)
library(glmnet)

#load data
train <- fread("./Midterm_Project/volume/data/raw/Stat_380_train.csv")
test <- fread("./Midterm_Project/volume/data/raw/Stat_380_test.csv")
covar <- fread("./Midterm_Project/volume/data/raw/covar_data.csv")

#push data together to include covar
id <- covar$sample_id
newcovar <- subset(covar, select = -c(sample_id))
total <- data.table(rowMeans(newcovar)) #use rowMeans to get average of each row
total$sample_id <- id

#merge data
newtrain <- merge(x = train, y = total, by = 'sample_id')
newtest <- merge(x = test, y = total, by = 'sample_id')

#keep ic50_omicron columns and testid column
train_y <- newtrain$ic50_Omicron
newtest$ic50_Omicron <- 0
sample_id <- newtest$sample_id
sample_id1 <- newtrain$sample_id

#prep for dummyVars
newtrain <- subset(newtrain, select = -c(sample_id))
newtest <- subset(newtest, select = -c(sample_id))

#fill all NA values in test and training tables
newtrain[is.na(newtrain)] <- 0
newtest[is.na(newtest)] <- 0

#readd sample_id
newtest$sample_id <- sample_id
newtrain$sample_id <- sample_id1

fwrite(newtrain, "./Midterm_Project/volume/data/interim/newtrain.csv")
fwrite(newtest, "./Midterm_Project/volume/data/interim/newtest.csv")
