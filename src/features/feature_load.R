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

#prep for data to come together
test$ic50_Omicron <- 0
test$train <- 1
train$train <- 0

#combine data
master <- rbind(test, train)

#feature engineering
master$days_sinceDose3 <- abs(master$days_sinceDose3)
master$days_sincePosTest_latest <- abs(master$days_sincePosTest_latest)
master$days_sinceSxLatest <- abs(master$days_sinceSxLatest)
master$days_sinceDose3[is.na(master$days_sinceDose3)] <- mean(master$days_sinceDose3, na.rm = TRUE)
master$Sx_severity_most_recent[is.na(master$Sx_severity_most_recent)] <- mean(master$Sx_severity_most_recent, na.rm = TRUE)
master$days_dose23interval[is.na(master$days_dose23interval)] <- mean(master$days_dose23interval, na.rm = TRUE)
master$days_sinceSxLatest[is.na(master$days_sinceSxLatest)] <- max(master$days_sinceSxLatest, na.rm = TRUE) + 1
master$days_sincePosTest_latest[is.na(master$days_sincePosTest_latest)] <- mean(master$days_sincePosTest_latest, na.rm = TRUE)

#new column creation
master$days_sinceDose1 <- abs((master$days_sinceDose2 - master$days_dose12interval))
master$muldose3interval <- (master$days_dose23interval * master$days_sinceDose3)
master$muldose2interval <- (master$days_dose12interval * master$days_sinceDose2)
master$agemul_avginterval <- ((master$days_dose12interval + master$days_dose23interval)/2) * master$age
master$Posmul_Sxlatest <- master$days_sinceSxLatest * master$days_sincePosTest_latest
master$muldoseinterval <- (master$muldose2interval * master$muldose3interval)
master$mulSx <- master$Sx_severity_most_recent * master$days_sinceSxLatest
master$Agemuldays_sinceDoses <- master$age * (master$days_sinceDose1 + master$days_sinceDose2 + master$days_sinceDose3)
master$posmuldays_sinceDoses <- master$days_sincePosTest_latest * (master$days_sinceDose1 + master$days_sinceDose2 + master$days_sinceDose3)
master$Sxmuldays_sinceDoses <- master$mulSx * (master$days_sinceDose1 + master$days_sinceDose2 + master$days_sinceDose3)

#separate into train and test
train <- master[train == 0]
test <- master[train == 1]

train <- subset(train, select = -c(train))
test <- subset(test, select = -c(train))

fwrite(newtrain, "./Midterm_Project/volume/data/interim/train.csv")
fwrite(newtest, "./Midterm_Project/volume/data/interim/test.csv")
