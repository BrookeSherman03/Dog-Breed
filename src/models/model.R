#load libraries
library(data.table)
library(dplyr)
library(Metrics)
library(caret)
library(xgboost)

#read files back in
train <- fread("./XGboost/volume/data/interim/train.csv")
test <- fread("./XGboost/volume/data/interim/test.csv")
sub <- fread("./XGboost/volume/data/raw/Example_sub.csv")

#save the train variable
train_y <- train$ic50_Omicron

#run dummyvars
dummies <- dummyVars(ic50_Omicron ~ ., data = train)
train <- predict(dummies, newdata = train)
test <- predict(dummies, newdata = test)

#turn them back into data tables
train <- data.table(train)
test <- data.table(test)

test$dose_3mRNA1272 <- 0
test <- test %>% relocate(dose_3mRNA1272, .after = dose_3BNT162b2)

#prep for cross validation and model fitting
dtrain <- xgb.DMatrix(data.matrix(train), label = train_y, missing = NA)
dtest <- xgb.DMatrix(data.matrix(test), missing = NA)

folds <- lapply(1:5, function(fold) {
  c(((fold - 1) * 10000 + 1):(fold * 10000))
})

hyper_perm_tune <- NULL

#cross validation
param <- list(  objective           = "reg:linear",
                gamma               = 0.00,
                booster             = "gbtree",
                eval_metric         = "rmse",
                eta                 = 0.1,
                max_depth           = 15,
                min_child_weight    = 5.0,
                subsample           = 1.0,
                colsample_bytree    = 1.0,
                tree_method = 'hist'
)

XGBm <- xgb.cv(params = param, folds = folds, nrounds = 10, missing = NA, data = dtrain, print_every_n = 1, early_stopping_rounds = 25)

#keep best iteration
best_ntrees <- unclass(XGBm)$best_iteration

new_row <- data.table(t(param))

new_row$best_ntrees <- best_ntrees

test_error <- unclass(XGBm)$evaluation_log[best_ntrees,]$test_rmse_mean
new_row$test_error <- test_error
hyper_perm_tune <- rbind(new_row, hyper_perm_tune)

#now create and fit model
watchlist <- list(train = dtrain)
XGBm <- xgb.train(params = param, nrounds = best_ntrees, missing = NA, data = dtrain, watchlist = watchlist, print_every_n = 1)

temp_pred <- predict(XGBm, newdata = dtrain)
rmse(train_y, temp_pred)

pred <- predict(XGBm, newdata = dtest)
sub$ic50_Omicron <- pred
fwrite(sub, ".XGboost/volume/data/processed/sub.csv")
