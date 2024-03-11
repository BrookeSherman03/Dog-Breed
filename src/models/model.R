#load libraries
library(data.table)
library(dplyr)
library(Metrics)
library(caret)
library(glmnet)

#read files back in
train <- fread("./Midterm_Project/volume/data/interim/train.csv")
test <- fread("./Midterm_Project/volume/data/interim/test.csv")

#store test sample_id column and drop both sample_id columns back out
train_y <- train$ic50_Omicron
sample_id <- test$sample_id
train <- subset(train, select = -c(sample_id))
test <- subset(test, select = -c(sample_id))

#dummyVars operation
dummies <- dummyVars(ic50_Omicron ~ ., data = train)
saveRDS(dummies, "./Midterm_Project/volume/models/dummies")
train <- predict(dummies, newdata = train)
test <- predict(dummies, newdata = test)

#save data back in datatables
train <- data.table(train)
test <- data.table(test)

#add this column, since test does not have it
test$dose_3mRNA1272 <- 0
test <- test %>% relocate(dose_3mRNA1272, .after = dose_3BNT162b2)

#cross validation model
cvfit <- cv.glmnet(data.matrix(train), train_y, alpha = 1, family = "gaussian", type.measure = 'mse')
saveRDS(cvfit, "./Midterm_Project/volume/models/cvfit")
bestlam <- cvfit$lambda.min

#create model
gl_model <- glmnet(data.matrix(train), train_y, alpha = 1, family = "gaussian")
saveRDS(gl_model, "./Midterm_Project/volume/models/cvfit")

#predict with train and test the similarity between columns
predtrain <- predict(gl_model, s = bestlam, newx = data.matrix(train))
predtrain <- abs(predtrain) #numbers should not be negative, so make any negative positive
#show train prediction mse
sqrt(mean((train_y - predtrain)^2))
#compare numbers as needed
compare <- data.table(predtrain)
compare$acttrain <- train_y

#create actual prediction
pred <- predict(gl_model, s = bestlam, newx = data.matrix(test))
pred <- abs(pred) #numbers should not be negative, so make any negative positive

#create submission file
submission <- data.table(sample_id)
submission$ic50_Omicron <- pred

#write submission csv
fwrite(submission, "./Midterm_Project/volume/data/processed/submission.csv")
