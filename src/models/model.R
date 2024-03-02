#load libraries
library(data.table)
library(dplyr)
library(Metrics)
library(caret)
library(glmnet)

#read files back in
newtrain <- fread("./Midterm_Project/volume/data/interim/newtrain.csv")
newtest <- fread("./Midterm_Project/volume/data/interim/newtest.csv")

#store test sample_id column and drop both sample_id columns back out
idtest <- newtest$sample_id
newtrain <- subset(newtrain, select = -c(sample_id))
newtest <- subset(newtest, select = -c(sample_id))

#dummyVars operation
dummies <- dummyVars(ic50_Omicron ~ ., data = newtrain)
saveRDS(dummies, "./Midterm_Project/volume/models/dummies")
train <- predict(dummies, newdata = newtrain)
test <- predict(dummies, newdata = newtest)

#save data back in datatables
train <- data.table(train)
test <- data.table(test)

#only train has this column, so add to test data for accuracy with the value needed
train <- subset(train, select = -c(dose_3mRNA1272))

#create crossvalidation model w/ lasso poisson regression
cvfit <- cv.glmnet(data.matrix(train), train_y, alpha = 1, family = "poisson", type.measure = 'mse')
saveRDS(cvfit, "./Midterm_Project/volume/models/cvfit")

#print out cv model to see values
print(cvfit)
predict(cvfit,s = cvfit$lambda.min, newx = test,type = "coefficients")

#save min
bestlam <- cvfit$lambda.min

#create logistic  w/ lasso poisson regression
gl_model1 <- glmnet(data.matrix(train), train_y, alpha = 1, family = "poisson")
saveRDS(gl_model1, "./Midterm_Project/volume/models/gl_model1")

#create prediction and print to see if the values look valid
pred <- predict(gl_model1,s = bestlam, newx = data.matrix(test), type = "response")
print(pred)

#create submission file
test$ic50_Omicron <- pred
submission <- data.table(idtest)
submission <- submission %>% rename_at('idtest', ~'sample_id')
submission$ic50_Omicron <- test$ic50_Omicron

#write submission csv
fwrite(submission, "./Midterm_Project/volume/data/processed/sub1.csv")
