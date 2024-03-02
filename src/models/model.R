#read files back in


#keep sample_id columns and drop them back out

#dummyVars operation
dummies <- dummyVars(ic50_Omicron ~ ., data = newtrain)
train <- predict(dummies, newdata = newtrain)
test <- predict(dummies, newdata = newtest)

#save data back in datatables
train <- data.table(train)
test <- data.table(test)

#only train has this column, so add to test data for accuracy with the value needed
train <- subset(train, select = -c(dose_3mRNA1272))

#create crossvalidation model w/ lasso poisson regression
cvfit <- cv.glmnet(data.matrix(train), train_y, alpha = 1, family = "poisson", type.measure = 'mse')

#print out cv model to see values
print(cvfit)
predict(cvfit,s = cvfit$lambda.min, newx = test,type = "coefficients")

#save min
bestlam <- cvfit$lambda.min

#create logistic  w/ lasso poisson regression
gl_model1 <- glmnet(data.matrix(train), train_y, alpha = 1, family = "poisson")

#create prediction and print to see if the values look valid
pred <- predict(gl_model1,s = bestlam, newx = data.matrix(test), type = "response")
print(pred)

#create submission file
test$ic50_Omicron <- pred
submission <- data.table(idtest)
submission <- submission %>% rename_at('idtest', ~'sample_id')
submission$ic50_Omicron <- test$ic50_Omicron

#write submission csv
fwrite(submission, "./sub1.csv")
