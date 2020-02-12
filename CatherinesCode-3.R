train <- read.csv("C:/Users/catmc/OneDrive/Documents/IAA Docs/Machine Learning/MLProject_train.csv", stringsAsFactors = FALSE)
valid <- read.csv("C:/Users/catmc/OneDrive/Documents/IAA Docs/Machine Learning/MLProject_valid.csv", stringsAsFactors = FALSE)
test <- read.csv("C:/Users/catmc/OneDrive/Documents/IAA Docs/Machine Learning/MLProject_test.csv", stringsAsFactors = FALSE)
library(dplyr)
#install.packages('yardstick')
library(yardstick)

#sample 20% of the rows
set.seed(1234)
sub_train1 <- sample_frac(train, 0.20)
sub_train1 <- sub_train1[is.na(sub_train1$target1)==FALSE,]

#clean and impute the data (roughly)
clean_data <- function(column){
  if (sum(is.na(column)) < 0.15*length(column)) {
    if (is.numeric(column)) {
      column = ifelse(is.na(column), median(column, na.rm = TRUE), column)
    } else {
      column = ifelse(is.na(column), "missing", column)
    }
  } else {
    column = ifelse(is.na(column), 0, 1)
  }
}

for(i in seq(1, ncol(sub_train1))){
  sub_train1[,i] <- clean_data(sub_train1[,i])
}
sub_train1=sub_train1 %>% 
  mutate(target1=as.factor(target1)) %>%
  select(-target2)


#--------------random forest--------------#
#this takes a hot minute
library('randomForest')
rf = randomForest(target1 ~ ., data=sub_train1, ntree=50, mtry=15, type='class')

# We can then examine the confusion matrix to see how our model predicts each class:

rf$confusion

#The classwise error rates are extremely small for the entire model! \blue{rf\$err.rate} will show the progression of misclassification rates as each individual tree is added to the forest for each class and overall (on the out of bag (OOB) data that was remaining after the data was sampled to train the tree).

library(lattice)
#Finally we should check our random forest model on the validation model as a final test of performance and screen for overfitting.

vscores = predict(rf,valid,type='prob')

install.packages('pROC')
library(pROC)
roc_obj <- roc(valid$target1, vscores[,1])
auc(roc_obj)

vscores = predict(rf,valid,type='response')
cat('Validation Misclassification Rate:', sum(vscores!=valid$target1)/nrow(valid))
ppv_vec(as.factor(valid$target1), vscores)

#Target 1v
#21 trees, mtry 10 Valid misclassification of 0.25895
#50 trees, mtry 20 Valid misclassification of 0.25415
#50 trees, mtry 15 Valid misclassification of 0.2440667 

#---------------Ridge Regression-------------------#
#install.packages("leaps")
library(leaps)
#install.packages("glmnet")
library(glmnet)

valid <- valid %>%
  select(-target2)

total_data = rbind(sub_train1, valid)

X = model.matrix(target1~. ,data=total_data[seq(1, nrow(sub_train1)),])[,-1]
y = sub_train1$target1

set.seed(1234)
cv.out = cv.glmnet(X, y, alpha=0, family="binomial")

plot(cv.out)

bestlambda=cv.out$lambda.min
bestlambda
total_data$target1 <- as.factor(total_data$target1)
X_valid = model.matrix(target1~. ,data=total_data[seq(nrow(sub_train1)+1,nrow(total_data)),])[,-1]
y_valid = as.factor(valid$target1)

ridge.mod.betas = coef(cv.out, s=bestlambda)
pred.ridge = predict(cv.out, s=bestlambda, newx=X_valid, type='response')
pred.ridge = ifelse(pred.ridge>0.5, 1, 0)
misclassification = sum(pred.ridge!=y_valid)/nrow(valid)
misclassification
#my misclassification = 0.2275167
pred.ridge<-as.numeric(pred.ridge)
y_valid<-as.numeric(y_valid)
roc_obj <- roc(y_valid, pred.ridge)
auc(roc_obj)


misclassification = sum(pred.ridge!=y_valid)/nrow(valid)
misclassification
ppv_vec(as.factor(y_valid), as.factor(pred.ridge))

#----------------LASSO Regression-----------------#
#this takes forever btw

set.seed(1234)
cvLASSO.out=cv.glmnet(X,y,alpha=1, family="binomial")
plot(cvLASSO.out)

bestLASSOlambda=cvLASSO.out$lambda.min
lasso.mod.betas = coef(cvLASSO.out, s=bestLASSOlambda)
bestLASSOlambda
pred.lasso = predict(cvLASSO.out, s=bestLASSOlambda, newx=X_valid, type='response')
pred.lasso = ifelse(pred.lasso>0.5,1,0)
misclassification = sum(pred.lasso!=y_valid)/nrow(valid)
misclassification
#misclassification = 0.2340167

roc_obj <- roc(y_valid, pred.lasso)
auc(roc_obj)

#-----------Neural Network--------------#
#this takes forever btw

#install.packages('neuralnet')
library(neuralnet)
sub_train1$target1 <- as.factor(sub_train1$target1)

scaleRange = function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

total_norm <- as.data.frame(lapply(total_data[,seq(1,ncol(total_data)-1)], scaleRange))
total_norm$target1 <- total_data$target1
train_norm <- total_norm[seq(1,nrow(sub_train1)),]
valid_norm <- total_norm[seq(nrow(sub_train1)+1, nrow(total_data)),]

nnet1 = neuralnet(target1~., data=train_norm, hidden=1, linear.output = FALSE)
plot(nnet1)

results1 = compute(nnet1, valid_norm)
nnet1Pred=results1$net.result

# nnet1Pred = ifelse(nnet1Pred[,2]>0.5, 1, 0)
# misclassification = sum(nnet1Pred!=y_valid)/nrow(valid)
# misclassification

#one hidden layer = misclassification of 0.2447833
#two hidden layers: did not converge

roc_obj <- roc(y_valid, nnet1Pred)
auc(roc_obj)

#-----------GAM----------#
#install.packages('earth')
library(earth)
#install.packages("mgcv")
#install.packages("gam")
library(mgcv)
library(splines)
library(gam)

model = earth(target1~., data=sub_train1, glm=list(family=binomial), degree=3)
pred=predict(model, valid, type = 'response')
roc_obj <- roc(y_valid, pred)
auc(roc_obj)
ppv_vec(y_valid, as.factor(pred))

#----------Naive Bayes------------#
#bin all the variables i guess?
#this is probably a terrible way to do this :) oh well :)
binned_train <- sub_train1
for(i in seq(1, ncol(sub_train1))){
  if (is.numeric(sub_train1[,i])){
    binned_train[,i] <- ifelse(binned_train[,i]>unlist(quantile(binned_train[,i]))[1], "a", ifelse(
      binned_train[,i]>unlist(quantile(binned_train[,i]))[2], "b", ifelse(
        binned_train[,i]>unlist(quantile(binned_train[,i]))[3], "c", ifelse(
          binned_train[,i]>unlist(quantile(binned_train[,i]))[4], "d", "e"
      )
    )))
  }
}
binned_train <- binned_train %>%
  select(-target1)

#install.packages('e1071')
library(e1071)
#install.packages('gmodels')
library(gmodels)
model = naiveBayes(binned_train, sub_train1$target1)
pred = predict(model,valid, type="class")
#couldn't a way to get probabilities from this function so no AUC :(
misclassification = sum(pred!=y_valid)/nrow(valid)
misclassification
#0.3206667

#--------TEST--------------#
library(earth)
library(mgcv)
library(splines)
library(gam)

final_train <- rbind(train, valid)

#clean and impute the data (roughly)
clean_data <- function(column){
  if (sum(is.na(column)) < 0.15*length(column)) {
    if (is.numeric(column)) {
      column = ifelse(is.na(column), median(column, na.rm = TRUE), column)
    } else {
      column = ifelse(is.na(column), "missing", column)
    }
  } else {
    column = ifelse(is.na(column), 0, 1)
  }
}

for(i in seq(1, ncol(final_train))){
  final_train[,i] <- clean_data(final_train[,i])
}

final_train <- final_train %>%
  select(-target2)

final_model = earth(target1~., data=final_train, glm=list(family=binomial), degree=3)
pred=predict(final_model, test, type = 'response')
pred.class=ifelse(pred>0.5,1,0)
roc_obj <- roc(y_valid, pred)
auc(roc_obj)
ppv_vec(as.factor(test$target1), as.factor(pred.class))
