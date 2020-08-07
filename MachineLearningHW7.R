library(MASS)
library(rpart)
library(caret)
library(tidyverse)
library(data.table)
install.packages("GGally")
library(GGally)
install.packages("corrplot")
library(corrplot)
install.packages("verification")
library(verification)
install.packages("ROCR")
library(ROCR)
install.packages("maptree")
library(maptree)
install.packages("glmnet")
library(glmnet)
install.packages("gridExtra")
library(gridExtra)
install.packages("randomForest")
library(randomForest)
install.packages("mgcv")
library(mgcv)
install.packages("nnet")
library(nnet)
install.packages("pROC")
library(pROC)
install.packages("gbm")
library(gbm)
install.packages("e1071")
library(e1071)
install.packages("xgboost")
library(xgboost)
install.packages("DT")
library(DT)
install.packages("NeuralNetTools")
library(NeuralNetTools)
install.packages("rpart.plot")
library(rpart.plot)
library(tree)

#setwd("~/Desktop/hw07_files(3)")

callCenter <- read.csv("training_data.csv", header = TRUE)

call_labels <- as.factor(read.csv("training_labels.csv", header = FALSE)[,1])
call_labels_nofactor <- (read.csv("training_labels.csv", header = FALSE)[,1])


Call_Answer <- cbind(callCenter,call_labels_nofactor)

names(Call_Answer)[143]<-'Labels'
names(Call_Answer)

bankCall<- Call_Answer
colSums(is.na(bankCall))
str(bankCall)


summary(bankCall)

set.seed(421)
indexes<-sample(nrow(bankCall),0.8*nrow(bankCall),replace = F)
train<-bankCall[indexes,]

train_nolabel<- train[,1:142]
train_labels<- as.vector(train[,143])

test<-bankCall[-indexes,]

test_nolabel<- test[,1:142]
test_labels<- test[,143]

lasso_test = callCenter[1:30000,]
lasso_label= call_labels[1:30000]


summary(train_nolabel)

#check some of the covariances

cor.test(train_nolabel$F01,train_nolabel$F03_A)
cor.test(train_nolabel$F01,train_nolabel$F03_B)
cor.test(train_nolabel$F01,train_nolabel$F03_C)
cor.test(train_nolabel$F01,train_nolabel$F03_D)
cor.test(train_nolabel$F01,train_nolabel$F03_E)
cor.test(train_nolabel$F01,train_nolabel$F02)


cost1 <- function(actual, predicted) {
  weight1 = 10
  weight0 = 1
  c1 = (actual == 1) & (predicted < cutoff)  #logical vector - true if actual bad but predicted good
  c0 = (actual == 0) & (predicted > cutoff)  #logical vecotr - true if actual good but predicted bad
  return(mean(weight1 * c1 + weight0 * c0))
}
## We will punish more with the cost1
## Area under the ROC Curve used as cost function. We will need this later

## Cost function
cost2 <- function(actual, predicted) {
  return(auc(roc(actual,predicted))[1])
}
## Prob thresholds to be used for ROC Curve
thresh<-seq(0,1,0.001)

full.log.probit<-glm(data = train_nolabel,train_labels~.,family = binomial(link=probit))

summary(full.log.probit)

cor.test(train_nolabel$F07,train_nolabel$F02)


full.log.probit.prediction<-predict(full.log.probit,type = "response")
roc.plot(x = train_labels == "1", pred = full.log.probit.prediction,thresholds = thresh)$roc.vol


full.log.probit2<-glm(data = test_nolabel,test_labels~.,family = binomial(link=probit))

summary(full.log.probit2)

full.log.probit.prediction2<-predict(full.log.probit2,type = "response")
roc.plot(x = test_labels == "1", pred = full.log.probit.prediction2,thresholds = thresh)$roc.vol


# the other try with lasso it works!

X<-scale(callCenter)
X<-as.matrix(X)
Y<- as.matrix(call_labels)
lasso.fit<- glmnet(x=X, y=Y, family = "binomial", alpha = 1)
plot(lasso.fit, xvar = "lambda")

cv.lasso<- cv.glmnet(x=X, y=Y,family = "binomial", alpha = 1, nfolds = 3)
plot(cv.lasso)
cv.lasso$lambda.min
cv.lasso$lambda.1se
coef(lasso.fit, s=cv.lasso$lambda.1se)
coef(lasso.fit, s=cv.lasso$lambda.mi)

pred.lasso<- predict(lasso.fit, newx = X, s=cv.lasso$lambda.1se,type = 'response')
roc.plot(x = call_labels == "1", pred = pred.lasso,thresholds = thresh)$roc.vol

#now try it with test data 


X<-scale(lasso_test)
X<-as.matrix(lasso_test)
Y<- as.matrix(lasso_label)
lasso.fit2<- glmnet(x=X, y=Y, family = "binomial", alpha = 1)
plot(lasso.fit2, xvar = "lambda")

cv.lasso2<- cv.glmnet(x=X, y=Y,family = "binomial", alpha = 1, nfolds = 3)
plot(cv.lasso2)
cv.lasso2$lambda.min
cv.lasso2$lambda.1se
coef(lasso.fit, s=cv.lasso2$lambda.1se)
coef(lasso.fit, s=cv.lasso2$lambda.mi)

pred.lasso2<- predict(lasso.fit, newx = X, s=cv.lasso2$lambda.1se,type = 'response')
roc.plot(x = lasso_label == "1", pred = pred.lasso2,thresholds = thresh)$roc.vol


#now I will sample the data three times and I will use the logistic regression on them 
#after that I will take the average of those values.

# First sample

indexes1<-sample(nrow(bankCall),0.5*nrow(bankCall),replace = F)
train1<-bankCall[indexes1,]

train_nolabel1<- train1[1:50000,1:142]
train_labels1<- as.vector(train1[1:50000,143])

test1<-bankCall[-indexes1,]

test_nolabel1<- test1[1:50000,1:142]
test_labels1<- test1[1:50000,143]

full.log.probit_first<-glm(data = train_nolabel1,train_labels1~.,family = binomial(link=probit))
full.log.probit.prediction_first<-predict(full.log.probit_first,type = "response")


# Second sample


indexes2<-sample(nrow(bankCall),0.5*nrow(bankCall),replace = F)
train2<-bankCall[indexes2,]

train_nolabel2<- train1[1:50000,1:142]
train_labels2<- as.vector(train1[1:50000,143])

test2<-bankCall[-indexes2,]

test_nolabel2<- test1[1:50000,1:142]
test_labels2<- test1[1:50000,143]

full.log.probit_second<-glm(data = train_nolabel1,train_labels2~.,family = binomial(link=probit))
full.log.probit.prediction_second<-predict(full.log.probit_second,type = "response")

#Third sample

indexes3<-sample(nrow(bankCall),0.5*nrow(bankCall),replace = F)
train3<-bankCall[indexes3,]

train_nolabel3<- train1[1:50000,1:142]
train_labels3<- as.vector(train1[1:50000,143])

test3<-bankCall[-indexes3,]

test_nolabel3<- test1[1:50000,1:142]
test_labels3<- test1[1:50000,143]

full.log.probit_third<-glm(data = train_nolabel1,train_labels3~.,family = binomial(link=probit))
full.log.probit.prediction_third<-predict(full.log.probit_third,type = "response")


full.log.probit.prediction_average <- (full.log.probit.prediction_first + full.log.probit.prediction_second + full.log.probit.prediction_third)/3


test_data_ML<- read.csv("test_data.csv", header = TRUE)

full.log.probit3<-glm(data = callCenter,call_labels~.,family = binomial(link=probit))
predictedY <- predict(full.log.probit3, test_data_ML, type="response") 
write.table(predictedY, file = "test_predictions.csv", row.names = FALSE, col.names = FALSE)
