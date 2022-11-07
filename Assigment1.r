# Exercise 1
library(caret)
library(dplyr)
library(kknn)
digits = read.csv("optdigits.csv")
set.seed(12345)

n = nrow(digits)

id = sample(1:n, floor(n*0.5))
train= digits[id,]
test_and_validation = digits[-id,]

n = nrow(test_and_validation)
id = sample(1:n, floor(n*0.5))
validation = test_and_validation[id,]
test = test_and_validation[-id,]

m1 = kknn(as.factor(X0.26) ~ ., train=train, test=train, k=30, kernel="rectangular")
m2 = kknn(as.factor(X0.26) ~ ., train=train, test=test, k=30, kernel="rectangular")


# Predictions on the test and train data set
Pred = m1$fitted.values
Pred2 = m2$fitted.values

 
missclass=function(X){
  n=length(X)
  return((1 - sum(diag(X))) / n)
}

# # It rounds up for > .5 and rounds down for <= 0.5
 confusion_matrix_test =  table(test$X0.26, Pred2)
 confusion_matrix_train =  table(train$X0.26, Pred)
# 
# 
missclass(confusion_matrix_test)
missclass(confusion_matrix_train)
