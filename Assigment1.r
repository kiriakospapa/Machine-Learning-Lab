# Exercise 1
library(caret)
library(dplyr)
library(kknn)
library(ggplot2)

# Q1
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

# Q2
m1 = kknn(as.factor(X0.26) ~ ., train=train, test=train, k=30, kernel="rectangular")
m2 = kknn(as.factor(X0.26) ~ ., train=train, test=test, k=30, kernel="rectangular")


# Predictions on the test and train data set
Pred = m1$fitted.values
Pred2 = m2$fitted.values

 
missclass=function(X){
  n=sum(X)
  a = sum(diag(X))/n
  return(1-a)
}

# # It rounds up for > .5 and rounds down for <= 0.5
confusion_matrix_test =  table(test$X0.26, Pred2)
confusion_matrix_train =  table(train$X0.26, Pred)
# 
# 
missclass(confusion_matrix_test)
missclass(confusion_matrix_train)

# Test: We can that we had many false predictions on number 5 as it misclassified 
# as 9, 6 times. Also 9 was hard to be detected as it was misclassified as 7, 
# 6 times. The most easy to predict was 0 as all 88 times that we had 0 it predicted
# it correctly. In general, we can see that the missclafication error is quite low
# so we can conclude that our model was very accurate on the test dataset

# Training: We can see that the model has predicted 1 as 8, 10 times and 9 as 5,
# 11 times. Also our model struggled with 9 as it had many wrong predictions.
# In addition, it was very good predicting 6 and 0. In this case too, we can see
# that the missclassification error was very small.

# Q3
#min = sort(m1$prob[, 9])[1]
min1 = sort(m1$prob[, 9])[1]
min2 = sort(m1$prob[, 9])[2]
min3 = sort(m1$prob[, 9])[3]

max1 = tail(sort(m1$prob[, 9]))[1]
max2 = tail(sort(m1$prob[, 9]))[2]

# Note: Add explenation in the presentation 
# Question: Should we take those with smallest/biggest value, or should we take unique.\\

# This one is easy to recognize it visually as 8
heatmap(t(matrix(as.numeric(train[which(m1$prob[,9] == max1)[1],1:64]), nrow=8, ncol=8)), Colv=NA,Rowv=NA)

# # This one is easy to recognize it visually as 8
heatmap(t(matrix(as.numeric(train[which(m1$prob[,9] == max2)[2],1:64]), nrow=8, ncol=8)), Colv=NA,Rowv=NA)

# This one is clear that it's not 8. Visually, looks like a 0
heatmap(t(matrix(as.numeric(train[which(m1$prob[,9] == min1)[1],1:64]), nrow=8, ncol=8)), Colv=NA,Rowv=NA)

# This one is clear that it's not 8. Visually, looks like a 3
heatmap(t(matrix(as.numeric(train[which(m1$prob[,9] == min2)[2],1:64]), nrow=8, ncol=8)), Colv=NA,Rowv=NA)

#  This one is clear that it's not 8. Visually, looks like a 3
heatmap(t(matrix(as.numeric(train[which(m1$prob[,9] == min3)[3],1:64]), nrow=8, ncol=8)), Colv=NA,Rowv=NA)


# Q4
# As the k increased the model gets more complicated as we have more parameters
# to take it into account. When the k is low we can see that the error on validation
# and training data is low. As k increases, the validation and training error increase
# as well. The optimal k taking into account the sum of validation and training errors
# is 2 and 3.

# So for k=2 the validation error is almost equal to test error and the training
# error is 0.

# So for k=3 the the validation error is almost equal to test error and the training
# error is  close to 0. We can conclude that the training error is smaller because
# the model was trained on the training dataset and validation and test datasets
# are new data and their losses are close to each other.


validation_errors = c()

# Question: Should we do it for training as well? 
# Question: Should the error be negative?
for(i in 1:30){
  #m1 = kknn(as.factor(X0.26) ~ ., train=train, test=train, k=i, kernel="rectangular")
  m3 = kknn(as.factor(X0.26) ~ ., train=train, test=validation, k=i, kernel="rectangular")
  Pred3 = m3$fitted.values
  confusion_matrix_valid =  table(validation$X0.26, Pred3)
  validation_errors = append(validation_errors ,missclass(confusion_matrix_valid))
}

train_errors = c()
for(i in 1:30){
  #m1 = kknn(as.factor(X0.26) ~ ., train=train, test=train, k=i, kernel="rectangular")
  m3 = kknn(as.factor(X0.26) ~ ., train=train, test=train, k=i, kernel="rectangular")
  Pred3 = m3$fitted.values
  confusion_matrix_train =  table(train$X0.26, Pred3)
  train_errors = append(train_errors ,missclass(confusion_matrix_train))
}


error_frame = data.frame(validation_errors, train_errors, validation_errors + train_errors)
error_frame["K"] = c(1:30)
colnames(error_frame) = c("vError", "tError", "vtError","K")

p = ggplot(error_frame, aes(x=K)) +
  geom_line(aes(y=vError), color="blue") + geom_line(aes(y=tError), color="red") +
  geom_line(aes(y=vtError), color="green")
p

best_k = which(min(validation_errors + train_errors) == (validation_errors + train_errors))
# The best ks are 1 and 2.

k1 = kknn(as.factor(X0.26) ~ ., train=train, test=test, k=2, kernel="rectangular")
k2 = kknn(as.factor(X0.26) ~ ., train=train, test=test, k=3, kernel="rectangular")

Predk1 = k1$fitted.values
confusion_matrix =  table(test$X0.26, Predk1)
missclass(confusion_matrix)
train_errors[2]
validation_errors[2]

Predk2 = k2$fitted.values
confusion_matrix =  table(test$X0.26, Predk2)
missclass(confusion_matrix)
train_errors[3]
validation_errors[3]


# Q5
# Question: Should we use base 2 or 10?
entropy <- function(d){
  x <- 0
  for (i in d){
    x <- x + (i * log((i+1e-15), base = 2))
  }
  return(-x)
}

validation_errors_2 = c()

for(i in 1:30){
  #m1 = kknn(as.factor(X0.26) ~ ., train=train, test=train, k=i, kernel="rectangular")
  m3 = kknn(as.factor(X0.26) ~ ., train=train, test=validation, k=i, kernel="rectangular")
  error_2 = entropy(m3$prob)
  validation_errors_2 = append(validation_errors_2 ,error_2)
}

error_frame_2 = data.frame(validation_errors_2)
error_frame_2["K"] = c(1:30)
colnames(error_frame_2) = c("Cross_entropy","K")

p2 = ggplot(error_frame_2, aes(x=K, y=Cross_entropy)) +
  geom_line()

p2

# Question: hint why cross entropy might be better here.

