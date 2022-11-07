# Exercise 1
library(caret)
library(dplyr)
library(kknn)
library(ggplot2)
library(plotly)

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
  a = 1 - sum(diag(X))
  return(a / n)
}

# # It rounds up for > .5 and rounds down for <= 0.5
confusion_matrix_test =  table(test$X0.26, Pred2)
confusion_matrix_train =  table(train$X0.26, Pred)
# 
# 
missclass(confusion_matrix_test)
missclass(confusion_matrix_train)


min = sort(m1$prob[, 9])[1]
#min1 = sort(m1$prob[, 9])[1]
#min2 = sort(m1$prob[, 9])[2]
#min3 = sort(m1$prob[, 9])[3]

max1 = tail(sort(m1$prob[, 9]))[1]
max2 = tail(sort(m1$prob[, 9]))[2]

# Note: Add explenation in the presentation 
# Question: Should we take those with smallest/biggest value, or should we take unique.
heatmap(t(matrix(as.numeric(train[which(m1$prob[,9] == max1)[1],1:64]), nrow=8, ncol=8)), Colv=NA,Rowv=NA)
heatmap(t(matrix(as.numeric(train[which(m1$prob[,9] == max2)[2],1:64]), nrow=8, ncol=8)), Colv=NA,Rowv=NA)

heatmap(t(matrix(as.numeric(train[which(m1$prob[,9] == min1)[1],1:64]), nrow=8, ncol=8)), Colv=NA,Rowv=NA)
heatmap(t(matrix(as.numeric(train[which(m1$prob[,9] == min2)[2],1:64]), nrow=8, ncol=8)), Colv=NA,Rowv=NA)
heatmap(t(matrix(as.numeric(train[which(m1$prob[,9] == min3)[3],1:64]), nrow=8, ncol=8)), Colv=NA,Rowv=NA)



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


error_frame = data.frame(validation_errors)
error_frame["K"] = c(1:30)
colnames(error_frame) = c("Error","K")

p = ggplot(error_frame, aes(x=K, y=Error)) +
  geom_line()

ggplotly(p)


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

ggplotly(p2)


