library(caret)

# Exercise 1
# Splittimg the data

data = read.csv("parkinsons.csv")
data = subset(data, select = -c(age, sex, test_time, subject., total_UPDRS))

set.seed(12345)

n = nrow(data)

id = sample(1:n, floor(n*0.6))
train_data = data[id,]
test_data = data[-id,]

scaler = preProcess(train_data)
trainS = predict(scaler, train_data)
testS = predict(scaler, test_data)


# Exercise 2
# Creating the linear regression model

# For intercept = 0
fit = lm(motor_UPDRS ~ 0 + ., data = trainS)

# Calculating the MSE on training
#model_summ <-summary(fit)
#mse_training = mean(model_summ$residuals^2) # That's a second way
results = predict(fit, trainS)
n = length(results)
mse_training = (1/n) * sum((results - trainS$motor_UPDRS)^2)

# Calculating the MSE on test
results = predict(fit, testS)
n = length(results)
mse_test = (1/n) * sum((results - testS$motor_UPDRS)^2)

# Finding the most significant variables
# Visualize the ones with the biggest values in t and visualize it(Damian's task)
#is a univariate real-valued function, i.e., f : R 
summary(fit)


# Exercise 3
# Log Likehood function
loglikelihood <- function(params, data){
  print("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n")
  sigma <- params[1]
  print(length(params))
  w <- params[2:ncol(data)]
  y <- data[,1]
  x <- data[, 2:ncol(data)]
  n <- nrow(x)
  loglikelihood <- -n/2 * log(2*pi*sigma^2) - 1/(2*sigma^2) * sum((y - as.matrix(x) %*% as.numeric(w))^2) 
  return(loglikelihood)
}

ridge <- function(params, data, lambda){
  w <- params[2:length(params)]
  loglikelihood <- -loglikelihood(params, data) + lambda*sum(w^2)
  return(loglikelihood)
}

ridgeOpt <- function(lambda, data){
  opt <- optim(fit$coefficients, fn=ridge, data=data, lambda=lambda, method="BFGS")
  return(opt)
}

df <- function(lambda, x){
  x<- as.matrix(x)
  out <- x %*% solve((t(x) %*% x) + diag(lambda, ncol(x))) %*% t(x)
  out <- sum(diag(out))
  return(out)
}

trainS_hat = trainS["motor_UPDRS"]
#trainS = subset(trainS, select = -c(motor_UPDRS))

testS_hat = testS["motor_UPDRS"]
#testS = subset(testS, select = -c(motor_UPDRS))


model1 = ridgeOpt(1, trainS)
model2 = ridgeOpt(100, trainS)
model3 = ridgeOpt(1000, trainS)

# p 17 s(x) * p is p. we have to find df by it's trace
# https://stats.stackexchange.com/questions/8309/how-to-calculate-regularization-parameter-in-ridge-regression-given-degrees-of-f

opt_predict <- function(x, model){
  out <- sum((x) * model$par)
  print(out)
  return(out)
}

train_output1 <- c()
train_output2 <- c()
train_output3 <- c()

for(i in 1:nrow(trainS)){
  
  train_output1[i] <- opt_predict(trainS[i, ], model1)
  train_output2[i] <- opt_predict(trainS[i, ], model2)
  train_output3[i] <- opt_predict(trainS[i, ], model3)
}

test_output1 <- c()
test_output2 <- c()
test_output3 <- c()

for(i in 1:nrow(testS)){
  test_output1[i] <- opt_predict1(testS[i, ], model1)
  test_output2[i] <- opt_predict1(testS[i, ], model2)
  test_output3[i] <- opt_predict1(testS[i, ], model3)
}

ms_train1 <- (1/nrow(trainS_hat)) * (sum(train_output1 - trainS_hat)) ^ 2
ms_train2 <- (1/nrow(trainS_hat)) * (sum(train_output2 - trainS_hat)) ^ 2
ms_train3 <- (1/nrow(trainS_hat)) * (sum(train_output3 - trainS_hat)) ^ 2


ms_test1 <- (1/nrow(testS_hat)) * (sum(test_output1 - testS_hat))^ 2
ms_test2 <- (1/nrow(testS_hat)) * (sum(test_output2 - testS_hat)) ^ 2
ms_test3 <- (1/nrow(testS_hat)) * (sum(test_output3 - testS_hat)) ^ 2

df(1, trainS)
df(100, trainS)
df(1000, trainS)

df(1, testS)
df(100, testS)
df(1000, testS)

# T value of a coefficient is the estimate devided by st error of the coefficent.
# As lower as p is as more statistically significant relationship with final exam score is
# If standard error is much smaller than the expected value then the p value will be sifniciant