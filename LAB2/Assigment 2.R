library(caret)
library(glmnet)


# Splitting the data
data = read.csv("tecator.csv")

# Removing the sample column
data = data[-1]
set.seed(12345)
n = nrow(data)
id = sample(1:n, floor(n*0.5))
train = data[id,]
test = data[-id,]


## 1.

fit = lm(Fat ~ . - Protein - Moisture , data = train)
summary(fit)


results_training = predict(fit, train)
mse_training = (1/n) * sum((results_training - train$Fat) ^ 2)

results_test = predict(fit, test)
mse_test = (1/n) * sum((results_test - test$Fat) ^ 2)

# Question 1: Can I use MSE as error function?

# Question 2: Lasso is widely used when p>>n, why to use it here?

# MSE of training is much smaller compared to the MSE of training. That's because
# of overfitting, so our model is very complicated and it can generalize well.
# Probably it needs less features


## 2.
# Question 1: Cost function at slide 20 2d?

# Question 2: According to LASSO we should scale our data, but exercise doesn't mention
# something like that


## 3.

# Question 1: Should we do it like that or use the optim function?
# Does it mean to show the degrees of freedom depending on lambda?

# The first column in the number of samples
scaler = preProcess(train)
trainS = predict(scaler, train)
testS = predict(scaler, test)



covariates = trainS[, !names(trainS) %in% c("Fat")]
response = trainS[, names(trainS) %in% c("Fat")]

model0 = glmnet(as.matrix(covariates), response, alpha = 1, family="gaussian")


lasso_function <- function(params, data, lambda){
  n = length(params)
  intercept = params[1]
  thetas = params[2: n]
  x = data[ , !names(trainS) %in% c("Fat")]
  y = data[ , names(trainS) %in% c("Fat")]

  out <- (1 / n) * sum((y - intercept -(thetas) * x) ^ 2)  + lambda * sum(abs(thetas)) 
  return(out)
}


optim(rep(100, ncol(data)), fn = lasso_function, data = trainS, lambda = 100)



