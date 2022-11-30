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

# Just write the formula of lasso regression


## 3.

# Question 1: Should we do it like that or use the optim function?
# Does it mean to show the degrees of freedom depending on lambda?

# Not sure if we need scaling

# The first column in the number of samples
# scaler = preProcess(train)
# trainS = predict(scaler, train)
# testS = predict(scaler, test)



covariates = train[, !names(train) %in% c("Fat", "Protein", "Moisture")]
response = train[, names(train) %in% c("Fat")]

modell = glmnet(as.matrix(covariates), response, alpha = 1, family="gaussian",
                lambda = seq(0, 1, 0.001))
plot(modell, xvar="lambda", label=TRUE)

values_of_lambda = c()
for(i in 1:length(modell$df)){
  if(modell$df[i] == 3){
    values_of_lambda <- append(values_of_lambda, modell$lambda[i])
  }
}

# The values of lambda that we hade df =  3
from = min(values_of_lambda)
to = max(values_of_lambda)

## 4.

modelr = glmnet(as.matrix(covariates), response, alpha = 0, family="gaussian",
                lambda = seq(0, 1, 0.001))

plot(modelr, xvar="lambda", label=TRUE)

# We can say that lasso gets rid of more features than ridge.

## 5.
modelcv=cv.glmnet(as.matrix(covariates), response, alpha=1, family="gaussian")
optimal_lambda = modelcv$lambda.min
plot(modelcv)
coef(modelcv, s="lambda.min")
cv_scores = modelcv$cvm
lambdas = modelcv$lambda


# This part of the code is not necessary as we do it with plot(modelcv)
# df = data.frame(cv_scores, lambdas)
# p = ggplot(data = df, aes(x=log(lambdas, base = 10), y=cv_scores)) +
#   geom_line()
# p
# For the annyoing error
# par(mar=c(1,1,1,1))



