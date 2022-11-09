library(caret)

# Exercise 1
# Splittimg the data

data = read.csv("parkinsons.csv")

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

fit = lm(motor_UPDRS ~ ., data = trainS)

# Calculating the MSE on training
# Intercept is very low(1.534e-15) So i can consider that it's zero

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
summary(fit)


# Exercise 3
# Log Likehood function

loglikehood <- function(theta, sigma){
  
}
