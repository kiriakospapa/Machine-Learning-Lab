library(randomForest)
# Q1: should i use seed(1234) in the first dataset as well?
# Q2: Should I round values because or is there any way to make it classifier instead
# of regressor?

# Training 
x1<-runif(100)
x2<-runif(100)
trdata<-cbind(x1,x2)
y<-as.numeric(x1<x2)
trlabels<-as.factor(y)

# Test
set.seed(1234)
x1<-runif(1000)
x2<-runif(1000)
tedata<-cbind(x1,x2)
y<-as.numeric(x1<x2)
telabels<-as.factor(y)
plot(x1,x2,col=(y+1))

# miss class function
missclass=function(X){
  n=sum(X)
  a = sum(diag(X))/n
  return(1-a)
}

# confusion_matrix_train = table(diabetes$V9, Pred)
# missclass(confusion_matrix_train)

# first dot
n = 1000

ms_error1 <- c()
ms_error10 <- c()
ms_error100 <- c()
for(i in 1:n){
  x1<-runif(100)
  x2<-runif(100)
  trdata<-cbind(x1,x2)
  y1<-as.numeric(x1<x2)
  trlabels<-as.factor(y1)
  df <- data.frame(x1, x2, trlabels)
  
  set.seed(1234)
  x1<-runif(1000)
  x2<-runif(1000)
  tedata<-cbind(x1,x2)
  y<-as.numeric(x1<x2)
  telabels<-as.factor(y)

  model1 <- randomForest(trlabels ~ x1 + x2, data = df, nodesize=25, keep.forest = TRUE,
                            ntree = 1)
  
  model2 <- randomForest(trlabels ~ x1 + x2, data = df, nodesize=25, keep.forest = TRUE,
                          ntree = 10)
  
  model3 <- randomForest(trlabels ~ x1 + x2, data = df, nodesize=25, keep.forest = TRUE,
                          ntree = 100)
  
  pred1 <- predict(model1, tedata)
  pred2 <- predict(model2, tedata)
  pred3 <- predict(model3, tedata)

  confusion_matrix_1 = table(y, pred1)
  confusion_matrix_10 = table(y, pred2)
  confusion_matrix_100 = table(y, pred3)
  
  ms_error1 <- append(ms_error1, missclass(confusion_matrix_1))
  ms_error10 <- append(ms_error10, missclass(confusion_matrix_10))
  ms_error100 <- append(ms_error100, missclass(confusion_matrix_100))
  
}

#df = data.frame(ms_error1, ms_error10[-1001], ms_error100[-1001], i <- 1:length(ms_error1))

mean(ms_error1)
mean(ms_error10)
mean(ms_error100)

var(ms_error1)
var(ms_error10)
var(ms_error100)


# second dot

# x1<-runif(100)
# x2<-runif(100)
# trdata<-cbind(x1,x2)
# y<-as.numeric((x1<0.5 & x2<0.5) | (x1>0.5 & x2>0.5))
# trlabels<-as.factor(y)

set.seed(1234)
x1<-runif(1000)
x2<-runif(1000)
tedata<-cbind(x1,x2)
y<-as.numeric(x1 < 0.5)
telabels<-as.factor(y)

n = 1000

ms2_error1 <- c()
ms2_error10 <- c()
ms2_error100 <- c()
for(i in 1:n){
  x1<-runif(100)
  x2<-runif(100)
  trdata<-cbind(x1,x2)
  y1<-as.numeric(x1 < 0.5)
  trlabels<-as.factor(y1)
  df <- data.frame(x1, x2, trlabels)
  

  model1 <- randomForest(trlabels ~ x1 + x2, data = df, nodesize=25, keep.forest = TRUE,
                         ntree = 1)
  
  model2 <- randomForest(trlabels ~ x1 + x2, data = df, nodesize=25, keep.forest = TRUE,
                         ntree = 10)
  
  model3 <- randomForest(trlabels ~ x1 + x2, data = df, nodesize=25, keep.forest = TRUE,
                         ntree = 100)
  
  pred1 <- predict(model1, tedata)
  pred2 <- predict(model2, tedata)
  pred3 <- predict(model3, tedata)
  
  confusion_matrix_1 = table(telabels, pred1)
  confusion_matrix_10 = table(telabels, pred2)
  confusion_matrix_100 = table(telabels, pred3)
  
  ms2_error1 <- append(ms2_error1, missclass(confusion_matrix_1))
  ms2_error10 <- append(ms2_error10, missclass(confusion_matrix_10))
  ms2_error100 <- append(ms2_error100, missclass(confusion_matrix_100))
  
}

mean(ms2_error1)
mean(ms2_error10)
mean(ms2_error100)

var(ms2_error1)
var(ms2_error10)
var(ms2_error100)

# Third dot

# x1<-runif(100)
# x2<-runif(100)
# trdata<-cbind(x1,x2)
# y<-as.numeric((x1<0.5 & x2<0.5) | (x1>0.5 & x2>0.5))
# trlabels<-as.factor(y)

set.seed(1234)
x1<-runif(1000)
x2<-runif(1000)
tedata<-cbind(x1,x2)
y<-as.numeric((x1<0.5 & x2<0.5) | (x1>0.5 & x2>0.5))
telabels<-as.factor(y)

n = 1000

ms3_error1 <- c()
ms3_error10 <- c()
ms3_error100 <- c()
for(i in 1:n){
  x1<-runif(100)
  x2<-runif(100)
  trdata<-cbind(x1,x2)
  y1<-as.numeric ((x1<0.5 & x2<0.5) | (x1>0.5 & x2>0.5)) 
  trlabels<-as.factor(y1)
  df <- data.frame(x1, x2, trlabels)
  
  model1 <- randomForest(trlabels ~ x1 + x2, data = df, nodesize=12, keep.forest = TRUE,
                         ntree = 1)
  
  model2 <- randomForest(trlabels ~ x1 + x2, data = df, nodesize=12, keep.forest = TRUE,
                         ntree = 10)
  
  model3 <- randomForest(trlabels ~ x1 + x2, data = df, nodesize=12, keep.forest = TRUE,
                         ntree = 100)
  
  pred1 <- predict(model1, tedata)
  pred2 <- predict(model2, tedata)
  pred3 <- predict(model3, tedata)
  
  confusion_matrix_1 = table(y, pred1)
  confusion_matrix_10 = table(y, pred2)
  confusion_matrix_100 = table(y, pred3)
  
  ms3_error1 <- append(ms3_error1, missclass(confusion_matrix_1))
  ms3_error10 <- append(ms3_error10, missclass(confusion_matrix_10))
  ms3_error100 <- append(ms3_error100, missclass(confusion_matrix_100))
  
}

mean(ms3_error1)
mean(ms3_error10)
mean(ms3_error100)

var(ms3_error1)
var(ms3_error10)
var(ms3_error100)

######

mean_errors1 <- c(mean(ms_error1), mean(ms_error10), mean(ms_error100))
mean_errors2 <- c(mean(ms2_error1), mean(ms2_error10), mean(ms2_error100))
mean_errors3 <- c(mean(ms3_error1), mean(ms3_error10), mean(ms3_error100))



# The answer for q1 is the diagram at page 8 

# For answer at q2: Less nodes, simpler model so it reduces the risk of
# overfitting more.

# from TA. Plot the data. it's easy to understand the difference if we plot
# the data in first and third case

# Visualazing test dataset 1
set.seed(1234)
x1<-runif(1000)
x2<-runif(1000)
tedata<-cbind(x1,x2)
y<-as.numeric(x1<x2)
telabels<-as.factor(y)
plot(x1,x2,col=(y+1))

# Visualazing test dataset 2
set.seed(1234)
x1<-runif(1000)
x2<-runif(1000)
tedata<-cbind(x1,x2)
y<-as.numeric(x1 < 0.5)
telabels<-as.factor(y)
plot(x1,x2,col=(y+1))

# Visualazing test dataset 3
set.seed(1234)
x1<-runif(1000)
x2<-runif(1000)
tedata<-cbind(x1,x2)
y<-as.numeric((x1<0.5 & x2<0.5) | (x1>0.5 & x2>0.5))
telabels<-as.factor(y)
plot(x1,x2,col=(y+1))