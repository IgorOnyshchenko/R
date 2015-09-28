#Generate random data 
v1<-runif(1000)
v2<-rnorm(1000,10,2)
v3<-rnorm(1000,100,10)
target<-round((v1*v2)/max(v1*v2),0)
data=cbind.data.frame(v1,v2,v3,target)

library(e1071)
# Fitting model
svm <-svm(target ~ ., data = data)
summary(svm)
#Predict Output 
predicted= predict(svm,data)

#plotting of Fact and Predicted values
plot(predicted[1:20], col='blue', type='l')
points(data$target[1:20], col='red', type='l')
