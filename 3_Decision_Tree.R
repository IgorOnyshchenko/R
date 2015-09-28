#Generate random data 
v1<-runif(1000)
v2<-rnorm(1000,10,2)
v3<-rnorm(1000,100,10)
target<-round((v1*v2)/max(v1*v2),0)
data=cbind.data.frame(v1,v2,v3,target)

# grow tree
library(rpart)
dtree <- rpart(target ~ ., data = data, method="class")
summary(dtree)

#Visualization tree 
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(dtree)

#Predict Output 
predicted = predict(dtree,data=data)

#plotting of Fact and Predicted values
plot(predicted[1:20], col='blue', type='l')
points(data$target[1:20], col='red', type='l')

