#Generate random data 
v1<-runif(1000)
v2<-rnorm(1000,10,2)
v3<-rnorm(1000,100,10)
target<-round((v1*v2)/max(v1*v2),0)
data=cbind.data.frame(v1,v2,v3,target)

## Sample splitting randomly on Training and Testing set with 70/30 proportion---------------------
library(caret)
inTrain<-createDataPartition(y=data$target,p=0.5,list=FALSE)
train<-data[inTrain,]
test<-data[-inTrain,]

library(e1071)
# Fitting model
NB <-naiveBayes(train$target~., train)
summary(NB)

#Predict Output 
predicted= predict(NB,x_test)

length(predicted)
