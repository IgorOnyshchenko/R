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


library(randomForest)
# Fitting model
RF <- randomForest(target ~ ., train ,ntree=10)
summary(RF)
#Predict Output 
predicted= predict(RF,test)


#ROC curve plot and AUC calculation
library(ROCR)
test$score<-predict(RF,test)
pred<-prediction(test$score,test$target)
perf<-performance(pred,"tpr","fpr")
plot(perf,main="ROC plot AR model")
auc.tmp<-performance(pred,"auc")
ar_auc<-as.numeric(auc.tmp@y.values)
ar_auc #0.92

#plotting of Fact and Predicted values
plot(test$score[1:20], col='blue', type='l')
points(test$target[1:20], col='red', type='l')
