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


library(knn)
library(class)

# Fitting model
KNN <-knn(target~., data=train,k=2)
summary(fit)
#Predict Output 
predicted= predict(fit,x_test)

