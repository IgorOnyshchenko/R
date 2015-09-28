#Generate random data 
v1<-runif(1000)
v2<-rnorm(1000,10,2)
v3<-rnorm(1000,100,10)
target<-v1*v3+v2^2

data=cbind.data.frame(v1,v2,v3,target)
head(data)

## Sample splitting randomly on Training and Testing set with 70/30 proportion---------------------
library(caret)
inTrain<-createDataPartition(y=data$target,p=0.5,list=FALSE)
train<-data[inTrain,]
test<-data[-inTrain,]

# Train the Linear Regression model using the training sets and check score
linear <- lm(target ~ ., data = train)
summary(linear)
plot(linear)

#Predict Output
test$pred= predict.lm(linear, test) 

#plotting of Fact and Predicted values
plot(test$pred[1:20], col='blue', type='l')
points(test$target[1:20], col='red', type='l')

#Counting and Plotting errors
errors=test$target-test$pred
hist(errors, breaks=10)

#Standard Deviation, median, min, max of errors
sd(errors)
median(abs(errors))
max(abs(errors))
min(abs(errors))

#comparing 2 model
linear2 <- lm(target ~ v1+v2, data = train)
anova(linear, linear2)

