#combining predictors
library("ISLR")
data(Wage)
Wage<-subset(Wage,select = -c(logwage))
inBuild<-createDataPartition(y=Wage$wage,p=0.7,list=FALSE)
validation<-Wage[-inBuild,]
buildData<-Wage[inBuild,]
inTrain<-createDataPartition(y=buildData$wage,p=0.7,list=FALSE)
training<-buildData[inTrain,]
testing<-buildData[-inTrain,]
mod1<-train(wage~.,method="glm",data=training)
mod2<-train(wage~.,method="rf",data=training,trControl=trainControl(method="cv"),number=3)
pred1<-predict(mod1,testing)
pred2<-predict(mod2,testing)
qplot(pred1,pred2,colour=wage,data=testing)

#fit a model that combines predictoers

predDF<-data.frame(pred1,pred2,wage=testing$wage)
combmodfit<-train(wage~.,method="gam",data=predDF)
combpred<-predict(combmodfit,predDF)
#testing errors
sqrt(sum((pred1-testing$wage)^2))
sqrt(sum((pred2-testing$wage)^2))
sqrt(sum((combpred-testing$wage)^2))
#lowest error on combined predictor

pred1v<-predict(mod1,validation)
pred2v<-predict(mod2,validation)
predvDF<-data.frame(pred1=pred1v,pred2=pred2v)
combpredv<-predict(combmodfit,predvDF)

sqrt(sum((pred1v-validation$wage)^2))
sqrt(sum((pred2v-validation$wage)^2))
sqrt(sum((combpredv-validation$wage)^2))