#predicting with regression

data("faithful")
inTrain<-createDataPartition(y=faithful$waiting,p=0.5,list=FALSE)
training<-faithful[inTrain,]
testing<-faithful[-inTrain,]
head(training)
plot(training$waiting,training$eruptions,pch=19,col="blue")
#fit a linear model now
lm1<-lm(eruptions~waiting,data=training)
summary(lm1)

# fit a line
lines(training$waiting,lm1$fitted,lwd=3)

#predict a new value
#say the new waiting time is 80 min
# one way is to manually do it
neweruption<-coef(lm1)[1]+ (coef(lm1)[2]*80)
neweruption
# other way is to use the predict fucntion
newdata<-data.frame(waiting=80)
predict(lm1,newdata)

par(mfrow=c(1,2))
plot(training$waiting,training$eruptions,pch=19,col="blue")
lines(training$waiting,predict(lm1),lwd=3)
plot(testing$waiting,testing$eruptions,pch=19,col="red")
lines(testing$waiting,predict(lm1,newdata=testing),lwd=3)

#get traing and test set errors
sqrt(sum((lm1$fitted-training$eruptions)^2))
sqrt(sum((predict(lm1,newdata=testing)-testing$eruptions)^2))

#prediction intervals

pred1<-predict(lm1,newdata=testing,interval="prediction")
ord<-order(testing$waiting)
plot(testing$waiting,testing$eruptions,col="blue")
matlines(testing$waiting[ord],pred1[ord,],type="l",col=c(1,2,2),lty=c(1,1,1),lwd = 3)

## how to do all this with caret package

modfit<-train(eruptions~waiting,data=training,method="lm")
summary(modfit$finalModel)
