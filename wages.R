library(ISLR)
data("Wage")
library(caret)
Wage<-subset(Wage,select = -c(logwage))
#we are trying to predict logwage so we removed it from the wage dataset

summary(Wage)
inTrain<-createDataPartition(y=Wage$wage,p=0.7,list=FALSE)
training<-Wage[inTrain,]
testing<-Wage[-inTrain,]
featurePlot(x=training[,c("age","education","jobclass")],y=training$wage,plot = "pairs")


qplot(age,wage,data=training)
qplot(age,wage,colour=jobclass,data=training)
qplot(age,wage,colour=education,data=training)

# fit a linear model now

modfit<-train(wage~ age + jobclass + education, method="lm" ,data = training)
finmodel<-modfit$finalModel
print(modfit)
finmodel
#diagnostic plot
plot(finmodel,1,pch=19,cex=0.5,col="red")

qplot(finmodel$fitted,finmodel$residuals,colour=race,data=training)
plot(finmodel$residuals,pch=19)

# predict

pred<-predict(modfit,testing)
qplot(wage,pred,colour=year,data=testing)
#ideally they should be a straight line


# to use all covariates in a model 

modfitall<-train(wage~.,data=training,method="lm")
pred1<-predict(modfitall,testing)
qplot(wage,pred1,colour=year,data=testing)