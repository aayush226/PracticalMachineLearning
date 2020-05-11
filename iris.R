#predicting with trees
data("iris")
inTrain<-createDataPartition(y=iris$Species,p=0.7,list=FALSE)
training<-iris[inTrain,]
testing<-iris[-inTrain,]

modfit<-train(Species~.,method="rpart",data=training)
print(modfit$finalModel)
plot(modfit$finalModel,uniform=TRUE,main="Classification Tree")
text(modfit$finalModel,use.n=TRUE,cex=.8)

#better plots can be made with rattle package
fancyRpartPlot(modfit$finalModel)
predict(modfit,newdata=testing)


