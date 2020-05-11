data("iris")
inTrain<-createDataPartition(y=iris$Species,p=0.7,list=FALSE)
training<-iris[inTrain,]
testing<-iris[-inTrain,]
modfit<-train(Species~.,data=training,method="rf",prox=TRUE)
modfit

#