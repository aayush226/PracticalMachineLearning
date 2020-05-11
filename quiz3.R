set.seed(125)
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
a<-segmentationOriginal
inTrain<-createDataPartition(y=a$Case,p=0.7,list=FALSE)
training<-a[inTrain,]
testing<-a[-inTrain,]
set.seed(125)

modfit<-train(Class~.,method="rpart",data=training)
fancyRpartPlot(modfit$finalModel)
pred1<-predict(modfit,newdata=testing)