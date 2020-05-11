#bootstarp agrregating or bagging

data("airquality")
airdata<-airquality
airdata<-airdata[order(airdata$Ozone),]
airdata<-airdata[complete.cases(airdata), ]
head(airdata)
#predict temperature as a function of ozone
nrow(airdata)
ll<-matrix(NA,nrow=10,ncol=111)
for(i in 1:10){
  
  ss<-sample(1:dim(airdata)[1],replace=T)
  air0<-airdata[ss,]
  air0<-air0[order(air0$Ozone),]
  loess0<-loess(Temp~Ozone,data=air0,span=0.2)
  ll[i,]<-predict(loess0,newdata=data.frame(airdata=1:111))
}

plot(airdata$Ozone,airdata$Temp,pch=19)
for(i in 1:10){lines(1:111,ll[i,],col="grey",lwd=2)}
lines(1:111,apply(ll,2,mean),col="red",lwd=2)

#red line is the bagged loess curve:ave
#of multiple fitted loess curves

#in caret
predictors = data.frame(airdata=airdata$Ozone)
temp<-airdata$Temp
treebag<-bag(predictors,temp,B=10,bagControl = bagControl(fit=ctreeBag$fit,predict=ctreeBag$pred,aggregate=ctreeBag$aggregate))

