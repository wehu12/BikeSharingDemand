library(lubridate)
rm(list=ls())

calc.te<-function(a,p){
  n<-length(a)
  p[p<0]=0
  p=round(p)
  error=(log(p+1)-log(a+1))^2
  return(mean(error))
}

test.model<-function(model,test,...){
  i<-which(colnames(test)=="count")
  pred <- predict(model, newdata=test[,-i],...)
  act<-test[,i]
  return(calc.te(act,pred))
}

cross.validate<-function(){
  te1.1=c()
  te1.2=c()
  te1.3=c()
  te1.4=c() 
  te2.1=c()
  te3.1=c()
  te3.2=c()
  for (i in 1:9){
    data<-data[data$weather!=4,]
    flag <- data$day==2*i-1|data$day==2*i
    train <-data[!flag,]
    test<-data[flag,]
    model.1.1<-lm(count~datetime+holiday+season+weather+workingday+temp+atemp+humidity+windspeed, data=train)
    model.1.2<-lm(count ~ hour + weekday + year + month + day+holiday + season+weather +temp + humidity + windspeed+workingday*hour,data=train)
    lm1d<-lm(abs(resid(model.1.2))~temp + humidity + windspeed, data=train)
    weight1d <- 1/ ( fitted(lm1d) )^2
    model.1.3<-lm(count ~ hour + weekday + year + month + day+holiday + season+weather +temp + humidity + windspeed+workingday*hour,data=train,weight=weight1d)
    model.1.4 <- lm(count~ hour + weekday + year + month + day + season + weather + temp + humidity + windspeed + workingday + hour*workingday + hour*weekday + hour*month + weather*temp + temp*humidity + weather*humidity,data=train)
    
    #poisson regression
    myFormula=count ~ hour + weekday + year + month + holiday + weather +temp + humidity + windspeed + workingday * hour
    model.2.1<-glm(myFormula, family=quasipoisson(link="identity"), data=train,start=rep(0.025,62))
    summary(model.2.1)
    
    #decision tree
    myFormula <- count ~ hour+weekday+year+month+holiday+season+weather+temp+humidity+windspeed
    model.3.1 <- rpart(myFormula, data = train,control = rpart.control(minsplit = 10))
    
    #random forest
    model.3.2 <- randomForest(myFormula, data=train, ntree=100, proximity=TRUE)
    te1.1=cbind(te1.1,test.model(model.1.1,test))
    te1.2=cbind(te1.2,test.model(model.1.2,test))
    te1.3=cbind(te1.3,test.model(model.1.3,test))
    te1.4=cbind(te1.4,test.model(model.1.4,test))
    te2.1=cbind(te2.1,test.model(model.2.1,test))
    te3.1=cbind(te3.1,test.model(model.3.1,test))
    te3.2=cbind(te3.2,test.model(model.3.2,test))  
  }
  errors = rbind(te1.1,te1.2,te1.3,te1.4,te2.1,te3.1,te3.2)
  return(errors)
}
test.expmodel<-function(model,test,...){
  i<-which(colnames(test)=="count")
  pred <- exp(predict(model, newdata=test[,-i],...))
  act<-test[,i]
  return(calc.te(act,pred))
}
test.output<-function(model,test,...){
  test_clean<-prep.data(test)
  pred<-round(predict(model,newdata=test_clean,...))
  pred[pred<0]=0
  submit<- data.frame(datetime = test$datetime, count=pred)
  write.csv(submit, file="submit_lh.csv",row.names=FALSE)
}
prep.data<-function(data){
  data$weather<-as.factor(data$weather)
  data$holiday<-as.factor(data$holiday)
  data$workingday<- as.factor(data$workingday)
  data$season<-as.factor(data$season)
  data$datetime<-as.character(data$datetime)
  data$datetime<-strptime(data$datetime, format ='%Y-%m-%d %H:%M:%S')
  
  #create date variable
  data$hour <- as.factor(hour(data$datetime))
  data$day <-day(data$datetime)
  data$month <- month(data$datetime)
  data$year <-year(data$datetime)
  data$weekday<-as.factor(wday(data$datetime))
  data$period<-as.factor(ceiling(as.numeric(data$hour)/6))
  data$datetime<-as.numeric(data$datetime)
  return(data)
}

create.plots<-function(){
  #exploratory data analysis
  library(ggplot2)
  # plot A:total daily count overtime
  daily<-aggregate(count~year*month*day,data=train,FUN=sum)
  daily$date<-as.Date(ISOdate(daily$year,daily$month,daily$day))
  ggplot(daily, aes(date, count)) + geom_line() + xlab("") + ylab("Daily Count")
  # plot B:average day hour
  day_hour_counts <- aggregate(count~weekday*hour, data=train, mean)
  #other plots
  qplot(hour, count, data=train, facets=workingday ~.)
  qplot(hour, count, data=train, facets=weekday ~.)
  qplot(temp,count,data=train, facets= weather~.)
  qplot(temp, count, data=train[which(train$timespan==3),], facets=workingday ~.)
  qplot(humidity, count, data=train, facets=workingday ~.)
  qplot(windspeed, count, data=train, facets=workingday ~.)
  
  #heatmap
  ggplot(day_hour_counts, aes(x = hour, y = weekday)) + geom_tile(aes(fill = count)) + scale_fill_gradient(name="Average Counts", low="white", high="red") + theme(axis.title.y = element_blank())
  ggplot(train[train$timespan!=1&train$weather==1&train$month==8&train$workingday==1,], aes(x=count, fill=hour)) + geom_density(alpha=.3)
}

#set working directory
setwd('/Users/linahu/Documents/Developer/Bike\ Sharing/')

data<-read.csv("train.csv")
validate <-read.csv("test.csv")

#clean data
data<-prep.data(data)
data<-data[data$weather!=4,]
summary(data)

#split the training set into training and validation
flag <- data$day>17
train <-data[!flag,]
test<-data[flag,]

#correlation between variables
cor(data$count,data[,6:12])

avg_count<- aggregate(count~weather*temp, data = train, FUN=mean)
qplot(temp,count,data=avg_count, color = weather,geom=c("point","smooth"),se=FALSE,size = 1)

#linear models with different time parameters and interaction terms
#lm1<-lm(count~holiday+season+weather+workingday+temp+atemp+humidity+windspeed, data=train )
#lm2<-lm(count~datetime+holiday+season+weather+workingday+temp+atemp+humidity+windspeed, data=train)
#lm3<-lm(count~hour+holiday+season+weather+workingday+temp+atemp+humidity+windspeed, data=train)
#lm4<-lm(count~hour+holiday+season+weather+workingday+temp+atemp+humidity+windspeed+holiday*season+workingday*season+weather*holiday+hour*weekday, data=train)

#Final models:
library(rpart)
library(randomForest)

model.1.1<-lm(count~datetime+holiday+season+weather+workingday+temp+atemp+humidity+windspeed, data=train)
model.1.2<-lm(count ~ hour + weekday + year + month + day+holiday + season+weather +temp + humidity + windspeed+workingday*hour,data=train)

lm1d<-lm(abs(resid(model.1.2))~temp + humidity + windspeed, data=train)
weight1d <- 1/ ( fitted(lm1d) )^2
model.1.3<-lm(count ~ hour + weekday + year + month + day+holiday + season+weather +temp + humidity + windspeed+workingday*hour,data=train,weight=weight1d)

lm.all<-lm(count ~ hour + weekday + year + month + day+holiday+weather +temp + humidity + windspeed
  +workingday*hour+weekday*hour +holiday*hour +month*hour+weather*temp+temp*humidity+weather*humidity,data=train)
model.1.4 <- step(lm.all, trace=FALSE); 

#poisson regression
myFormula=count ~ hour + weekday + year + month + holiday + weather +temp + humidity + windspeed + workingday * hour
model.2.1<-glm(myFormula, family=quasipoisson(link="identity"), data=train,start=rep(0.025,62))
summary(model.2.1)

#decision tree
myFormula <- count ~ hour+weekday+year+month+holiday+season+weather+temp+humidity+windspeed
model.3.1 <- rpart(myFormula, data = train,control = rpart.control(minsplit = 10))

#random forest
model.3.2 <- randomForest(myFormula, data=train, ntree=100, proximity=TRUE)

te1.1<-test.model(model.1.1,test)
te1.2<-test.model(model.1.2,test)
te1.3<-test.model(model.1.3,test)
te1.4<-test.model(model.1.4,test)
te2.1<-test.model(model.2.1,test)
te3.1<-test.model(model.3.1,test)
te3.2<-test.model(model.3.2,test)
cbind(te1.1,te1.2,te1.3,te1.4,te2.1,te3.1,te3.2)

errors = cross.validate()


#boosting
#library(gbm)
#model.boost=gbm(myFormula,data=train,distribution= "poisson",n.trees=5000, interaction.depth=6)
#test.model(model.boost,test,n.trees=5000)

#final model to submit
#model.rf.submit <- randomForest(myFormula, data=data, ntree=100, proximity=TRUE)
#model.boost.submit=gbm(myFormula,data=data,distribution= "gaussian",n.trees=5000, interaction.depth=6)
#test.output(model.boost.submit,validate,n.trees=5000)
#test.output(model.rf.submit,validate)
#time series
#library(xts)
#<<<<<<< HEAD
#train.xts<-as.xts(x=train[,"count"],order.by=train[,"datetime"])
#plot(train.xts[(1:100),])
#f<-decompose(train.ts)  
#par(mfrow=c(2,1))
#acf(train.ts)
#pacf(train.ts)



>>>>>>> FETCH_HEAD
