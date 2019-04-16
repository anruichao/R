library(ElemStatLearn)
data("prostate")
summary(prostate)
str(prostate)
small = prostate[1:5,]
lm(lpsa~.,data = small)
library(ISLR)
data(Wage)
library(ggplot2)
library(caret)
wage<-subset(Wage,select = -c(logwage))
inbuild<-createDataPartition(y = wage$wage,p=0.7,list=FALSE)
vali<- wage[-inbuild,]
build <- wage[inbuild,]
intrain<-createDataPartition(y=build$wage,p=0.7,list=FALSE)
training<-build[intrain,]
testing<-build[-intrain,]
mod1<-train(wage~.,method="glm",data=training)
mod2<-train(wage~.,method="rf",data = training,trControl = trainControl(method = "cv"),number=3)
pred1 <- predict(mod1,testing)
pred2<- predict(mod2,testing)
qplot(pred1,pred2,color=wage,data=testing)

predDF<-data.frame(pred1,pred2,wage=testing$wage)
comb<-train(wage~.,method="gam",data=predDF)
combpred<-predict(comb,predDF)
#sum r sqrt
sqrt(sum((pred1-testing$wage)^2))
sqrt(sum((pred2-testing$wage)^2))
sqrt(sum((combpred-testing$wage)^2))
#comb has smaller error
pv1<-predict(mod1,vali)
pv2<-predict(mod2,vali)
combv<-data.frame(pred1=pv1,pred2=pv2)
predictv<-predict(comb,combv)
sqrt(sum((pv1-vali$wage)^2))
sqrt(sum((pv2-vali$wage)^2))
sqrt(sum((predictv-vali$wage)^2))
#> sqrt(sum((pv1-vali$wage)^2))  
#[1] 996.1821
#> sqrt(sum((pv2-vali$wage)^2))
#[1] 1024.248
#> sqrt(sum((predictv-vali$wage)^2))
#[1] 997.4051