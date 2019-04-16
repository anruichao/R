
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
library(dplyr)
training<-filter(segmentationOriginal,segmentationOriginal$Case=="Train")
testing<-filter(segmentationOriginal,segmentationOriginal$Case=="Test")
set.seed(125)
modfit<-train(Class~.,method="rpart",data=training)
print(modfit$finalModel)
plot(modfit$finalModel,uniform = TRUE)
text(modfit$finalModel,use.n = TRUE,all = TRUE, cex=.9)

load("olive.rda")
olive = olive[,-1]
modfit<-train(Area~.,method="rpart",data=olive)
print(modfit$finalModel)
newdata = as.data.frame(t(colMeans(olive)))
predict(modfit,newdata = newdata)

install.packages("ElemStatLearn")
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
set.seed(13234)
modfit <- train(chd~age+alcohol+obesity+tobacco+typea+ldl,method="glm",family="binomial",data=trainSA)
print(modfit$finalModel)
prediction<-predict(modfit,newdata = trainSA)
prediction<-as.factor(prediction)
head(prediction)
missClass = function(V,prediction){sum(((prediction > 0.5)*1) != V)/length(V)}
#>0.5=1 
V<-trainSA$chd
missClass(V,prediction)
prediction<-predict(modfit,newdata = testSA)
missClass(V,prediction)
#[1] 0.3116883
prediction<-predict(modfit,newdata = trainSA)
V<-trainSA$chd
missClass(V,prediction)
#[1] 0.2727273

library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
vowel.test$y<-factor(vowel.test$y)
vowel.train$y<-factor(vowel.train$y)
set.seed(33833)
modfit<-train(y~.,data=vowel.train,method="rf",importance=T)
modfit
varImp(modfit)
modfit<-randomForest(y~.,data=vowel.train,importance=T)
library("randomForest")
