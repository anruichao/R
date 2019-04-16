install.packages("caret")
library(caret)
library(kernlab)
install.packages("kernlab")
data(spam)
intrain <- createDataPartition(spam$type,p=0.75,list = FALSE)
training <- spam[intrain,]
testing <- spam[-intrain,]
dim(training)
set.seed(32323)
folds <- createFolds(spam$type,k=10,list = TRUE, returnTrain = TRUE)
folds2 <- createFolds(spam$type,k=10,list = TRUE, returnTrain = FALSE)
folds2
folds[[10]][1:10]

library(AppliedPredictiveModeling)

tme <- 1:1000
foldss <- createTimeSlices(tme,initialWindow = 10,horizon = 5)
foldss$train[[2]]
modelfit <- train(type~.,data = training,method="glm")
install.packages("e1071")
modelfit

install.packages("ISLR")
install.packages("ggplot2")
library(ggplot2)
library(ISLR)
data("Wage")
summary(Wage)
intrain <- createDataPartition(Wage$wage,p=0.7,list = FALSE)
training <- Wage[intrain,]
testing <- Wage[-intrain,]
dim(training);dim(testing)


install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
data("concrete")
library(caret)
set.seed(1000)
t = createDataPartition(mixtures$CompressiveStrength,p=3/4)[[1]]
train = mixtures[t,]
test = mixtures[-t,]
qplot(CompressiveStrength,train$index,colour=train$FlyAsh,data=train)
install.packages("Hmisc")
library(Hmisc)
train$a<- cut2(train$FlyAsh,g=2)
table(train$a)
qplot(CompressiveStrength,train$b,colour=train$FlyAsh,data=train)
train$b<- train$index
#cut plot
train$a<- cut2(train$Age,g=2)
plot(train$CompressiveStrength,train$index,col=train$Age)
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
preProcess(training[,58:69],method="pca",thresh=0.8)

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
preProcess(training[,58:69],method="pca",thresh=0.9)

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
mfit<-train(CompressiveStrength~.,data=training,method="lm")
pre <-predict(mfit,testing)
testing$a<- cut2(testing$CoarseAggregate,g=2)
plot(pre,testing$index,col=testing$a)
plot(pre,testing$CompressiveStrength,col=testing$a)


data("iris")
library(caret)
library(randomForest)
intrain<-createDataPartition(y=iris$Species,p=0.7,list=FALSE)
training<-iris[intrain,]
testing<-iris[-intrain,]
modelfit<-train(Species~.,data=training,method="rf",prox=TRUE)
getTree(modelfit$finalModel,k=2,labelVar = TRUE)

library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
library(dplyr)
training<-filter(segmentationOriginal,segmentationOriginal$Case=="Train")
testing<-filter(segmentationOriginal,segmentationOriginal$Case=="Test")
set.seed(125)
modfit<-train(