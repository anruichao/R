churn <- read.csv('WA_FnUseC_TelcoCustomerChurn.csv')
str(churn); dim(churn);churn <- read.csv('WA_FnUseC_TelcoCustomerChurn.csv')
str(churn); dim(churn);
View(churn)
library(plyr)
install.packages("corrplot")
library(corrplot)
library(ggplot2)
library(gridExtra)
summary(churn)
lm(MonthlyCharges~.,data= churn,method = "qr")
lm(tenure~.,data= churn,method = "qr")
lm(tenure~MonthlyCharges,data= churn,method = "qr")
plot(churn$tenure,churn$MonthlyCharges,col=churn$gender)
plot(churn$TotalCharges,churn$gender,col=churn$Churn)

