
loan  <- read.csv('train.csv')

str(loan)
head(loan)
library(ggplot2)
library(caret)
colSums(is.na(loan))

summary(loan$Gender)
library(dplyr)
table(is.na(loan$LoanAmount))
mean = mean(loan$LoanAmount[!is.na(loan$LoanAmount)])
loan$LoanAmount = ifelse(is.na(loan$LoanAmount), mean, loan$LoanAmount)
summary(loan$Loan_Amount_Term)
loan$Loan_Amount_Term = ifelse(is.na(loan$Loan_Amount_Term), 360, loan$Loan_Amount_Term)
table(is.na(loan$Credit_History))
mean = mean(loan$Credit_History[!is.na(loan$Credit_History)])
loan$Credit_History = ifelse(is.na(loan$Credit_History), mean, loan$Credit_History)
colSums(is.na(loan))
mod1<-train(Loan_Status~.,method = "glm", data = loan)
test  <- read.csv('test.csv')
predict(mod1,test)







