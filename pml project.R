MyData <- read.csv(file="pml.csv")
dim(MyData)
str(MyData)
head(MyData)
summary(MyData)
table(is.na(MyData))
colSums(is.na(MyData))
library(dplyr)
library(ggplot2)
##remove N/A
train<- MyData[ , colSums(is.na(MyData)) == 0]
