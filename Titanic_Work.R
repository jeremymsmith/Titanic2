#This is a new attempt at the kaggle Titanic problem
#Using version control

#Loading Libraries
library(randomForest)
library(tree)
library(randomForest)
library(ISLR)
library(boot)
library(tidyverse)
library(caret)
library(ggplot2)

#Script that adds features to training data
#Stores featured data as "full"
full <- train
full <- full%>%mutate(HasCabin=(ifelse(Cabin=="",0,1)))%>%
  mutate(Famille=Parch+SibSp)%>%
  mutate(FilledAge=ifelse(is.na(Age),mean(Age,na.rm=TRUE),Age))%>%
  mutate(Child=ifelse(FilledAge<16,TRUE,FALSE))
full$Embarked <- replace(full$Embarked,which(is.na(full$Embarked)),"S")
full$Survived <- full$Survived%>%as.factor()
full$Age <- NULL
full$Ticket <- NULL
full$Name <- NULL
full$Cabin <- NULL

#Creating a training and testing subset randomly with features from full
training.samples <- full$Survived%>%
  createDataPartition(p=.25,list=FALSE)
full_train <- full[training.samples,]
full_test <- full[-training.samples,]

#Trying a random forest. 
rf.full_train <- randomForest(Survived~.-PassengerId,data=full_train,importance=TRUE)

#Testing random forest on full_test
mean(predict(rf.full_train,full_test)==full_test$Survived)

#Imported test into global environment. 
#Creating "final" for submission to kaggle
final <- test
final <- final%>%mutate(HasCabin=(ifelse(Cabin=="",0,1)))%>%
  mutate(Famille=Parch+SibSp)%>%
  mutate(FilledAge=ifelse(is.na(Age),mean(Age,na.rm=TRUE),Age))%>%
  mutate(Child=ifelse(FilledAge<16,TRUE,FALSE))
final$Embarked <- replace(final$Embarked,which(is.na(final$Embarked)),"S")
final$Age <- NULL
final$Ticket <- NULL
final$Name <- NULL
final$Cabin <- NULL

#I seem to need to do this to force everything to the same class
final <- rbind(full_train[1,-2],final)
final <- final[-1,]


Survived <- predict(rf.full_train, newdata = final)
final <- cbind(final,Survived)
Titanic15 <- cbind(final$PassengerId,final$Survived)%>%
  as.data.frame()%>%
  rename(PassengerId=V1)%>%
  mutate(Survived=V2-1)%>%
  select(1,3)

Titanic15[is.na(Titanic15)] <- 0

write_csv(Titanic15,"Titanic15.csv")

#Might attempt to add gradient boosting next. 

