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
View(full)

#Splitting full data frame by who lived and who died for some visualization
full_survived <- full%>%filter(Survived=="1")
full_died <- full%>%filter(Survived=="0")

#Count visualization of who survived and who died by Sex
ggplot(data=full_survived)+
  geom_bar(mapping=aes(x=Sex))
ggsave("Lived_Sex.png")
ggplot(data=full_died)+
  geom_bar(mapping=aes(x=Sex))
ggsave("Died_Sex.png")

#Same for Pclass. 
#It looks like Pclass might matter, but not as much as Sex.
hist(full_survived$Pclass)
ggsave("Lived_Class.png")
hist(full_died$Pclass)
ggsave("Died_Class.png")

#Creating a training and testing subset randomly with features from full
training.samples <- full$Survived%>%
  createDataPartition(p=.8,list=FALSE)
full_train <- full[training.samples,]
full_test <- full[-training.samples,]

#Trying a random forest. 
#Realized that I need to get rid of cat. variables with too many levels
#Also need to make sure that vectors don't have NAs
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
final <- final%>%select(1,12)
write_csv(final,"Titanic15.csv")


