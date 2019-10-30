#I am going to try some random forests and boosts in this workspace. 

library(randomForest)
install.packages("tree")
library(tree)
install.packages("randomForest")
library(randomForest)
library(ISLR)
library(boot)
library(tidyverse)
library(caret)

rm(full)

#Copying variable creation from TW2, and modifying for this workspace.
full <- train
full <- full%>%mutate(HasCabin=(ifelse(Cabin=="",0,1)))%>%
  mutate(Famille=Parch+SibSp)%>%
  mutate(FilledAge=ifelse(is.na(Age),mean(Age,na.rm=TRUE),Age))%>%
  mutate(Child=ifelse(FilledAge<16,TRUE,FALSE))%>%
  mutate(WCG=ifelse(Child==TRUE|Sex=="female",TRUE,FALSE))

full$Embarked <- replace(full$Embarked,which(is.na(full$Embarked)),"S")

#Let's split this into survivors and not survivors this time
full_survived <- full%>%filter(Survived==1)
full_died <- full%>%filter(Survived==0)

#Now, let's look at histograms and descriptions of the variables
ggplot(data=full_survived)+
  geom_bar(mapping=aes(x=Sex))
ggplot(data=full_died)+
  geom_bar(mapping=aes(x=Sex))

hist(full_survived$Pclass)
hist(full_died$Pclass)
hist(full$Pclass)
mean(full_survived$Pclass)
mean(full_died$Pclass)

hist(full_survived$FilledAge,breaks=16)
hist(full_died$FilledAge,breaks=16)
hist(full$FilledAge,breaks=16)
mean(full_survived$FilledAge)
mean(full_died$FilledAge)
median(full_survived$FilledAge)
median(full_died$FilledAge)

hist(full_survived$Fare)
hist(full_died$Fare)

hist(full_survived$SibSp,breaks=12)
hist(full_died$SibSp,breaks=12)
mean(full_survived$SibSp)
mean(full_died$SibSp)

ggplot(data=full_survived)+
  geom_bar(mapping=aes(Child))
ggplot(data=full_died)+
  geom_bar(mapping=aes(Child))

ggplot(data=full_survived)+
  geom_bar(mapping=aes(WCG))
ggplot(data=full_died)+
  geom_bar(mapping=aes(WCG))

ggplot(data=full_survived)+
  geom_bar(mapping=aes(HasCabin))
ggplot(data=full_died)+
  geom_bar(mapping=aes(WCG))

#Creating a training and testing subset randomly
training.samples <- full$Survived%>%
  createDataPartition(p=.8,list=FALSE)
full_train <- full[training.samples,]

#Ready to grow forests?
full_test <- full[-training.samples,]