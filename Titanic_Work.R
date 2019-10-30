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
  mutate(Child=ifelse(FilledAge<16,TRUE,FALSE))%>%
  mutate(WCG=ifelse(Child==TRUE|Sex=="female",TRUE,FALSE))
full$Embarked <- replace(full$Embarked,which(is.na(full$Embarked)),"S")

#Splitting full data frame by who lived and who died for some visualization
full_survived <- full%>%filter(Survived==1)
full_died <- full%>%filter(Survived==0)

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

