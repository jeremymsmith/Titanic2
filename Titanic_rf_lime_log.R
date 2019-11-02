#Here, I will use a random forest with lime to pick out important features.
#Then I will use log regression to build a model from those features.
#I will look at model p-values and test the model through cross-validation

#Loading Libraries
library(randomForest)
library(tree)
library(ISLR)
library(boot)
library(tidyverse)
library(caret)
library(ggplot2)
library(gbm)
library(lime)


#Script that adds features to training data
#Stores featured data as "full"
full <- train
full <- full%>%mutate(HasCabin=(ifelse(Cabin=="",0,1)))%>%
  mutate(Famille=Parch+SibSp)%>%
  mutate(FilledAge=ifelse(is.na(Age),mean(Age,na.rm=TRUE),Age))%>%
  mutate(Child=ifelse(FilledAge<16,TRUE,FALSE))%>%
  mutate(HasFamily=ifelse(Famille>0,TRUE,FALSE))%>%
  mutate(WCG=ifelse(Sex=="female"|Child==TRUE,TRUE,FALSE))
full$Embarked <- replace(full$Embarked,which(is.na(full$Embarked)),"S")
full$Survived <- full$Survived%>%as.factor()
full$Child <- full$Child%>%as.factor()
full$HasFamily <- full$HasFamily%>%as.factor()
full$WCG <- full$WCG%>%as.factor()
full$Age <- NULL
full$Ticket <- NULL
full$Name <- NULL
full$Cabin <- NULL
full$Famille <- NULL

#Creating a training and testing subset randomly
#For use with lime; final log model validated through LOOCV
training.samples <- full$Survived%>%
  createDataPartition(p=.8,list=FALSE)
full_train <- full[training.samples,]
full_test <- full[-training.samples,]

#Building random forest and then explaining with lime
rf.full <- train(full_train[,-2],full_train$Survived,method="rf")
explainer.rf <- lime(full_train,rf.full)
explanation.rf <- explain(full_test, explainer.rf, n_labels = 1, n_features = 4)
view(explanation.rf)

#On viewing the table, I notice that WCG > Sex > Pclass > everything else
#Also notice that Pclass is important when Pclass > 2
#Building logistic model based on these parameters. 
#Using threshold of >.1 for what features to include

full <- full%>%mutate(HighClass=ifelse(Pclass>2,TRUE,FALSE))
full$HighClass <- full$HighClass%>%as.factor()
log.full <- glm(Survived~WCG+Sex+HighClass,data=full,family="binomial")

#Need to go get bread and do things. 





