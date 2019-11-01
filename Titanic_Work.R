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
library(gbm)

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

full_female <- full%>%filter(Sex=="female")
full_male <- full%>%filter(Sex=="male")

#Building separate models 
rf.full <- randomForest(Survived~.,data=full,importance=TRUE)
boost.full <-  gbm(Survived ~.,
              data = full,
              distribution = "multinomial",
              shrinkage = .02,
              n.minobsinnode = 100,
              n.trees = 5000)
log.full <- glm(Survived~WCG+Pclass,data=full,family="binomial")
log.full_female <- glm(Survived~Pclass,data=full, family="binomial")
log.full_male <- glm(Survived~Pclass+Child,data=full, family="binomial")

summary(log.full)
summary(log.full_female)
summary(log.full_male)

#Imported test into global environment. 
#Creating "final" for submission to kaggle
final <- test
final <- final%>%mutate(HasCabin=(ifelse(Cabin=="",0,1)))%>%
  mutate(Famille=Parch+SibSp)%>%
  mutate(FilledAge=ifelse(is.na(Age),mean(Age,na.rm=TRUE),Age))%>%
  mutate(Child=ifelse(FilledAge<16,TRUE,FALSE))%>%
  mutate(HasFamily=ifelse(Famille>0,TRUE,FALSE))%>%
  mutate(WCG=ifelse(Sex=="female"|Child==TRUE,TRUE,FALSE))
final$Embarked <- replace(final$Embarked,which(is.na(final$Embarked)),"S")
final$Child <- final$Child%>%as.factor()
final$HasFamily <- final$HasFamily%>%as.factor()
final$WCG <- final$WCG%>%as.factor()
final$Age <- NULL
final$Ticket <- NULL
final$Name <- NULL
final$Cabin <- NULL
final$Famille <- NULL

final_female <- final%>%filter(Sex=="female")
final_male <- final%>%filter(Sex=="male")

#Making predictions onto kaggle test data frame

pred.boost <-  predict.gbm(object = boost.full_train,
                                newdata = final,
                                n.trees = 5000,
                                type = "response")
Survived.boost <-  colnames(pred.boost)[apply(pred.boost, 1, which.max)]

log.probs <- predict(log.full,final,type="response")
log.pred <- rep(0,418)
log.pred[log.probs>.5] <- 1
Survived.log <- log.pred

log.probs_female <- predict(log.full_female,final,type="response")
log.pred_female <- rep(0,418)
log.pred_female[log.probs_female>.5] <- 1
Survived_female.log <- log.pred_female

log.probs_male <- predict(log.full_male,final,type="response")
log.pred_male <- rep(0,418)
log.pred_male[log.probs_male>.5] <- 1
Survived_male.log <- log.pred_male

final <- rbind(full[1,-2],final)
final <- final[-1,]
Survived.rf <- predict(rf.full,final)
Survived.rf <- as.numeric(Survived.rf)-1
Survived.boost <- as.numeric(Survived.boost)

final <- final%>%
  data.frame(Survived.boost)%>%
  data.frame(Survived.rf)%>%
  data.frame(Survived.log)%>%
  data.frame(Survived_male.log)%>%
  data.frame(Survived_female.log)%>%
  mutate(Survival_Mean=(Survived.boost+
                          Survived.rf+
                          Survived.log+
                          Survived_male.log+
                          Survived_female.log)/5)%>%
  mutate(Survived=ifelse(Survival_Mean>.5,1,0))

 

Titanic16 <- cbind(final$PassengerId,final$Survived)%>%
  as.data.frame()%>%
  rename(PassengerId=V1)%>%
  mutate(Survived=V2)%>%
  select(1,3)

Titanic16[is.na(Titanic16)] <- 0



write_csv(Titanic16,"Titanic16.csv")

#Next thing to try: Mixed method, including...
#logistic regression (WCG), random forest, split/logistic, and gradient boost
