# titanic.R
# Jing Sun (js6mj)
# Aug 29, 2018

library(caret)
library(tidyverse)

train <- read.csv('train.csv')
summary(train)

test <- read.csv('test.csv')
summary(test)

# fill in missing age
# will fill based on median age by pclass
train[is.na(train$Age) & train$Pclass=="1",]$Age = median(train[train$Pclass=="1",]$Age, na.rm=T)
train[is.na(train$Age) & train$Pclass=="2",]$Age <- median(train[train$Pclass=="2",]$Age, na.rm=T)
train[is.na(train$Age) & train$Pclass=="3",]$Age <- median(train[train$Pclass=="3",]$Age, na.rm=T)


set.seed(1996)
ind <- createDataPartition(train$Survived, p = 0.8, list = FALSE)
training <- train[ind,]
valid <- train[-ind,]

train_control <- trainControl(method="repeatedcv", number=10, repeats=5)
model1 <- train(as.factor(Survived) ~ as.factor(Pclass) + Sex + Age + Embarked + Fare, 
                data = training, method="glm", trControl=train_control)
summary(model1)

model1
# Accuracy   Kappa    
# 0.7843232  0.5329079

prediction1 <- predict(model1, valid)
table(valid$Survived, prediction1)
prediction1
#    0  1
# 0 96 13
# 1 19 50


resultmodel1 <- train(as.factor(Survived) ~ as.factor(Pclass) + Sex + Age + Embarked + Fare, 
                      data = train, method="glm", trControl=train_control)

# fill in missing age for test set
test[is.na(test$Age) & test$Pclass=="1",]$Age = median(test[test$Pclass=="1",]$Age, na.rm=T)
test[is.na(test$Age) & test$Pclass=="2",]$Age = median(test[test$Pclass=="2",]$Age, na.rm=T)
test[is.na(test$Age) & test$Pclass=="3",]$Age = median(test[test$Pclass=="3",]$Age, na.rm=T)

# find Pclass for the obs with missing Fare
test[is.na(test$Fare),]$Pclass  # 3
# fill in missing Fare with median fare for Pclass3
test[153,]$Fare <- median(test[test$Pclass=="3",]$Fare, na.rm=T)


Survived <- predict(resultmodel1, newdata = test)
result1 <- data.frame(PassengerId = test$PassengerId, Survived)
write.csv(result1, "js6mj_submission1.csv", row.names=FALSE)


# remove Embarked
resultmodel2 <- train(as.factor(Survived) ~ as.factor(Pclass) + Sex + Age + Fare, 
                      data = train, method="glm", trControl=train_control)
Survived <- predict(resultmodel2, newdata = test)
result2 <- data.frame(PassengerId = test$PassengerId, Survived)
write.csv(result2, "js6mj_submission2.csv", row.names=FALSE)
