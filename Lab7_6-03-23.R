#Naive Bayes
library(tidyverse)
data <- read.csv("penguins.csv")
#print(data)
data <- na.omit(data) #removing na values from dataset
data

library(e1071)
library(caTools)
library(caret)


split <- sample.split(data, SplitRatio = 0.75)
train_cl <- subset(data, split == "TRUE")
test_cl <- subset(data, split == "FALSE")

train_scale <- scale(train_cl[, 3:5]) #scaling based on numeric attributes
test_scale <- scale(test_cl[, 3:5])

set.seed(120) 
classifier_cl <- naiveBayes(species ~ ., data = train_cl)
classifier_cl

y_pred <- predict(classifier_cl, newdata = test_cl)
cm <- table(test_cl$species, y_pred)
cm
confusionMatrix(cm)

#Random forest
library(caTools)
library(randomForest)
data <- read.csv("accelerometer.csv")
data <- na.omit(data) #removing na values from dataset
data
split <- sample.split(data, SplitRatio = 0.8)

train <- subset(data, split == "TRUE")
test <- subset(data, split == "FALSE")

set.seed(120)  # Setting seed
classifier_RF = randomForest(x = train[-5], y = train$wconfid,ntree = 50)

classifier_RF

y_pred = predict(classifier_RF, newdata = test[-5])

confusion_matrix = table(test[, 1], y_pred)
confusion_matrix

plot(classifier_RF)
importance(classifier_RF)
varImpPlot(classifier_RF)

#SVM
library(caret)
library(e1071)
data <- read.csv("heart.csv")
data <- na.omit(data) #removing na values from dataset
data
set.seed(123)
split = sample.split(data$target, SplitRatio = 0.75)

training_set = subset(data, split == TRUE)
test_set = subset(data, split == FALSE)

#feature scaling the training and testing dataset
training_set[-14] = scale(training_set[-14])
test_set[-14] = scale(test_set[-14])

classifier = svm(formula = target ~ ., data = training_set,type = 'C-classification',kernel = 'linear')
classifier

y_pred = predict(classifier, newdata = test_set[-14])

cm = table(test_set[, 14], y_pred)
cm


