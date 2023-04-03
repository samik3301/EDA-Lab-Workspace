library(caTools)
library(ROCR)
library(datasets)
library(party)

iris_df <-iris
head(iris_df)
#iris_df <-filter(iris_df,Species != "setosa") 
#removing setosa and trying the decision tree model with 2 classes in which one class is considered positive and other one is negative
divide <-sample.split(Y=iris_df$Species,SplitRatio= 0.7)
train <- iris_df[divide,] #70% of the dataset is used in training
test <-iris_df[!divide,] #30% of the dataset is used in testing


png(file = "decision_tree.png")

output.tree <- ctree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width , data = train)
summary(output.tree)
plot(output.tree)
dev.off()

tree_pred <- predict(output.tree,data= test)
tree_pred 

#assuming virginica to be positive class

accuracy <-mean(test$Species==tree_pred)
accuracy

#tree_pred is predicted value
#test$Species is actual value 

truepositive=sum(test$Species==tree_pred & tree_pred=="virginca")
truepositive
falsenegative=sum(test$Species==tree_pred & tree_pred=="versicolor")
falsenegative
recall=truepositive/(truepositive+falsenegative)
recall


falsepositive=sum(test$Species!=tree_pred & tree_pred=="virginica")
falsepositive
precision=truepositive/(truepositive+falsepositive)
precision


