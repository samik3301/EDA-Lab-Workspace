library(caTools)
library(ROCR)
library(datasets)
library(tidyverse)

iris_df <-iris
#iris_df

sum(is.na(iris_df))
head(iris_df)
iris_df <-filter(iris_df,Species != "setosa") 
#removing setosa and trying the logistic regression model with 2 classes
nrow(iris_df)

head(iris_df)

ggplot(data = iris_df, aes(x = Sepal.Length, y = Sepal.Width)) + 
    geom_point(aes(color = Species))



ggplot(data = iris_df, aes(x = Petal.Length, y = Petal.Width)) + 
     geom_point(aes(color = Species))

#from these 2 plots we can infer that Petal.Length and Petal.Width would yield better results as potential predictors
divide <-sample.split(Y=iris_df$Species,SplitRatio= 0.7)
train <- iris_df[divide,] #70% of the dataset is used in training
test <-iris_df[!divide,] #30% of the dataset is used in testing


model <- glm(Species ~ Petal.Length + Petal.Width, data=train, family = binomial)
summary(model)

result_predictions <-predict(model,test,type ="response")
head(result_predictions)

species_pred=ifelse(result_predictions>0.5,1,0)
species_pred


species=1*(test$Species=="virginica")+0
species

accuracy <-mean(species==species_pred)
accuracy

truepositive=sum(species==species_pred & species==1)
truepositive
falsenegative=sum(species!=species_pred & species==1)
falsenegative
recall=truepositive/(truepositive+falsenegative)
recall


falsepositive=sum(species!=species_pred & species_pred==1)
falsepositive
precision=truepositive/(truepositive+falsepositive)
precision




# Question 2
X <- c(81,42,61,59,78,49)
Y <-c("pass","fail","pass","fail","pass","fail")
b0 <-c(-199,-120,-121)
accuracy = 0
b0_param = 1
for(i in b0){
  y_pred <-c()
  for(j in X){
    e <-exp(i + 2*j)
    y_output <- e/(e+1)
    if(y_output>=0.5){
      y_pred <-c(y_pred,"pass")
    }else{
      y_pred <-c(y_pred,"fail")
    }
  }
  calc_accuracy = mean(y_pred ==Y)
  if(calc_accuracy>accuracy){
    accuracy = calc_accuracy
    b0_param = i
  }
}

sprintf("b0 = %.0f and b1 = 2",b0_param)
x = 1/2 *(0.95/0.05-b0_param)
sprintf("The minimum mark to ensure that student gets Pass grade with 0.95 probability is %f ",x)
