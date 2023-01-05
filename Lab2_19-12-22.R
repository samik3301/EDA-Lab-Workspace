#Lab2 20BRS1205
#EDA Lab - Regression Analysis


#1. Take any synthetic dataset with a dependent and an independent variable

#i) Read the CSV form of the dataset
library(tidyverse)
data <- read.csv("https://gist.githubusercontent.com/nstokoe/7d4717e96c21b8ad04ec91f361b000cb/raw/bf95a2e30fceb9f2ae990eac8379fc7d844a0196/weight-height.csv")
print(data)

dataset_new <-select(data,Height,Weight)
dataset_new

#ii) Find the dimension
nrow(dataset_new)
ncol(dataset_new)
dim(dataset_new)
length(dataset_new)

#iii) Display the structure of the dataset
str(dataset_new)

#iv) Display the top 10 observation in the dataset
head(dataset_new,10)

#v) Display the summary statistics of the dependent variable
#considering Height as independent and Weight as dependent variable
summary(dataset_new$Weight)

#vi) Is there any outlier in the attributes?
#checking for the height
boxplot(dataset_new$Height,
        ylab = "Height"
)
#checking for the weight
boxplot(dataset_new$Weight,
        ylab = "Weight"
)

#as it can be infered from the boxplots there are some outliers in both height and weight

#vii) Perform 5 different visualisation plot to show the analytics of the dataset
dataset_new %>%
  ggplot(mapping = aes(x = Height, y = Weight))+
  geom_point()

dataset_new %>%
  ggplot(mapping = aes(x = Height, y = Weight))+
  geom_boxplot()

dataset_new %>%
  ggplot(mapping = aes(x = Height, y = Weight))+
  geom_line()

dataset_new %>%
  ggplot(mapping = aes(x = Height))+
  geom_histogram()

dataset_new %>%
  ggplot(mapping = aes(x = Weight))+
  geom_histogram()


#viii) Find the interquartile range of dependent variable
IQR(dataset_new$Weight)

#ix) See how the variables are correlation to each other
x <-dataset_new$Height
y <-dataset_new$Weight
x_mean <- mean(x)
y_mean <- mean(y)
x_mean
y_mean

df_copy = data.frame(x,y)
df_copy

df_copy<-mutate(df_copy,x_mean_dev = x-mean(x),y_mean_dev = y-mean(y),x_mean_dev_sq = x_mean_dev^2,y_mean_dev_sq=y_mean_dev^2,temp1 = x_mean_dev * y_mean_dev)
a_numerator = sum(df_copy$temp1)
a_denominator = sum(df_copy$x_mean_dev_sq)
coefficient_a = a_numerator/a_denominator
coefficient_a

coefficient_b = y_mean -coefficient_a*x_mean
coefficient_b

sprintf("The linear model equation is: Y= %f.x + %f ",coefficient_a,coefficient_b)


df_copy <-mutate(df_copy,y_pred = coefficient_a *x+ coefficient_b , error = y-y_pred,error_sq = error^2)
df_copy


r = sum(df_copy$temp1)/(sqrt((sum(df_copy$x_mean_dev_sq))*sum(df_copy$y_mean_dev_sq)))
r

if (r<1 & r>0){
  print("Strong correlation")
}else if(r<-1 & r<0){
  print("Weak correlation")
}else{
  print("No correlation")
}


#x) Compute the measure of dispersion
range(dataset_new)
IQR(dataset_new$Weight)
IQR(dataset_new$Height)
sd(dataset_new$Weight)
sd(dataset_new$Height)

#xi) Identify the relation between dependent and independent variable. Find the
#best fit line and visualize the regression.

dataset_new%>%
  ggplot(mapping=aes(x=Height, y=Weight)) +
    geom_point() +
    geom_smooth(method=lm, se=FALSE)

#xii) Display the regression coefficients using statistical approach
coefficient_a
coefficient_b

#xiii) Find R squared value
rss = sum(df_copy$error_sq)
tss = sum(df_copy$y_mean_dev_sq)

r_squared = 1 - (rss/tss)
r_squared

#xiv) Find Standard error
se = sd(dataset_new$Weight)/sqrt(length(dataset_new$Weight))
se

#xv) Find RSE
rse = sqrt((1/(n-2))*rss)
rse

#xvi) Find MAE
mean_absolute_error = 1/n * (sum(abs(df_copy$error)))
mean_absolute_error

#xvii) Find MAPE
n = nrow(df_copy)
n #number of rows or records

mean_absolute_percentage_error = 1/n * (sum((df_copy$error)/df_copy$y))
mean_absolute_percentage_error

#xviii) Find RMSE
root_mean_square_error =  sqrt(1/n *(sum(df_copy$error)))
root_mean_square_error


#2. Complete the above said tasks for a regression dataset from Kaggle or UCI
#repository. The dataset should be a latest one from 2020 with atleast 8 attributes
#and 250 instances.

#number of attributes = 13
#number of instances = 299
#year = 2020
uci_data <-read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00519/heart_failure_clinical_records_dataset.csv")
head(uci_data)

which(is.na(uci_data)) #no missing values in the dataset

#considering the age and the number of platelets as the attributes for the linear regression
dataset_uci <-select(uci_data,age,platelets)
dataset_uci


#ii) Find the dimension
nrow(dataset_uci)
ncol(dataset_uci)
dim(dataset_uci)
length(dataset_uci)

#iii) Display the structure of the dataset
str(dataset_uci)

#iv) Display the top 10 observation in the dataset
head(dataset_uci,10)

#v) Display the summary statistics of the dependent variable
#considering age as independent and platelets as dependent variable
summary(dataset_uci$platelets)

#vi) Is there any outlier in the attributes?
#checking for the age
boxplot(dataset_uci$age,
        ylab = "Age"
)
#checking for the platelets
boxplot(dataset_uci$platelets,
        ylab = "Platelets"
)

#there are some outlier in the dependent variable platelets here

#vii) Perform 5 different visualisation plot to show the analytics of the dataset
dataset_uci %>%
  ggplot(mapping = aes(x = age, y = platelets))+
  geom_point()

dataset_uci %>%
  ggplot(mapping = aes(x = age, y = platelets))+
  geom_boxplot()

dataset_uci %>%
  ggplot(mapping = aes(x = age, y = platelets))+
  geom_line()

dataset_uci %>%
  ggplot(mapping = aes(x = platelets))+
  geom_histogram()

dataset_uci %>%
  ggplot(mapping = aes(x = age))+
  geom_histogram()

#viii) Find the interquartile range of dependent variable
IQR(dataset_uci$platelets)

#ix) See how the variables are correlation to each other
x <-dataset_uci$age
y <-dataset_uci$platelets
x_mean <- mean(x)
y_mean <- mean(y)
x_mean
y_mean

df_copy = data.frame(x,y)
df_copy

df_copy<-mutate(df_copy,x_mean_dev = x-mean(x),y_mean_dev = y-mean(y),x_mean_dev_sq = x_mean_dev^2,y_mean_dev_sq=y_mean_dev^2,temp1 = x_mean_dev * y_mean_dev)
a_numerator = sum(df_copy$temp1)
a_denominator = sum(df_copy$x_mean_dev_sq)
coefficient_a = a_numerator/a_denominator
coefficient_a

coefficient_b = y_mean -coefficient_a*x_mean
coefficient_b

sprintf("The linear model equation is: Y= %f.x + %f ",coefficient_a,coefficient_b)


df_copy <-mutate(df_copy,y_pred = coefficient_a *x+ coefficient_b , error = y-y_pred,error_sq = error^2)
df_copy


r = sum(df_copy$temp1)/(sqrt((sum(df_copy$x_mean_dev_sq))*sum(df_copy$y_mean_dev_sq)))
r

if (r<1 & r>0){
  print("Strong correlation")
}else if(r<-1 & r<0){
  print("Weak correlation")
}else{
  print("No correlation")
}


#x) Compute the measure of dispersion
range(dataset_uci)
IQR(dataset_uci$age)
IQR(dataset_uci$platelets)
sd(dataset_uci$age)
sd(dataset_uci$platelets)


#xi) Identify the relation between dependent and independent variable. Find the
#best fit line and visualize the regression.

dataset_uci%>%
  ggplot(mapping=aes(x=age, y=platelets)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE)

#xii) Display the regression coefficients using statistical approach
coefficient_a
coefficient_b

#xiii) Find R squared value
rss = sum(df_copy$error_sq)
tss = sum(df_copy$y_mean_dev_sq)

r_squared = 1 - (rss/tss)
r_squared

#xiv) Find Standard error
se = sd(dataset_uci$platelets)/sqrt(length(dataset_uci$platelets))
se

#xv) Find RSE
rse = sqrt((1/(n-2))*rss)
rse

#xvi) Find MAE
mean_absolute_error = 1/n * (sum(abs(df_copy$error)))
mean_absolute_error

#xvii) Find MAPE
n = nrow(df_copy)
n #number of rows or records

mean_absolute_percentage_error = 1/n * (sum((df_copy$error)/df_copy$y))
mean_absolute_percentage_error

#xviii) Find RMSE
root_mean_square_error =  sqrt(1/n *(sum(df_copy$error)))
root_mean_square_error

