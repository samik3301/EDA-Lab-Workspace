#Lab3 20BRS1205  week1

#Correlation

#Compute the relationship that exists between each variable (x1, x2, x3) with target(y) of the given dataset by finding the Correlation. Out of the given three variables which one would you choose for modelling. Simulate the above task using R code.

library(tidyverse)
x1 <-c(1,3,4,5,6,7,8)
x2 <-c(8,7,5,6,4,3,2)
x3 <-c(8,2,4,6,3,7,5)
y <-c(2,5,7,11,12,15,17)

x1_y_cor_test = cor(x1,y,method = c("pearson"))
x1_y_cor_test
x2_y_cor_test = cor(x3,y,method = c("pearson"))
x2_y_cor_test
x3_y_cor_test = cor(x3,y,method = c("pearson"))
x3_y_cor_test

if(x1_y_cor_test>0 & x1_y_cor_test<0.5){
  print("Weak Correlation between the variables x1 and y")
}else
{
  print("Strong Correlation between the variables x1 and y")
}

if(x2_y_cor_test>0 & x2_y_cor_test<0.5){
  print("Weak Correlation between the variables x2 and y")
}else
{
  print("Strong Correlation between the variables x2 and y")
}

if(x3_y_cor_test>0 & x3_y_cor_test<0.5){
  print("Weak Correlation between the variables x3 and y")
}else
{
  print("Strong Correlation between the variables x3 and y")
}



#Correlation

# 2. Consider the following five training examples 
#X = [2 3 4 5 6] 
#Y = [12 17 23 28 32] 

x <-c(2,3,4,5,6)
y <-c(12,17,23,28,32)

# a)Find the best linear fit (Y=aX+b)
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


# b)Plot the graph of the model
df_copy %>%
  ggplot(mapping = aes(x = x, y = y))+
  geom_point()

# c)Determine the minimum RSS 
# RSS = sum[(y-y_pred) ^2]
df_copy = mutate(df_copy, y_pred = coefficient_a*x+coefficient_b)
df_copy
RSS = sum((df_copy$y- df_copy$y_pred)^2)
RSS

# d)Draw the residual plot for the best linear fit and comment on the suitability of the linear model to this training data.
df_copy%>%
  ggplot(mapping=aes(x=x, y=y)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE)


# e)Evaluate the standard errors associated with a and b.
rse = sqrt(RSS/(nrow(df_copy)-2))
se_a = rse * sqrt(1/nrow(df_copy) + ((x_mean)^2/(sum(df_copy$x_mean_dev_sq))))
se_a                 
  
se_b = rse * (1/sum(df_copy$x_mean_dev_sq))
se_b

# f)Determine the 95% confidence interval for a and b
print("The 95% confidence interval for a is ")
sprintf("%f %f",coefficient_a-2*se_a,coefficient_a+2*se_a)

print("The 95% confidence interval for b is ")
sprintf("%f %f",coefficient_b-2*se_b,coefficient_b+2*se_b)
  
# g)Compute R2 statistic
RSS
tss = sum(df_copy$y_mean_dev_sq)
r_squared = 1 - (RSS/tss)
r_squared

# h)Predict the value of a test instance from the dataset
print("Predicting the value of y for x = 8 ")
x =8
y = coefficient_a + coefficient_b*x
y


#Linear Regression
#. [till here first link submission]



#ANOVA [different link]

first_year <-c(82,93,61,74,69,53)
second_year <-c(62,85,94,78,71,66)
third_year <-c(64,73,87,91,56,78)
first_year_mean = mean(first_year)
first_year_mean
second_year_mean = mean(second_year)
third_year_mean = mean(third_year)

overall_mean = (first_year_mean+second_year_mean+third_year_mean)/3
overall_mean

df_new <-data.frame(first_year,second_year,third_year)
df_new

n=nrow(df_new)
c=3


df_new = mutate(df_new, first_mean_dev_sq = (first_year-mean(first_year))^2,second_mean_sq_dev=((second_year-mean(second_year))^2),third_mean_sq_dev= ((third_year-mean(third_year)))^2)
df_new

sse = sum(df_new$first_mean_dev_sq)+sum(df_new$second_mean_sq_dev)+sum(df_new$third_mean_sq_dev)
sse

ssc = n*((first_year_mean-overall_mean)^2) + n*((second_year_mean-overall_mean)^2) + n*((third_year_mean-overall_mean)^2)
ssc

mse = sse/(n*c-c)
mse
dof1= n*c-c
dof1
dof2 = c-1
dof2
msc = ssc/(c-1)
msc

F_calc = msc/mse
F_calc

f_critical=qf(0.05,dof2,dof1,lower.tail=FALSE) 
f_critical 

if(F_calc<f_critical)
{
  print("mean of scores employees is about same,therefore H0 is accepted")
} else {
  print("significant difference between employee scores mean,therefore H0 is rejected")
}


#Q2 2.  Apply ANOVA on the following datasets and state your inferences:
#1. PlantGrowth dataset


df_pg = PlantGrowth
df_pg
#State the Null and Alternate Hypothesis. 
#H0(Null Hypothesis) = there is no significant difference between the respective means 
#H1(Alternative hypothesis) = there is significant difference between the respective means

#Find Group wise Mean
group_wise_mean =group_by(df_pg,group) %>%
  summarise(count=n(),mean=mean(weight,na.RM= TRUE))
group_wise_mean

#Draw the Box Plot 
ggplot(data=df_pg, 
       mapping=aes(x=group, y=weight))+geom_boxplot()

#Apply ANOVA – One way 
anova_pg <- aov(weight~group,df_pg)
anova_pg
summary(anova_pg)
F_calc = summary(anova_pg)[[1]][1,4]
F_calc
dof1_pg = summary(anova_pg)[[1]][1,1]
dof2_pg =summary(anova_pg)[[1]][2,1]
f_critical_pg = qf(0.05,dof1_pg,dof2_pg,lower.tail=FALSE) 
f_critical_pg
if(F_calc<f_critical)
{
  print("mean of weights is about same,therefore H0 is accepted")
} else {
  print("significant difference between weight mean,therefore H0 is rejected")
}

#2. Poison Dataset
library(BHH2)
data("poison.data")
df_poison <- poison.data
head(df_poison,10)

#State the Null and Alternate Hypothesis. 
#H0(Null Hypothesis) = there is no significant difference between the respective means 
#H1(Alternative hypothesis) = there is significant difference between the respective means


#Find Group wise Mean
group_wise_mean =group_by(df_poison,poison) %>%
  summarise(count=n(),mean=mean(y,na.RM= TRUE))
group_wise_mean

#Draw the Box Plot 
ggplot(data=df_poison, 
       mapping=aes(x=poison, y=y))+geom_boxplot()


#Apply ANOVA – One way 
anova_poison <- aov(y~poison,df_poison)
anova_poison
summary(anova_poison)
F_calc = summary(anova_poison)[[1]][1,4]
F_calc
dof1_poison = summary(anova_poison)[[1]][1,1]
dof2_poison =summary(anova_poison)[[1]][2,1]
f_critical_poison = qf(0.05,dof1_poison,dof2_poison,lower.tail=FALSE) 
f_critical_poison

if(F_calc<f_critical)
{
  print("mean of poison groups is about same,therefore H0 is accepted")
} else {
  print("significant difference between poison group mean,therefore H0 is rejected")
}


