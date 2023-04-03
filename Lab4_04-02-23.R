#Simple moving average
year <-c(1,2,3,4,5,6,7,8,9,10,11,12)
Sales <-c(5.2,4.9,5.5,4.9,5.2,5.7,5.4,5.8,5.9,6,5.2,4.8)

df <-data.frame(year,Sales)

for(i in 1:12){
  df$MA[i]=(df$Sales[i-1]+df$Sales[i-2]+df$Sales[i-3])/3
}
df

#error metrics for simple moving average
df <-mutate(df, error = df$Sales-df$MA, abs_error = abs(error),error_sq = error^2,percent_error= abs(error/df$Sales)*100)
df


mean_absolute_error = mean(df$abs_error[4:12])
mean_absolute_error

mean_sq_error = mean(df$error_sq[4:12])
mean_sq_error

rmse = sqrt(mean_sq_error)
rmse

mape = mean(df$percent_error[4:12])
mape



#Weighted moving average 
weights <-c(1,2,1)
n=sum(weights)
year <-c(1,2,3,4,5,6,7,8,9,10,11,12)
Sales <-c(5.2,4.9,5.5,4.9,5.2,5.7,5.4,5.8,5.9,6,5.2,4.8)
df <-data.frame(year,Sales)
for(i in 1:12){
  df$MA[i]=(df$Sales[i-1]*weights[1]+df$Sales[i-2]*weights[2]+df$Sales[i-3]*weights[3])/n
}
df

#error metrics for weighted moving average

df <-mutate(df, error = df$Sales-df$MA, abs_error = abs(error),error_sq = error^2,percent_error= abs(error/df$Sales)*100)
df

mean_absolute_error = mean(df$abs_error[4:12])
mean_absolute_error

mean_sq_error = mean(df$error_sq[4:12])
mean_sq_error

rmse = sqrt(mean_sq_error)
rmse

mape = mean(df$percent_error[4:12])
mape



#Exponential smoothing

year <-c(1,2,3,4,5,6,7,8,9,10)
Sales <-c(30,25,35,25,20,30,35,40,30,45)

df <-data.frame(year,Sales)
alpha =0.3
df$predicted[1] = df$Sales[1]
for(i in 2:10){
  df$predicted[i] = df$predicted[i-1]*(1-alpha) + df$Sales[i-1]*(alpha)
}

df


#error metrics for exponential smoothing

df <-mutate(df, error = df$Sales-df$predicted, abs_error = abs(error),error_sq = error^2,percent_error= abs(error/df$Sales)*100)
df

mean_absolute_error = mean(df$abs_error[4:10])
mean_absolute_error

mean_sq_error = mean(df$error_sq[4:10])
mean_sq_error

rmse = sqrt(mean_sq_error)
rmse

mape = mean(df$percent_error[4:10])
mape


#Autocorrelation
y <-c(2,3,5,7,9,10)
df_auto_cor <-data_frame(y)
for(i in 2:6){
  mutate(df_auto_cor,y_shifted[i] = y[i-1])
}
df_auto_cor$y_shifted
df_new = data.frame(y,df_auto_cor$y_shifted)
df_new

df_new %>%
  mutate(y_mean_dev=y-mean(y),y_shifted_mean_dev=df_auto_cor$y_shifted-mean(y))

df_new<-mutate(df_new,y_mean_dev=y-mean(y),y_shifted_mean_dev=df_auto_cor$y_shifted-mean(y))
df_new
df_new<-mutate(df_new,mean_dev_product = y_mean_dev *y_shifted_mean_dev)

df_new <-mutate(df_new,y_mean_dev_sq = y_mean_dev^2)
df_new

sum(df_new$mean_dev_product,na.rm=TRUE)
sum(df_new$y_mean_dev_sq,na.rm=TRUE)
r_auto_cor = sum(df_new$mean_dev_product,na.rm=TRUE)/sum(df_new$y_mean_dev_sq,na.rm=TRUE)
r_auto_cor
