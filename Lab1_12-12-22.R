#Lab1 20BRS1205

#EDA Lab 1 Basics of FDA- Revision

#1. Create the following
#a. Create a character vector contains first 10 alphabets in English
First <-c(letters[seq(from=1,to=10)])

#alternatively 
First_alt <-c("a","b","c","d","e","f","g","h","i","j")

#b. Create a character vector contains Next 10 alphabets in English
Middle <-c(letters[seq(from=11,to=20)])

#alternatively
Middle_alt <-c("k","l","m","n","o","p","q","r","s","t")

#c. Create a character vector contains with rest of the alphabets in English
#Name then First, Middle and Last
Last <-c(letters[seq(from=21,to=26)])

#alternatively
Last_alt <-c("u","v","w","x","y","z")

#Combine all the results in a single list.
result <-c(First,Middle,Last)
result

#alternatively
result_alt <-c(First,Middle,Last)
result_alt


#2. Using R check if the given number is Armstrong or not (use for loop without builtin methods)

#using for loop
num = as.integer(readline(prompt="Enter a number to be checked : "))
temp = num
sum =0
for(i in 1:n){
  if(temp>0){
    digit = temp %% 10
    sum = sum + (digit ^ 3)
    temp = floor(temp / 10)
  }
  else{
    break
  }
}
if(num == sum) {
  print(paste(num, "is an Armstrong number"))
} else {
  print(paste(num, "is not an Armstrong number"))
}


#using while loop
num = as.integer(readline(prompt="Enter a number to be checked : "))
sum = 0
temp = num
while(temp > 0) {
  digit = temp %% 10
  sum = sum + (digit ^ 3)
  temp = floor(temp / 10)
}

if(num == sum) {
  print(paste(num, "is an Armstrong number"))
} else {
  print(paste(num, "is not an Armstrong number"))
}


# 3. compute the sum of the series: 

sum =0
for(i in 1:25){
  f1 = (2^i)/i
  f2 =(4^i)/2*i
  term = f1+f2
  sum = sum+term
}

print(paste("The sum of the given series is",sum))


# 4. Create a pattern that generates the following
#(A1, A2, A3,………..,A30) separated with single space.
for (i in 1:30){
  cat('A',i," ",sep="")
}

 #another approach
pattern <-c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12","A13","A14","A15","A16","A17","A18","A19","A20","A21","A22","A23","A24","A25","A26","A27","A28","A29","A30")
for (x in pattern){
  cat(x," ",sep="")
}

#5. Create a random vector X and Y and perform various functions sort, order, mean, sum and sqrt.
X<-sample(1:100,30,replace =TRUE)
Y<-sample(1:100,30,replace= TRUE)

X_sorted <- lapply(X,sort)
Y_sorted <- lapply(Y,sort)

df <-data.frame(X,Y)
X_ranked<-rank(df$X)
Y_ranked <-rank(df$Y)

X_mean <-mean(X)
Y_mean <-mean(Y)

X_sum <-sum(X)
Y_sum <-sum(Y)

X_sqrt <-sqrt(X)
Y_sqrt <-sqrt(Y)


#6. Create a categorical R object other than dataframe that includes the following details of 5 students.
#{RegisterNo, Name, Subjects, YearofStudy, Name of the Department, Institution}


dt = data.table(
  RegisterNo <-c("20BRS1201","20BRS1202","20BRS1203","20BRS1204","20BRS1205"),
  Name <-c("dummy1","dummy2","dummy3","dummy4","dummy5"),
  Subjects <-c("subject1","subject2","subject3","subject4","subject5"),
  YearOfStudy <-c(2022,2023,2022,2021,2022),
  NameofDept <-c("Dept1","Dept2","Dept3","Dept4","Dept5"),
  Inst <-c("inst1","inst2","inst3","inst4","inst5")
)
dt


#7. Create an nominal matrix named Results that includes {“Pass”,”Fail”, “Pass”, ”Fail”, ”Fail”, ”Fail”, “Pass”, “Pass”, ”Fail”, ”Fail”, “Pass”} and display the same
#Display the count of each category of the above create nominal matrix
data<-c("Pass","Fail", "Pass", "Fail", "Fail", "Fail", "Pass", "Pass", "Fail", "Fail", "Pass")
M <- matrix(data, nrow = 1, byrow = TRUE)
print(M)
count_of_each <-table(data)
count_of_each

#8. Create a dataframe named student from the below three vectors.

#a. Name {“Aby”,“Arya”, “Ash”, "Adhi"}
#b. Age {20, 19, 19, 20}
#c. Number {“18MIS2022”, “ 18MIS2012”, “18MIS0022”, “18MIS0002”}
Name <-c("Aby","Arya","Ash","Adhi")
Age <-c(20,19,19,20)
Number <-c("18MIS2022","18MIS2012","18MIS0022","18MIS0002")
student <-data.frame(Name,Age,Number)
student

#Perform the following for the created dataframe
#d. Head()
head(student)

#e. Tail()
tail(student)

#f. Summary
summary(student)

#g. Str()
str(student)


#9. A company maintains the details of the employees as shown in Table
#Write R code for the following:

#(i) Create a data frame for the details given in Table
library(tidyverse)
Dept <- c("Software","Hardware","Finance","Software","Hardware","Finance")
Name <-c("AAA","BBB","CCC","DDD","EEE","FFF")
Gender <-c("F","M","F","F","M","F")
Number_hrs_worked <-c(80,88,98,95,76,43)
Wage_per_hr <-c(3000,2500,1500,2000,1500,1000)

employee <-data.frame(Dept,Name,Gender,Number_hrs_worked,Wage_per_hr)
employee


#(ii) Add a field ‘payroll’ which has the salary of the employee obtained by no. of hours worked and wage per hour.
employee %>%
  mutate(payroll = Number_hrs_worked*Wage_per_hr)
employee

#(iii) Display the total salary of male and female employees in each department.
employee %>%
  mutate(payroll = Number_hrs_worked*Wage_per_hr)%>%
  filter(Gender =="M")%>%
  summarise(sum_of_males<-sum(payroll)) 

employee %>%
  mutate(payroll = Number_hrs_worked*Wage_per_hr)%>%
  filter(Gender =="F")%>%
  summarise(sum_of_males<-sum(payroll)) 


#(iv) Access the fields name and payroll by indexing.
employee %>%
  mutate(payroll = Number_hrs_worked*Wage_per_hr)%>%
  select(Name,payroll)


#(v) Find the name and department of the person who is drawing a maximum salary.
employee %>%
  mutate(payroll = Number_hrs_worked*Wage_per_hr)%>%
  filter(payroll==max(payroll))%>%
  select(Name,Dept)
  
#10. Consider the information about Sports Club:
#Write R code to perform the following

RId <-c("R1","R5","R3","R7","R8","R2","R4","R6")
Fname <-c("Akash","Soorya","Balaji","Yash","Raju","Amrita","Sree","Sathya")
Lname <-c("Kumar","Prasad","Agarwal","Gupta","K","Lakshmi","Ramya","PRiya")
Education <-c("B.A","B.Tech","X","XII","MCA","BSc","VIII","BA")
Age <-c(19,20,15,17,22,18,12,20)
Location <-c("Medavakam","Velachery","Pallavaram","Guindy","Chrompet","Velarchery","Tamabaram","Guindy")
Gender <-c("M","M","M","M","M","F","F","F")
Club <-c("Football","Basketball","Badminton","Volleyball","Cricket","Football","Basketball","Badminton")
DateOfJoining <-c("28/02/2019","12/07/2020","01/05/2015","22/02/2021","11/11/2013","28/02/2016","12/07/2020","01/05/2015")
Level <-c("Beginner","Medium","Expert","Beginner","Expert","Medium","Beginner","Expert")

sports_club <-data.frame(RId,Fname,Lname,Education,Age,Location,Gender,Club,DateOfJoining,Level)
sports_club

#a) List the name and location of students enrolled (age above 17) belonging to Football.
sports_club %>%
  filter(Age>17 & Club=="Football")%>%
  select(Fname,Lname,Location)

#b) Count the level earned in each club by the students.
sports_club %>%
  count(Level,Club)

#c) List the name and date of joining of all girls in various club.
sports_club %>%
  filter(Gender == "F")%>%
  select(Fname,Lname,DateOfJoining)

#d) List the First names of students enrolled from Guindy.
sports_club %>%
  filter(Location =="Guindy") %>%
  select(Fname)

#e) List the DateOfJoining of the students enrolled whose qualification starts with letter ‘B’.
sports_club %>%
  filter(Education == "B.A" | Education == "B.Tech" | Education =="BSc" | Education == "BA")%>%
  select(DateOfJoining)
  
