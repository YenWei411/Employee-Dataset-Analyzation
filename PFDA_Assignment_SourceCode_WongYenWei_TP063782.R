#Wong Yen Wei
#TP063782

#IMPORT DATA
#Read data from csv file
data=read.csv(file="E:\\Year 2\\Sem 1\\PFDA\\Asgnmt\\employee_attrition.csv", header = TRUE, sep = ",")

#CLEANING DATA
#Cleans data which have duplicated rows
data<-data[!duplicated(data$EmployeeID,fromLast = TRUE),]
#Clean data which have <na> rows
na.omit(data)
#Delete unnecessary data
data=select(data,-gender_short)
data=select(data,-STATUS_YEAR)

#Package installation, update and open
#install package
install.packages("lessR")
install.packages("tidyverse")
#Update packages before using
update.packages("dplyr")
update.packages("ggplot2")
update.packages("plotrix")
update.packages("lessR")
update.packages("tidyverse")
#Open packages
library(dplyr)
library(ggplot2)
library(plotrix)
library(lessR)
library(tidyverse)

##data manipulation
#Sort data
arrange(data)

##all active employee data
active_employee <- subset(data, Status == "ACTIVE")
##all terminated employee data
terminated_employee <- subset(data, Status == "TERMINATED")
##all layoff employee
layoff_employee = subset(data,Termination_Reason == "Layoff")
##all vancouver employee
vancouver_employee = subset(data,City == "Vancouver")
##all terminated meats department employee
terminated_meat = subset(terminated_employee,Department == "Meats")
##all layoff and resignation meats department employee
layoffresig_meat = terminated_meat[!(terminated_meat$Termination_Reason =="Retirement"),]
##all retirement meats department employee
retirement_meat = subset(terminated_meat,Termination_Reason =="Retirement")
##all terminated customer service department employee
terminated_cust_serv = subset(terminated_employee,Department == "Customer Service")
##all resignation and retirement customer service department employee
resigretire_custserv = terminated_cust_serv[!(terminated_cust_serv$Termination_Reason =="Layoff"),]
##all layoff customer service department employee
layoff_custserv = subset(terminated_meat,Termination_Reason =="Layoff")

View(active_employee)
View(terminated_employee)
View(layoff_employee)
View(vancouver_employee)
View(terminated_meat)
View(layoffresig_meat)
View(retirement_meat)
View(terminated_cust_serv)
View(resigretire_custserv)
View(layoff_custserv)

#Change data header to let analysis process easier to recognize
names(data) <- c('EmployeeID','Recode_Date','Birth_Date','Orignal_Hire_Date'
                 ,'Termination_Date','Age','Length_Of_Service','City','Department'
                 ,'Job','Store_ID','Gender','Termination_Reason','Termination_Type','Status','Business_Unit')

#Save to file
save(data,file="myfile.Rdata")
save(active_employee,file="myfile.Rdata")
save(terminated_employee,file="myfile.Rdata")
save(layoff_employee,file="myfile.Rdata")
save(vancouver_employee,file="myfile.Rdata")
save(terminated_meat,file="myfile.Rdata")
save(layoffresig_meat,file="myfile.Rdata")
save(retirement_meat,file="myfile.Rdata")
save(terminated_cust_serv,file="myfile.Rdata")
save(resigretire_custserv,file="myfile.Rdata")
save(layoff_custserv,file="myfile.Rdata")

#DATA EXPLORATION
#Load R data
load("myfile.Rdata")
#Find header
names(data)
#Length of data
length(data)
ncol(data)
nrow(data)
#Data class
class(data)
#show data frame
range(data$EmployeeID)
range(data$Recode_Date)
range(data$Birth_Date)
range(data$Orig_Hire_Date)
range(data$Termination_Date)
range(data$Age)
range(data$Length_of_Service)
#Levels of data
levels(factor(data$City))
levels(factor(data$Department))
levels(factor(data$Job))
levels(factor(data$Store_ID))
levels(factor(data$Gender))
levels(factor(data$Termination_Reason))
levels(factor(data$Termination_Type))
levels(factor(data$Status))
levels(factor(data$Business_Unit))

#number of levels of data
nlevels(factor(data$City))
nlevels(factor(data$Department))
nlevels(factor(data$Job))
nlevels(factor(data$Store_ID))
nlevels(factor(data$Gender))
nlevels(factor(data$Termination_Reason))
nlevels(factor(data$Termination_Type))
nlevels(factor(data$Status))
nlevels(factor(data$Business_Unit))

##Summary of data
summary(data)

##unload library after analysis
detach("package:dplyr",unload=TRUE)
detach("package:ggplot2",unload=TRUE)
detach("package:plotrix",unload=TRUE)
detach("package:lessR",unload=TRUE)
detach("package:tidyverse",unload=TRUE)



##Q1 What is the current status of the company?
#1.1 The relationship between employees and their termination reason
ggplot(data, 
       mapping = aes(x = Gender, fill = Termination_Reason)) + 
  labs(x = "Age",
       y = "Number_of_Staff") + 
  geom_bar() +
  theme_bw() +
  facet_wrap(~Status) +
  geom_text(stat ='count',aes(label=..count..),vjust=-1)

#1.2 Relationship between terminated employees and their job. 
ggplot(terminated_employee, 
       mapping = aes(y = Job, fill = Termination_Reason)) + 
  labs(x = "Terminated",
       y = "Job") + 
  geom_bar() +
  theme_bw() +
  facet_wrap(~Termination_Reason) +
  geom_text(stat ='count',aes(label=..count..),vjust=-1)

#1.3 Relationship between terminated employees and their department. 
ggplot(terminated_employee, 
       mapping = aes(y = Termination_Reason, fill = Department)) + 
  labs(x = "Department",
       y = "Termination Reason") + 
  geom_bar() +
  theme_bw() +
  facet_wrap(~Department) +
  geom_text(stat ='count',aes(label=..count..),vjust=-0.3)

#1.4 Relationship between employees and their city.
ggplot(terminated_employee, 
       mapping = aes(y = Termination_Reason, fill = Business_Unit)) + 
  labs(x = "City",
       y = "Termination Reason") + 
  geom_bar() +
  theme_bw() +
  facet_wrap(~City) +
  geom_text(stat ='count',aes(label=..count..),vjust=-1)

##Q2 Why are they lay-offing the employees
#2.1 Relationship between terminated employees' gender and their job
ggplot(terminated_employee, 
       mapping = aes(y = Job, fill = Gender)) + 
  labs(x = "Number of Staff",
       y = "Job") + 
  geom_bar(position = "dodge") +
  theme_bw() +
  geom_text(stat ='count',aes(label=..count..),vjust=-1)

#2.2 Relationship between terminated employee's job and their department
ggplot(terminated_employee, 
       mapping = aes(y = Department, fill = Job)) + 
  labs(x = "Number of Employee",
       y = "Department") + 
  geom_bar(position = "dodge") +
  theme_bw() +
  facet_wrap(~Termination_Reason) +
  geom_text(stat ='count',aes(label=..count..),vjust=-1)

#2.3 Relationship between terminated employee and their job
ggplot(terminated_employee, 
mapping = aes(y = Job, x = Age, fill = Termination_Reason)) + 
  labs(x = "Age",
       y = "Job") + 
  geom_point(size = 1) +
  theme_bw() +
  facet_wrap(~Termination_Reason)

#2.4 Relationship between terminated employee and their age
ggplot(terminated_employee, 
       mapping = aes(x = Age, fill = Gender)) + 
  labs(y = "Number of Staff",
       x = "Age") + 
  geom_bar(position = "dodge") +
  theme_bw() +
  geom_text(stat ='count',aes(label=..count..),vjust=-1,position = position_dodge())

##Q3 Why Vancouver has the most employee
#3.1 Relation ship between age and vancouver's employee
ggplot(vancouver_employee, 
       mapping = aes(y = Age, fill = Gender)) + 
  labs(x = "Total_Staff",
       y = "Age") + 
  geom_bar(position = "dodge") +
  theme_bw() +
  geom_text(stat = 'count',aes(label=..count..),vjust=-1)

#3.2 Relationship between length of service and Vancouver's employee
ggplot(vancouver_employee, 
       mapping = aes(x = Length_Of_Service, fill = Gender)) + 
  labs(x = "Length_Of_Service",
       y = "Total_Staff") + 
  geom_bar(position = "dodge") +
  theme_bw() +
  geom_text(stat = 'count',aes(label=..count..),vjust=-1,position = position_dodge(width = 1),inherit.aes = TRUE)

#3.3 Relationship between business unit and Vancouver's employee
ggplot(vancouver_employee, 
       mapping = aes(x = Business_Unit, fill = Gender)) + 
  labs(x = "Business_Unit",
       y = "Total_Staff") + 
  geom_bar(position = "dodge") +
  theme_bw() +
  geom_text(stat = 'count',aes(label=..count..),vjust=-0.5,hjust = 1,position = position_dodge(width = 1))

#3.4 Relationship between stores and employee gender in vancouver
ggplot(vancouver_employee, 
       mapping = aes(x = Store_ID, fill = Business_Unit)) + 
  labs(x = "Store_ID",
       y = "Total_Staff") + 
  geom_bar(position = "dodge") +
  theme_bw() +
  facet_wrap(~Gender) + 
  geom_text(stat = 'count',aes(label=..count..),vjust=-0.5,hjust = 1,position = position_dodge(width = 1),inherit.aes = TRUE)

##Q4 why meat department have the most terminated employee, What are their age and what 
#4.1 Relationship between termination reason and retired meat department employees' age
ggplot(layoffresig_meat, 
       mapping = aes(x = Age, fill = Termination_Reason)) + 
  labs(x = "Age",
       y = "Total_Staff") + 
  geom_bar(position = "dodge") +
  theme_bw() +
  facet_wrap(~Termination_Reason) + 
  geom_text(stat = 'count',aes(label=..count..),vjust=-0.5,hjust = 0.5,position = position_dodge(width = 2),inherit.aes = TRUE)

ggplot(retirement_meat, 
       mapping = aes(x = Age, fill = Termination_Reason)) + 
  labs(x = "Age",
       y = "Total_Staff") + 
  geom_bar(position = "dodge") +
  theme_bw() +
  facet_wrap(~Termination_Reason) + 
  geom_text(stat = 'count',aes(label=..count..),vjust=-0.5,hjust = 0.5,position = position_dodge(width = 2),inherit.aes = TRUE)

#4.2 Relationship between meat department's jobs and employee gender
ggplot(terminated_meat, 
       mapping = aes(x = Job, fill = Gender)) + 
  labs(x = "Meats Department's Job",
       y = "Total_Staff") + 
  geom_bar(position = "dodge") +
  theme_bw() +
  facet_wrap(~Termination_Reason) + 
  geom_text(stat = 'count',aes(label=..count..),vjust=-0.5,hjust = 0.5,position = position_dodge(width = 1),inherit.aes = TRUE)

#4.3 Relationship between length of service of terminated meat department employee and their termination reason
ggplot(layoffresig_meat, 
       mapping = aes(x = Length_Of_Service, fill = Termination_Reason)) + 
  labs(x = "Length of Service",
       y = "Terminated Staff") + 
  geom_bar() +
  theme_bw() +
  facet_wrap(~Termination_Reason) + 
  geom_text(stat = 'count',aes(label=..count..),vjust=-0.5,hjust = 0.5,position = position_dodge(width = 1),inherit.aes = TRUE)

ggplot(retirement_meat, 
       mapping = aes(x = Length_Of_Service, fill = Termination_Reason)) + 
  labs(x = "Length of Service",
       y = "Terminated Staff") + 
  geom_bar() +
  theme_bw() +
  facet_wrap(~Termination_Reason) + 
  geom_text(stat = 'count',aes(label=..count..),vjust=-0.5,hjust = 0.5,position = position_dodge(width = 1),inherit.aes = TRUE)

##Q5 Why customer service has the most layoff employee
#5.1 Relationship between termination reason and layoff customer service department employees' age
ggplot(resigretire_custserv, 
       mapping = aes(x = Age, fill = Termination_Reason)) + 
  labs(x = "Age",
       y = "Total Staff") + 
  geom_bar(position = "dodge", width = 1) +
  theme_bw() +
  facet_wrap(~Termination_Reason) + 
  geom_text(stat = 'count',aes(label=..count..),vjust=-0.5,hjust = 0.5,position = position_dodge(width = 2),inherit.aes = TRUE)

ggplot(layoff_custserv, 
       mapping = aes(x = Age, fill = Termination_Reason)) + 
  labs(x = "Age",
       y = "Total Staff") + 
  geom_bar(position = "dodge") +
  theme_bw() +
  facet_wrap(~Termination_Reason) + 
  geom_text(stat = 'count',aes(label=..count..),vjust=-0.5,hjust = 0.5,position = position_dodge(width = 2),inherit.aes = TRUE)

#5.2 Relationship between customer service department's jobs and employee gender
ggplot(terminated_cust_serv, 
       mapping = aes(x = Job, fill = Gender)) + 
  labs(x = "Customer Service Department's Job",
       y = "Total Staff") + 
  geom_bar(position = "dodge") +
  theme_bw() +
  facet_wrap(~Termination_Reason) + 
  geom_text(stat = 'count',aes(label=..count..),vjust=-0.5,hjust = 0.5,position = position_dodge(width = 1),inherit.aes = TRUE)

#5.3 Relationship between length of service of terminated customer service department employee and their termination reason
ggplot(resigretire_custserv, 
       mapping = aes(x = Length_Of_Service, fill = Termination_Reason)) + 
  labs(x = "Length of Service",
       y = "Terminated Staff") + 
  geom_bar() +
  theme_bw() +
  facet_wrap(~Termination_Reason) + 
  geom_text(stat = 'count',aes(label=..count..),vjust=-0.5,hjust = 0.5,position = position_dodge(width = 1),inherit.aes = TRUE)

ggplot(layoff_custserv, 
       mapping = aes(x = Length_Of_Service, fill = Termination_Reason)) + 
  labs(x = "Length of Service",
       y = "Terminated Staff") + 
  geom_bar() +
  theme_bw() +
  facet_wrap(~Termination_Reason) + 
  geom_text(stat = 'count',aes(label=..count..),vjust=-0.5,hjust = 0.5,position = position_dodge(width = 1),inherit.aes = TRUE)