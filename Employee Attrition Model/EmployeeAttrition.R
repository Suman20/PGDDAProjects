library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(lubridate)
library(dplyr)
library(tidyr)
library(scales)
library(ROCR)
employee_survey<-read.csv("employee_survey_data.csv",stringsAsFactors = F)
general<- read.csv("general_data.csv",stringsAsFactors = F)
in_time<- read.csv("in_time.csv",stringsAsFactors = F)
manager_survey<- read.csv("manager_survey_data.csv",stringsAsFactors = F)
out_time<- read.csv("out_time.csv",stringsAsFactors = F)

########################## Data Preparation & Exploratory Data Analysis###################

# cleaning employee_survey data set.
#------------------------------------
#Checking for duplicates
anyDuplicated(employee_survey)
sum(duplicated(employee_survey$EmployeeID))


# Treaing NA values
#Creating a function to calculate mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#Retrieving columns with NA values.
sapply(employee_survey, function(x) sum(is.na(x)))
indices<- which(is.na(employee_survey$EnvironmentSatisfaction))

#We would first get the vector of EnvironmentSatisfaction which does not have any NA values
temp_EnvironmentSatisfaction <- employee_survey$EnvironmentSatisfaction[which(!is.na(employee_survey$EnvironmentSatisfaction))]

#Now , we will replace the NA values with the Mode of the EnvironmentSatisfaction
mode_EnvironmentSatisfaction<- Mode(temp_EnvironmentSatisfaction)
employee_survey$EnvironmentSatisfaction[which(is.na(employee_survey$EnvironmentSatisfaction))] <- mode_EnvironmentSatisfaction

#Similarly , We would get the vector of JobSatisfaction which does not have any NA values
temp_JobSatisfaction <- employee_survey$JobSatisfaction[which(!is.na(employee_survey$JobSatisfaction))]

#Now , we will replace the NA values with the Mode of the JobSatisfaction
mode_JobSatisfaction<- Mode(temp_JobSatisfaction)
employee_survey$JobSatisfaction[which(is.na(employee_survey$JobSatisfaction))] <- mode_JobSatisfaction

#Similarly , We would get the vector of WorkLifeBalance which does not have any NA values
temp_WorkLifeBalance <- employee_survey$WorkLifeBalance[which(!is.na(employee_survey$WorkLifeBalance))]

#Now , we will replace the NA values with the Mode of the WorkLifeBalance
mode_WorkLifeBalance <- Mode(temp_WorkLifeBalance)
employee_survey$WorkLifeBalance[which(is.na(employee_survey$WorkLifeBalance))] <- mode_WorkLifeBalance



#Identify factor variables
employee_survey$EnvironmentSatisfaction <- as.factor(employee_survey$EnvironmentSatisfaction)
employee_survey$JobSatisfaction <- as.factor(employee_survey$JobSatisfaction)
employee_survey$WorkLifeBalance <- as.factor(employee_survey$WorkLifeBalance)

#Renaming the levels of EnvironmentSatisfaction,JobSatisfaction and WorkLifeBalance 
#variable as given in Data Dictionary

levels(employee_survey$EnvironmentSatisfaction)<- c('Low','Medium','High','Very High')
levels(employee_survey$JobSatisfaction)<- c('Low','Medium','High','Very High')
levels(employee_survey$WorkLifeBalance)<- c('Bad','Good','Better','Best')

#creating the dummy variable for the variables which is having more than 2 levels
dummy_1<- data.frame(model.matrix(~EnvironmentSatisfaction,data = employee_survey))
dummy_1<- dummy_1[,-1]

dummy_2<- data.frame(model.matrix(~JobSatisfaction,data = employee_survey))
dummy_2<- dummy_2[,-1]

dummy_3<- data.frame(model.matrix(~WorkLifeBalance,data = employee_survey))
dummy_3<- dummy_3[,-1]

#Combining the dummy variables and the EmployeeID of employee survey
#dataset, in a new dataset called employee_survey_1

employee_survey_1<-NULL
employee_survey_1<-cbind(employee_survey,dummy_1,dummy_2,dummy_3)
employee_survey_1 <- employee_survey_1[,-c(2,3,4)]

#cleaning manager_survey data set
#---------------------------------

#checking duplicate records
anyDuplicated(manager_survey)
sum(duplicated(manager_survey$EmployeeID))
#No duplicate is present.

#Treating NA values
sapply(manager_survey, function(x) sum(is.na(x)))#No NA

#Identify factor variables
manager_survey$JobInvolvement <- as.factor(manager_survey$JobInvolvement)
manager_survey$PerformanceRating <- as.factor(manager_survey$PerformanceRating)

#Renaming the levels of JobInvolvement,PerformanceRating  
#variable as given in Data Dictionary
levels(manager_survey$JobInvolvement)<- c('Low','Medium','High','Very High')
levels(manager_survey$PerformanceRating)<- c('Low','Good','Excellent','Outstanding')

#creating the dummy variable for the variables which is having more than 2 levels
dummy_1<- data.frame(model.matrix(~JobInvolvement,data = manager_survey))
dummy_1<- dummy_1[,-1]

dummy_2 <- data.frame(model.matrix(~PerformanceRating,data = manager_survey))
dummy_2<- dummy_2[,-1]

#Combining the dummy variables and the EmployeeID of manager survey
#dataset, in a new dataset called manager_survey_1

manager_survey_1<-NULL
manager_survey_1<-cbind(manager_survey,dummy_1,dummy_2)
manager_survey_1 <- manager_survey_1[,!(colnames(manager_survey_1) %in% c("JobInvolvement","PerformanceRating"))]

#in-time dataset:

#Checking for duplicates
anyDuplicated(in_time)
sum(duplicated(in_time$EmployeeID))

#Checking for NA's
sapply(in_time, function(x) sum(is.na(x)))#No NA

#Remove the columns where all values are NA
in_time_1<- in_time[, colSums(is.na(in_time)) != nrow(in_time)]
colnames(in_time_1)[1] <- "EmployeeID"

in_time_2<-NULL
in_time_2<- gather(in_time_1,key = date,value = InTime,2:ncol(in_time_1))
in_time_2 <- in_time_2 %>% arrange(EmployeeID)
in_time_2$date<- gsub("[X]","",in_time_2$date)
in_time_2$date<-as.Date(in_time_2$date,format = "%Y.%m.%d")
in_time_2$InTime<- parse_date_time(x=in_time_2$InTime,orders = "%Y-%m-%d %H:%M:%S", locale = "eng")

#Out time data set

#Checking for duplicates
anyDuplicated(out_time)
sum(duplicated(out_time$EmployeeID))

#Checking for NA's
sapply(in_time, function(x) sum(is.na(x)))#No NA

#remove the columns where all values are NA
out_time_1<- out_time[, colSums(is.na(out_time)) != nrow(out_time)]
colnames(out_time_1)[1] <- "EmployeeID"

out_time_2<-NULL
out_time_2<- gather(out_time_1,key = date,value = OutTime,2:ncol(out_time_1))
out_time_2 <- out_time_2 %>% arrange(EmployeeID)
out_time_2$date<- gsub("[X]","",out_time_2$date)
out_time_2$date<-as.Date(out_time_2$date,format = "%Y.%m.%d")
out_time_2$OutTime<- parse_date_time(x=out_time_2$OutTime,orders = "%Y-%m-%d %H:%M:%S", locale = "eng")

#Now combining in_time_2 and out_time_2 data sets
emp_in_out_time  <- cbind(in_time_2,out_time_2)

#Remove the duplicate EmployeeID and date colmns 
emp_in_out_time <- emp_in_out_time[,-c(4,5)]

emp_in_out_time$duration<- as.numeric(round(with(emp_in_out_time,difftime(OutTime,InTime,units = "hours")),2))

sum(is.na(emp_in_out_time))
sum(is.na(emp_in_out_time$InTime))
sum(is.na(emp_in_out_time$OutTime))
sum(is.na(emp_in_out_time$duration))

#We need to check whether there are any cases where in_time is not NA and out_time is NA
# or vice versa.If any such case found,those cases should be considered as data discrepancies.
sum(!is.na(emp_in_out_time$InTime) & is.na(emp_in_out_time$OutTime)) #No such cases
sum(is.na(emp_in_out_time$InTime) & !is.na(emp_in_out_time$OutTime)) #No such cases

#Now , let's figure out if our count is matching with total NA counts 
#in InTime and OutTime
sum(is.na(emp_in_out_time$InTime) & is.na(emp_in_out_time$OutTime))
#The above count is same as NA's count of in_time and out_time. It means the was on leave on that day.

emp_in_out_time$Year<-NULL
emp_in_out_time$Month<-NULL
emp_in_out_time$Date_of_month <- NULL
emp_in_out_time$Day_of_Week <- NULL

emp_in_out_time$Year <- format(emp_in_out_time$date,"%Y")
emp_in_out_time$Month <- format(emp_in_out_time$date,"%m")
emp_in_out_time$DayNo <- format(emp_in_out_time$date,"%d")
emp_in_out_time$WeekDay <- wday(emp_in_out_time$date,label = TRUE)

emp_in_out_time$OnLeave <- ifelse(is.na(emp_in_out_time$duration),1,0) 

emp_in_out_time_1 <- NULL

# Now, we are making a new data set which will have consolidated information for each employee.
emp_in_out_time_1<- emp_in_out_time %>% group_by(EmployeeID) %>% summarise(
  AverageDuration=round(mean(duration,na.rm = TRUE),2),
  TotalDuration=round(sum(duration,na.rm=TRUE),2),
  TolalLeaves=sum(OnLeave))

#cleaning general data set
#-------------------------
#Checking for duplicates
anyDuplicated(general)
sum(duplicated(general$EmployeeID))

#Treating NA value
sapply(general, function(x) sum(is.na(x)))

#Treating NA's of NumCompaniesWorked

#After analysing the general data set we found that NA,0 and 1 should be treated as below:
#if totalworkingyears = yearsAtCompany+1 => replace NumcompaniesWorked = 1
#if totalworkingyears = yearsAtCompany  => replace NumCompaniesWorked = 0

general$NumCompaniesWorked[which(general$TotalWorkingYears==(general$YearsAtCompany+1))]<- 1
general$NumCompaniesWorked[which(general$TotalWorkingYears==general$YearsAtCompany)]<- 0

#remaining NA's in NumCompaniesWorked are being replaced with median value.
general$NumCompaniesWorked[which(is.na(general$NumCompaniesWorked))]<- median(general$NumCompaniesWorked,na.rm = T)

#Treating NA's of TotalWorkingYears
#After analysing the general data set we found that NA should be treated as below:
#if NumCompaniesWorked = 0 => replace  TotalWorkingYears with YearsAtCompany.
#NumCompaniesWorked = 1 => replace  TotalWorkingYears with YearsAtCompany+1.

general$TotalWorkingYears[which(is.na(general$TotalWorkingYears) & general$NumCompaniesWorked == 0)]<-
  general$YearsAtCompany[which(is.na(general$TotalWorkingYears) & general$NumCompaniesWorked == 0)]

general$TotalWorkingYears[which(is.na(general$TotalWorkingYears) & general$NumCompaniesWorked == 1)]<-
  general$YearsAtCompany[which(is.na(general$TotalWorkingYears) & general$NumCompaniesWorked == 1)]+1

#Remaining NA's of TotalWorkingYears are being replaced with median value.
general$TotalWorkingYears[which(is.na(general$TotalWorkingYears))]<- median(general$TotalWorkingYears,na.rm = T)

#Checking for outlires

boxplot(general$MonthlyIncome)
#since outlires are present in MonthlyIncome, we have to treat it.

quantile(general$MonthlyIncome,seq(0,1,0.01))

#some bivariate analysis on MonthalyIncome
boxplot(MonthlyIncome ~ Department, data = general)
boxplot(MonthlyIncome ~ Education, data = general)
boxplot(MonthlyIncome ~ JobLevel, data = general)

qnt <- quantile(general$MonthlyIncome, probs=c(.25, .75))
caps <- quantile(general$MonthlyIncome, probs=c(.05, .92))
H <- 1.5 * IQR(general$MonthlyIncome)
general$MonthlyIncome[general$MonthlyIncome < (qnt[1] - H)] <- caps[1]
general$MonthlyIncome[general$MonthlyIncome > (qnt[2] + H)] <- caps[2]


boxplot(general$Age)#No outlires

boxplot(general$DistanceFromHome)#No outlires

boxplot(general$NumCompaniesWorked) #No outlires

boxplot(general$PercentSalaryHike)#No outlires

boxplot(general$TotalWorkingYears)
#since outlires are present in TotalWorkingYears, we have to treat it

qnt <- quantile(general$TotalWorkingYears, probs=c(.25, .75),na.rm = TRUE)
caps <- quantile(general$TotalWorkingYears, probs=c(.05, .95),na.rm = TRUE)
H <- 1.5 * IQR(general$TotalWorkingYears,na.rm = TRUE)
general$TotalWorkingYears[general$TotalWorkingYears < (qnt[1] - H)] <- caps[1]
general$TotalWorkingYears[general$TotalWorkingYears > (qnt[2] + H)] <- caps[2]

  
boxplot(general$YearsAtCompany)
#since outlires are present in YearsAtCompany, we have to treat it

qnt <- quantile(general$YearsAtCompany, probs=c(.25, .75))
caps <- quantile(general$YearsAtCompany, probs=c(.05, .92))
H <- 1.5 * IQR(general$YearsAtCompany)
general$YearsAtCompany[general$YearsAtCompany < (qnt[1] - H)] <- caps[1]
general$YearsAtCompany[general$YearsAtCompany > (qnt[2] + H)] <- caps[2]

boxplot(general$YearsSinceLastPromotion)
#since outlires are present in YearsSinceLastPromotion, we have to treat it
qnt <- quantile(general$YearsSinceLastPromotion, probs=c(.25, .75))
caps <- quantile(general$YearsSinceLastPromotion, probs=c(.05, .92))
H <- 1.5 * IQR(general$YearsSinceLastPromotion)
general$YearsSinceLastPromotion[general$YearsSinceLastPromotion < (qnt[1] - H)] <- caps[1]
general$YearsSinceLastPromotion[general$YearsSinceLastPromotion > (qnt[2] + H)] <- caps[2]

boxplot(general$YearsWithCurrManager)
#since outlires are present in YearsWithCurrManager, we have to treat it
qnt <- quantile(general$YearsWithCurrManager, probs=c(.25, .75))
caps <- quantile(general$YearsWithCurrManager, probs=c(.05, .99))
H <- 1.5 * IQR(general$YearsWithCurrManager)
general$YearsWithCurrManager[general$YearsWithCurrManager < (qnt[1] - H)] <- caps[1]
general$YearsWithCurrManager[general$YearsWithCurrManager > (qnt[2] + H)] <- caps[2]

#since variabls EmployeeCount, Over18 and StandardHours are having single value hence removing them from data set.
general<- general[,-c(8,16,18)]

#Indentify factor variables
general$Attrition<- as.factor(general$Attrition)
general$BusinessTravel<- as.factor(general$BusinessTravel)
general$Department<- as.factor(general$Department)
general$Education<- as.factor(general$Education)
general$EducationField<- as.factor(general$EducationField)
general$Gender<- as.factor(general$Gender)
general$JobLevel<- as.factor(general$JobLevel)
general$JobRole<- as.factor(general$JobRole)
general$MaritalStatus<- as.factor(general$MaritalStatus)
general$StockOptionLevel<- as.factor(general$StockOptionLevel)
general$TrainingTimesLastYear <- as.factor(general$TrainingTimesLastYear)


#Checking uppercase-lowercase issue and consistency of value of factor variable
summary(general$Attrition)#No issue 
summary(general$BusinessTravel) #No issue 
summary(general$Department) #No issue
summary(general$Education) #No issue
summary(general$EducationField) #No issue
summary(general$Gender) #No issue 
summary(general$JobLevel) #No issue
summary(general$JobRole) # No issue
summary(general$MaritalStatus) # No issue
summary(general$StockOptionLevel) # No issue
summary(general$TrainingTimesLastYear) #No issue

#Renaming the levels of Education variable as given in Data Dictionary
levels(general$Education)<- c('Below College','College','Bachelor','Master','Doctor')

#############################################################################################################

# Data Visualization:
#--------------------

bar_theme1<- theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 0.5), 
                   legend.position="top") 

df1 = data.frame(employee_survey,Attrition= general$Attrition)
df2 = data.frame(manager_survey,Attrition= general$Attrition)

plot_grid( 
          ggplot(general, aes(x=JobLevel,fill=Attrition))+ geom_bar(position = "fill")+ scale_y_continuous(labels=scales::percent)+ bar_theme1,
          ggplot(general, aes(x=Department,fill=Attrition))+ geom_bar(position = "fill")+ scale_y_continuous(labels=scales::percent) + bar_theme1,
          ggplot(general, aes(x=EducationField,fill=Attrition))+ geom_bar(position = "fill")+scale_y_continuous(labels=scales::percent) + bar_theme1,
          ggplot(general, aes(x=MaritalStatus,fill=Attrition))+ geom_bar(position = "fill")+ scale_y_continuous(labels=scales::percent) + bar_theme1,
          ggplot(general,aes(x=Education,fill=Attrition))+ geom_bar(position = "fill")+ scale_y_continuous(labels=scales::percent)+bar_theme1)

  plot_grid(ggplot(general, aes(x=Gender,fill=Attrition))+geom_bar(position = "fill")+ scale_y_continuous(labels=scales::percent)+ bar_theme1,
         ggplot(general, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar(position = "fill")+ scale_y_continuous(labels=scales::percent)+ bar_theme1,
         ggplot(general, aes(x=TrainingTimesLastYear,fill=Attrition))+geom_bar(position = "fill")+ scale_y_continuous(labels=scales::percent)+ bar_theme1,
         ggplot(df1, aes(x=WorkLifeBalance,fill = Attrition))+ geom_bar(position = "fill")+ scale_y_continuous(labels=scales::percent)+ bar_theme1,
         ggplot(df1, aes(x=EnvironmentSatisfaction,fill = Attrition))+ geom_bar(position = "fill")+ scale_y_continuous(labels=scales::percent)+ bar_theme1,
         ggplot(df1, aes(x=JobSatisfaction,fill = Attrition))+ geom_bar(position = "fill")+ scale_y_continuous(labels=scales::percent)+ bar_theme1)

plot_grid(ggplot(df2, aes(x= Attrition ,fill=JobInvolvement))+geom_bar(position = "fill")+ scale_y_continuous(labels=scales::percent)+ bar_theme1,
          ggplot(df2, aes(x= Attrition ,fill=PerformanceRating))+geom_bar(position = "fill")+ scale_y_continuous(labels=scales::percent)+ bar_theme1)

#Attrition by Gender
ggplot(general, aes(x=Attrition,fill= Gender))+ geom_bar()
ggplot(general, aes(x=Gender,fill= Attrition))+ geom_bar(position = "fill") + 
scale_y_continuous(labels=scales::percent)

#### Attrition rate
ggplot(general, aes(factor(Attrition))) +
  geom_bar(fill="dodgerblue3")+
  labs(x = NULL,y = NULL)+
  stat_count(aes(label = sprintf("%.2f%%",prop.table(..count..) * 100)),
             vjust = 1, geom = "text", position = "identity", color ="white")

#plots for numeric variables

# Histogram for numeric variables 

ggplot(general, aes(x = YearsAtCompany,fill=Attrition)) +
  geom_histogram(position = "dodge",binwidth=5) +
  scale_x_continuous(name = "Years at the company") 


plot_grid(ggplot(general, aes(x = YearsAtCompany,fill=Attrition)) +
          geom_histogram(position = "dodge",binwidth=5) +
          scale_x_continuous(name = "Years at the company"),
          ggplot(general, aes(x = TotalWorkingYears,fill=Attrition)) +
            geom_bar(position="fill") + scale_y_continuous(labels=scales::percent)+
            scale_x_continuous(name = "TotalWorkingYears"),
          ggplot(general, aes(x = YearsWithCurrManager,fill=Attrition)) +
            geom_histogram(position = "dodge",binwidth=5) +
            scale_x_continuous(name = "Years with current manager"))
         
# Bar plot PercentSalaryHike
ggplot(general, aes(x=PercentSalaryHike,fill= Attrition))+ geom_bar(position="fill") +scale_y_continuous(labels=scales::percent)

#Bar plot of YearsSinceLastPromotion
ggplot(general, aes(x=YearsSinceLastPromotion,fill= Attrition))+ geom_bar(position="fill") +scale_y_continuous(labels=scales::percent)

#Histogram for Age attribute
ggplot(general, aes(x = Age,fill=Attrition)) +
            geom_histogram(position = position_dodge(),binwidth=5) +
            scale_x_continuous(name = "Age", breaks = seq(18, 60, 5),limits=c(18, 60))
          
# Histogram for DistanceFromHome
plot_grid(ggplot(general, aes(x = DistanceFromHome,fill=Attrition)) +
  geom_histogram(position = "dodge",bins=10) +
  scale_x_continuous(name = "DistanceFromHome", breaks = seq(0, 30, 3),limits=c(0, 30)),
  ggplot(general, aes(x = MonthlyIncome,fill=Attrition)) +
              geom_histogram(position = "dodge",bins = 20) +
              scale_x_continuous(name = "MonthlyIncome",breaks = seq(0,200000,10000))+bar_theme1)


# bar for NumCompaniesWorked
ggplot(general, aes(x=NumCompaniesWorked,fill= Attrition))+ geom_bar(position="fill") +scale_y_continuous(labels=scales::percent)

####################################################################################################################

###################################### Model building ################################

#Converting the factors with 2 levels to numerical variables
levels(general$Attrition)<-c(1,0)
general$Attrition <- as.numeric(levels(general$Attrition))[general$Attrition]

levels(general$Gender)<-c(1,0)
general$Gender <- as.numeric(levels(general$Gender))[general$Gender]

#creating the dummy variable for the variables which is having more than 2 levels
dummy_1<- data.frame(model.matrix(~BusinessTravel,data = general))
dummy_1<- dummy_1[,-1]

dummy_2<- data.frame(model.matrix(~Department,data = general))
dummy_2<- dummy_2[,-1]

dummy_3<- data.frame(model.matrix(~Education,data = general))
dummy_3<- dummy_3[,-1] 

dummy_4<- data.frame(model.matrix(~EducationField,data = general))
dummy_4<- dummy_4[,-1] 

dummy_5<- data.frame(model.matrix(~JobLevel,data = general))
dummy_5<- dummy_5[,-1] 

dummy_6<- data.frame(model.matrix(~JobRole,data = general))
dummy_6<- dummy_6[,-1] 

dummy_7<- data.frame(model.matrix(~MaritalStatus,data = general))
dummy_7<- dummy_7[,-1] 

dummy_8<- data.frame(model.matrix(~StockOptionLevel,data = general))
dummy_8<- dummy_8[,-1] 

dummy_9 <- data.frame(model.matrix(~TrainingTimesLastYear,data = general))
dummy_9 <- dummy_9[,-1]

#Combining the dummy variables and the numeric columns of general dataset, in a new dataset called general_1
general_1<- cbind(general[,-c(3,4,6,7,10,11,12,16,18)],dummy_1,dummy_2,dummy_3,dummy_4,dummy_5,dummy_6,dummy_7,dummy_8,dummy_9)


#Checking whether any mismatch of employee ID is present among the given data sets
setdiff(general_1$EmployeeID,emp_in_out_time_1$EmployeeID)
setdiff(employee_survey_1$EmployeeID,emp_in_out_time_1$EmployeeID)
setdiff(manager_survey_1$EmployeeID,emp_in_out_time_1$EmployeeID)

#We are using cbind() function to combine four datasets as datasets are very large and merge operation
#is taking very long time. So, we are doing this operation in two steps:
#1.Creating a master data set by combining processed general,in-out,employee_survey and maneger_survey data sets.
master_dataset <- cbind(general_1,emp_in_out_time_1,employee_survey_1,manager_survey_1)

#2.Remove the extra EmployeeID columns from the master data set
which(colnames(master_dataset)=="EmployeeID")
master_dataset <- master_dataset[,-c(49,53,63)]

#Rearranging the column EmployeeID : from 4th position to 1st position and shifting other columns
master_dataset <- master_dataset[,c(4,1:3,5:ncol(master_dataset))]

########### Building Logistic Regression Modle ########
# Feature standardisation

# Normalising continuous features
master_dataset$Age<- scale(master_dataset$Age)
master_dataset$DistanceFromHome<- scale(master_dataset$DistanceFromHome)
master_dataset$MonthlyIncome<- scale(master_dataset$MonthlyIncome)
master_dataset$NumCompaniesWorked<- scale(master_dataset$NumCompaniesWorked)
master_dataset$PercentSalaryHike<- scale(master_dataset$PercentSalaryHike)
master_dataset$TotalWorkingYears<- scale(master_dataset$TotalWorkingYears)
master_dataset$YearsAtCompany<- scale(master_dataset$YearsAtCompany)
master_dataset$YearsSinceLastPromotion<- scale(master_dataset$YearsSinceLastPromotion)
master_dataset$YearsWithCurrManager<- scale(master_dataset$YearsWithCurrManager)
master_dataset$AverageDuration<- scale(master_dataset$AverageDuration)
master_dataset$TotalDuration<- scale(master_dataset$TotalDuration)
master_dataset$TolalLeaves<- scale(master_dataset$TolalLeaves)

# splitting the data between train and test
set.seed(100)
trainindices= sample(1:nrow(master_dataset), 0.7*nrow(master_dataset))
train = master_dataset[trainindices,]
test = master_dataset[-trainindices,]

# Logistic Regression: 

#Initial model
model_1 = glm(Attrition ~ ., data = train[,-1], family = "binomial")
summary(model_1) 

# Stepwise Backward selection
model_2<- stepAIC(model_1, direction="both")

summary(model_2)#AIC: 2128.6
sort(vif(model_2),decreasing = TRUE)

#EducationFieldLife.Sciences
#DepartmentResearch...Development

#p value of EducationFieldLife.Sciences is very high and vif is also pretty high
#So , removing this variable

model_3 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
      TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
      BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
      DepartmentResearch...Development + DepartmentSales + EducationDoctor + 
      EducationFieldMarketing + EducationFieldMedical + 
      EducationFieldOther + EducationFieldTechnical.Degree + JobLevel5 + 
      JobRoleLaboratory.Technician + JobRoleManufacturing.Director + 
      JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
      MaritalStatusSingle + StockOptionLevel1 + TrainingTimesLastYear4 + 
      TrainingTimesLastYear5 + TrainingTimesLastYear6 + AverageDuration + 
      EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
      EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
      JobSatisfactionHigh + JobSatisfactionVery.High + WorkLifeBalanceGood + 
      WorkLifeBalanceBetter + WorkLifeBalanceBest + JobInvolvementHigh, 
    family = "binomial", data = train[, -1])

summary(model_3)
sort(vif(model_3),decreasing = TRUE)

#EducationFieldMarketing : p value is pretty high , so removing it

model_4 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                   TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                   BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                   DepartmentResearch...Development + DepartmentSales + EducationDoctor + 
                   EducationFieldMedical + 
                   EducationFieldOther + EducationFieldTechnical.Degree + JobLevel5 + 
                   JobRoleLaboratory.Technician + JobRoleManufacturing.Director + 
                   JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                   MaritalStatusSingle + StockOptionLevel1 + TrainingTimesLastYear4 + 
                   TrainingTimesLastYear5 + TrainingTimesLastYear6 + AverageDuration + 
                   EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                   EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
                   JobSatisfactionHigh + JobSatisfactionVery.High + WorkLifeBalanceGood + 
                   WorkLifeBalanceBetter + WorkLifeBalanceBest + JobInvolvementHigh, 
                 family = "binomial", data = train[, -1])

summary(model_4)
sort(vif(model_4),decreasing = TRUE)

#EducationFieldOther : removing this variable as it has got pretty high p-value

model_5 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                 TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                 BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                 DepartmentResearch...Development + DepartmentSales + EducationDoctor + 
                 EducationFieldMedical + EducationFieldTechnical.Degree + JobLevel5 + 
                 JobRoleLaboratory.Technician + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                 MaritalStatusSingle + StockOptionLevel1 + TrainingTimesLastYear4 + 
                 TrainingTimesLastYear5 + TrainingTimesLastYear6 + AverageDuration + 
                 EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                 EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
                 JobSatisfactionHigh + JobSatisfactionVery.High + WorkLifeBalanceGood + 
                 WorkLifeBalanceBetter + WorkLifeBalanceBest + JobInvolvementHigh, 
               family = "binomial", data = train[, -1])
summary(model_5)
sort(vif(model_5),decreasing = TRUE)

#EducationFieldMedical : removing this variable as it has got pretty high p-value

model_6 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                 TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                 BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                 DepartmentResearch...Development + DepartmentSales + EducationDoctor + 
                 EducationFieldTechnical.Degree + JobLevel5 + 
                 JobRoleLaboratory.Technician + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                 MaritalStatusSingle + StockOptionLevel1 + TrainingTimesLastYear4 + 
                 TrainingTimesLastYear5 + TrainingTimesLastYear6 + AverageDuration + 
                 EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                 EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
                 JobSatisfactionHigh + JobSatisfactionVery.High + WorkLifeBalanceGood + 
                 WorkLifeBalanceBetter + WorkLifeBalanceBest + JobInvolvementHigh, 
               family = "binomial", data = train[, -1])

summary(model_6)
sort(vif(model_6),decreasing = TRUE)
#EducationFieldTechnical.Degree : this variable has pretty high p-value , so removing it


model_7 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                 TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                 BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                 DepartmentResearch...Development + DepartmentSales + EducationDoctor + 
                 JobLevel5 + 
                 JobRoleLaboratory.Technician + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                 MaritalStatusSingle + StockOptionLevel1 + TrainingTimesLastYear4 + 
                 TrainingTimesLastYear5 + TrainingTimesLastYear6 + AverageDuration + 
                 EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                 EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
                 JobSatisfactionHigh + JobSatisfactionVery.High + WorkLifeBalanceGood + 
                 WorkLifeBalanceBetter + WorkLifeBalanceBest + JobInvolvementHigh, 
               family = "binomial", data = train[, -1])
summary(model_7)
sort(vif(model_7),decreasing = TRUE)
#TrainingTimesLastYear4 : removing this variable as it has got pretty high p-value


model_8 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                 TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                 BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                 DepartmentResearch...Development + DepartmentSales + EducationDoctor + 
                 JobLevel5 + 
                 JobRoleLaboratory.Technician + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                 MaritalStatusSingle + StockOptionLevel1 +  
                 TrainingTimesLastYear5 + TrainingTimesLastYear6 + AverageDuration + 
                 EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                 EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
                 JobSatisfactionHigh + JobSatisfactionVery.High + WorkLifeBalanceGood + 
                 WorkLifeBalanceBetter + WorkLifeBalanceBest + JobInvolvementHigh, 
               family = "binomial", data = train[, -1])

summary(model_8)
sort(vif(model_8),decreasing = TRUE)
#MonthlyIncome : removing this variable as it has got high p-value

model_9 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                 TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                 BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                 DepartmentResearch...Development + DepartmentSales + EducationDoctor + 
                 JobLevel5 + 
                 JobRoleLaboratory.Technician + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                 MaritalStatusSingle + StockOptionLevel1 +  
                 TrainingTimesLastYear5 + TrainingTimesLastYear6 + AverageDuration + 
                 EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                 EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
                 JobSatisfactionHigh + JobSatisfactionVery.High + WorkLifeBalanceGood + 
                 WorkLifeBalanceBetter + WorkLifeBalanceBest + JobInvolvementHigh, 
               family = "binomial", data = train[, -1])
summary(model_9)
sort(vif(model_9),decreasing = TRUE)

#StockOptionLevel1: removing this variable as it has got pretty high p-value


model_10 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                 TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                 BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                 DepartmentResearch...Development + DepartmentSales + EducationDoctor + 
                 JobLevel5 + 
                 JobRoleLaboratory.Technician + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                 MaritalStatusSingle +  
                 TrainingTimesLastYear5 + TrainingTimesLastYear6 + AverageDuration + 
                 EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                 EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
                 JobSatisfactionHigh + JobSatisfactionVery.High + WorkLifeBalanceGood + 
                 WorkLifeBalanceBetter + WorkLifeBalanceBest + JobInvolvementHigh, 
               family = "binomial", data = train[, -1])

summary(model_10)
sort(vif(model_10),decreasing = TRUE)
#JobRoleLaboratory.Technician : removing this variable as it has high p-value


model_11 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                  BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                  DepartmentResearch...Development + DepartmentSales + EducationDoctor + 
                  JobLevel5 + 
                  JobRoleManufacturing.Director + 
                  JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                  MaritalStatusSingle +  
                  TrainingTimesLastYear5 + TrainingTimesLastYear6 + AverageDuration + 
                  EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                  EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
                  JobSatisfactionHigh + JobSatisfactionVery.High + WorkLifeBalanceGood + 
                  WorkLifeBalanceBetter + WorkLifeBalanceBest + JobInvolvementHigh, 
                family = "binomial", data = train[, -1])

summary(model_11)
sort(vif(model_11),decreasing = TRUE)
#JobRoleResearch.Scientist : removing this variable as it has high p-value
model_12 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                  BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                  DepartmentResearch...Development + DepartmentSales + EducationDoctor + 
                  JobLevel5 + 
                  JobRoleManufacturing.Director + 
                  JobRoleResearch.Director + JobRoleSales.Executive + 
                  MaritalStatusSingle +  
                  TrainingTimesLastYear5 + TrainingTimesLastYear6 + AverageDuration + 
                  EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                  EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
                  JobSatisfactionHigh + JobSatisfactionVery.High + WorkLifeBalanceGood + 
                  WorkLifeBalanceBetter + WorkLifeBalanceBest + JobInvolvementHigh, 
                family = "binomial", data = train[, -1])

summary(model_12)
sort(vif(model_12),decreasing = TRUE)
#JobRoleSales.Executive: removing this variable as it has high p-value

model_13 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                  BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                  DepartmentResearch...Development + DepartmentSales + EducationDoctor + 
                  JobLevel5 + 
                  JobRoleManufacturing.Director + 
                  JobRoleResearch.Director + 
                  MaritalStatusSingle +  
                  TrainingTimesLastYear5 + TrainingTimesLastYear6 + AverageDuration + 
                  EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                  EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
                  JobSatisfactionHigh + JobSatisfactionVery.High + WorkLifeBalanceGood + 
                  WorkLifeBalanceBetter + WorkLifeBalanceBest + JobInvolvementHigh, 
                family = "binomial", data = train[, -1])

summary(model_13)
sort(vif(model_13),decreasing = TRUE)
#JobRoleResearch.Director : removing this variable as it has high p-value


model_14 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                  BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                  DepartmentResearch...Development + DepartmentSales + EducationDoctor + 
                  JobLevel5 + 
                  JobRoleManufacturing.Director +  
                  MaritalStatusSingle +  
                  TrainingTimesLastYear5 + TrainingTimesLastYear6 + AverageDuration + 
                  EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                  EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
                  JobSatisfactionHigh + JobSatisfactionVery.High + WorkLifeBalanceGood + 
                  WorkLifeBalanceBetter + WorkLifeBalanceBest + JobInvolvementHigh, 
                family = "binomial", data = train[, -1])

summary(model_14)
sort(vif(model_14),decreasing = TRUE)
#JobLevel5 : remove this variable as it has high p-value


model_15 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                  BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                  DepartmentResearch...Development + DepartmentSales + EducationDoctor +  
                  JobRoleManufacturing.Director +  
                  MaritalStatusSingle +  
                  TrainingTimesLastYear5 + TrainingTimesLastYear6 + AverageDuration + 
                  EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                  EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
                  JobSatisfactionHigh + JobSatisfactionVery.High + WorkLifeBalanceGood + 
                  WorkLifeBalanceBetter + WorkLifeBalanceBest + JobInvolvementHigh, 
                family = "binomial", data = train[, -1])

summary(model_15)
sort(vif(model_15),decreasing = TRUE)

#EducationDoctor : removing this variable as it has high p-value
model_16 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                  BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                  DepartmentResearch...Development + DepartmentSales +  
                  JobRoleManufacturing.Director +  
                  MaritalStatusSingle +  
                  TrainingTimesLastYear5 + TrainingTimesLastYear6 + AverageDuration + 
                  EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                  EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
                  JobSatisfactionHigh + JobSatisfactionVery.High + WorkLifeBalanceGood + 
                  WorkLifeBalanceBetter + WorkLifeBalanceBest + JobInvolvementHigh, 
                family = "binomial", data = train[, -1])

summary(model_16)
sort(vif(model_16),decreasing = TRUE)

#TrainingTimesLastYear5  : removing this variable as it has high p-value
model_17 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                  BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                  DepartmentResearch...Development + DepartmentSales +  
                  JobRoleManufacturing.Director +  
                  MaritalStatusSingle +  
                  TrainingTimesLastYear6 + AverageDuration + 
                  EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                  EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
                  JobSatisfactionHigh + JobSatisfactionVery.High + WorkLifeBalanceGood + 
                  WorkLifeBalanceBetter + WorkLifeBalanceBest + JobInvolvementHigh, 
                family = "binomial", data = train[, -1])

summary(model_17)
sort(vif(model_17),decreasing = TRUE)

#JobInvolvementHigh : removing this variable as it has high p-value

model_18 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                  BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                  DepartmentResearch...Development + DepartmentSales +  
                  JobRoleManufacturing.Director +  
                  MaritalStatusSingle +  
                  TrainingTimesLastYear6 + AverageDuration + 
                  EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                  EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
                  JobSatisfactionHigh + JobSatisfactionVery.High + WorkLifeBalanceGood + 
                  WorkLifeBalanceBetter + WorkLifeBalanceBest , 
                family = "binomial", data = train[, -1])

summary(model_18)
sort(vif(model_18),decreasing = TRUE)

#JobSatisfactionHigh : removing this variable as it has comparatively high p-value

model_19 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                  BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                  DepartmentResearch...Development + DepartmentSales +  
                  JobRoleManufacturing.Director +  
                  MaritalStatusSingle +  
                  TrainingTimesLastYear6 + AverageDuration + 
                  EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                  EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
                  JobSatisfactionVery.High + WorkLifeBalanceGood + 
                  WorkLifeBalanceBetter + WorkLifeBalanceBest , 
                family = "binomial", data = train[, -1])

summary(model_19)
sort(vif(model_19),decreasing = TRUE)

#JobSatisfactionMedium : removing the variable as it has high p-value

model_20 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                  BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                  DepartmentResearch...Development + DepartmentSales +  
                  JobRoleManufacturing.Director +  
                  MaritalStatusSingle +  
                  TrainingTimesLastYear6 + AverageDuration + 
                  EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                  EnvironmentSatisfactionVery.High + 
                  JobSatisfactionVery.High + WorkLifeBalanceGood + 
                  WorkLifeBalanceBetter + WorkLifeBalanceBest , 
                family = "binomial", data = train[, -1])

summary(model_20)
sort(vif(model_20),decreasing = TRUE)

#We need to now check for the variables which are having high
#VIF values . We will be removing them one by one and see the AIC values
#Let's remove BusinessTravelTravel_Frequently first and see the impact

model_21_a <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                    TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                    BusinessTravelTravel_Rarely + 
                    DepartmentResearch...Development + DepartmentSales +  
                    JobRoleManufacturing.Director +  
                    MaritalStatusSingle +  
                    TrainingTimesLastYear6 + AverageDuration + 
                    EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                    EnvironmentSatisfactionVery.High + 
                    JobSatisfactionVery.High + WorkLifeBalanceGood + 
                    WorkLifeBalanceBetter + WorkLifeBalanceBest , 
                  family = "binomial", data = train[, -1])

summary(model_21_a)
sort(vif(model_21_a),decreasing = TRUE)

#AIC value is changed from  2156.1 (model_20) to 2206.4 (model_21_a).

# Now Let's keep BusinessTravelTravel_Frequently and remove BusinessTravelTravel_Rarely
model_21_b <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                    TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager +  
                    DepartmentResearch...Development + DepartmentSales +  
                    JobRoleManufacturing.Director +  
                    MaritalStatusSingle +  
                    TrainingTimesLastYear6 + AverageDuration + 
                    EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                    EnvironmentSatisfactionVery.High + 
                    JobSatisfactionVery.High + WorkLifeBalanceGood + 
                    WorkLifeBalanceBetter + WorkLifeBalanceBest + BusinessTravelTravel_Frequently , 
                  family = "binomial", data = train[, -1])


summary(model_21_b)
sort(vif(model_21_b),decreasing = TRUE)

#As we can see , now , AIC value changed from 2156.1 (model_20) to 2173.4
# So , in this case change is less. So, going with this model

#Removing TrainingTimesLastYear6 as it has got more p-value
model_22 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager +  
                  DepartmentResearch...Development + DepartmentSales +  
                  JobRoleManufacturing.Director +  
                  MaritalStatusSingle +  
                  AverageDuration + 
                  EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                  EnvironmentSatisfactionVery.High + 
                  JobSatisfactionVery.High + WorkLifeBalanceGood + 
                  WorkLifeBalanceBetter + WorkLifeBalanceBest + BusinessTravelTravel_Frequently , 
                family = "binomial", data = train[, -1])
summary(model_22)
sort(vif(model_22),decreasing = TRUE)

#Since WorkLifeBalanceBest is having relatively high p value so removing it and making a new model.
model_23 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + DepartmentResearch...Development + 
                  DepartmentSales + JobRoleManufacturing.Director + MaritalStatusSingle + 
                  AverageDuration + EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                  EnvironmentSatisfactionVery.High + JobSatisfactionVery.High + 
                  WorkLifeBalanceGood + WorkLifeBalanceBetter + 
                  BusinessTravelTravel_Frequently, family = "binomial", data = train[,-1])
summary(model_23)
sort(vif(model_23),decreasing = TRUE)

#Removing WorkLifeBalanceGood variable as it is having relatively high p-value.
model_24 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + DepartmentResearch...Development + 
                  DepartmentSales + JobRoleManufacturing.Director + MaritalStatusSingle + 
                  AverageDuration + EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                  EnvironmentSatisfactionVery.High + JobSatisfactionVery.High +WorkLifeBalanceBetter + 
                  BusinessTravelTravel_Frequently, family = "binomial", data = train[,-1])
summary(model_24)
sort(vif(model_24),decreasing = TRUE)

#Removing WorkLifeBalanceBetter variable as it is having relatively high p-value.
model_25 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + DepartmentResearch...Development + 
                  DepartmentSales + JobRoleManufacturing.Director + MaritalStatusSingle + 
                  AverageDuration + EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                  EnvironmentSatisfactionVery.High + JobSatisfactionVery.High +
                  BusinessTravelTravel_Frequently, family = "binomial", data = train[,-1])
summary(model_25)#AIC: 2208.9
sort(vif(model_25),decreasing = TRUE)

#Since DepartmentSales and DepartmentResearch...Development are having high vif, we will remove one at a time 
#and make two different model and will check difference in AIC.

#Removing DepartmentSales
model_26a <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                   YearsSinceLastPromotion + YearsWithCurrManager + DepartmentResearch...Development + 
                   JobRoleManufacturing.Director + MaritalStatusSingle + 
                   AverageDuration + EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                   EnvironmentSatisfactionVery.High + JobSatisfactionVery.High +
                   BusinessTravelTravel_Frequently, family = "binomial", data = train[,-1])

summary(model_26a)#AIC: 2229.6
sort(vif(model_26a))

#Removing DepartmentResearch...Development
model_26b <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                   YearsSinceLastPromotion + YearsWithCurrManager +
                   DepartmentSales + JobRoleManufacturing.Director + MaritalStatusSingle + 
                   AverageDuration + EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                   EnvironmentSatisfactionVery.High + JobSatisfactionVery.High +
                   BusinessTravelTravel_Frequently, family = "binomial", data = train[,-1])
summary(model_26b)#AIC: 2228
sort(vif(model_26b))
#Since jump in AIC in model_26b is less, we are proceeding with this model.
#DepartmentSales is having pretty high p-value so, we are removing it.
model_27 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager +
                  JobRoleManufacturing.Director + MaritalStatusSingle + 
                  AverageDuration + EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                  EnvironmentSatisfactionVery.High + JobSatisfactionVery.High +
                  BusinessTravelTravel_Frequently, family = "binomial", data = train[,-1])
summary(model_27)#AIC: 2228.2
sort(vif(model_27),decreasing = T)

#Removing variable TotalWorkingYears as it is having high vif value.
model_28 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  YearsSinceLastPromotion + YearsWithCurrManager +
                  JobRoleManufacturing.Director + MaritalStatusSingle + 
                  AverageDuration + EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                  EnvironmentSatisfactionVery.High + JobSatisfactionVery.High +
                  BusinessTravelTravel_Frequently, family = "binomial", data = train[,-1])
summary(model_28)#AIC: 2246.9
sort(vif(model_28),decreasing = T)

# With 12 significant variables in the model

final_model<- model_28
##############################################################
#Call:
 # glm(formula = Attrition ~ Age + NumCompaniesWorked + YearsSinceLastPromotion + 
 #     AverageDuration + EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
 #       EnvironmentSatisfactionVery.High + JobSatisfactionVery.High + 
 #        BusinessTravelTravel_Frequently, family = "binomial", data = train[, 
 #                                                                          -1])

#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-3.4140   0.2037   0.3809   0.5781   1.9379  

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                       1.46892    0.12811  11.466  < 2e-16 ***
#  Age                               0.54097    0.06481   8.347  < 2e-16 ***
#  NumCompaniesWorked               -0.29619    0.05681  -5.213 1.85e-07 ***
#  YearsSinceLastPromotion          -0.42551    0.06897  -6.169 6.87e-10 ***
#  YearsWithCurrManager              0.61629    0.07763   7.939 2.04e-15 ***
#  JobRoleManufacturing.Director     0.85156    0.21814   3.904 9.47e-05 ***
#  MaritalStatusSingle              -0.90845    0.11176  -8.129 4.34e-16 ***
#  AverageDuration                  -0.66033    0.05319 -12.415  < 2e-16 ***
#  EnvironmentSatisfactionMedium     0.62272    0.16346   3.810 0.000139 ***
# EnvironmentSatisfactionHigh       0.77678    0.15001   5.178 2.24e-07 ***
#  EnvironmentSatisfactionVery.High  1.15953    0.15674   7.398 1.39e-13 ***
#  JobSatisfactionVery.High          0.84361    0.12910   6.534 6.39e-11 ***
#  BusinessTravelTravel_Frequently  -0.79617    0.12635  -6.301 2.95e-10 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 2747.7  on 3086  degrees of freedom
#Residual deviance: 2220.9  on 3074  degrees of freedom
#AIC: 2246.9

#Number of Fisher Scoring iterations: 5

#Final model is:
#log of odds for Attrition = 1.46892 + 0.54097*Age + (-0.29619)*NumCompaniesWorked + (-0.42551)*YearsSinceLastPromotion+
#                            0.61629*YearsWithCurrManager + 0.85156*JobRoleManufacturing.Director+
#                            (-0.90845)*MaritalStatusSingle + (-0.66033)*AverageDuration + 
#                            0.62272*EnvironmentSatisfactionMedium + 0.77678*EnvironmentSatisfactionHigh+
#                            1.15953*EnvironmentSatisfactionVery.High + 0.84361*JobSatisfactionVery.High+
#                            (-0.79617)*BusinessTravelTravel_Frequently
#######################################################################################


### Model Evaluation ####

### Test Data ####

#predicted probabilities of Attrition for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-1])
summary(test_pred)# probabilities range from 13% to 99%
test$prob<- test_pred

# Let's Choose the cutoff value. 

# Let's find out the optimal probalility cutoff 


test_actual_Attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

perform_fn <- function(cutoff) 
{
  predicted_Attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_Attrition, test_actual_Attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}



# Summary of test probability

summary(test_pred)

s = seq(.01,.95,length=1000)

OUT = matrix(0,1000,3)


for(i in 1:1000)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)|
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]


# Let's choose a cutoff value of 0.8116817 for final model

test_cutoff_Attrition <- factor(ifelse(test_pred >=0.8116817, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_Attrition, test_actual_Attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc #0.7399849

sens #0.7392473

spec #0.7439614

test$predicted_Attrition<- ifelse(test$prob >=0.8116817, 1,0)

test<- test[,c(1:3,68,4:67)]
View(test)

#Since accuracy, sensitivity and specificity are almost same, so this a pretty good model.

### KS -statistic - Test Data ######

test_cutoff_Attrition <- ifelse(test_cutoff_Attrition=="Yes",1,0)
test_actual_Attrition <- ifelse(test_actual_Attrition=="Yes",1,0)

#on testing  data
pred_object_test<- prediction(test_cutoff_Attrition, test_actual_Attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)#0.4832087
#KS-statistic is 48.3% which is greater than 40% which implies our model is a good model.

# Lift & Gain Chart 

# plotting the lift chart

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_actual_Attrition, test_pred, groups = 10)
Attrition_decile
#At 4th decile cummulative gain is 44.9% and lift is 1.12 i.e. our model is 112% better than
#random model.
