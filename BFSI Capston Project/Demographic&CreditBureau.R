#############################################################################
##################### Loading of required libraries #########################
#############################################################################
library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)
library(stringr)
library(MASS)
library(car)
library(caTools)
library(rpart)
library(rattle)
library(rpart.plot)
library(randomForest)
library(caret)
library(DMwR)
library(Information)

############################################################################
#################### Loding data & Data Understanding ######################
############################################################################

################### Loading the Demographic data ###################

demographic <- read.csv("Demographic data.csv", stringsAsFactors = F)

colnames(demographic)
#  [1] "Application.ID"                                                                      
#  [2] "Age"
#  [3] "Gender"                                      
#  [4] "Marital.Status..at.the.time.of.application."
#  [5] "No.of.dependents"                            
#  [6] "Income"                                     
#  [7] "Education"                                   
#  [8] "Profession"                                 
#  [9] "Type.of.residence"                           
# [10] "No.of.months.in.current.residence"          
# [11] "No.of.months.in.current.company"             
# [12] "Performance.Tag"                            

# Rename the columns to a meaningful name such that the column names are readable
colnames(demographic)[which(names(demographic) == "Marital.Status..at.the.time.of.application.")] <- "Marital.Status"
colnames(demographic)[which(names(demographic) == "No.of.months.in.current.residence")] <- "NbrMnthsCurrResidence"
colnames(demographic)[which(names(demographic) == "No.of.months.in.current.company")] <- "NbrMnthsCurrCompany"

# Verifying column names after renaming.
colnames(demographic)

#  [1] "Application.ID"        
#  [2] "Age"                   
#  [3] "Gender"               
#  [4] "Marital.Status"        
#  [5] "No.of.dependents"      
#  [6] "Income"               
#  [7] "Education"             
#  [8] "Profession"            
#  [9] "Type.of.residence"    
# [10] "NbrMnthsCurrResidence" 
# [11] "NbrMnthsCurrCompany"   
# [12] "Performance.Tag"      

# Checking for duplicate record
sum(duplicated(demographic)) 
# Obs: There are No duplicate records

# Checking for duplicate Application.ID
sum(duplicated(demographic$Application.ID)) 
# Obs: There are 3 duplicate Application.ID

# checkout those duplicated records -
demographic$Application.ID[which(duplicated(demographic$Application.ID))]
# Obs: Following Applications are duplicated -
# 765011468, 653287861, 671989187

demographic[demographic$Application.ID %in% c(765011468, 653287861, 671989187),]
# Obs: These records indicate erroneous data source and it may mislead us
# So since these are 6 records we would dropping them from further analysis.

#Removing the records corresponding to duplicated Application.ID
demographic <- demographic[!demographic$Application.ID %in% c(765011468, 653287861, 671989187),]


################### Loading the credit Burea data ###################

credit_bureau <- read.csv("Credit Bureau data.csv",stringsAsFactors = F)

# Check the structure and summary of the columns/variables
str(credit_bureau)              
# Obs:
# There are 19 variables and 71295 observations
# All the columns are numeric of type int

# These are the following columns -
colnames(credit_bureau)
# [1] "Application.ID"                                                 
# [2] "No.of.times.90.DPD.or.worse.in.last.6.months"                   
# [3] "No.of.times.60.DPD.or.worse.in.last.6.months"                   
# [4] "No.of.times.30.DPD.or.worse.in.last.6.months"                   
# [5] "No.of.times.90.DPD.or.worse.in.last.12.months"                  
# [6] "No.of.times.60.DPD.or.worse.in.last.12.months"                  
# [7] "No.of.times.30.DPD.or.worse.in.last.12.months"                  
# [8] "Avgas.CC.Utilization.in.last.12.months"                         
# [9] "No.of.trades.opened.in.last.6.months"                           
#[10] "No.of.trades.opened.in.last.12.months"                          
#[11] "No.of.PL.trades.opened.in.last.6.months"                        
#[12] "No.of.PL.trades.opened.in.last.12.months"                       
#[13] "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans." 
#[14] "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans."
#[15] "Presence.of.open.home.loan"                                     
#[16] "Outstanding.Balance"                                            
#[17] "Total.No.of.Trades"                                             
#[18] "Presence.of.open.auto.loan"                                     
#[19] "Performance.Tag"                        

# Renaming the columns to a meaningful name such that the column names are readable

colnames(credit_bureau)[which(names(credit_bureau) == "No.of.times.90.DPD.or.worse.in.last.6.months")] <- "NbrOf90DPDLast6Mnths"
colnames(credit_bureau)[which(names(credit_bureau) == "No.of.times.60.DPD.or.worse.in.last.6.months")] <- "NbrOf60DPDLast6Mnths"
colnames(credit_bureau)[which(names(credit_bureau) == "No.of.times.30.DPD.or.worse.in.last.6.months")] <- "NbrOf30DPDLast6Mnths"
colnames(credit_bureau)[which(names(credit_bureau) == "No.of.times.90.DPD.or.worse.in.last.12.months")] <- "NbrOf90DPDLast12Mnths"
colnames(credit_bureau)[which(names(credit_bureau) == "No.of.times.60.DPD.or.worse.in.last.12.months")] <- "NbrOf60DPDLast12Mnths"
colnames(credit_bureau)[which(names(credit_bureau) == "No.of.times.30.DPD.or.worse.in.last.12.months")] <- "NbrOf30DPDLast12Mnths"
colnames(credit_bureau)[which(names(credit_bureau) == "Avgas.CC.Utilization.in.last.12.months")] <- "AvgCCUtilLast12Mnths"
colnames(credit_bureau)[which(names(credit_bureau) == "No.of.trades.opened.in.last.6.months")] <- "NbrOfTradesOpenLast6Mnths"
colnames(credit_bureau)[which(names(credit_bureau) == "No.of.trades.opened.in.last.12.months")] <- "NbrOfTradesOpenLast12Mnths"
colnames(credit_bureau)[which(names(credit_bureau) == "No.of.PL.trades.opened.in.last.6.months")] <- "NbrOfPLTradesOpenLast6Mnths"
colnames(credit_bureau)[which(names(credit_bureau) == "No.of.PL.trades.opened.in.last.12.months")] <- "NbrOfPLTradesOpenLast12Mnths"
colnames(credit_bureau)[which(names(credit_bureau) == "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.")] <- "NbrOfInq_ExclHomeAutoLoan_Last6Mnths"
colnames(credit_bureau)[which(names(credit_bureau) == "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.")] <- "NbrOfInq_ExclHomeAutoLoan_Last12Mnths"
colnames(credit_bureau)[which(names(credit_bureau) == "Presence.of.open.home.loan")] <- "HasOpenHomeLoan"
colnames(credit_bureau)[which(names(credit_bureau) == "Presence.of.open.auto.loan")] <- "HasOpenAutoLoan"
colnames(credit_bureau)[which(names(credit_bureau) == "Outstanding.Balance")] <- "OutstandingBal"
colnames(credit_bureau)[which(names(credit_bureau) == "Total.No.of.Trades")] <- "TotalNbrOfTrades"


# All the 19 Columns of credit_bureau - 
colnames(credit_bureau)
# [1] "Application.ID"                        
# [2] "NbrOf90DPDLast6Mnths"                 
# [3] "NbrOf60DPDLast6Mnths"                  
# [4] "NbrOf30DPDLast6Mnths"                 
# [5] "NbrOf90DPDLast12Mnths"                 
# [6] "NbrOf60DPDLast12Mnths"                
# [7] "NbrOf30DPDLast12Mnths"                 
# [8] "AvgCCUtilLast12Mnths"                 
# [9] "NbrOfTradesOpenLast6Mnths"             
#[10] "NbrOfTradesOpenLast12Mnths"           
#[11] "NbrOfPLTradesOpenLast6Mnths"           
#[12] "NbrOfPLTradesOpenLast12Mnths"         
#[13] "NbrOfInq_ExclHomeAutoLoan_Last6Mnths"  
#[14] "NbrOfInq_ExclHomeAutoLoan_Last12Mnths"
#[15] "HasOpenHomeLoan"                       
#[16] "OutstandingBal"                       
#[17] "TotalNbrOfTrades"                      
#[18] "HasOpenAutoLoan"                      
#[19] "Performance.Tag"          

summary(credit_bureau)          
# Obs: would check them in univariate analysis in detail 

#Checking for duplicate record
sum(duplicated(credit_bureau)) 
# Obs: No duplicate record

# Checking for duplicate Application.ID
sum(duplicated(credit_bureau$Application.ID)) 
# Obs: There are 3 duplicate Application.ID

credit_bureau$Application.ID[which(duplicated(credit_bureau$Application.ID))]
# Obs: Following Applications are duplicated - 
# 765011468, 653287861, 671989187

# checkout those duplicated records -
credit_bureau[credit_bureau$Application.ID %in% c(765011468, 653287861, 671989187),]
# Obs: These records indicate erroneous data source and it may mislead us
# So since these are 6 records we would dropping them from further analysis.

#Removing the records corresponding to duplicated Application.ID
credit_bureau <- credit_bureau[!credit_bureau$Application.ID %in% c(765011468, 653287861, 671989187),]

##############################################################################
#### Creating a master file by merging demographic and credit bureau data ####
##############################################################################

# Verify before merging whether the Applcation.IDs are same
# in both demographic and credit_bureau datasets.
setdiff(credit_bureau$Application.ID,demographic$Application.ID)
# Obs: integer(0)
# There are no difference in the ApplicationIDs between demographic and credit_bureau
# All the application IDs are unique in both the data sets.

master_ds_original<- merge(demographic,credit_bureau,by=c('Application.ID','Performance.Tag'))

# Verify the rows in the 2 individual data frames and the master data frame
# to conclude that they have been merged successfully.

nrow(master_ds_original)     ## 71289 rows
nrow(demographic)   ## 71289 rows
nrow(credit_bureau) ## 71289 rows

length(unique(master_ds_original$Application.ID))      ## 71289 rows
length(unique(demographic$Application.ID))    ## 71289 rows
length(unique(credit_bureau$Application.ID))  ## 71289 rows

ncol(master_ds_original)     ## 29 columns = 12 + 19 - 2(Application.Id + Performance.Tag)
ncol(demographic)   ## 12 columns
ncol(credit_bureau) ## 19 columns


# conclusion: demographic and credit_bureau have been merged successfully.

# Putting Performance.Tag in the last of the data frame
master_ds_original <- master_ds_original[,c(1,3:ncol(master_ds_original),2)]

str(master_ds_original)

#Checking missing values in master data set

sapply(master_ds_original,function(x)sum(is.na(x)))
# Obs :
# Application.ID                          0
# Age                                     0 
# Gender                                  0
# No.of.dependents                        3       <====
# Education                               0
# Type.of.residence                       0
# NbrMnthsCurrCompany                     0
# Marital.Status                          0 
# Income                                  0 
# Profession                              0 
# NbrMnthsCurrResidence                   0 
# NbrOf30DPDLast6Mnths                    0 
# NbrOf60DPDLast6Mnths                    0
# NbrOf90DPDLast6Mnths                    0 
# NbrOf30DPDLast12Mnths                   0
# NbrOf60DPDLast12Mnths                   0  
# NbrOf90DPDLast12Mnths                   0
# NbrOfTradesOpenLast6Mnths               1     <=====
# NbrOfTradesOpenLast12Mnths              0 
# NbrOfPLTradesOpenLast6Mnths             0
# NbrOfPLTradesOpenLast12Mnths            0 
# TotalNbrOfTrades                        0
# NbrOfInq_ExclHomeAutoLoan_Last12Mnths   0 
# NbrOfInq_ExclHomeAutoLoan_Last6Mnths    0
# HasOpenHomeLoan                         272   <=====
# HasOpenAutoLoan                         0 
# AvgCCUtilLast12Mnths                    1058  <=====
# OutstandingBal                          272   <=====
# Performance.Tag                         1425  <=====


# Remove the Rejected applications from the total master dataset
master_ds_reject <- master_ds_original[which(is.na(master_ds_original$Performance.Tag)),]

master_ds <- master_ds_original[-which(is.na(master_ds_original$Performance.Tag)),]

################ Exploratory Data Analysis on the columns ###################

#############################################################################
#################### Performing Univariate Analysis #########################
#############################################################################

#Since Information packge treat 1 as 'good', we are adding a new variable 'Reverse.Performance.Tag'in 
#master data frame  
master_ds$Reverse.Performance.Tag <- ifelse(master_ds$Performance.Tag == 0,1,0)

####################################
# 1. Age (Continuous -> Binned) :- 
#####################################

# Checking summary
summary(master_ds$Age) 
# Obs: There are some -ve values which are wrong as age can't be <0

# Checking how many Age values are <= 0
sum(master_ds$Age <= 0)
# Obs: 20
# Since only 20 erroneous values are there in Age column which is 
# very small w.r.t total no records. So, we can directly remove 
# corresponding records from master data set.

nrow(master_ds)
master_ds <- master_ds[-which(master_ds$Age <= 0),]
nrow(master_ds)
# Obs: Now there are 69844 rows

summary(master_ds$Age)

# Checking outliers 
boxplot(master_ds$Age) 
# Obs: No outliers 

# Plotting histogram 
ggplot(master_ds,aes(Age))+geom_histogram()

# Binning the age variable and store it into "binning.age".
master_ds$binning.age <- as.factor(cut(master_ds$Age, breaks = c(10,20,30,40,50,60,70)))

ggplot(master_ds,aes(binning.age))+geom_bar()

# Create WOE variable for age buckets
temp_ds <- create_infotables(data=master_ds[,c("binning.age","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percent and WOE of each bucket
temp_ds$Tables$binning.age

# Merging (10,20] bucket into (20,30] bucket 
# as (10,20] bucket has less than 5% of observations.
levels(master_ds$binning.age)
levels(master_ds$binning.age)[which(levels(master_ds$binning.age)=="(10,20]")] <- 
  levels(master_ds$binning.age)[which(levels(master_ds$binning.age)=="(50,60]")]
levels(master_ds$binning.age)

# Create WOE variable for age buckets
temp_ds <- create_infotables(data=master_ds[,c("binning.age","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percent and WOE of each bucket
temp_ds$Tables$binning.age

# Create a scatter plot to find the monotonicity of data points.
ggplot(temp_ds$Tables$binning.age,aes(x=binning.age,y=WOE,group=1))+
  geom_point() + 
  geom_line()

# Removing Age column as its been replaced with Age Buckets
ncol(master_ds)
master_ds <- subset(master_ds, select = -c(Age))
ncol(master_ds)

####################################
# 2. Gender (Categorical):-
####################################

str(master_ds$Gender)

# checking Summary
summary(as.factor(master_ds$Gender))

# Obs: 1 blank value
# Since only one blank value is there we can directly 
# remove correponding record from master data set.

nrow(master_ds)
master_ds <- master_ds[- which(master_ds$Gender == ""),]
nrow(master_ds)
# Obs: There are 69843 rows remaining now.

summary(as.factor(master_ds$Gender))

# Converting gender as factor type
master_ds$Gender <- as.factor(master_ds$Gender) 

# Create WOE variable for Gender buckets
temp_ds <- create_infotables(data=master_ds[,c("Gender","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$Gender

# Create a scatter plot to find the monotonicity of data points.
ggplot(temp_ds$Tables$Gender,aes(x=Gender,y=WOE,group=1))+
  geom_point() + 
  geom_line()

####################################
# 3. Marital.Status (Categorical):-
####################################

#Checking summary 
summary(as.factor(master_ds$Marital.Status)) 
# Obs: 5 blank values
# Since only 5 blank values are there which is very small
# corresponding to total no records, we can remove corresponding 
# records from master data set.

nrow(master_ds)
master_ds <- master_ds[- which(master_ds$Marital.Status == ""),]
nrow(master_ds)
# There are 69838 obs remaining

summary(as.factor(master_ds$Marital.Status))

# Converting Marital.Status as factor type
master_ds$Marital.Status <- as.factor(master_ds$Marital.Status)

# Create WOE variable for categories Marital.Status
temp_ds <- create_infotables(data=master_ds[,c("Marital.Status","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$Marital.Status

# Create a scatter plot to find the monotonicity of data points.
ggplot(temp_ds$Tables$Marital.Status,aes(x=Marital.Status,y=WOE,group=1))+
  geom_point() + 
  geom_line()

####################################
# 4. No.of.Dependents (Categorical):-
####################################

#Checking summary
summary(as.factor(master_ds$No.of.dependents)) 
# Obs: 2 NAs 
# Since only 2 NAs are there which is very small out of total records,
# we can remove corresponding records from master data set

nrow(master_ds)
master_ds <- master_ds[- which(is.na(master_ds$No.of.dependents)),] 
nrow(master_ds)
# There are 69836 records remaining

summary(as.factor(master_ds$No.of.dependents))

# Converting No.of.dependents as factor type
master_ds$No.of.dependents <- as.factor(master_ds$No.of.dependents)

levels(master_ds$No.of.dependents)


# Create WOE variable for categories No.of.dependents
temp_ds <- create_infotables(data=master_ds[,c("No.of.dependents","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$No.of.dependents

# Create a scatter plot to find the monotonicity of data points.
ggplot(temp_ds$Tables$No.of.dependents,aes(x=No.of.dependents,y=WOE,group=1))+
  geom_point() + 
  geom_line()

####################################
# 5. Income (Continuous -> Binned):-
####################################

str(master_ds$Income)

# Checking summary
summary(master_ds$Income) 
# Obs: There are some -ve values which are wrong as Income can't be <0

# Checking how many Income values are < 0
sum(master_ds$Income < 0) 
# Obs: 81
# Since only 81 values of Income are < 0 
# which is very small out of total records,
# we can remove corresponding records from master data set

nrow(master_ds)
master_ds <- master_ds[- which(master_ds$Income < 0),]
nrow(master_ds)
# There are 69755 records remaining 

summary(master_ds$Income)

#Checking for outliers
boxplot(master_ds$Income) #No outliers

#Checking histogram
ggplot(master_ds, aes(Income)) + geom_histogram()

# Binning Income in different bin and storing it in binning.income variable
master_ds$binning.income <- as.factor(cut(master_ds$Income, breaks = c(0,10,20, 30, 40, 50, 60), include.lowest = T))

ggplot(master_ds,aes(binning.income))+geom_bar()

# Create WOE variable for categories Income
temp_ds <- create_infotables(data=master_ds[,c("binning.income","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$binning.income

# Merging 4th bucket into 3rd as they both have similar WOE
levels(master_ds$binning.income)
levels(master_ds$binning.income)[which(levels(master_ds$binning.income)=="(40,50]")] <- 
  levels(master_ds$binning.income)[which(levels(master_ds$binning.income)=="(30,40]")]
levels(master_ds$binning.income)
levels(master_ds$binning.income)[which(levels(master_ds$binning.income)=="(30,40]")] <- 
  "(30,50]"
levels(master_ds$binning.income)

# Create WOE variable for categories Income
temp_ds <- create_infotables(data=master_ds[,c("binning.income","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$binning.income

# Create a scatter plot to find the monotonicity of data points.
ggplot(temp_ds$Tables$binning.income,aes(x=binning.income,y=WOE,group=1))+
  geom_point() + 
  geom_line()

# Removing Income column as its been replaced with Income Buckets
ncol(master_ds)
master_ds <- subset(master_ds, select = -c(Income))
ncol(master_ds)


####################################
# 6. Education (Categorical):-
####################################

#Checking summary
summary(as.factor(master_ds$Education))
# There are 118 blanks in Education and we would remove the data as 
# its far less than total set.

nrow(master_ds)
master_ds <- master_ds[- which(master_ds$Education == ""),]
nrow(master_ds)
# There are 69637 records remaining

summary(as.factor(master_ds$Education))

# Converting Education variable as factor type
master_ds$Education <- as.factor(master_ds$Education)

# Create WOE variable for categories Education
temp_ds <- create_infotables(data=master_ds[,c("Education","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$Education

# Merging Others into Bachelor as Others are <5% of total observations
levels(master_ds$Education)

levels(master_ds$Education)[which(levels(master_ds$Education)=="Others")] <- 
  levels(master_ds$Education)[which(levels(master_ds$Education)=="Bachelor")]

levels(master_ds$Education)

#Renaming the merged group
levels(master_ds$Education)[which(levels(master_ds$Education)=="Bachelor")] <- 
  "Bachelor_others"
levels(master_ds$Education)

# Create WOE variable for categories Education
temp_ds <- create_infotables(data=master_ds[,c("Education","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$Education

# Create a scatter plot to find the monotonicity of data points.
ggplot(temp_ds$Tables$Education,aes(x=Education,y=WOE,group=1))+
  geom_point() + 
  geom_line()

###############################
# 7. Profession (Categorical):-
###############################

#Checking summary
summary(as.factor(master_ds$Profession))
# Obs:There is 12 blanks which is very small in number compare to 
# total no of records. so, we can remove records corresponding to these 
# blanks from master data set.

nrow(master_ds)
master_ds <- master_ds[- which(master_ds$Profession == ""),]
nrow(master_ds)
# There are 69625 observations remaining

summary(as.factor(master_ds$Profession))

#Converting Profession as factor type
master_ds$Profession <- as.factor(master_ds$Profession)

# Create WOE variable for categories Profession
temp_ds <- create_infotables(data=master_ds[,c("Profession","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$Profession

# Create a scatter plot to find the monotonicity of data points.
ggplot(temp_ds$Tables$Profession,aes(x=Profession,y=WOE,group=1))+
  geom_point() + 
  geom_line()

######################################
# 8. Type.of.residence (Categorical):-
######################################

#Checking summary
summary(as.factor(master_ds$Type.of.residence))
# Obs: There is 8 blanks which is very small in number compare to total no of records. 
# So, we can remove records corresponding to these blanks from master data set.

nrow(master_ds)
master_ds <- master_ds[- which(master_ds$Type.of.residence == ""),]
nrow(master_ds)
# there are 69617 rows remaining 

summary(as.factor(master_ds$Type.of.residence))

#Converting Type.of.residence as factor type
master_ds$Type.of.residence <- as.factor(master_ds$Type.of.residence)

# Create WOE variable for categories Type.of.residence
temp_ds <- create_infotables(data=master_ds[,c("Type.of.residence","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$Type.of.residence

# Merging category - "Others" into "Rented" as Others have less than 5%
levels(master_ds$Type.of.residence)
levels(master_ds$Type.of.residence)[which(levels(master_ds$Type.of.residence)=="Others")] <- 
  levels(master_ds$Type.of.residence)[which(levels(master_ds$Type.of.residence)=="Rented")]
levels(master_ds$Type.of.residence)

#Renaming the merged groups
levels(master_ds$Type.of.residence)[which(levels(master_ds$Type.of.residence)=="Rented")] <- 
  "Rented_Others"
levels(master_ds$Type.of.residence)

# Create WOE variable for categories Type.of.residence
temp_ds <- create_infotables(data=master_ds[,c("Type.of.residence","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$Type.of.residence

# Merging category - "Living with Parents" into "Company Provided" 
# as Living with Parents have less than 5% and renaming the level to Others
levels(master_ds$Type.of.residence)
levels(master_ds$Type.of.residence)[which(levels(master_ds$Type.of.residence)=="Company provided")] <- 
  levels(master_ds$Type.of.residence)[which(levels(master_ds$Type.of.residence)=="Living with Parents")]
levels(master_ds$Type.of.residence)

levels(master_ds$Type.of.residence)[which(levels(master_ds$Type.of.residence)=="Living with Parents")] <- 
  "CompanyProvided_LivingWithParents"
levels(master_ds$Type.of.residence)

# Create WOE variable for categories Type.of.residence
temp_ds <- create_infotables(data=master_ds[,c("Type.of.residence","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$Type.of.residence

# Create a scatter plot to find the monotonicity of data points.
ggplot(temp_ds$Tables$Type.of.residence,aes(x=Type.of.residence,y=WOE,group=1))+
  geom_point() + 
  geom_line()


###################################################
# 9. NbrMnthsCurrResidence (Continuous -> Binned):-
####################################

#Checking summary
summary(master_ds$NbrMnthsCurrResidence) 
#Obs: No wrong value

#Checking outliers
boxplot(master_ds$NbrMnthsCurrResidence) 
#Obs: No outliers

#Checking histogram
ggplot(master_ds,aes(NbrMnthsCurrResidence))+geom_histogram()

#Binning NbrMnthsCurrResidence and storing it in binning.NbrMnthsCurrResidence variable
master_ds$binning.NbrMnthsCurrResidence <- as.factor(cut(master_ds$NbrMnthsCurrResidence, breaks = c(0,24,48,72,96,120,144)))

ggplot(master_ds,aes(binning.NbrMnthsCurrResidence))+geom_bar()

# Create WOE variable for categories NbrMnthsCurrResidence
temp_ds <- create_infotables(data=master_ds[,c("binning.NbrMnthsCurrResidence","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$binning.NbrMnthsCurrResidence

# Merging 6th bucket (120,144] into 2nd bucket (24,48] as (120,144] has <5%
levels(master_ds$binning.NbrMnthsCurrResidence)
levels(master_ds$binning.NbrMnthsCurrResidence)[which(levels(master_ds$binning.NbrMnthsCurrResidence)=="(120,144]")] <- 
  levels(master_ds$binning.NbrMnthsCurrResidence)[which(levels(master_ds$binning.NbrMnthsCurrResidence)=="(24,48]")]

levels(master_ds$binning.NbrMnthsCurrResidence)

# Create WOE variable for categories NbrMnthsCurrResidence
temp_ds <- create_infotables(data=master_ds[,c("binning.NbrMnthsCurrResidence","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$binning.NbrMnthsCurrResidence

# Create a scatter plot to find the monotonicity of data points.
ggplot(temp_ds$Tables$binning.NbrMnthsCurrResidence,aes(x=binning.NbrMnthsCurrResidence,y=WOE,group=1))+
  geom_point() + 
  geom_line()

# Removing NbrMnthsCurrResidence column as its been replaced with NbrMnthsCurrResidence Buckets
ncol(master_ds)
master_ds <- subset(master_ds, select = -c(NbrMnthsCurrResidence))
ncol(master_ds)
############
##################################################
# 10. NbrMnthsCurrCompany (Continuous -> Binned):-
##################################################

#Checking summary
summary(master_ds$NbrMnthsCurrCompany) 
#Obs: No wrong value

#Checking outliers
boxplot(master_ds$NbrMnthsCurrCompany)
# There are some outliers which will be taken care be WOE transformation.

#Checking histogram
ggplot(master_ds,aes(NbrMnthsCurrCompany))+geom_histogram()

#Binning NbrMnthsCurrCompany and storing it in binning.NbrMnthsCurrCompany variable
master_ds$binning.NbrMnthsCurrCompany <- as.factor(cut(master_ds$NbrMnthsCurrCompany, breaks = c(0,12,24,48,60,72,84,133)))

ggplot(master_ds,aes(binning.NbrMnthsCurrCompany))+geom_bar()

# Create WOE variable for categories NbrMnthsCurrCompany
temp_ds <- create_infotables(data=master_ds[,c("binning.NbrMnthsCurrCompany","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$binning.NbrMnthsCurrCompany

# Merging 7th bucket into 6th bucket as 7th bucket has less than 5%
levels(master_ds$binning.NbrMnthsCurrCompany)
levels(master_ds$binning.NbrMnthsCurrCompany)[which(levels(master_ds$binning.NbrMnthsCurrCompany)=="(84,133]")] <- 
  levels(master_ds$binning.NbrMnthsCurrCompany)[which(levels(master_ds$binning.NbrMnthsCurrCompany)=="(72,84]")]
levels(master_ds$binning.NbrMnthsCurrCompany)

# Create WOE variable for categories NbrMnthsCurrCompany
temp_ds <- create_infotables(data=master_ds[,c("binning.NbrMnthsCurrCompany","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$binning.NbrMnthsCurrCompany

# Merging 6th bucket into 5th bucket as 7th bucket has less than 5%
levels(master_ds$binning.NbrMnthsCurrCompany)
levels(master_ds$binning.NbrMnthsCurrCompany)[which(levels(master_ds$binning.NbrMnthsCurrCompany)=="(72,84]")] <- 
  levels(master_ds$binning.NbrMnthsCurrCompany)[which(levels(master_ds$binning.NbrMnthsCurrCompany)=="(60,72]")]
levels(master_ds$binning.NbrMnthsCurrCompany)

levels(master_ds$binning.NbrMnthsCurrCompany)[which(levels(master_ds$binning.NbrMnthsCurrCompany)=="(60,72]")] <-
  "(60,133]"
levels(master_ds$binning.NbrMnthsCurrCompany)

# Create WOE variable for categories Marital.Status
temp_ds <- create_infotables(data=master_ds[,c("binning.NbrMnthsCurrCompany","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$binning.NbrMnthsCurrCompany

# Merging 2nd bucket into 1st as they have similar WOE
levels(master_ds$binning.NbrMnthsCurrCompany)
levels(master_ds$binning.NbrMnthsCurrCompany)[which(levels(master_ds$binning.NbrMnthsCurrCompany)=="(12,24]")] <- 
  levels(master_ds$binning.NbrMnthsCurrCompany)[which(levels(master_ds$binning.NbrMnthsCurrCompany)=="(0,12]")]
levels(master_ds$binning.NbrMnthsCurrCompany)

levels(master_ds$binning.NbrMnthsCurrCompany)[which(levels(master_ds$binning.NbrMnthsCurrCompany)=="(0,12]")] <-
  "(0,24]"

# Create a scatter plot to find the monotonicity of data points.
ggplot(temp_ds$Tables$binning.NbrMnthsCurrCompany,aes(x=binning.NbrMnthsCurrCompany,y=WOE,group=1))+
  geom_point() + 
  geom_line()

# Removing NbrMnthsCurrCompany column as its been replaced with NbrMnthsCurrCompany Buckets
ncol(master_ds)
master_ds <- subset(master_ds, select = -c(NbrMnthsCurrCompany))
ncol(master_ds)

##########################################
# 11. NbrOf90DPDLast6Mnths (Categorical):-
####################################

#Checking summary
summary(master_ds$NbrOf90DPDLast6Mnths)
#Obs: There are no wrong values

#Checking summary by converting as factor type
summary(as.factor(master_ds$NbrOf90DPDLast6Mnths)) 
#Obs: there are only 4 labels so we will convert them as factor type

master_ds$NbrOf90DPDLast6Mnths <- as.factor(master_ds$NbrOf90DPDLast6Mnths)

ggplot(master_ds,aes(NbrOf90DPDLast6Mnths))+geom_bar()

summary(master_ds$NbrOf90DPDLast6Mnths)

# Create WOE variable for categories NbrOf90DPDLast6Mnths
temp_ds <- create_infotables(data=master_ds[,c("NbrOf90DPDLast6Mnths","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$NbrOf90DPDLast6Mnths

# Clubbing 4th bucket into 3rd bucket as 3rd bucket has less than 5% entries.
levels(master_ds$NbrOf90DPDLast6Mnths)
levels(master_ds$NbrOf90DPDLast6Mnths)[which(levels(master_ds$NbrOf90DPDLast6Mnths)=="3")] <- 
  levels(master_ds$NbrOf90DPDLast6Mnths)[which(levels(master_ds$NbrOf90DPDLast6Mnths)=="2")]
levels(master_ds$NbrOf90DPDLast6Mnths)

levels(master_ds$NbrOf90DPDLast6Mnths)[which(levels(master_ds$NbrOf90DPDLast6Mnths)=="2")] <- "[2,3]"
levels(master_ds$NbrOf90DPDLast6Mnths)

# Create WOE variable for categories NbrOf90DPDLast6Mnths
temp_ds <- create_infotables(data=master_ds[,c("NbrOf90DPDLast6Mnths","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$NbrOf90DPDLast6Mnths

# Clubbing 3rd bucket into 2nd bucket as 3rd bucket has less than 5% entries.
levels(master_ds$NbrOf90DPDLast6Mnths)
levels(master_ds$NbrOf90DPDLast6Mnths)[which(levels(master_ds$NbrOf90DPDLast6Mnths)=="[2,3]")] <- 
  levels(master_ds$NbrOf90DPDLast6Mnths)[which(levels(master_ds$NbrOf90DPDLast6Mnths)=="1")]
levels(master_ds$NbrOf90DPDLast6Mnths)

levels(master_ds$NbrOf90DPDLast6Mnths)[which(levels(master_ds$NbrOf90DPDLast6Mnths)=="1")] <-
  "[1,3]"
levels(master_ds$NbrOf90DPDLast6Mnths)

# Create WOE variable for categories NbrOf90DPDLast6Mnths
temp_ds <- create_infotables(data=master_ds[,c("NbrOf90DPDLast6Mnths","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$NbrOf90DPDLast6Mnths

# Create a scatter plot to find the monotonicity of data points.
ggplot(temp_ds$Tables$NbrOf90DPDLast6Mnths,aes(x=NbrOf90DPDLast6Mnths,y=WOE,group=1))+
  geom_point() + 
  geom_line()
###
##########################################
# 12. NbrOf60DPDLast6Mnths (Categorical):-
####################################

#Checking summary
summary(master_ds$NbrOf60DPDLast6Mnths)
#Obs: There are no wrong values

#Checking summary by converting as factor type
summary(as.factor(master_ds$NbrOf60DPDLast6Mnths))
#Obs: there are only 6 labels so we will convert to factor type

master_ds$NbrOf60DPDLast6Mnths <- as.factor(master_ds$NbrOf60DPDLast6Mnths)

# Create WOE variable for categories NbrOf60DPDLast6Mnths
temp_ds <- create_infotables(data=master_ds[,c("NbrOf60DPDLast6Mnths","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$NbrOf60DPDLast6Mnths

levels(master_ds$NbrOf60DPDLast6Mnths)
levels(master_ds$NbrOf60DPDLast6Mnths)[which(levels(master_ds$NbrOf60DPDLast6Mnths) %in% c("3","4","5"))] <- 
  levels(master_ds$NbrOf60DPDLast6Mnths)[which(levels(master_ds$NbrOf60DPDLast6Mnths)=="2")]
levels(master_ds$NbrOf60DPDLast6Mnths)

levels(master_ds$NbrOf60DPDLast6Mnths)[which(levels(master_ds$NbrOf60DPDLast6Mnths)=="2")] <- 
 "[2,5]"
levels(master_ds$NbrOf60DPDLast6Mnths)

# Create WOE variable for categories NbrOf60DPDLast6Mnths
temp_ds <- create_infotables(data=master_ds[,c("NbrOf60DPDLast6Mnths","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$NbrOf60DPDLast6Mnths

# Create a scatter plot to find the monotonicity of data points.
ggplot(temp_ds$Tables$NbrOf60DPDLast6Mnths,aes(x=NbrOf60DPDLast6Mnths,y=WOE,group=1))+
  geom_point() + 
  geom_line()
###
##########################################
# 13. NbrOf30DPDLast6Mnths (Categorical):- 
####################################

#Checking summary
summary(master_ds$NbrOf30DPDLast6Mnths)
#Obs: There are no wrong values

#Checking summary by converting as factor type
summary(as.factor(master_ds$NbrOf30DPDLast6Mnths)) 
#Obs: There are only 8 labels so we will convert it to factor type

master_ds$NbrOf30DPDLast6Mnths <- as.factor(master_ds$NbrOf30DPDLast6Mnths)

# Create WOE variable for categories NbrOf30DPDLast6Mnths
temp_ds <- create_infotables(data=master_ds[,c("NbrOf30DPDLast6Mnths","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$NbrOf30DPDLast6Mnths

levels(master_ds$NbrOf30DPDLast6Mnths)
levels(master_ds$NbrOf30DPDLast6Mnths)[which(levels(master_ds$NbrOf30DPDLast6Mnths) %in% c("4","5","6","7"))] <- 
  levels(master_ds$NbrOf30DPDLast6Mnths)[which(levels(master_ds$NbrOf30DPDLast6Mnths)=="3")]
levels(master_ds$NbrOf30DPDLast6Mnths)

levels(master_ds$NbrOf30DPDLast6Mnths)[which(levels(master_ds$NbrOf30DPDLast6Mnths)=="3")] <- 
 "[3,7]"
levels(master_ds$NbrOf30DPDLast6Mnths)

# Create WOE variable for categories NbrOf30DPDLast6Mnths
temp_ds <- create_infotables(data=master_ds[,c("NbrOf30DPDLast6Mnths","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$NbrOf30DPDLast6Mnths

# Create a scatter plot to find the monotonicity of data points.
ggplot(temp_ds$Tables$NbrOf30DPDLast6Mnths,aes(x=NbrOf30DPDLast6Mnths,y=WOE,group=1))+
  geom_point() + 
  geom_line()
###
###########################################
# 14. NbrOf30DPDLast12Mnths (Categorical):-
####################################

#Checking summary
summary(master_ds$NbrOf30DPDLast12Mnths)
#Obs: There are no wrong values

#Checking summary by converting as factor type
summary(as.factor(master_ds$NbrOf30DPDLast12Mnths))
#Obs: There are only 10 labels so we will convert it to factor type

master_ds$NbrOf30DPDLast12Mnths <- as.factor(master_ds$NbrOf30DPDLast12Mnths)

# Create WOE variable for categories NbrOf30DPDLast12Mnths
temp_ds <- create_infotables(data=master_ds[,c("NbrOf30DPDLast12Mnths","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$NbrOf30DPDLast12Mnths

levels(master_ds$NbrOf30DPDLast12Mnths)
levels(master_ds$NbrOf30DPDLast12Mnths)[which(levels(master_ds$NbrOf30DPDLast12Mnths) %in% c("4","5","6","7","8","9"))] <- 
  levels(master_ds$NbrOf30DPDLast12Mnths)[which(levels(master_ds$NbrOf30DPDLast12Mnths)=="3")]
levels(master_ds$NbrOf30DPDLast12Mnths)

levels(master_ds$NbrOf30DPDLast12Mnths)[which(levels(master_ds$NbrOf30DPDLast12Mnths)=="3")] <- 
 "[3,9]"
levels(master_ds$NbrOf30DPDLast12Mnths)

# Create WOE variable for categories NbrOf30DPDLast12Mnths
temp_ds <- create_infotables(data=master_ds[,c("NbrOf30DPDLast12Mnths","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$NbrOf30DPDLast12Mnths

# Create a scatter plot to find the monotonicity of data points.
ggplot(temp_ds$Tables$NbrOf30DPDLast12Mnths,aes(x=NbrOf30DPDLast12Mnths,y=WOE,group=1))+
  geom_point() + 
  geom_line()

####
###########################################
# 15. NbrOf60DPDLast12Mnths (Categorical):-
####################################

#Checking summary
summary(master_ds$NbrOf60DPDLast12Mnths)
#Obs: There are no wrong values

#Checking summary by converting as factor type
summary(as.factor(master_ds$NbrOf60DPDLast12Mnths)) 
#Obs: There are only 8 labels so we will convert it to factor type

master_ds$NbrOf60DPDLast12Mnths <- as.factor(master_ds$NbrOf60DPDLast12Mnths)

# Create WOE variable for categories NbrOf60DPDLast12Mnths
temp_ds <- create_infotables(data=master_ds[,c("NbrOf60DPDLast12Mnths","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$NbrOf60DPDLast12Mnths

levels(master_ds$NbrOf60DPDLast12Mnths)
levels(master_ds$NbrOf60DPDLast12Mnths)[which(levels(master_ds$NbrOf60DPDLast12Mnths) %in% c("4","5","6","7"))] <- 
  levels(master_ds$NbrOf60DPDLast12Mnths)[which(levels(master_ds$NbrOf60DPDLast12Mnths)=="3")]
levels(master_ds$NbrOf60DPDLast12Mnths)

levels(master_ds$NbrOf60DPDLast12Mnths)[which(levels(master_ds$NbrOf60DPDLast12Mnths)=="3")] <- 
 "[3,7]"
levels(master_ds$NbrOf60DPDLast12Mnths)

# Create WOE variable for categories NbrOf60DPDLast12Mnths
temp_ds <- create_infotables(data=master_ds[,c("NbrOf60DPDLast12Mnths","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$NbrOf60DPDLast12Mnths

# Create a scatter plot to find the monotonicity of data points.
ggplot(temp_ds$Tables$NbrOf60DPDLast12Mnths,aes(x=NbrOf60DPDLast12Mnths,y=WOE,group=1))+
  geom_point() + 
  geom_line()
####
###########################################
# 16. NbrOf90DPDLast12Mnths (Categorical):-
####################################
#Checking summary
summary(master_ds$NbrOf90DPDLast12Mnths)
#Obs: There are no wrong values

#Checking summary by converting as factor type
summary(as.factor(master_ds$NbrOf90DPDLast12Mnths)) 
#Obs: There are only 6 labels so we will convert it to factor type

master_ds$NbrOf90DPDLast12Mnths <- as.factor(master_ds$NbrOf90DPDLast12Mnths)

# Create WOE variable for categories NbrOf90DPDLast12Mnths
temp_ds <- create_infotables(data=master_ds[,c("NbrOf90DPDLast12Mnths","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$NbrOf90DPDLast12Mnths

levels(master_ds$NbrOf90DPDLast12Mnths)
levels(master_ds$NbrOf90DPDLast12Mnths)[which(levels(master_ds$NbrOf90DPDLast12Mnths) %in% c("3","4","5"))] <- 
  levels(master_ds$NbrOf90DPDLast12Mnths)[which(levels(master_ds$NbrOf90DPDLast12Mnths)=="2")]
levels(master_ds$NbrOf90DPDLast12Mnths)

levels(master_ds$NbrOf90DPDLast12Mnths)[which(levels(master_ds$NbrOf90DPDLast12Mnths)=="2")] <- 
 "[2,5]"
levels(master_ds$NbrOf90DPDLast12Mnths)

# Create WOE variable for categories NbrOf90DPDLast12Mnths
temp_ds <- create_infotables(data=master_ds[,c("NbrOf90DPDLast12Mnths","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$NbrOf90DPDLast12Mnths

# Create a scatter plot to find the monotonicity of data points.
ggplot(temp_ds$Tables$NbrOf90DPDLast12Mnths,aes(x=NbrOf90DPDLast12Mnths,y=WOE,group=1))+
  geom_point() + 
  geom_line()
####
###############################################
# 17. NbrOfTradesOpenLast6Mnths (Categorical):-
####################################
#Checking summary
summary(master_ds$NbrOfTradesOpenLast6Mnths)
#Obs: There is single NA value and no wrong values in it.

#Checking summary by converting as factor type
summary(as.factor(master_ds$NbrOfTradesOpenLast6Mnths)) 
# Obs: There are only 13 labels and 1 NA value
# We shall remove the NA value as it is insignificant w.r.t. total number of observations.

nrow(master_ds)
master_ds <- master_ds[- which(is.na(master_ds$NbrOfTradesOpenLast6Mnths)),] 
nrow(master_ds)
# There are 69616 records remaining

summary(as.factor(master_ds$NbrOfTradesOpenLast6Mnths)) 

master_ds$NbrOfTradesOpenLast6Mnths <- as.factor(master_ds$NbrOfTradesOpenLast6Mnths)

# Create WOE variable for categories NbrOfTradesOpenLast6Mnths
temp_ds <- create_infotables(data=master_ds[,c("NbrOfTradesOpenLast6Mnths","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
arrange(temp_ds$Tables$NbrOfTradesOpenLast6Mnths,desc(Percent))

levels(master_ds$NbrOfTradesOpenLast6Mnths)
levels(master_ds$NbrOfTradesOpenLast6Mnths)[which(levels(master_ds$NbrOfTradesOpenLast6Mnths) %in% c("10","11","12"))] <- 
  levels(master_ds$NbrOfTradesOpenLast6Mnths)[which(levels(master_ds$NbrOfTradesOpenLast6Mnths)=="9")]
levels(master_ds$NbrOfTradesOpenLast6Mnths)

levels(master_ds$NbrOfTradesOpenLast6Mnths)[which(levels(master_ds$NbrOfTradesOpenLast6Mnths)=="9")] <- 
 "[9,12]"
levels(master_ds$NbrOfTradesOpenLast6Mnths)

# Create WOE variable for categories NbrOfTradesOpenLast6Mnths
temp_ds <- create_infotables(data=master_ds[,c("NbrOfTradesOpenLast6Mnths","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
arrange(temp_ds$Tables$NbrOfTradesOpenLast6Mnths,desc(Percent))

levels(master_ds$NbrOfTradesOpenLast6Mnths)
levels(master_ds$NbrOfTradesOpenLast6Mnths)[which(levels(master_ds$NbrOfTradesOpenLast6Mnths) %in% c("7","[9,12]"))] <- 
  levels(master_ds$NbrOfTradesOpenLast6Mnths)[which(levels(master_ds$NbrOfTradesOpenLast6Mnths)=="1")]
levels(master_ds$NbrOfTradesOpenLast6Mnths)

levels(master_ds$NbrOfTradesOpenLast6Mnths)[which(levels(master_ds$NbrOfTradesOpenLast6Mnths)=="1")] <- 
 "[1,7,[9,12]]"
levels(master_ds$NbrOfTradesOpenLast6Mnths)

# Create WOE variable for categories NbrOfTradesOpenLast6Mnths
temp_ds <- create_infotables(data=master_ds[,c("NbrOfTradesOpenLast6Mnths","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
arrange(temp_ds$Tables$NbrOfTradesOpenLast6Mnths,desc(Percent))

levels(master_ds$NbrOfTradesOpenLast6Mnths)
levels(master_ds$NbrOfTradesOpenLast6Mnths)[which(levels(master_ds$NbrOfTradesOpenLast6Mnths) %in% c("8"))] <- 
  levels(master_ds$NbrOfTradesOpenLast6Mnths)[which(levels(master_ds$NbrOfTradesOpenLast6Mnths)=="6")]
levels(master_ds$NbrOfTradesOpenLast6Mnths)

levels(master_ds$NbrOfTradesOpenLast6Mnths)[which(levels(master_ds$NbrOfTradesOpenLast6Mnths)=="6")] <- 
 "6&8"
levels(master_ds$NbrOfTradesOpenLast6Mnths)

# Create WOE variable for categories NbrOfTradesOpenLast6Mnths
temp_ds <- create_infotables(data=master_ds[,c("NbrOfTradesOpenLast6Mnths","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
arrange(temp_ds$Tables$NbrOfTradesOpenLast6Mnths,desc(WOE))

# Create a scatter plot to find the monotonicity of data points.
ggplot(temp_ds$Tables$NbrOfTradesOpenLast6Mnths,aes(x=NbrOfTradesOpenLast6Mnths,y=WOE,group=1))+
  geom_point() + 
  geom_line()

########
#########################################################
# 18. NbrOfTradesOpenLast12Mnths (Continuous -> Binned):-
#########################################################

str(master_ds$NbrOfTradesOpenLast12Mnths)

#Checking summary
summary(master_ds$NbrOfTradesOpenLast12Mnths)
#Obs: There no no wrong values in it.

#Checking summary by converting as factor type
summary(as.factor(master_ds$NbrOfTradesOpenLast12Mnths)) 
#Obs: There are only 29 levels, need to check for outliers

ggplot(master_ds,aes(NbrOfTradesOpenLast12Mnths))+geom_histogram()

#Binning TotalNbrOfTrades and storing it in NbrOfTradesOpenLast12Mnths variable
master_ds$binning.NbrOfTradesOpenLast12Mnths <- as.factor(cut(master_ds$NbrOfTradesOpenLast12Mnths, breaks = seq(0,30,5), include.lowest = T))

ggplot(master_ds,aes(binning.NbrOfTradesOpenLast12Mnths))+geom_bar()

# check for outliers
boxplot(master_ds$NbrOfTradesOpenLast12Mnths)
# there are outliers which would be handled by WOE imputation

# Create WOE variable for categories NbrOfTradesOpenLast12Mnths
temp_ds <- create_infotables(data=master_ds[,c("binning.NbrOfTradesOpenLast12Mnths","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
arrange(temp_ds$Tables$binning.NbrOfTradesOpenLast12Mnths,desc(Percent))

levels(master_ds$binning.NbrOfTradesOpenLast12Mnths)
levels(master_ds$binning.NbrOfTradesOpenLast12Mnths)[which(levels(master_ds$binning.NbrOfTradesOpenLast12Mnths) %in% c("(25,30]"))] <- 
  levels(master_ds$binning.NbrOfTradesOpenLast12Mnths)[which(levels(master_ds$binning.NbrOfTradesOpenLast12Mnths)=="[0,5]")]
levels(master_ds$binning.NbrOfTradesOpenLast12Mnths)

levels(master_ds$binning.NbrOfTradesOpenLast12Mnths)[which(levels(master_ds$binning.NbrOfTradesOpenLast12Mnths)=="[0,5]")] <- 
 "[0,5]&[(25,30]]"
levels(master_ds$binning.NbrOfTradesOpenLast12Mnths)

# Create WOE variable for categories NbrOfTradesOpenLast12Mnths
temp_ds <- create_infotables(data=master_ds[,c("binning.NbrOfTradesOpenLast12Mnths","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
arrange(temp_ds$Tables$binning.NbrOfTradesOpenLast12Mnths,desc(Percent))

levels(master_ds$binning.NbrOfTradesOpenLast12Mnths)
levels(master_ds$binning.NbrOfTradesOpenLast12Mnths)[which(levels(master_ds$binning.NbrOfTradesOpenLast12Mnths) %in% c("(20,25]"))] <- 
  levels(master_ds$binning.NbrOfTradesOpenLast12Mnths)[which(levels(master_ds$binning.NbrOfTradesOpenLast12Mnths)=="(15,20]")]
levels(master_ds$binning.NbrOfTradesOpenLast12Mnths)

levels(master_ds$binning.NbrOfTradesOpenLast12Mnths)[which(levels(master_ds$binning.NbrOfTradesOpenLast12Mnths)=="(15,20]")] <- 
"(15,25]"
levels(master_ds$binning.NbrOfTradesOpenLast12Mnths)

# Create WOE variable for categories NbrOfTradesOpenLast12Mnths
temp_ds <- create_infotables(data=master_ds[,c("binning.NbrOfTradesOpenLast12Mnths","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
arrange(temp_ds$Tables$binning.NbrOfTradesOpenLast12Mnths,desc(Percent))

# Create a scatter plot to find the monotonicity of data points.
ggplot(temp_ds$Tables$binning.NbrOfTradesOpenLast12Mnths,aes(x=binning.NbrOfTradesOpenLast12Mnths,y=WOE,group=1))+
  geom_point() + 
  geom_line()

# Removing Income column as its been replaced with Income Buckets
ncol(master_ds)
master_ds <- subset(master_ds, select = -c(NbrOfTradesOpenLast12Mnths))
ncol(master_ds)

#################################################
# 19. NbrOfPLTradesOpenLast6Mnths (Categorical):-
#################################################
#Checking summary
summary(master_ds$NbrOfPLTradesOpenLast6Mnths)
#Obs: There no no wrong values in it.

#Checking summary by converting as factor type
summary(as.factor(master_ds$NbrOfPLTradesOpenLast6Mnths)) 
#Obs: There are only 6 labels, need to check for outliers

# check for outliers
boxplot(master_ds$NbrOfPLTradesOpenLast6Mnths)
# there is single outlier which would be handled by WOE imputation

master_ds$NbrOfPLTradesOpenLast6Mnths <- as.factor(master_ds$NbrOfPLTradesOpenLast6Mnths)

# Create WOE variable for categories NbrOfPLTradesOpenLast6Mnths
temp_ds <- create_infotables(data=master_ds[,c("NbrOfPLTradesOpenLast6Mnths","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
arrange(temp_ds$Tables$NbrOfPLTradesOpenLast6Mnths,desc(Percent))

levels(master_ds$NbrOfPLTradesOpenLast6Mnths)
levels(master_ds$NbrOfPLTradesOpenLast6Mnths)[which(levels(master_ds$NbrOfPLTradesOpenLast6Mnths) %in% c("6"))] <- 
  levels(master_ds$NbrOfPLTradesOpenLast6Mnths)[which(levels(master_ds$NbrOfPLTradesOpenLast6Mnths)=="0")]
levels(master_ds$NbrOfPLTradesOpenLast6Mnths)[which(levels(master_ds$NbrOfPLTradesOpenLast6Mnths) %in% c("5"))] <- 
  levels(master_ds$NbrOfPLTradesOpenLast6Mnths)[which(levels(master_ds$NbrOfPLTradesOpenLast6Mnths)=="1")]

levels(master_ds$NbrOfPLTradesOpenLast6Mnths)

levels(master_ds$NbrOfPLTradesOpenLast6Mnths)[which(levels(master_ds$NbrOfPLTradesOpenLast6Mnths)=="0")] <- 
"[0&6]"
levels(master_ds$NbrOfPLTradesOpenLast6Mnths)[which(levels(master_ds$NbrOfPLTradesOpenLast6Mnths)=="1")] <- 
"[1&5]"

levels(master_ds$NbrOfPLTradesOpenLast6Mnths)

# Create WOE variable for categories NbrOfPLTradesOpenLast6Mnths
temp_ds <- create_infotables(data=master_ds[,c("NbrOfPLTradesOpenLast6Mnths","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
arrange(temp_ds$Tables$NbrOfPLTradesOpenLast6Mnths,desc(WOE))

# Create a scatter plot to find the monotonicity of data points.
ggplot(temp_ds$Tables$NbrOfPLTradesOpenLast6Mnths,aes(x=NbrOfPLTradesOpenLast6Mnths,y=WOE,group=1))+
  geom_point() + 
  geom_line()
##########
##################################################
# 20. NbrOfPLTradesOpenLast12Mnths (Categorical):-
##################################################
#Checking summary
summary(master_ds$NbrOfPLTradesOpenLast12Mnths)
#Obs: There no no wrong values in it.

#Checking summary by converting as factor type
summary(as.factor(master_ds$NbrOfPLTradesOpenLast12Mnths)) 
#Obs: There are only 13 labels, need to check for outliers

# check for outliers
boxplot(master_ds$NbrOfPLTradesOpenLast12Mnths)
# there are 2 outliers which would be handled by WOE computation.

master_ds$NbrOfPLTradesOpenLast12Mnths <- as.factor(master_ds$NbrOfPLTradesOpenLast12Mnths)

# Create WOE variable for categories NbrOfPLTradesOpenLast12Mnths
temp_ds <- create_infotables(data=master_ds[,c("NbrOfPLTradesOpenLast12Mnths","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
arrange(temp_ds$Tables$NbrOfPLTradesOpenLast12Mnths,desc(Percent))

levels(master_ds$NbrOfPLTradesOpenLast12Mnths)
levels(master_ds$NbrOfPLTradesOpenLast12Mnths)[which(levels(master_ds$NbrOfPLTradesOpenLast12Mnths) %in% c("1"))] <- 
  levels(master_ds$NbrOfPLTradesOpenLast12Mnths)[which(levels(master_ds$NbrOfPLTradesOpenLast12Mnths)=="10")]
levels(master_ds$NbrOfPLTradesOpenLast12Mnths)[which(levels(master_ds$NbrOfPLTradesOpenLast12Mnths) %in% c("4"))] <- 
  levels(master_ds$NbrOfPLTradesOpenLast12Mnths)[which(levels(master_ds$NbrOfPLTradesOpenLast12Mnths)=="12")]
levels(master_ds$NbrOfPLTradesOpenLast12Mnths)[which(levels(master_ds$NbrOfPLTradesOpenLast12Mnths) %in% c("7","8","9","11"))] <- 
  levels(master_ds$NbrOfPLTradesOpenLast12Mnths)[which(levels(master_ds$NbrOfPLTradesOpenLast12Mnths)=="7")]
levels(master_ds$NbrOfPLTradesOpenLast12Mnths)

levels(master_ds$NbrOfPLTradesOpenLast12Mnths)[which(levels(master_ds$NbrOfPLTradesOpenLast12Mnths)=="10")] <- 
"[1&10]"
levels(master_ds$NbrOfPLTradesOpenLast12Mnths)[which(levels(master_ds$NbrOfPLTradesOpenLast12Mnths)=="12")] <- 
"[4&12]"
levels(master_ds$NbrOfPLTradesOpenLast12Mnths)[which(levels(master_ds$NbrOfPLTradesOpenLast12Mnths)=="7")] <- 
"[[7,9]&11]"

levels(master_ds$NbrOfPLTradesOpenLast12Mnths)

# Create WOE variable for categories NbrOfPLTradesOpenLast12Mnths
temp_ds <- create_infotables(data=master_ds[,c("NbrOfPLTradesOpenLast12Mnths","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
arrange(temp_ds$Tables$NbrOfPLTradesOpenLast12Mnths,desc(Percent))

# Create a scatter plot to find the monotonicity of data points.
ggplot(temp_ds$Tables$NbrOfPLTradesOpenLast12Mnths,aes(x=NbrOfPLTradesOpenLast12Mnths,y=WOE,group=1))+
  geom_point() + 
  geom_line()
###########
###########################################
# 21. TotalNbrOfTrades (Continuous - Bin):-
###########################################
#Checking summary
summary(master_ds$TotalNbrOfTrades)
#Obs: There no no wrong values in it.
#     There are 44 values, need to check for outliers

# check for outliers
boxplot(master_ds$TotalNbrOfTrades)
# there are number of outliers which would be handled by WOE computation.

ggplot(master_ds,aes(TotalNbrOfTrades))+geom_histogram()

#Binning TotalNbrOfTrades and storing it in binning.TotalNbrOfTrades variable
master_ds$binning.TotalNbrOfTrades <- as.factor(cut(master_ds$TotalNbrOfTrades, breaks = seq(0,50,5), include.lowest = T))

ggplot(master_ds,aes(binning.TotalNbrOfTrades))+geom_bar()

# Create WOE variable for categories TotalNbrOfTrades
temp_ds <- create_infotables(data=master_ds[,c("binning.TotalNbrOfTrades","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
arrange(temp_ds$Tables$binning.TotalNbrOfTrades,desc(Percent))

levels(master_ds$binning.TotalNbrOfTrades)
levels(master_ds$binning.TotalNbrOfTrades)[which(levels(master_ds$binning.TotalNbrOfTrades) %in% c("(40,45]","(25,30]","(15,20]"))] <- 
  levels(master_ds$binning.TotalNbrOfTrades)[which(levels(master_ds$binning.TotalNbrOfTrades)=="(20,25]")]
levels(master_ds$binning.TotalNbrOfTrades)[which(levels(master_ds$binning.TotalNbrOfTrades) %in% c("(35,40]"))] <- 
  levels(master_ds$binning.TotalNbrOfTrades)[which(levels(master_ds$binning.TotalNbrOfTrades)=="[0,5]")]
levels(master_ds$binning.TotalNbrOfTrades)[which(levels(master_ds$binning.TotalNbrOfTrades) %in% c("(30,35]"))] <- 
  levels(master_ds$binning.TotalNbrOfTrades)[which(levels(master_ds$binning.TotalNbrOfTrades)=="(5,10]")]
levels(master_ds$binning.TotalNbrOfTrades)

levels(master_ds$binning.TotalNbrOfTrades)[which(levels(master_ds$binning.TotalNbrOfTrades)=="(20,25]")] <- 
"[(15,30]&(40,45]]"
levels(master_ds$binning.TotalNbrOfTrades)[which(levels(master_ds$binning.TotalNbrOfTrades)=="[0,5]")] <- 
"[[0,5]&(35,40]]"
levels(master_ds$binning.TotalNbrOfTrades)[which(levels(master_ds$binning.TotalNbrOfTrades)=="(5,10]")] <- 
"[(5,10]&(30,35]]"

levels(master_ds$binning.TotalNbrOfTrades)

# Create WOE variable for categories TotalNbrOfTrades
temp_ds <- create_infotables(data=master_ds[,c("binning.TotalNbrOfTrades","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
arrange(temp_ds$Tables$binning.TotalNbrOfTrades,desc(Percent))

# Create a scatter plot to find the monotonicity of data points.
ggplot(temp_ds$Tables$binning.TotalNbrOfTrades,aes(x=binning.TotalNbrOfTrades,y=WOE,group=1))+
  geom_point() + 
  geom_line()

# Removing TotalNbrOfTrades column as its been replaced with TotalNbrOfTrades Buckets
ncol(master_ds)
master_ds <- subset(master_ds, select = -c(TotalNbrOfTrades))
ncol(master_ds)
####
################################################################
# 22. NbrOfInq_ExclHomeAutoLoan_Last12Mnths (Continuous - Bin):-
################################################################
#Checking summary
summary(master_ds$NbrOfInq_ExclHomeAutoLoan_Last12Mnths)
summary(as.factor(master_ds$NbrOfInq_ExclHomeAutoLoan_Last12Mnths))
#Obs: There no no wrong values in it.
#     The max value being 20

# check for outliers
boxplot(master_ds$NbrOfInq_ExclHomeAutoLoan_Last12Mnths)
# there are outliers which would be handled by WOE computation.

ggplot(master_ds,aes(NbrOfInq_ExclHomeAutoLoan_Last12Mnths))+geom_histogram()

#Binning NbrOfInq_ExclHomeAutoLoan_Last12Mnths and storing it in binning.NbrOfInq_ExclHomeAutoLoan_Last12Mnths variable
master_ds$binning.NbrOfInq_ExclHomeAutoLoan_Last12Mnths <- as.factor(cut(master_ds$NbrOfInq_ExclHomeAutoLoan_Last12Mnths, breaks = seq(0,20,2), include.lowest = T))

ggplot(master_ds,aes(binning.NbrOfInq_ExclHomeAutoLoan_Last12Mnths))+geom_bar()

# Create WOE variable for categories NbrOfInq_ExclHomeAutoLoan_Last12Mnths
temp_ds <- create_infotables(data=master_ds[,c("binning.NbrOfInq_ExclHomeAutoLoan_Last12Mnths","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
arrange(temp_ds$Tables$binning.NbrOfInq_ExclHomeAutoLoan_Last12Mnths,desc(Percent))

levels(master_ds$binning.NbrOfInq_ExclHomeAutoLoan_Last12Mnths)
levels(master_ds$binning.NbrOfInq_ExclHomeAutoLoan_Last12Mnths)[which(levels(master_ds$binning.NbrOfInq_ExclHomeAutoLoan_Last12Mnths) %in% c("(12,14]","(14,16]","(18,20]"))] <- 
  levels(master_ds$binning.NbrOfInq_ExclHomeAutoLoan_Last12Mnths)[which(levels(master_ds$binning.NbrOfInq_ExclHomeAutoLoan_Last12Mnths)=="[0,2]")]
levels(master_ds$binning.NbrOfInq_ExclHomeAutoLoan_Last12Mnths)[which(levels(master_ds$binning.NbrOfInq_ExclHomeAutoLoan_Last12Mnths)=="[0,2]")] <- 
  "[0,2]&(12,16]&(18,20]"
levels(master_ds$binning.NbrOfInq_ExclHomeAutoLoan_Last12Mnths)[which(levels(master_ds$binning.NbrOfInq_ExclHomeAutoLoan_Last12Mnths) %in% c("(10,12]","(16,18]"))] <- 
  levels(master_ds$binning.NbrOfInq_ExclHomeAutoLoan_Last12Mnths)[which(levels(master_ds$binning.NbrOfInq_ExclHomeAutoLoan_Last12Mnths)=="(8,10]")]
levels(master_ds$binning.NbrOfInq_ExclHomeAutoLoan_Last12Mnths)[which(levels(master_ds$binning.NbrOfInq_ExclHomeAutoLoan_Last12Mnths)=="(8,10]")] <- 
  "(8,10]&(10,12]&(16,18]"
levels(master_ds$binning.NbrOfInq_ExclHomeAutoLoan_Last12Mnths)

# Create WOE variable for categories NbrOfInq_ExclHomeAutoLoan_Last12Mnths
temp_ds <- create_infotables(data=master_ds[,c("binning.NbrOfInq_ExclHomeAutoLoan_Last12Mnths","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
arrange(temp_ds$Tables$binning.NbrOfInq_ExclHomeAutoLoan_Last12Mnths,desc(Percent))

# Create a scatter plot to find the monotonicity of data points.
ggplot(temp_ds$Tables$binning.NbrOfInq_ExclHomeAutoLoan_Last12Mnths,aes(x=binning.NbrOfInq_ExclHomeAutoLoan_Last12Mnths,y=WOE,group=1))+
  geom_point() + 
  geom_line()

# Removing NbrOfInq_ExclHomeAutoLoan_Last12Mnths column as 
# its been replaced with NbrOfInq_ExclHomeAutoLoan_Last12Mnths Buckets
ncol(master_ds)
master_ds <- subset(master_ds, select = -c(NbrOfInq_ExclHomeAutoLoan_Last12Mnths))
ncol(master_ds)


################################################################
# 23. NbrOfInq_ExclHomeAutoLoan_Last6Mnths (Continuous - Bin):-
################################################################
#Checking summary
summary(master_ds$NbrOfInq_ExclHomeAutoLoan_Last6Mnths)
#Obs: There no no wrong values in it.
#     It has a maximum value of 10 

# check for outliers
boxplot(master_ds$NbrOfInq_ExclHomeAutoLoan_Last6Mnths)
# there are outliers which would be handled by WOE computation.

#Binning NbrOfInq_ExclHomeAutoLoan_Last6Mnths and storing it in binning.NbrOfInq_ExclHomeAutoLoan_Last6Mnths variable
master_ds$binning.NbrOfInq_ExclHomeAutoLoan_Last6Mnths <- as.factor(cut(master_ds$NbrOfInq_ExclHomeAutoLoan_Last6Mnths, breaks = seq(0,10,2), include.lowest = T))

ggplot(master_ds,aes(binning.NbrOfInq_ExclHomeAutoLoan_Last6Mnths))+geom_bar()

# Create WOE variable for categories NbrOfInq_ExclHomeAutoLoan_Last6Mnths
temp_ds <- create_infotables(data=master_ds[,c("binning.NbrOfInq_ExclHomeAutoLoan_Last6Mnths","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
arrange(temp_ds$Tables$binning.NbrOfInq_ExclHomeAutoLoan_Last6Mnths,desc(Percent))

levels(master_ds$binning.NbrOfInq_ExclHomeAutoLoan_Last6Mnths)
levels(master_ds$binning.NbrOfInq_ExclHomeAutoLoan_Last6Mnths)[which(levels(master_ds$binning.NbrOfInq_ExclHomeAutoLoan_Last6Mnths) %in% c("(6,8]","(8,10]"))] <- 
  levels(master_ds$binning.NbrOfInq_ExclHomeAutoLoan_Last6Mnths)[which(levels(master_ds$binning.NbrOfInq_ExclHomeAutoLoan_Last6Mnths)=="[0,2]")]
levels(master_ds$binning.NbrOfInq_ExclHomeAutoLoan_Last6Mnths)[which(levels(master_ds$binning.NbrOfInq_ExclHomeAutoLoan_Last6Mnths)=="[0,2]")] <- 
  "[0,2]&(6,10]"
levels(master_ds$binning.NbrOfInq_ExclHomeAutoLoan_Last6Mnths)

# Create WOE variable for categories NbrOfInq_ExclHomeAutoLoan_Last6Mnths
temp_ds <- create_infotables(data=master_ds[,c("binning.NbrOfInq_ExclHomeAutoLoan_Last6Mnths","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
arrange(temp_ds$Tables$binning.NbrOfInq_ExclHomeAutoLoan_Last6Mnths,desc(Percent))

# Create a scatter plot to find the monotonicity of data points.
ggplot(temp_ds$Tables$binning.NbrOfInq_ExclHomeAutoLoan_Last6Mnths,aes(x=binning.NbrOfInq_ExclHomeAutoLoan_Last6Mnths,y=WOE,group=1))+
  geom_point() + 
  geom_line()

# Removing NbrOfInq_ExclHomeAutoLoan_Last6Mnths column as 
# its been replaced with NbrOfInq_ExclHomeAutoLoan_Last6Mnths Buckets
ncol(master_ds)
master_ds <- subset(master_ds, select = -c(NbrOfInq_ExclHomeAutoLoan_Last6Mnths))
ncol(master_ds)

###############################################
# 24. HasOpenHomeLoan (Categorical):-
###############################################
#Checking summary
summary(master_ds$HasOpenHomeLoan)
#Obs: There are 271 NAs and we shallremove them 
# as those are insignificant w.r.t. total rows.

# Remove the NA values.
nrow(master_ds)
master_ds <- master_ds[- which(is.na(master_ds$HasOpenHomeLoan)),] 
nrow(master_ds)

#Checking summary by converting as factor type
summary(as.factor(master_ds$HasOpenHomeLoan)) 
#Obs: There are 2 labels 0s and 1s so its better to keep them as factor.

master_ds$HasOpenHomeLoan <- as.factor(master_ds$HasOpenHomeLoan)


###############################################
# 25. HasOpenAutoLoan (Categorical):-
###############################################
#Checking summary
summary(master_ds$HasOpenAutoLoan)
# There are no wrong values in it

#Checking summary by converting as factor type
summary(as.factor(master_ds$HasOpenAutoLoan)) 
#Obs: There are 2 labels 0s and 1s so its better to keep them as factor.

master_ds$HasOpenAutoLoan <- as.factor(master_ds$HasOpenAutoLoan)

###############################################
# 26. AvgCCUtilLast12Mnths (Continuous - Bin):-
###############################################
#Checking summary
summary(master_ds$AvgCCUtilLast12Mnths)
#Obs: There are 748 NAs and we shall remove them 
# as those are insignificant w.r.t. total rows.

boxplot(master_ds$AvgCCUtilLast12Mnths)

ggplot(master_ds,aes(AvgCCUtilLast12Mnths))+geom_histogram()
# There are huge number of observations under 0-10 bucket.

#Binning AvgCCUtilLast12Mnths and storing it in binning.AvgCCUtilLast12Mnths variable
master_ds$binning.AvgCCUtilLast12Mnths <- as.character(cut(master_ds$AvgCCUtilLast12Mnths, breaks = seq(0,120,10), include.lowest = T))

master_ds$binning.AvgCCUtilLast12Mnths[which(is.na(master_ds$binning.AvgCCUtilLast12Mnths))] <- "NA"

master_ds$binning.AvgCCUtilLast12Mnths <- as.factor(master_ds$binning.AvgCCUtilLast12Mnths)

summary(master_ds$binning.AvgCCUtilLast12Mnths)
levels(master_ds$binning.AvgCCUtilLast12Mnths)

ggplot(master_ds,aes(binning.AvgCCUtilLast12Mnths))+geom_bar()

# Create WOE variable for categories AvgCCUtilLast12Mnths
temp_ds <- create_infotables(data=master_ds[,c("binning.AvgCCUtilLast12Mnths","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
arrange(temp_ds$Tables$binning.AvgCCUtilLast12Mnths,desc(WOE))

levels(master_ds$binning.AvgCCUtilLast12Mnths)
levels(master_ds$binning.AvgCCUtilLast12Mnths)[which(levels(master_ds$binning.AvgCCUtilLast12Mnths) %in% c("(100,110]","(110,120]","NA"))] <- 
  levels(master_ds$binning.AvgCCUtilLast12Mnths)[which(levels(master_ds$binning.AvgCCUtilLast12Mnths)=="(90,100]")]
levels(master_ds$binning.AvgCCUtilLast12Mnths)[which(levels(master_ds$binning.AvgCCUtilLast12Mnths)=="(90,100]")] <- 
  "(90,120]&NA"
levels(master_ds$binning.AvgCCUtilLast12Mnths)[which(levels(master_ds$binning.AvgCCUtilLast12Mnths) %in% c("(80,90]"))] <- 
  levels(master_ds$binning.AvgCCUtilLast12Mnths)[which(levels(master_ds$binning.AvgCCUtilLast12Mnths)=="(30,40]")]
levels(master_ds$binning.AvgCCUtilLast12Mnths)[which(levels(master_ds$binning.AvgCCUtilLast12Mnths)=="(30,40]")] <- 
  "(30,40]&(80,90]"

levels(master_ds$binning.AvgCCUtilLast12Mnths)

# Create WOE variable for categories AvgCCUtilLast12Mnths
temp_ds <- create_infotables(data=master_ds[,c("binning.AvgCCUtilLast12Mnths","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
arrange(temp_ds$Tables$binning.AvgCCUtilLast12Mnths,desc(Percent))

# Create a scatter plot to find the monotonicity of data points.
ggplot(temp_ds$Tables$binning.AvgCCUtilLast12Mnths,aes(x=binning.AvgCCUtilLast12Mnths,y=WOE,group=1))+
  geom_point() + 
  geom_line()

# Removing AvgCCUtilLast12Mnths column as 
# its been replaced with AvgCCUtilLast12Mnths Buckets
ncol(master_ds)
master_ds <- subset(master_ds, select = -c(AvgCCUtilLast12Mnths))
ncol(master_ds)


#########################################
# 27. OutstandingBal (Continuous - Bin):-
#########################################

options(scipen = 6000000)

str(master_ds$OutstandingBal)
# This variable is continuous and is of type int

ggplot(master_ds,aes(OutstandingBal))+geom_histogram()
# There are huge number of observations under 0-10 bucket. ## How do u know it is 0-10 bucket?

boxplot(master_ds$OutstandingBal)

#Checking summary
summary(master_ds$OutstandingBal) ####Check comments
#Obs: The mean outstanding balance is 1,249K 
#     There are number of observations having 775K
#     We have a customer who has 5,219K outstanding value (Need to keep a watch on this customer)
#     There are 271 NAs and WOE transformation would take decision on the target variable.

#Binning OutstandingBal and storing it in binning.OutstandingBal variable
options(scipen = 60000000)
master_ds$binning.OutstandingBal <- as.factor(cut(master_ds$OutstandingBal, breaks = seq(0,5500000,500000), include.lowest = T))

ggplot(master_ds,aes(binning.OutstandingBal))+geom_bar()

summary(master_ds$binning.OutstandingBal)
levels(master_ds$binning.OutstandingBal)

levels(master_ds$binning.OutstandingBal)[which(levels(master_ds$binning.OutstandingBal)=="[0,5e+05]")] <- 
  "(0,0.5]M"
levels(master_ds$binning.OutstandingBal)[which(levels(master_ds$binning.OutstandingBal)=="(5e+05,1e+06]")] <- 
  "(0.5,1]M"
levels(master_ds$binning.OutstandingBal)[which(levels(master_ds$binning.OutstandingBal)=="(1e+06,1.5e+06]")] <- 
  "(1,1.5]M"
levels(master_ds$binning.OutstandingBal)[which(levels(master_ds$binning.OutstandingBal)=="(1.5e+06,2e+06]")] <- 
  "(1.5,2]M"
levels(master_ds$binning.OutstandingBal)[which(levels(master_ds$binning.OutstandingBal)=="(2e+06,2.5e+06]")] <- 
  "(2,2.5]M"
levels(master_ds$binning.OutstandingBal)[which(levels(master_ds$binning.OutstandingBal)=="(2.5e+06,3e+06]")] <- 
  "(2.5,3]M"
levels(master_ds$binning.OutstandingBal)[which(levels(master_ds$binning.OutstandingBal)=="(3e+06,3.5e+06]")] <- 
  "(3,3.5]M"
levels(master_ds$binning.OutstandingBal)[which(levels(master_ds$binning.OutstandingBal)=="(3.5e+06,4e+06]")] <- 
  "(3.5,4]M"
levels(master_ds$binning.OutstandingBal)[which(levels(master_ds$binning.OutstandingBal)=="(4e+06,4.5e+06]")] <- 
  "(4,4.5]M"
levels(master_ds$binning.OutstandingBal)[which(levels(master_ds$binning.OutstandingBal)=="(4.5e+06,5e+06]")] <- 
  "(4.5,5]M"
levels(master_ds$binning.OutstandingBal)[which(levels(master_ds$binning.OutstandingBal)=="(5e+06,5.5e+06]")] <- 
  "(5,5.5]M"

levels(master_ds$binning.OutstandingBal)

# Create WOE variable for categories OutstandingBal
temp_ds <- create_infotables(data=master_ds[,c("binning.OutstandingBal","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
arrange(temp_ds$Tables$binning.OutstandingBal,desc(WOE))

levels(master_ds$binning.OutstandingBal)
levels(master_ds$binning.OutstandingBal)[which(levels(master_ds$binning.OutstandingBal) %in% c("(4.5,5]M","(5,5.5]M"))] <- 
  levels(master_ds$binning.OutstandingBal)[which(levels(master_ds$binning.OutstandingBal)=="(1,1.5]M")]
levels(master_ds$binning.OutstandingBal)[which(levels(master_ds$binning.OutstandingBal)=="((1,1.5]M")] <- 
  "(1,1.5]M&(4.5,5.5]M"

levels(master_ds$binning.OutstandingBal)[which(levels(master_ds$binning.OutstandingBal) %in% c("(4,4.5]M"))] <- 
  levels(master_ds$binning.OutstandingBal)[which(levels(master_ds$binning.OutstandingBal)=="(1.5,2]M")]
levels(master_ds$binning.OutstandingBal)[which(levels(master_ds$binning.OutstandingBal)=="(1.5,2]M")] <- 
  "(1.5,2]M&(4,4.5]M"

levels(master_ds$binning.OutstandingBal)[which(levels(master_ds$binning.OutstandingBal) %in% c("(2,2.5]M"))] <- 
  levels(master_ds$binning.OutstandingBal)[which(levels(master_ds$binning.OutstandingBal)=="(0,0.5]M")]
levels(master_ds$binning.OutstandingBal)[which(levels(master_ds$binning.OutstandingBal)=="(0,0.5]M")] <- 
  "(0,0.5]M&(2,2.5]M"

levels(master_ds$binning.OutstandingBal)

# Create WOE variable for categories OutstandingBal
temp_ds <- create_infotables(data=master_ds[,c("binning.OutstandingBal","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
arrange(temp_ds$Tables$binning.OutstandingBal,desc(Percent))

levels(master_ds$binning.OutstandingBal)[which(levels(master_ds$binning.OutstandingBal) %in% c("(3.5,4]M"))] <- 
  levels(master_ds$binning.OutstandingBal)[which(levels(master_ds$binning.OutstandingBal)=="(1,1.5]M")]
levels(master_ds$binning.OutstandingBal)[which(levels(master_ds$binning.OutstandingBal)=="(1,1.5]M")] <- 
  "(1,1.5]M&(3.5,4]M"

levels(master_ds$binning.OutstandingBal)

# Create WOE variable for categories OutstandingBal
temp_ds <- create_infotables(data=master_ds[,c("binning.OutstandingBal","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
arrange(temp_ds$Tables$binning.OutstandingBal,desc(WOE))

# Create a scatter plot to find the monotonicity of data points.
ggplot(temp_ds$Tables$binning.OutstandingBal,aes(x=binning.OutstandingBal,y=WOE,group=1))+
  geom_point() + 
  geom_line()

# Removing OutstandingBal column as 
# its been replaced with OutstandingBal Buckets
ncol(master_ds)
master_ds <- subset(master_ds, select = -c(OutstandingBal))
ncol(master_ds)

###################################################################
# Checking for blanks in each vriable of woe_master_ds
sapply(master_ds,function(x)length(which(as.character(x)==""))) # No blanks available

# Checking for NAs in each variable of woe_master_ds
sapply(master_ds,function(x)length(which(is.na(x)))) # No NA 


#############################################################################
#################### Performing BiVariate Analysis #########################
#############################################################################

# Creating a function to plot default rate corresponding to each variable

plot_default_master <- function(cat_var, var_name){
  a <- aggregate(Performance.Tag ~ cat_var, master_ds, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_defaulte <- cbind(a, count)
  
  colnames(agg_defaulte) <- c(var_name, "defaulte_rate","total_applicant")
  agg_defaulte[, 2] <- format(round(agg_defaulte[, 2], 2))
  
  ggplot(agg_defaulte, aes(agg_defaulte[, 1], total_applicant, label = defaulte_rate)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5) + xlab(var_name)+
    ggtitle(paste("Default rate by",var_name))+theme(plot.title = element_text(hjust = 0.5))
  
}

# Now performing bi-variate analysis for each variable

#1. Gender

plot_default_master(master_ds$Gender,"Gender")

#2.Marital.Status

plot_default_master(master_ds$Marital.Status,"Marital Status")

#3. No.of.Dependence

plot_default_master(master_ds$No.of.dependents, "No of dependemce")

#4. Education

plot_default_master(master_ds$Education, "Education")

#5. Profession

plot_default_master(master_ds$Profession ,"Profession")

#6. Type of residence

plot_default_master(master_ds$Type.of.residence ,"Type of residence")

# 7. NbrOf90DPDLast6Mnths :-

plot_default_master(master_ds$NbrOf90DPDLast6Mnths,"90 DPD or worse in 6 months")

# 8. NbrOf60DPDLast6Mnths :-

plot_default_master(master_ds$NbrOf60DPDLast6Mnths,"60 DPD or worse in 6 months")

# 9. NbrOf30DPDLast6Mnths :-

plot_default_master(master_ds$NbrOf30DPDLast6Mnths,"30 DPD or worse in 6 months")

#10. NbrOf90DPDLast12Mnths :-

plot_default_master(master_ds$NbrOf90DPDLast12Mnths,"90 DPD or worse in 12 months")
#Obs: NbrOf90DPDLast12Mnths also seems to be a key parameter becuase we can observe that
# as the customers who cross 90DPD or worse in last 12 months >= 1 are more likely to default.

#11. NbrOf60DPDLast12Mnths :-

plot_default_master(master_ds$NbrOf60DPDLast12Mnths,"60 DPD or worse in 12 months")
#Obs: NbrOf60DPDLast12Mnths also seems to be a key parameter becuase we can observe that
#     as the customers who cross 60DPD or worse in last 12 months >= 1 are more likely to default.


# 12. NbrOf30DPDLast12Mnths :-

plot_default_master(master_ds$NbrOf30DPDLast12Mnths,"30 DPD or worse in 12 months")
#Obs: NbrOf60DPDLast12Mnths also seems to be a key parameter becuase we can observe that
#     as the customers who cross 30DPD or worse in last 12 months >= 1 are more likely to default.

# 13. NbrOfTradesOpenLast6Mnths

plot_default_master(master_ds$NbrOfTradesOpenLast6Mnths ,"No. of trades open in last 6 months")

# 14. NbrOfTradesOpenLast12Mnths

plot_default_master(master_ds$binning.NbrOfTradesOpenLast12Mnths, "No.of trades open in last 12 months")

# 15. NbrOfPLTradesOpenLast6Mnths

plot_default_master(master_ds$NbrOfPLTradesOpenLast6Mnths, "No.of PL trades open in last 6 months")

# 16. NbrOfPLTradesOpenLast12Mnths
plot_default_master(master_ds$NbrOfPLTradesOpenLast12Mnths, "No.of PL trades open in last 12 months")

#17.HasOpenHomeLoan

plot_default_master(master_ds$HasOpenHomeLoan , "Has open home loan")

#18. HasOpenAutoLoan

plot_default_master(master_ds$HasOpenAutoLoan , "Has open auto loan")
  
#19. binning.age

plot_default_master(master_ds$binning.age , "Age Group")

#20. binning.income

plot_default_master(master_ds$binning.income, "Income Range")

#21. binning.NbrMnthsCurrResidence

plot_default_master(master_ds$binning.NbrMnthsCurrResidence , "No. of months in current Residence")


#22. binning.NbrMnthsCurrCompany

plot_default_master(master_ds$binning.NbrMnthsCurrCompany , "No. of months in current company")


#24.binning.NbrOfInq_ExclHomeAutoLoan_Last12Mnths

plot_default_master(master_ds$binning.NbrOfInq_ExclHomeAutoLoan_Last12Mnths,"NbrOfInq_ExclHomeAutoLoan_Last12Mnths")

#25.binning.NbrOfInq_ExclHomeAutoLoan_Last6Mnths

plot_default_master(master_ds$binning.NbrOfInq_ExclHomeAutoLoan_Last6Mnths ,"NbrOfInq_ExclHomeAutoLoan_Last6Mnths")

#26. binning.AvgCCUtilLast12Mnths

plot_default_master(master_ds$binning.AvgCCUtilLast12Mnths,"AvgCCUtilLast12Mnths")

#27. binning.OutstandingBal

plot_default_master(master_ds$binning.OutstandingBal , "Outstanding Balance")

############################Checking for IV value for important variables####################################
#############################################################################################################

IV_master_ds <- create_infotables(data = subset(master_ds,select = -c(Application.ID,Performance.Tag)), y="Reverse.Performance.Tag", parallel=FALSE)
IV_master_ds
print(IV_master_ds$Summary, row.names=FALSE)

#based on IV values  we found following variables significant. So, we will make model on only those variables further.

#        Variable                                     IV     
#binning.AvgCCUtilLast12Mnths                    0.3152942440
#NbrOfPLTradesOpenLast12Mnths                    0.3152742860
#NbrOf30DPDLast6Mnths                            0.2487699890
#NbrOfPLTradesOpenLast6Mnths                     0.2328579301
#NbrOf30DPDLast12Mnths                           0.2216175208
#NbrOf90DPDLast12Mnths                           0.2187071405
#NbrOf60DPDLast6Mnths                            0.2138644337
#binning.TotalNbrOfTrades                        0.2079698822
#binning.OutstandingBal                          0.1988146724
#NbrOfTradesOpenLast6Mnths                       0.1971412364
#NbrOf60DPDLast12Mnths                           0.1910412199
#binning.NbrOfTradesOpenLast12Mnths              0.1903222273
#binning.NbrOfInq_ExclHomeAutoLoan_Last12Mnths   0.1758961235
#NbrOf90DPDLast6Mnths                            0.1638949606
#binning.NbrOfInq_ExclHomeAutoLoan_Last6Mnths    0.0741983880
#binning.income                                  0.0398875388
#binning.NbrMnthsCurrResidence                   0.0303962964
#binning.NbrMnthsCurrCompany                     0.0194808771


# storing significant variable names into a different vector

significant_variables<- c("Application.ID",
                          "Performance.Tag",
                          "binning.AvgCCUtilLast12Mnths",
                          "NbrOfPLTradesOpenLast12Mnths",
                          "NbrOf30DPDLast6Mnths",
                          "NbrOfPLTradesOpenLast6Mnths",
                          "NbrOf30DPDLast12Mnths",
                          "NbrOf90DPDLast12Mnths",
                          "NbrOf60DPDLast6Mnths",
                          "binning.TotalNbrOfTrades",
                          "binning.OutstandingBal",
                          "NbrOfTradesOpenLast6Mnths",
                          "NbrOf60DPDLast12Mnths",
                          "binning.NbrOfTradesOpenLast12Mnths",
                          "binning.NbrOfInq_ExclHomeAutoLoan_Last12Mnths",
                          "NbrOf90DPDLast6Mnths",
                          "binning.NbrOfInq_ExclHomeAutoLoan_Last6Mnths",
                          "binning.income",
                          "binning.NbrMnthsCurrResidence",
                          "binning.NbrMnthsCurrCompany")

# filtering significant variables value from master_ds and storing them in a new data frame

woe_master_ds <- master_ds[,which(names(master_ds) %in% significant_variables)]

# Converting all the variables except 'Application.ID' and 'Performance.Tag' as character type
#so that we can replace these variables' values with corresponding WOE values

str(woe_master_ds)
woe_master_ds[significant_variables[3:length(significant_variables)]]<- 
  lapply(woe_master_ds[significant_variables[3:length(significant_variables)]],as.character)
str(woe_master_ds)

# Putting our target variable Performance.Tag at the end of the data frame 
Performance.Tag <- woe_master_ds$Performance.Tag
woe_master_ds <- subset(woe_master_ds, select = -c(Performance.Tag))
woe_master_ds<- cbind(woe_master_ds,Performance.Tag)

#Converting Performance.Tag as factor type.
woe_master_ds$Performance.Tag <- as.factor(woe_master_ds$Performance.Tag)

#Now checking structure of the data frame
str(woe_master_ds)

# Now we will replace all the significance variables values with their WOE values.
# Generalized loop to replace variables value with woe value.

for (j in 3:length(significant_variables))
{
  start_index <- 1
  column <- significant_variables[j]
  
  na_indices <- which(is.na(woe_master_ds[[column]])) 
  if ( sum(is.na(woe_master_ds[[column]])) > 0 )
  {
    start_index <- 2
  }
  
  for(i in start_index:nrow(IV_master_ds$Tables[[column]])){
    
    woe_master_ds[[column]][which(woe_master_ds[[column]] == IV_master_ds$Tables[[column]][[column]][i])] <- 
      IV_master_ds$Tables[[column]]$WOE[i]
    
  }
  
 if(start_index == 2) {
    woe_master_ds[[column]][na_indices] <- IV_master_ds$Tables[[column]]$WOE[1]
  }
}

woe_master_ds[significant_variables[3:length(significant_variables)]]<- 
  lapply(woe_master_ds[significant_variables[3:length(significant_variables)]],as.numeric)

########################
######Model Creation####
########################

#-| Split into test and train datasets |-#
set.seed(100)
split_indices <- sample.split(woe_master_ds$Performance.Tag, SplitRatio = 0.70)
train_data <- woe_master_ds[split_indices, ]
test_data <- woe_master_ds[!split_indices, ]

#-| Using SMOTE to balance the data |-#
table(train_data$Performance.Tag)
prop.table(table(train_data$Performance.Tag))

train_data_SMOTE <- SMOTE(Performance.Tag ~ ., train_data, perc.over = 100, perc.under=200)

table(train_data_SMOTE$Performance.Tag)
prop.table(table(train_data_SMOTE$Performance.Tag))

#shuffling SMOTE data

train_data_SMOTE <- train_data_SMOTE[sample(nrow(train_data_SMOTE)), ]


########################
#Logistic Regression
#########################

# Model 1
model_1 <- glm(Performance.Tag ~ . - Application.ID, family = "binomial", data = train_data_SMOTE)
summary(model_1)
sort(vif(model_1),decreasing = TRUE)

# Model 2
model_2 <- stepAIC(model_1, direction = "both")
summary(model_2)
sort(vif(model_2),decreasing = TRUE)

# Obs:
# Comparing NbrOf30DPDLast6Mnths and NbrOf30DPDLast12Mnths which have high VIF 
# Both are significant and have same significance level
# comparing NbrOf30DPDLast12Mnths and NbrOf60DPDLast12Mnths which are next in order of high VIF
# Both are significant but NbrOf60DPDLast12Mnths is more significant than NbrOf30DPDLast12Mnths
# But would take a call to remove binning.NbrOfInq_ExclHomeAutoLoan_Last12Mnths which is less significant

#Model_3
#Removing binning.NbrOfInq_ExclHomeAutoLoan_Last6Mnths 

model_3 <- glm(formula = Performance.Tag ~ NbrOf30DPDLast6Mnths + NbrOf60DPDLast12Mnths + 
                 NbrOf30DPDLast12Mnths + NbrOfPLTradesOpenLast12Mnths + binning.NbrMnthsCurrResidence + 
                 binning.NbrMnthsCurrCompany + binning.AvgCCUtilLast12Mnths, 
                family = "binomial", data = train_data_SMOTE)

summary(model_3)
sort(vif(model_3),decreasing = TRUE)

# Obs:
# NbrOf30DPDLast6Mnths and NbrOf30DPDLast12Mnths are still having high VIF 
# Both are significant enough not be removed but NbrOf30DPDLast6Mnths is less significant than NbrOf30DPDLast12Mnths
# Would take a call to remove binning.NbrMnthsCurrCompany which is less signficant.

#Model_4
# Removing binning.NbrMnthsCurrCompany

model_4 <- glm(formula = Performance.Tag ~ NbrOf30DPDLast6Mnths + NbrOf60DPDLast12Mnths + 
                 NbrOf30DPDLast12Mnths + NbrOfPLTradesOpenLast12Mnths + binning.NbrMnthsCurrResidence + 
                 binning.AvgCCUtilLast12Mnths, 
               family = "binomial", data = train_data_SMOTE)

summary(model_4)
sort(vif(model_4),decreasing = TRUE)

# Obs :
# We can still see that NbrOf30DPDLast6Mnths and NbrOf30DPDLast12Mnths are having high variance
# thus the next model should be prepared removing NbrOf30DPDLast6Mnths

#Model_5
#Removing NbrOf30DPDLast6Mnths

model_5 <- glm(formula = Performance.Tag ~ NbrOf60DPDLast12Mnths + 
                 NbrOf30DPDLast12Mnths + NbrOfPLTradesOpenLast12Mnths + binning.NbrMnthsCurrResidence + 
                 binning.AvgCCUtilLast12Mnths, 
               family = "binomial", data = train_data_SMOTE)

summary(model_5)
sort(vif(model_5),decreasing = TRUE)

# Obs: 
# NbrOf30DPDLast12Mnths and NbrOf60DPDLast12Mnths are next in high colliearity
# Thus removing the next insignificant variable - NbrOf60DPDLast12Mnths

#Model_6
#Removing NbrOf60DPDLast12Mnths 

model_6 <- glm(formula = Performance.Tag ~ NbrOf30DPDLast12Mnths + 
                 NbrOfPLTradesOpenLast12Mnths + binning.NbrMnthsCurrResidence + 
                 binning.AvgCCUtilLast12Mnths, 
               family = "binomial", data = train_data_SMOTE)


summary(model_6)
sort(vif(model_6),decreasing = TRUE)


final_model <- model_6


#Model Evaluation
#----------------

#predicted probabilities of default for test data
test_pred_default_prob <- predict(final_model,type = "response", newdata = test_data[,-c(1,20)]) 
summary(test_pred_default_prob) 

# Find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_default <- factor(ifelse(test_pred_default_prob >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted_default, test_data$Performance.Tag, positive = "1")
  acc  <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out  <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability
summary(test_pred_default_prob)

s = seq(.2,.80,length=100)

OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

cutoff

# Let's choose a cutoff value of 0.54 for final model

test_pred_cutoff_default <- factor(ifelse(test_pred_default_prob >=cutoff, "1", "0"))

conf_final <- confusionMatrix(test_pred_cutoff_default, test_data$Performance.Tag, positive = "1")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

# Output -
# Cut Off probability - 0.54 
# Accuracy              0.6345303 
# Sensitivity           0.6336406 
# Specificity           0.6345695 


################
#Decision Tree
################

#Building decision tree

tree_1 <- rpart(Performance.Tag ~ ., data= train_data_SMOTE[,-1],method = "class")
               
                
prp(tree_1)

# make predictions on the test set
tree.predict <- predict(tree_1, test_data[,-c(1,20)], type = "class")

# evaluate the results
confusionMatrix(test_data$Performance.Tag, tree.predict, positive = "1")
#sensitivity is very low.

tree_2 <- rpart(Performance.Tag ~.,
                data = train_data_SMOTE[,-1],
                method = "class",
                parms = list(split = "information")
                )


prp(tree_2)

# make predictions on the test set
tree.predict <- predict(tree_2, test_data[-c(1,20)], type = "class")

# evaluate the results
confusionMatrix(test_data$Performance.Tag, tree.predict, positive = "1")
#sensitivity is very low.


tree_3  <- rpart(Performance.Tag ~.,
                 data = train_data_SMOTE[,-1],
                 method = "class",
                 control = rpart.control(minsplit = 1000,  
                                         minbucket = 1000, 
                                         cp = 0.05))       
                 

prp(tree_3)

# make predictions on the test set
tree.predict <- predict(tree_3, test_data[,-c(1,20)], type = "class")

# evaluate the results
confusionMatrix(test_data$Performance.Tag, tree.predict, positive = "1")
#sensitivity is very low.

tree_4  <- rpart(Performance.Tag ~.,
                 data = train_data_SMOTE[,-1],
                 method = "class",
                 control = rpart.control(minsplit = 1,  
                                         minbucket = 1, 
                                         cp = 0.01))       


prp(tree_4)

# make predictions on the test set
tree.predict <- predict(tree_4,test_data[,-c(1,20)], type = "class")

# evaluate the results
confusionMatrix(test_data$Performance.Tag, tree.predict, positive = "1")
#sensitivity is very low.

#5 Cross test to choose CP ----

# set the number of folds in cross test to 5
tree.control = trainControl(method = "cv", number = 5)

# set the search space for CP
tree.grid = expand.grid(cp = seq(0, 0.02, 0.0025))

# train model
tree_5 <- train(Performance.Tag ~ .,
                    data = train_data_SMOTE[,-1],
                    method = "rpart",
                    metric = "Accuracy",
                    trControl = tree.control,
                    tuneGrid = tree.grid,
                    control = rpart.control(minsplit = 50,
                                            minbucket = 20))

# look at cross validated model results
tree_5

# look at best value of hyperparameter
tree_5$bestTune

# make predictions on test set
tree.predict <- predict.train(tree_5, test_data[,-c(1,20)])

# accuracy
confusionMatrix(tree.predict, test_data$Performance.Tag,positive = "1")  



#We can see that we are getting very less sensitivity of all decision tree models. It means decision tree 
#won't be right choice for the given data set.



#################
#Random Forest
#################

master_rf <- randomForest(Performance.Tag ~., data = train_data_SMOTE[,-1], proximity = F, do.trace = T, mtry = 5)

# Predict response for test data

rf_pred <- predict(master_rf, test_data[, -c(1,20)], type = "prob")
# Cutoff for randomforest to assign yes or no

perform_fn_rf <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(rf_pred[, 2] >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted_response, test_data$Performance.Tag, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

#---------------------------------------------------------    

summary(rf_pred[,2])

s = seq(.00,.91,length=100)

OUT_rf = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

min(abs(OUT_rf[,1]-OUT_rf[,2]))

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.002)]
cutoff_rf

# The plot shows that cutoff value of around 39% optimises sensitivity and accuracy

predicted_response <- factor(ifelse(rf_pred[, 2] >= 0.39, "1", "0"))

conf_forest <- confusionMatrix(predicted_response, test_data$Performance.Tag, positive = "1")

conf_forest

# Sensitivity
conf_forest$byClass[1]

# Specificity 
conf_forest$byClass[2]

# Accuracy 
conf_forest$overall[1]


#--------------------------------------------------

#So, among Logistic Regression, decision tree and random forrest we found that LOGISTIC REGRESSION is the BEST model.



############################ Calculating Application Score ####################################
#----------------------------------------------------------------------------------------------

test_data$prob.default<- test_pred_default_prob
test_data$prob.nondefault<- 1 - test_data$prob.default

#odd
test_data$odds <- test_data$prob.nondefault/test_data$prob.default

#log of odds
test_data$log_odds <- log(test_data$odds)

#Application Scorecard
test_data$Application.Score <- 400 - (20/log(2)) * (log(10) - test_data$log_odds)

summary(test_data$Application.Score)
ggplot(test_data,aes(Application.Score))+geom_histogram()

#Calculating cut-off application score
#--------------------------------------

# As per our logistic regression module, our cut-off probability of being default is 0.54 

cut_off.Application.score <- 400 - (20/log(2)) * (log(10) - log(0.46/0.54))
cut_off.Application.score
# our cut-off Application score is ~330. It means CredX should approve applications of those applicants who are having 
#Application Score more than 330.

test_data$status <- ifelse(test_data$Application.Score >= 330 ,"Approve","Reject")

ggplot(test_data,aes(x = Application.Score, group = status, fill = status))+
  geom_histogram()+scale_fill_manual(values = c("green","red"))+
  ggtitle("Application Score Card")
  

### Calculation of Application score card for rejected population #########

#Checking for missing values
sapply(master_ds_reject,function(x)sum(is.na(x))) #No NA values except in Performance.Tag

sapply(master_ds_reject,function(x)length(which(as.character(x)==""))) #There is 1-1 blank value in Education and Profession
#But as per our final model of logistic regression we have found only below variables are significane:

#NbrOf30DPDLast12Mnths               
#NbrOfPLTradesOpenLast12Mnths
#NbrMnthsCurrResidence  
#AvgCCUtilLast12Mnths

#So, Filtering out significant variables from demographic_rejected. 

sig_var_rejected <- c("Application.ID","Performance.Tag",
                      "NbrOf30DPDLast12Mnths","NbrOfPLTradesOpenLast12Mnths",
                      "NbrMnthsCurrResidence","AvgCCUtilLast12Mnths"
                     )


# filtring significant variables value from demographic and storing them in a new data frame

woe_master_ds_rejected <- master_ds_reject[,which(names(master_ds_reject) %in% sig_var_rejected)]

#Binning variables as exactly same bin size of demographic(non-rejected) data set.

#NbrOf30DPDLast12Mnths
#---------------------
summary(woe_master_ds_rejected$NbrOf30DPDLast12Mnths) 

woe_master_ds_rejected$NbrOf30DPDLast12Mnths <- as.factor(woe_master_ds_rejected$NbrOf30DPDLast12Mnths)

levels(woe_master_ds_rejected$NbrOf30DPDLast12Mnths)
levels(woe_master_ds_rejected$NbrOf30DPDLast12Mnths) <- c( "0", "1" ,"2", "[3,9]", "[3,9]", "[3,9]" ,"[3,9]", "[3,9]" ,"[3,9]" ,"[3,9]")
levels(woe_master_ds_rejected$NbrOf30DPDLast12Mnths)


#NbrOfPLTradesOpenLast12Mnths
#----------------------------
summary(woe_master_ds_rejected$NbrOfPLTradesOpenLast12Mnths) 

woe_master_ds_rejected$NbrOfPLTradesOpenLast12Mnths <- as.factor(woe_master_ds_rejected$NbrOfPLTradesOpenLast12Mnths)

levels(woe_master_ds_rejected$NbrOfPLTradesOpenLast12Mnths)
levels(woe_master_ds_rejected$NbrOfPLTradesOpenLast12Mnths) <- c( "0","[1&10]" ,"2" ,"3", "[4&12]", "5" ,"6" ,"[[7,9]&11]", "[[7,9]&11]")
levels(woe_master_ds_rejected$NbrOfPLTradesOpenLast12Mnths)


#NbrMnthsCurrResidence
#----------------------
summary(woe_master_ds_rejected$NbrMnthsCurrResidence) 

woe_master_ds_rejected$binning.NbrMnthsCurrResidence <- as.factor(cut(woe_master_ds_rejected$NbrMnthsCurrResidence, breaks = c(0,24,48,72,96,120,144)))

levels(woe_master_ds_rejected$binning.NbrMnthsCurrResidence)
levels(woe_master_ds_rejected$binning.NbrMnthsCurrResidence) <- c( "(0,24]" ,   "(24,48]"  , "(48,72]"  , "(72,96]"  , "(96,120]"  ,"(96,120]")
levels(woe_master_ds_rejected$binning.NbrMnthsCurrResidence)

# Removing NbrMnthsCurrCompany column as its been replaced with NbrMnthsCurrCompany Buckets
ncol(woe_master_ds_rejected)
woe_master_ds_rejected <- subset(woe_master_ds_rejected, select = -c(NbrMnthsCurrResidence))
ncol(woe_master_ds_rejected)

#AvgCCUtilLast12Mnths
#--------------------

summary(woe_master_ds_rejected$AvgCCUtilLast12Mnths) 

woe_master_ds_rejected$binning.AvgCCUtilLast12Mnths <- as.character(cut(woe_master_ds_rejected$AvgCCUtilLast12Mnths, breaks = seq(0,120,10), include.lowest = T))
woe_master_ds_rejected$binning.AvgCCUtilLast12Mnths[which(is.na(woe_master_ds_rejected$binning.AvgCCUtilLast12Mnths))] <- "NA"

woe_master_ds_rejected$binning.AvgCCUtilLast12Mnths <- as.factor(woe_master_ds_rejected$binning.AvgCCUtilLast12Mnths)

levels(woe_master_ds_rejected$binning.AvgCCUtilLast12Mnths)
levels(woe_master_ds_rejected$binning.AvgCCUtilLast12Mnths) <- c( "(10,20]" ,  "(90,120]&NA", "(20,30]"  , "(30,40]&(80,90]" ,  "(40,50]"  , "(50,60]" ,  "(60,70]" ,  "(70,80]", 
                                                                  "(30,40]&(80,90]" ,  "(90,120]&NA" , "[0,10]"  ,  "(90,120]&NA")
levels(woe_master_ds_rejected$binning.AvgCCUtilLast12Mnths)

# Removing NbrMnthsCurrCompany column as its been replaced with NbrMnthsCurrCompany Buckets
ncol(woe_master_ds_rejected)
woe_master_ds_rejected <- subset(woe_master_ds_rejected, select = -c(AvgCCUtilLast12Mnths))
ncol(woe_master_ds_rejected)



#making a vector of significance variables

sig_var_rejected <- c("Application.ID","Performance.Tag",
                      "NbrOf30DPDLast12Mnths","NbrOfPLTradesOpenLast12Mnths",
                      "binning.NbrMnthsCurrResidence","binning.AvgCCUtilLast12Mnths"
                     )



# Converting all the variables except 'Application.ID' and 'Performance.Tag' as character type
#so that we can replace these variables' values with corresponding WOE values

str(woe_master_ds_rejected)
woe_master_ds_rejected[sig_var_rejected[3:length(sig_var_rejected)]]<- 
  lapply(woe_master_ds_rejected[sig_var_rejected[3:length(sig_var_rejected)]],as.character)
str(woe_master_ds_rejected)

# Putting our target variable Performance.Tag at the end of the data frame 
Performance.Tag <- woe_master_ds_rejected$Performance.Tag
woe_master_ds_rejected <- subset(woe_master_ds_rejected, select = -c(Performance.Tag))
woe_master_ds_rejected<- cbind(woe_master_ds_rejected,Performance.Tag)

#Now checking structure of the data frame
str(woe_master_ds_rejected)

# Now we will replace all the significance variables values with their WOE values.
# Generalized loop to replace variables value with woe value.

for (j in 3:length(sig_var_rejected))
{
  start_index <- 1
  column <- sig_var_rejected[j]
  
  na_indices <- which(is.na(woe_master_ds_rejected[[column]])) 
  if ( sum(is.na(woe_master_ds_rejected[[column]])) > 0 )
  {
    start_index <- 2
  }
  
  for(i in start_index:nrow(IV_master_ds$Tables[[column]])){
    
    woe_master_ds_rejected[[column]][which(woe_master_ds_rejected[[column]] == IV_master_ds$Tables[[column]][[column]][i])] <- 
      IV_master_ds$Tables[[column]]$WOE[i]
    
  }
  
  if(start_index == 2) {
    woe_master_ds_rejected[[column]][na_indices] <- IV_master_ds$Tables[[column]]$WOE[1]
  }
}

woe_master_ds_rejected[sig_var_rejected[3:length(sig_var_rejected)]]<- 
  lapply(woe_master_ds_rejected[sig_var_rejected[3:length(sig_var_rejected)]],as.numeric)


predicted_probability_rejected <- predict(final_model,type = "response", newdata = woe_master_ds_rejected[,-c(1,6)])
summary(predicted_probability_rejected)


woe_master_ds_rejected$predicted_Performance.Tag<- factor(ifelse(predicted_probability_rejected >=0.54, "1","0"))

table(woe_master_ds_rejected$predicted_Performance.Tag)
#We can see that our model accuracy is 1399/(26+1399) = 0.98 i.e. 98%. which quite good. 
#It means our model is also predicting that 98% of rejected population should be rejected.

#Calculating Application score for rejected applicants:
#-----------------------------------------------------

woe_master_ds_rejected$prob.default<- predicted_probability_rejected
woe_master_ds_rejected$prob.nondefault<- 1 - woe_master_ds_rejected$prob.default

#odd
woe_master_ds_rejected$odds <- woe_master_ds_rejected$prob.nondefault/woe_master_ds_rejected$prob.default

#log of odds
woe_master_ds_rejected$log_odds <- log(woe_master_ds_rejected$odds)

#Application Scorecard
woe_master_ds_rejected$Application.Score <- 400 - (20/log(2)) * (log(10) - woe_master_ds_rejected$log_odds)

summary(woe_master_ds_rejected$Application.Score)

#Since our cut-off score is 330 we will check how many Appliction scores of rejected applicants are above and below of cut-off score
ggplot(woe_master_ds_rejected,aes(Application.Score))+geom_histogram()

#By looking the histogram it is clear that most of the application score of rejected applictions are below 330 which meets 
#our expectation.

woe_master_ds_rejected$model_prediction <- ifelse(woe_master_ds_rejected$Application.Score < 330 ,"Reject","Approve")
table(woe_master_ds_rejected$model_prediction)

#So, on the basis of score card also we can see that % of correctly predicted rejected applicant is 
#1403/(22+1403) i.e. ~98%

#--------------------------------------------
#Assessing the financial benefit of our model
#--------------------------------------------


test_data<- arrange(test_data, desc(prob.default))

lift <- function(labels , predicted_prob, groups=10) {
  
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

# Create a Table of cumulative gain and lift 

#prospect_acquisition$actual_response <- as.factor(ifelse(prospect_acquisition$actual_response=="yes",1,0))

LG = lift(test_data$Performance.Tag, test_data$prob.default, groups = 10)
LG
LG$random_Gain <- c(10,20,30,40,50,60,70,80,90,100)

#By looking gain and lift table we can see that if we target 60% of applicatnts using our model we are able to identify ~82% of 
#defaulters. If we have not used the model we would have identified only 60% of defaulters. Due to this company might have given 
#credit card to 22% more defaulters. Thus, by using this model we able to reduce credit risk by not acquiring those risky customers.

#Plotting Gain chart

plot(LG$bucket,LG$Gain,col="darkred",type="l",main="Gain Chart",xlab="Decile No",ylab = "Gain",lwd =2)
lines(LG$bucket,LG$random_Gain,col="darkgreen",lwd = 2)
legend("bottomright",col=c("darkgreen","darkred"),lwd=c(2,2),c("Random Model","Logistic Model"),cex = .65)


#Plotting Lift Chart

plot(LG$bucket,LG$Cumlift,col="darkred",type="l",main="Lift Chart",xlab="Decile No",ylab = "Lift",lwd = 2)
abline(a=1,b=0,col="darkgreen",lwd= 2)
legend("topright",col=c("darkgreen","darkred"),lwd=c(2,2),c("Random Model","Logistic Model"),cex = .65)

#Analyzing Confusion Matrix in terms of financial benifit
 
#         Reference	
#Prediction		0   	1
#  0	       12459	334
#  1         7465	  546

# We able to indentify 546 defaulters correctly out of 880 using our model. 
#If we would have used any random model and assuming random guess is 50:50, it would have identified
#maximum 440 defaulters. In this way our model is able to predict 106 more defaulters compare to any random model.
#thus, this model will help CREDX minimising credit loss and revenue loss.







