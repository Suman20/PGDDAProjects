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

#Loading demographic data in working directory
demographic<- read.csv("Demographic data.csv",stringsAsFactors =F)
str(demographic)
summary(demographic)

colnames(demographic)

# Rename the columns to a meaningful name such that the column names are readable
colnames(demographic)[which(names(demographic) == "Marital.Status..at.the.time.of.application.")] <- "Marital.Status"
colnames(demographic)[which(names(demographic) == "No.of.months.in.current.residence")] <- "NbrMnthsCurrResidence"
colnames(demographic)[which(names(demographic) == "No.of.months.in.current.company")] <- "NbrMnthsCurrCompany"

colnames(demographic)

#Checking for duplicate record
sum(duplicated(demographic)) # No duplicate record

#Checking for duplicate Application.ID
sum(duplicated(demographic$Application.ID)) #There are 3 duplicate Application.ID

demographic$Application.ID[which(duplicated(demographic$Application.ID))]

demographic[demographic$Application.ID %in% c(765011468, 653287861, 671989187),]

#Removing the records corresponding to duplicated Application.ID
demographic <- demographic[!demographic$Application.ID %in% c(765011468, 653287861, 671989187),]


#storing rejected applicantions in a different data frame and removing those records from original data set.

demographic_rejected <- demographic[which(is.na(demographic$Performance.Tag)),]

demographic<- demographic[-which(is.na(demographic$Performance.Tag)),]

#Checking default rate
default_rate<- sum(demographic$Performance.Tag)/length(demographic$Performance.Tag)

################ Exploratory Data Analysis on the columns ###################

#############################################################################
#################### Performing Univariate Analysis #########################
#############################################################################

#Since Information packge treat 1 as 'good', we are adding a new variable 'Reverse.Performance.Tag'in 
#master data frame  
demographic$Reverse.Performance.Tag <- ifelse(demographic$Performance.Tag == 0,1,0)

####################################
# 1. Age (Continuous -> Binned) :- 
#####################################

# Checking summary
summary(demographic$Age) 
# Obs: There are some -ve values which are wrong as age can't be <0

# Checking how many Age values are <= 0
sum(demographic$Age <= 0)
# Obs: 20
# Since only 20 erroneous values are there in Age column which is 
# very small w.r.t total no records. So, we can directly remove 
# corresponding records from master data set.

nrow(demographic)
demographic <- demographic[-which(demographic$Age <= 0),]
nrow(demographic)
# Obs: Now there are 69844 rows

summary(demographic$Age)

# Checking outliers 
boxplot(demographic$Age) 
# Obs: No outliers 

# Plotting histogram 
ggplot(demographic,aes(Age))+geom_histogram()

# Binning the age variable and store it into "binning.age".
demographic$binning.age <- as.factor(cut(demographic$Age, breaks = c(10,20,30,40,50,60,70)))

ggplot(demographic,aes(binning.age))+geom_bar()

# Create WOE variable for age buckets
check_woe_df <- create_infotables(data=demographic[,c("binning.age","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percent and WOE of each bucket
check_woe_df$Tables$binning.age

# Merging (10,20] bucket into (20,30] bucket 
# as (10,20] bucket has less than 5% of observations.
levels(demographic$binning.age)
levels(demographic$binning.age)[1] <- levels(demographic$binning.age)[5]
levels(demographic$binning.age)

# Create WOE variable for age buckets
temp_ds <- create_infotables(data=demographic[,c("binning.age","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percent and WOE of each bucket
temp_ds$Tables$binning.age

# Create a scatter plot to find the monotonicity of data points.
ggplot(temp_ds$Tables$binning.age,aes(x=binning.age,y=WOE,group=1))+
  geom_point() + 
  geom_line()

# Removing Age column as its been replaced with Age Buckets
ncol(demographic)
demographic <- subset(demographic, select = -c(Age))
ncol(demographic)

####################################
# 2. Gender (Categorical):-
####################################

str(demographic$Gender)

# checking Summary
summary(as.factor(demographic$Gender))

# Obs: 1 blank value
# Since only one blank value is there we can directly 
# remove correponding record from master data set.

nrow(demographic)
demographic <- demographic[- which(demographic$Gender == ""),]
nrow(demographic)
# Obs: There are 69843 rows remaining now.

summary(as.factor(demographic$Gender))

# Converting gender as factor type
demographic$Gender <- as.factor(demographic$Gender) 

# Create WOE variable for Gender buckets
temp_ds <- create_infotables(data=demographic[,c("Gender","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

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
summary(as.factor(demographic$Marital.Status)) 
# Obs: 5 blank values
# Since only 5 blank values are there which is very small
# corresponding to total no records, we can remove corresponding 
# records from master data set.

nrow(demographic)
demographic <- demographic[- which(demographic$Marital.Status == ""),]
nrow(demographic)
# There are 69838 obs remaining

summary(as.factor(demographic$Marital.Status))

# Converting Marital.Status as factor type
demographic$Marital.Status <- as.factor(demographic$Marital.Status)

# Create WOE variable for categories Marital.Status
temp_ds <- create_infotables(data=demographic[,c("Marital.Status","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

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
summary(as.factor(demographic$No.of.dependents)) 
# Obs: 2 NAs 
# Since only 2 NAs are there which is very small out of total records,
# we can remove corresponding records from master data set

nrow(demographic)
demographic <- demographic[- which(is.na(demographic$No.of.dependents)),] 
nrow(demographic)
# There are 69836 records remaining

summary(as.factor(demographic$No.of.dependents))

# Converting No.of.dependents as factor type
demographic$No.of.dependents <- as.factor(demographic$No.of.dependents)

levels(demographic$No.of.dependents)


# Create WOE variable for categories No.of.dependents
temp_ds <- create_infotables(data=demographic[,c("No.of.dependents","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$No.of.dependents

# Create a scatter plot to find the monotonicity of data points.
ggplot(temp_ds$Tables$No.of.dependents,aes(x=No.of.dependents,y=WOE,group=1))+
  geom_point() + 
  geom_line()

####################################
# 5. Income (Continuous -> Binned):-
####################################

str(demographic$Income)

# Checking summary
summary(demographic$Income) 
# Obs: There are some -ve values which are wrong as Income can't be <0

# Checking how many Income values are < 0
sum(demographic$Income < 0) 
# Obs: 81
# Since only 81 values of Income are < 0 
# which is very small out of total records,
# we can remove corresponding records from master data set

nrow(demographic)
demographic <- demographic[- which(demographic$Income < 0),]
nrow(demographic)
# There are 69755 records remaining 

summary(demographic$Income)

#Checking for outliers
boxplot(demographic$Income) #No outliers

#Checking histogram
ggplot(demographic, aes(Income)) + geom_histogram()

# Binning Income in different bin and storing it in binning.income variable
demographic$binning.income <- as.factor(cut(demographic$Income, breaks = c(0,10,20, 30, 40, 50, 60), include.lowest = T))

ggplot(demographic,aes(binning.income))+geom_bar()

# Create WOE variable for categories Income
temp_ds <- create_infotables(data=demographic[,c("binning.income","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$binning.income

# Merging 4th bucket into 3rd as they both have similar WOE
levels(demographic$binning.income)
levels(demographic$binning.income)[5] <- levels(demographic$binning.income)[4]
levels(demographic$binning.income)
levels(demographic$binning.income)[4] <- "(30,50]"
levels(demographic$binning.income)

# Create WOE variable for categories Income
temp_ds <- create_infotables(data=demographic[,c("binning.income","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$binning.income

# Create a scatter plot to find the monotonicity of data points.
ggplot(temp_ds$Tables$binning.income,aes(x=binning.income,y=WOE,group=1))+
  geom_point() + 
  geom_line()

# Removing Income column as its been replaced with Income Buckets
ncol(demographic)
demographic <- subset(demographic, select = -c(Income))
ncol(demographic)


####################################
# 6. Education (Categorical):-
####################################

#Checking summary
summary(as.factor(demographic$Education))
# There are 118 blanks in Education and we would remove the data as 
# its far less than total set.

nrow(demographic)
demographic <- demographic[- which(demographic$Education == ""),]
nrow(demographic)
# There are 69637 records remaining

summary(as.factor(demographic$Education))

# Converting Education variable as factor type
demographic$Education <- as.factor(demographic$Education)

# Create WOE variable for categories Education
temp_ds <- create_infotables(data=demographic[,c("Education","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$Education

# Merging Others into Bachelor as Others are <5% of total observations
levels(demographic$Education)
levels(demographic$Education)[3] <- levels(demographic$Education)[1]
levels(demographic$Education)

#Renaming the merged group
levels(demographic$Education)[1] <- "Bachelor_others"

# Create WOE variable for categories Education
temp_ds <- create_infotables(data=demographic[,c("Education","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$Education

# Create a scatter plot to find the monotonicity of data points.
ggplot(temp_ds$Tables$Education,aes(x=Education,y=WOE,group=1))+
  geom_point() + 
  geom_line()

####################################
# 7. Profession (Categorical):-
####################################

#Checking summary
summary(as.factor(demographic$Profession))
# Obs:There is 12 blanks which is very small in number compare to 
# total no of records. so, we can remove records corresponding to these 
# blanks from master data set.

nrow(demographic)
demographic <- demographic[- which(demographic$Profession == ""),]
nrow(demographic)
# There are 69625 observations remaining

summary(as.factor(demographic$Profession))

#Converting Profession as factor type
demographic$Profession <- as.factor(demographic$Profession)

# Create WOE variable for categories Profession
temp_ds <- create_infotables(data=demographic[,c("Profession","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$Profession

# Create a scatter plot to find the monotonicity of data points.
ggplot(temp_ds$Tables$Profession,aes(x=Profession,y=WOE,group=1))+
  geom_point() + 
  geom_line()

####################################
# 8. Type.of.residence (Categorical):-
####################################

#Checking summary
summary(as.factor(demographic$Type.of.residence))
# Obs: There is 8 blanks which is very small in number compare to total no of records. 
# So, we can remove records corresponding to these blanks from master data set.

nrow(demographic)
demographic <- demographic[- which(demographic$Type.of.residence == ""),]
nrow(demographic)
# there are 69617 rows remaining 

summary(as.factor(demographic$Type.of.residence))

#Converting Type.of.residence as factor type
demographic$Type.of.residence <- as.factor(demographic$Type.of.residence)

# Create WOE variable for categories Type.of.residence
temp_ds <- create_infotables(data=demographic[,c("Type.of.residence","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$Type.of.residence

# Merging category - "Others" into "Rented" as Others have less than 5%
levels(demographic$Type.of.residence)
levels(demographic$Type.of.residence)[3] <- levels(demographic$Type.of.residence)[5]
levels(demographic$Type.of.residence)

#Renaming the merged groups
levels(demographic$Type.of.residence)[which(levels(demographic$Type.of.residence)=="Rented")] <- "Rented_Others"
levels(demographic$Type.of.residence)

# Create WOE variable for categories Type.of.residence
temp_ds <- create_infotables(data=demographic[,c("Type.of.residence","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$Type.of.residence

# Merging category - "Living with Parents" into "Company Provided" 
# as Living with Parents have less than 5% and renaming the level to Others
levels(demographic$Type.of.residence)
levels(demographic$Type.of.residence)[which(levels(demographic$Type.of.residence)=="Company provided")] <- 
  levels(demographic$Type.of.residence)[which(levels(demographic$Type.of.residence)=="Living with Parents")]
levels(demographic$Type.of.residence)

levels(demographic$Type.of.residence)[which(levels(demographic$Type.of.residence)=="Living with Parents")] <- 
  "CompanyProvided_LivingWithParents"
levels(demographic$Type.of.residence)

# Create WOE variable for categories Type.of.residence
temp_ds <- create_infotables(data=demographic[,c("Type.of.residence","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$Type.of.residence

# Create a scatter plot to find the monotonicity of data points.
ggplot(temp_ds$Tables$Type.of.residence,aes(x=Type.of.residence,y=WOE,group=1))+
  geom_point() + 
  geom_line()


####################################
# 9. NbrMnthsCurrResidence (Continuous -> Binned):-
####################################

#Checking summary
summary(demographic$NbrMnthsCurrResidence) 
#Obs: No wrong value

#Checking outliers
boxplot(demographic$NbrMnthsCurrResidence) 
#Obs: No outliers

#Checking histogram
ggplot(demographic,aes(NbrMnthsCurrResidence))+geom_histogram()

#Binning NbrMnthsCurrResidence and storing it in binning.NbrMnthsCurrResidence variable
demographic$binning.NbrMnthsCurrResidence <- as.factor(cut(demographic$NbrMnthsCurrResidence, breaks = c(0,24,48,72,96,120,144)))

ggplot(demographic,aes(binning.NbrMnthsCurrResidence))+geom_bar()

# Create WOE variable for categories binning.NbrMnthsCurrResidence
temp_ds <- create_infotables(data=demographic[,c("binning.NbrMnthsCurrResidence","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$binning.NbrMnthsCurrResidence

# Merging 6th bucket (120,144] into 2nd bucket (24,48] as (120,144] has <5%
levels(demographic$binning.NbrMnthsCurrResidence)
levels(demographic$binning.NbrMnthsCurrResidence)[which(levels(demographic$binning.NbrMnthsCurrResidence)== "(120,144]")] <- 
  levels(demographic$binning.NbrMnthsCurrResidence)[which(levels(demographic$binning.NbrMnthsCurrResidence)== "(24,48]")]
levels(demographic$binning.NbrMnthsCurrResidence)[which(levels(demographic$binning.NbrMnthsCurrResidence)== "(24,48]")] <- "(24,48]&(120,144]"
levels(demographic$binning.NbrMnthsCurrResidence)

# Create WOE variable for categories NbrMnthsCurrResidence
temp_ds <- create_infotables(data=demographic[,c("binning.NbrMnthsCurrResidence","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$binning.NbrMnthsCurrResidence


# Create a scatter plot to find the monotonicity of data points.
ggplot(temp_ds$Tables$binning.NbrMnthsCurrResidence,aes(x=binning.NbrMnthsCurrResidence,y=WOE,group=1))+
  geom_point() + 
  geom_line()

# Removing NbrMnthsCurrResidence column as its been replaced with NbrMnthsCurrResidence Buckets
ncol(demographic)
demographic <- subset(demographic, select = -c(NbrMnthsCurrResidence))
ncol(demographic)

####################################
# 10. NbrMnthsCurrCompany (Categorical):-
####################################

#Checking summary
summary(demographic$NbrMnthsCurrCompany) 
#Obs: No wrong value

#Checking outliers
boxplot(demographic$NbrMnthsCurrCompany)
# There are some outliers which will be taken care be WOE transformation.

#Checking histogram
ggplot(demographic,aes(NbrMnthsCurrCompany))+geom_histogram()

#Binning NbrMnthsCurrCompany and storing it in binning.NbrMnthsCurrCompany variable
demographic$binning.NbrMnthsCurrCompany <- as.factor(cut(demographic$NbrMnthsCurrCompany, breaks = c(0,12,24,48,60,72,84,133)))

ggplot(demographic,aes(binning.NbrMnthsCurrCompany))+geom_bar()

# Create WOE variable for categories NbrMnthsCurrCompany
temp_ds <- create_infotables(data=demographic[,c("binning.NbrMnthsCurrCompany","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$binning.NbrMnthsCurrCompany

# Merging "(60,72]" , "(72,84]" and "(84,133]"
levels(demographic$binning.NbrMnthsCurrCompany)
levels(demographic$binning.NbrMnthsCurrCompany) <- c("(0,12]","(12,24]","(24,48]","(48,60]","(60,133]","(60,133]","(60,133]")
levels(demographic$binning.NbrMnthsCurrCompany)

# Create WOE variable for categories NbrMnthsCurrCompany
temp_ds <- create_infotables(data=demographic[,c("binning.NbrMnthsCurrCompany","Reverse.Performance.Tag")], y="Reverse.Performance.Tag", parallel=FALSE)

# Observe the Percentage of records under each category(level)
temp_ds$Tables$binning.NbrMnthsCurrCompany

# Create a scatter plot to find the monotonicity of data points.
ggplot(temp_ds$Tables$binning.NbrMnthsCurrCompany,aes(x=binning.NbrMnthsCurrCompany,y=WOE,group=1))+
  geom_point() + 
  geom_line()

# Removing NbrMnthsCurrCompany column as its been replaced with NbrMnthsCurrCompany Buckets
ncol(demographic)
demographic <- subset(demographic, select = -c(NbrMnthsCurrCompany))
ncol(demographic)



# Checking for blanks in each vriable of woe_master_ds
sapply(demographic,function(x)length(which(as.character(x)==""))) # No blanks available

# Checking for NAs in each variable of woe_master_ds
sapply(demographic,function(x)length(which(is.na(x)))) # No NA 



#############################################################################
#################### Performing BiVariate Analysis #########################
#############################################################################

# Creating a function to plot default rate corresponding to each variable

plot_default_rate <- function(cat_var, var_name){
  a <- aggregate(Performance.Tag ~ cat_var, demographic, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_defaulte <- cbind(a, count)
  
  colnames(agg_defaulte) <- c(var_name, "defaulte_rate","total_applicant")
  agg_defaulte[, 2] <- format(round(agg_defaulte[, 2], 2))
  
  ggplot(agg_defaulte, aes(agg_defaulte[, 1], total_applicant, label = defaulte_rate)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5) + xlab(var_name)+
    ggtitle(paste("Default rate by",var_name))+theme(plot.title = element_text(hjust = 0.5))
  
}

# Now performing bi-variate analysis for each variable

#1. Age
#------

plot_default_rate(demographic$binning.age,"Age Group")

#2. Gender
#---------

plot_default_rate(demographic$Gender, "Gender")

#3. Marital.Status
#------------------

plot_default_rate(demographic$Marital.Status ,"Marital.Status")

#4.No.of.dependents
#------------------

plot_default_rate(demographic$No.of.dependents ,"No of Dependents")

#5.Education
#-----------
plot_default_rate(demographic$Education, "Education")

#6.Profession
#------------

plot_default_rate(demographic$Profession ,"Profession")

#7.Type.of.residence
#-------------------

plot_default_rate(demographic$Type.of.residence ,"Residence Type")

#8.Income
#---------

plot_default_rate(demographic$binning.income ,"Income Renge")

#9.NbrMnthsCurrResidence
#------------------------

plot_default_rate(demographic$binning.NbrMnthsCurrResidence ,"NbrMnthsCurrCompany")

#10.binning.NbrMnthsCurrCompany
#-------------------------------

plot_default_rate(demographic$binning.NbrMnthsCurrCompany , "NbrMnthsCurrCompany")

############################Checking for IV value for important variables####################################
#############################################################################################################

IV_demographic <- Information::create_infotables(data = subset(demographic,select = -c(Application.ID,Performance.Tag)), y="Reverse.Performance.Tag", parallel=FALSE)
IV_demographic
print(IV_demographic$Summary, row.names=FALSE)

#based on IV values  we found following variables significant. So, we will make model on only those variables
#Variable                          IV
#--------                          ----   
#binning.income                    0.0405634763
#binning.NbrMnthsCurrResidence     0.0304218855
#binning.NbrMnthsCurrCompany       0.0185364875

# storing significant variable names into a different vector

significant_variables <- c("Application.ID","Performance.Tag",
                          "binning.income","binning.NbrMnthsCurrResidence",
                          "binning.NbrMnthsCurrCompany"
                          )


# filtring significant variables value from demographic and storing them in a new data frame

woe_demographic <- demographic[,which(names(demographic) %in% significant_variables)]

# Converting all the variables except 'Application.ID' and 'Performance.Tag' as character type
#so that we can replace these variables' values with corresponding WOE values

woe_demographic[significant_variables[3:length(significant_variables)]]<- 
  lapply(woe_demographic[significant_variables[3:length(significant_variables)]],as.character)

#Converting Performance.Tag as factor type.

woe_demographic$Performance.Tag <- as.factor(woe_demographic$Performance.Tag)

# Putting our target variable Performance.Tag at the end of the data frame 

PT_index <- grep('Performance.Tag',colnames(woe_demographic))

woe_demographic <- woe_demographic[c(1:PT_index-1,(PT_index+1):ncol(woe_demographic),PT_index)]

#Now checking structure of the data frame
str(woe_demographic)

#Now we will replace all the significance variables values with their WOE values.

# Writing a generalized loop to replace variables value with woe value.


for (j in 3:length(significant_variables))
{
  start_index <- 1
  column <- significant_variables[j]
  
  na_indices <- which(is.na(woe_demographic[[column]])) 
  if ( sum(is.na(woe_demographic[[column]])) > 0 )
  {
    start_index <- 2
  }
  
  for(i in start_index:nrow(IV_demographic$Tables[[column]])){
    
    woe_demographic[[column]][which(woe_demographic[[column]] == IV_demographic$Tables[[column]][[column]][i])] <- IV_demographic$Tables[[column]]$WOE[i]
    
  }
  
  if(start_index == 2) {
    woe_demographic[[column]][na_indices] <- IV_demographic$Tables[[column]]$WOE[1]
  }
}


woe_demographic[significant_variables[3:length(significant_variables)]]<- 
  lapply(woe_demographic[significant_variables[3:length(significant_variables)]],as.numeric)


# splitting into train and test data

set.seed(100)

split_indices <- sample.split(woe_demographic$Performance.Tag, SplitRatio = 0.70)

train_demographic <- woe_demographic[split_indices, ]

test_demographic <- woe_demographic[!split_indices, ]

nrow(train_demographic)/nrow(woe_demographic)

nrow(test_demographic)/nrow(woe_demographic)


#-| Using SMOTE() function to balance the data |-#
table(train_demographic$Performance.Tag)
prop.table(table(train_demographic$Performance.Tag))

train_demographic_SMOTE <- SMOTE(Performance.Tag ~ ., train_demographic, perc.over = 100, perc.under=200)

table(train_demographic_SMOTE$Performance.Tag)
prop.table(table(train_demographic_SMOTE$Performance.Tag))


#Logistic Regression
#--------------------

#Initial model

model_1 <- glm(Performance.Tag ~ .- Application.ID, family = "binomial", data = train_demographic_SMOTE)
summary(model_1)
sort(vif(model_1),decreasing = TRUE)

#Since all variables of model_1 are significants and vif values are also very less so considering this as final model

final_model <- model_1



# Model evaluation on test_demographic
#-------------------------------------

#Getting predicted probabilities of Performance.Tag for test data.

predicted_probability <- predict(final_model,type = "response", newdata = test_demographic[,-5]) 
summary(predicted_probability) #probabilities ranges from ~34% to ~65%
test_demographic$predicted_probability<- predicted_probability

#Let's choose the cutoff value.
#Let's find the optimal probability cutoff

library(caret)
library(e1071)

perform_fn <- function(cutoff) 
{
  predicted_cutoff_Performance.Tag<- factor(ifelse(predicted_probability >= cutoff, "1", "0"))
  conf <- caret::confusionMatrix(predicted_cutoff_Performance.Tag, test_demographic$Performance.Tag, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability

summary(predicted_probability)

s = seq(.34,.65,length=500)

OUT = matrix(0,500,3)


for(i in 1:500)
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

min(abs(OUT[,1]-OUT[,2]))#0.00345401
cutoff <- s[which(abs(OUT[,1]-OUT[,2]) < 0.003)]
cutoff

# Let's choose a cutoff value of 0.04 for final model

predicted_cutoff_Performance.Tag <- factor(ifelse(predicted_probability >= 0.5, "1", "0"))

conf_final <- confusionMatrix(predicted_cutoff_Performance.Tag, test_demographic$Performance.Tag, positive = "1")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc #0.5616471 

sens #0.5612245

spec #0.5616658 

test_demographic$predicted_Performance.Tag<- factor(ifelse(test_demographic$predicted_probability >=0.5, "1","0"))


#Model Evaluation on demographic_rejected
#----------------------------------------

#Checking for missing values
sapply(demographic_rejected,function(x)sum(is.na(x))) #No NA values except in Performance.Tag

sapply(demographic_rejected,function(x)length(which(as.character(x)==""))) #There is 1-1 blank value in Education and Profession
#But during EDA on demographic data we have found that only below indipendent variables were significance:

#income               
#NbrMnthsCurrResidence
#NbrMnthsCurrCompany  

#So, Filtering out significant variables from demographic_rejected. 

sig_var_rejected <- c("Application.ID","Performance.Tag",
                       "Income","NbrMnthsCurrResidence",
                       "NbrMnthsCurrCompany"
                      )


# filtring significant variables value from demographic and storing them in a new data frame

woe_demographic_rejected <- demographic_rejected[,which(names(demographic_rejected) %in% sig_var_rejected)]

#Binning variables as exactly same bin size of demographic(non-rejected) data set.

#Income
#-----
summary(woe_demographic_rejected$Income) # No -ve value

woe_demographic_rejected$binning.income <- as.factor(cut(woe_demographic_rejected$Income, breaks = c(0,10,20, 30, 50, 60), include.lowest = T))
levels(woe_demographic_rejected$binning.income)

# Removing Income column as its been replaced with Income Buckets
ncol(woe_demographic_rejected)
woe_demographic_rejected <- subset(woe_demographic_rejected, select = -c(Income))
ncol(woe_demographic_rejected)

#NbrMnthsCurrResidence
#---------------------
summary(woe_demographic_rejected$NbrMnthsCurrResidence)

woe_demographic_rejected$binning.NbrMnthsCurrResidence <- as.factor(cut(woe_demographic_rejected$NbrMnthsCurrResidence, breaks = c(0,24,48,72,96,120,144)))
levels(woe_demographic_rejected$binning.NbrMnthsCurrResidence)

#making bins exactly same as demographic(non-rejected) data set
levels(woe_demographic_rejected$binning.NbrMnthsCurrResidence) <- c("(0,24]","(24,48]&(120,144]","(48,72]","(72,96]","(96,120]","(24,48]&(120,144]")
levels(woe_demographic_rejected$binning.NbrMnthsCurrResidence)

# Removing NbrMnthsCurrResidence column as its been replaced with NbrMnthsCurrResidence Buckets
ncol(woe_demographic_rejected)
woe_demographic_rejected <- subset(woe_demographic_rejected, select = -c(NbrMnthsCurrResidence))
ncol(woe_demographic_rejected)

#NbrMnthsCurrCompany
#-------------------
summary(woe_demographic_rejected$NbrMnthsCurrCompany)

#making bins exactly same as demographic(non-rejected) data set
woe_demographic_rejected$binning.NbrMnthsCurrCompany <- as.factor(cut(woe_demographic_rejected$NbrMnthsCurrCompany, breaks = c(0,12,24,48,60,133)))
levels(woe_demographic_rejected$binning.NbrMnthsCurrCompany)

# Removing NbrMnthsCurrCompany column as its been replaced with NbrMnthsCurrCompany Buckets
ncol(woe_demographic_rejected)
woe_demographic_rejected <- subset(woe_demographic_rejected, select = -c(NbrMnthsCurrCompany))
ncol(woe_demographic_rejected)

#making a vector of significance variables

sig_var_rejected <- c("Application.ID","Performance.Tag",
                      "binning.income","binning.NbrMnthsCurrResidence",
                      "binning.NbrMnthsCurrCompany"
                   )


# Converting all the variables except 'Application.ID' and 'Performance.Tag' as character type
#so that we can replace these variables' values with corresponding WOE values

str(woe_demographic_rejected)
woe_demographic_rejected[sig_var_rejected[3:length(sig_var_rejected)]]<- 
  lapply(woe_demographic_rejected[sig_var_rejected[3:length(sig_var_rejected)]],as.character)
str(woe_demographic_rejected)

# Putting our target variable Performance.Tag at the end of the data frame 
Performance.Tag <- woe_demographic_rejected$Performance.Tag
woe_demographic_rejected <- subset(woe_demographic_rejected, select = -c(Performance.Tag))
woe_demographic_rejected<- cbind(woe_demographic_rejected,Performance.Tag)

#Converting Performance.Tag as factor type.
woe_demographic_rejected$Performance.Tag <- as.factor(woe_demographic_rejected$Performance.Tag)

#Now checking structure of the data frame
str(woe_demographic_rejected)

# Now we will replace all the significance variables values with their WOE values.
# Generalized loop to replace variables value with woe value.

for (j in 3:length(sig_var_rejected))
{
  start_index <- 1
  column <- sig_var_rejected[j]
  
  na_indices <- which(is.na(woe_demographic_rejected[[column]])) 
  if ( sum(is.na(woe_demographic_rejected[[column]])) > 0 )
  {
    start_index <- 2
  }
  
  for(i in start_index:nrow(IV_demographic$Tables[[column]])){
    
    woe_demographic_rejected[[column]][which(woe_demographic_rejected[[column]] == IV_demographic$Tables[[column]][[column]][i])] <- 
      IV_demographic$Tables[[column]]$WOE[i]
    
  }
  
  if(start_index == 2) {
    woe_demographic_rejected[[column]][na_indices] <- IV_demographic$Tables[[column]]$WOE[1]
  }
}

woe_demographic_rejected[sig_var_rejected[3:length(sig_var_rejected)]]<- 
  lapply(woe_demographic_rejected[sig_var_rejected[3:length(sig_var_rejected)]],as.numeric)


predicted_probability_rejected <- predict(final_model,type = "response", newdata = woe_demographic_rejected[,-5])
summary(predicted_probability_rejected)


woe_demographic_rejected$predicted_Performance.Tag<- factor(ifelse(predicted_probability_rejected >=0.5, "1","0"))

table(woe_demographic_rejected$predicted_Performance.Tag)
#We can see that our model accuracy is 1117/(308+1117) = 0.78 i.e. ~78%. 
#It means our model is also predicting that 78% of rejected population should be rejected.


