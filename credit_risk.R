################# Installing and Loading the required packages ##################
#install.packages("MASS")
#install.packages("car")
#install.packages("e1071")
#install.packages("caret", dependencies = c("Depends", "Suggests"))
#install.packages("cowplot")
#install.packages("ggplot2")
#install.packages("GGally")
#install.packages("devtools")
#install.packages("woe")
#install.packages("dummies")
#install.packages("information")
#install.packages("dplyr")
#install.packages("ROSE")
#install.packages("gsubfn")
#install.packages("riv")
#install,packages("ROCR")
#install.packages("randomForest")
#install.packages("pROC")
install_github("riv","tomasgreif")
install_github("woe","tomasgreif")
# *************************************************************************************
# Libraries Used 
# *************************************************************************************
library(randomForest)
library(ROCR)
library(pROC)
library(ggplot2)
library(devtools)
library(woe)
library(dplyr)
library(gsubfn)
library(cowplot)
library(dummies)
library(Information)
library(caTools)
library(car)
library(e1071)
library(ROSE)
library(MASS)
library(riv)
library(GGally)
library(caret)

####LOading Datasets####
#Loading Credit Bureau Data set
Credit_Bureau_Data <- read.csv("Credit Bureau data.csv",stringsAsFactors = F)

# Checking structure of Credit Bureau dataset 
str(Credit_Bureau_Data)

# Summary of dataset
summary(Credit_Bureau_Data)

#Loading Demographic Data set
Demographic_Data <- read.csv("Demographic data.csv", stringsAsFactors = F)

# Checking structure of Demographic dataset 
str(Demographic_Data)

# Summary of Demographic dataset
summary(Demographic_Data)

#Checking Unique Application ID in demographic and credit bureau datasets
length(Demographic_Data$Application.ID) #71295
length(unique(Credit_Bureau_Data$Application.ID)) #71292

#Changing the column name for demographic dataset columns
colnames(Demographic_Data)[colnames(Demographic_Data) == "Marital.Status..at.the.time.of.application."] <- "Marital_Status"
colnames(Demographic_Data)[colnames(Demographic_Data) == "No.of.dependents"] <- "No_of_dependents"
colnames(Demographic_Data)[colnames(Demographic_Data) == "Type.of.residence"] <- "Type_of_Residence"
colnames(Demographic_Data)[colnames(Demographic_Data) == "No.of.months.in.current.company"] <- "No_of_months_in_current_company"
colnames(Demographic_Data)[colnames(Demographic_Data) == "No.of.months.in.current.residence"] <- "No_of_months_in_current_residence"

#Changing the column name for Credit Bureau dataset columns
colnames(Credit_Bureau_Data)[colnames(Credit_Bureau_Data) == "No.of.times.90.DPD.or.worse.in.last.6.months"] <- "No_of_times_90_DPD_or_worse_in_last_6_months"
colnames(Credit_Bureau_Data)[colnames(Credit_Bureau_Data) == "No.of.times.60.DPD.or.worse.in.last.6.months"] <- "No_of_times_60_DPD_or_worse_in_last_6_months"
colnames(Credit_Bureau_Data)[colnames(Credit_Bureau_Data) == "No.of.times.30.DPD.or.worse.in.last.6.months"] <- "No_of_times_30_DPD_or_worse_in_last_6_months"
colnames(Credit_Bureau_Data)[colnames(Credit_Bureau_Data) == "No.of.times.90.DPD.or.worse.in.last.12.months"] <- "No_of_times_90_DPD_or_worse_in_last_12_months"
colnames(Credit_Bureau_Data)[colnames(Credit_Bureau_Data) == "No.of.times.60.DPD.or.worse.in.last.12.months"] <- "No_of_times_60_DPD_or_worse_in_last_12_months"
colnames(Credit_Bureau_Data)[colnames(Credit_Bureau_Data) == "No.of.times.30.DPD.or.worse.in.last.12.months"] <- "No_of_times_30_DPD_or_worse_in_last_12_months"
colnames(Credit_Bureau_Data)[colnames(Credit_Bureau_Data) == "Avgas.CC.Utilization.in.last.12.months"] <- "Avgas_CC_Utilization_in_last_12_months"
colnames(Credit_Bureau_Data)[colnames(Credit_Bureau_Data) == "No.of.trades.opened.in.last.6.months"] <- "No_of_trades_opened_in_last_6_months"
colnames(Credit_Bureau_Data)[colnames(Credit_Bureau_Data) == "No.of.trades.opened.in.last.12.months"] <- "No_of_trades_opened_in_last_12_months"
colnames(Credit_Bureau_Data)[colnames(Credit_Bureau_Data) == "No.of.PL.trades.opened.in.last.6.months"] <- "No_of_PL_trades_opened_in_last_6_months"
colnames(Credit_Bureau_Data)[colnames(Credit_Bureau_Data) == "No.of.PL.trades.opened.in.last.12.months"] <- "No_of_PL_trades_opened_in_last_12_months"
colnames(Credit_Bureau_Data)[colnames(Credit_Bureau_Data) == "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans."] <- "No_of_Inquiries_in_last_6_months_excluding_home_auto_loans"
colnames(Credit_Bureau_Data)[colnames(Credit_Bureau_Data) == "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans."] <- "No_of_Inquiries_in_last_12_months_excluding_home_auto_loans"
colnames(Credit_Bureau_Data)[colnames(Credit_Bureau_Data) == "Presence.of.open.home.loan"] <- "Presence_of_open_home_loan"
colnames(Credit_Bureau_Data)[colnames(Credit_Bureau_Data) == "Outstanding.Balance"] <- "Outstanding_Balance"
colnames(Credit_Bureau_Data)[colnames(Credit_Bureau_Data) == "Total.No.of.Trades"] <- "Total_No_of_Trades"
colnames(Credit_Bureau_Data)[colnames(Credit_Bureau_Data) == "Presence.of.open.auto.loan"] <- "Presence_of_open_auto_loan"

#Creating a dataframe from the demographic data which have a value for the Performance tag
Demographic_Data_New <- subset(Demographic_Data,Demographic_Data$Performance.Tag!= "")

#Find the rows containing the duplicate application id
Demographic_Data_New[duplicated(Demographic_Data_New$Application.ID),]

# 3 duplicate application ids.
#Creating a dataframe enlisting the duplicate application ids.
new.data <- Demographic_Data_New[ which( Demographic_Data_New$Application.ID == '671989187' | Demographic_Data_New$Application.ID == '765011468' | Demographic_Data_New$Application.ID == '653287861' ) , ]

#Removing the rows with duplicate application id having performance tag= 0
Demographic_Data_New <- Demographic_Data_New[-c(5244, 24387, 48603), ]

#Extracting Rejected candidates from Demographic dataframe
Demographic_Data_Rejected <- Demographic_Data[is.na(Demographic_Data$Performance.Tag),]

length(unique(Demographic_Data_New$Application.ID)) #69867 rows

summary(Demographic_Data_New$Performance.Tag)

#Calculating Response rate
table(Demographic_Data_New$Performance.Tag)
response <- 2948/(2948+66919)
response  #4.219%

# Exploring Each Variable from Demographic dataset
##################################################################
#                           AGE
##################################################################
#Checking outliers for Age
quantile(Demographic_Data_New$Age,seq(0,1,0.01))
boxplot(Demographic_Data_New$Age)

#Treating erroneous values in age column, replacing it with the mean of age
Demographic_Data_New$Modified_Age <- ifelse(Demographic_Data_New$Age<15,round(mean(Demographic_Data_New$Age,na.rm=T),0),Demographic_Data_New$Age)

# Creating Modified Age for rejected applications
Demographic_Data_Rejected$Modified_Age <- ifelse(Demographic_Data_Rejected$Age<15,round(mean(Demographic_Data_Rejected$Age,na.rm=T),0),Demographic_Data_Rejected$Age)

#Now checking for outliers in the new column Modified_Date
quantile(Demographic_Data_New$Modified_Age,seq(0,1,0.01))
boxplot(Demographic_Data_New$Modified_Age)

#Plotting age to derive insights from customer age
ggplot(Demographic_Data_New,aes(x=Modified_Age)) +geom_bar(color="black",fill="lightgreen")

#Creating age bins
Demographic_Data_New$BinningAge <- as.factor(cut(Demographic_Data_New$Modified_Age, breaks = c(14, 25, 35, 45, 55, 65)))

#Creating Modified Age column for Rejected applications
Demographic_Data_Rejected$BinningAge <- as.factor(cut(Demographic_Data_Rejected$Modified_Age, breaks = c(14, 25, 35, 45, 55, 65))) 

agg_age <- merge(aggregate(Performance.Tag ~ BinningAge, Demographic_Data_New, mean),aggregate(Performance.Tag ~ BinningAge,
                                                                                               Demographic_Data_New, sum),by = "BinningAge") 
# Adding No.of_prospect
count <- data.frame(table(Demographic_Data_New$BinningAge))
count <- count[,-1]
agg_age <- cbind(agg_age,count)
colnames(agg_age) <- c("age", "response_rate", "count_prospects","No.of_prospect")

# Round Off the values
agg_age$response_rate <- format(round(agg_age$response_rate, 4))
agg_age

#Plotting age to derive insights from customer age Bins
ggplot(Demographic_Data_New,aes(x=BinningAge)) +geom_bar(color="black",fill="lightgreen") + xlab("Age Bins") +
  ylab("Number of customers in the age bin")
summary(Demographic_Data_New$BinningAge)

# Let's see the response rate of each age bucket in the plot
ggplot(agg_age, aes(age, count_prospects,label = response_rate)) + 
  geom_bar(stat = 'identity', color = "black",fill="green") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)+ xlab("Age Bins") +
  ylab("Number of customers in the age bin")

#####################################################################################
#                     Deriving Insights of Income Variable
#####################################################################################
#Checking NA values in Income Column
sum(is.na(Demographic_Data_New$Income))  #No NA values are present

##Checking outliers for Income
quantile(Demographic_Data_New$Income,seq(0,1,0.01))
boxplot(Demographic_Data_New$Income)
summary(Demographic_Data_New$Income)

#Treating negative income values
Demographic_Data_New$Modified_Income <- ifelse(Demographic_Data_New$Income<0,round(mean(Demographic_Data_New$Income,na.rm=T),0),Demographic_Data_New$Income)

#Treating negative income values for Rejected applications
Demographic_Data_Rejected$Modified_Income <- ifelse(Demographic_Data_Rejected$Income<0,round(mean(Demographic_Data_Rejected$Income,na.rm=T),0),Demographic_Data_Rejected$Income)

#Now checking for outliers in the new column Modified_Date
quantile(Demographic_Data_New$Modified_Income,seq(0,1,0.01))
boxplot(Demographic_Data_New$Modified_Income)

#Plotting age to derive insights from customer Income
ggplot(Demographic_Data_New,aes(x=Modified_Income)) +geom_bar(color="black",fill="lightgreen")

#Creating Income Bins
Demographic_Data_New$BinningIncome <- as.factor(cut(Demographic_Data_New$Modified_Income, breaks = c(0, 10, 20, 30, 40, 50, 60),include.lowest = TRUE))

#Creating Income Bins for Rejected applications
Demographic_Data_Rejected$BinningIncome <- as.factor(cut(Demographic_Data_Rejected$Modified_Income, breaks = c(0, 10, 20, 30, 40, 50, 60),include.lowest = TRUE))

agg_Income <- merge(aggregate(Performance.Tag ~ BinningIncome, Demographic_Data_New, mean),aggregate(Performance.Tag ~ BinningIncome,
                                                                                                     Demographic_Data_New, sum),by = "BinningIncome") 
# Adding No.of_prospect
count1 <- data.frame(table(Demographic_Data_New$BinningIncome))
count1 <- count1[,-1]
agg_Income <- cbind(agg_Income,count1)
colnames(agg_Income) <- c("Income", "response_rate", "count_prospects","No.of_prospect")

# Round Off the values
agg_Income$response_rate <- format(round(agg_Income$response_rate, 4))
agg_Income

#Plotting age to derive insights from customer Income Bins
ggplot(Demographic_Data_New,aes(x=BinningIncome)) +geom_bar(color="black",fill="lightgreen") + xlab("Income Bins") +
  ylab("Number of customers in the Income bin")
summary(Demographic_Data_New$BinningIncome)

# Let's see the response rate of each Income bucket in the plot
ggplot(agg_Income, aes(Income, count_prospects,label = response_rate)) + 
  geom_bar(stat = 'identity', color = "black",fill="green") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)+ xlab("Income Bins") +
  ylab("Number of customers in the Income bin")

###############################################################################################
#           Deriving Insights of No of Months in Current Residence Variable
###############################################################################################
#Checking NA values in No of Months in Current Residence Variable
sum(is.na(Demographic_Data_New$No_of_months_in_current_residence))    #No NA values are present

##Checking outliers for No of Months in Current Residence
quantile(Demographic_Data_New$No_of_months_in_current_residence,seq(0,1,0.01))
boxplot(Demographic_Data_New$No_of_months_in_current_residence)
summary(Demographic_Data_New$No_of_months_in_current_residence)

#Plotting to derive insights from No.of.months.in.current.residence Variable
ggplot(Demographic_Data_New,aes(x=No_of_months_in_current_residence)) +geom_bar(color="black",fill="lightgreen")

#Creating Bins for No.of.months.in.current.residence Variable
Demographic_Data_New$BinningCurentResidence <- as.factor(cut(Demographic_Data_New$No_of_months_in_current_residence, breaks = c(0, 20, 40, 60, 80, 100, 130)))

#Creating Bins for No.of.months.in.current.residence Variable for rejected applications
Demographic_Data_Rejected$BinningCurentResidence <- as.factor(cut(Demographic_Data_Rejected$No_of_months_in_current_residence, breaks = c(0, 20, 40, 60, 80, 100, 130)))

agg_CurrentResidence <- merge(aggregate(Performance.Tag ~ BinningCurentResidence, Demographic_Data_New, mean),aggregate(Performance.Tag ~ BinningCurentResidence,
                                                                                                                        Demographic_Data_New, sum),by = "BinningCurentResidence") 

# Adding No.of_prospect
count2 <- data.frame(table(Demographic_Data_New$BinningCurentResidence))
count2 <- count2[,-1]
agg_CurrentResidence <- cbind(agg_CurrentResidence,count2)
colnames(agg_CurrentResidence) <- c("CurrentResidenceMonths", "response_rate", "count_prospects","No.of_prospect")

# Round Off the values
agg_CurrentResidence$response_rate <- format(round(agg_CurrentResidence$response_rate, 4))
agg_CurrentResidence

#Plotting to derive insights from No.of.months.in.current.residence Variable
ggplot(Demographic_Data_New,aes(x=BinningCurentResidence)) +geom_bar(color="black",fill="lightgreen") + xlab("Current Residence Bins") +
  ylab("Number of customers in the Current Residence bin")
summary(Demographic_Data_New$BinningCurentResidence)

# Let's see the response rate of each bucket in the plot
ggplot(agg_CurrentResidence, aes(CurrentResidenceMonths, count_prospects,label = response_rate)) + 
  geom_bar(stat = 'identity', color = "black",fill="green") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)+ xlab("Current Residence Bins") +
  ylab("Number of customers in the Current Residence bin")

#######################################################################################
#             Deriving Insights for No of Months in Current Company Variable
#######################################################################################
#Checking NA values in No of Months in Current Company Variable
sum(is.na(Demographic_Data_New$No_of_months_in_current_company))    #No NA values are present

##Checking outliers for No of Months in Current Company
quantile(Demographic_Data_New$No_of_months_in_current_company,seq(0,1,0.01))
boxplot(Demographic_Data_New$No_of_months_in_current_company)
summary(Demographic_Data_New$No_of_months_in_current_company)

# Capping the Upper values of Current Months in company with 75
Demographic_Data_New[(which(Demographic_Data_New$No_of_months_in_current_company > 75)),]$No_of_months_in_current_company <- 75

#Plotting to derive insights from No.of.months.in.current.company Variable
ggplot(Demographic_Data_New,aes(x=No_of_months_in_current_company)) +geom_bar(color="black",fill="lightgreen")

#Creating Bins for No.of.months.in.current.company Variable
Demographic_Data_New$BinningCurrentCompany <- as.factor(cut(Demographic_Data_New$No_of_months_in_current_company, breaks = c(0, 15, 30, 45, 60, 75)))

#Creating Bins for No.of.months.in.current.company Variable for rejected applications
Demographic_Data_Rejected$BinningCurrentCompany <- as.factor(cut(Demographic_Data_Rejected$No_of_months_in_current_company, breaks = c(0, 15, 30, 45, 60, 75)))

agg_CurrentCompany <- merge(aggregate(Performance.Tag ~ BinningCurrentCompany, Demographic_Data_New, mean),aggregate(Performance.Tag ~ BinningCurrentCompany,
                                                                                                                     Demographic_Data_New, sum),by = "BinningCurrentCompany") 
# Adding No.of_prospect
count3 <- data.frame(table(Demographic_Data_New$BinningCurrentCompany))
count3 <- count3[,-1]
agg_CurrentCompany <- cbind(agg_CurrentCompany,count3)
colnames(agg_CurrentCompany) <- c("CurrentCompanyMonths", "response_rate", "count_prospects","No.of_prospect")

# Round Off the values
agg_CurrentCompany$response_rate <- format(round(agg_CurrentCompany$response_rate, 4))
agg_CurrentCompany

#Plotting to derive insights from No.of.months.in.current.residence Variable
ggplot(Demographic_Data_New,aes(x=BinningCurrentCompany)) +geom_bar(color="black",fill="lightgreen") + xlab("Current Company Bins") +
  ylab("Number of customers in the Current Company bin")
summary(Demographic_Data_New$BinningCurrentCompany)

# Let's see the response rate of each bucket in the plot
ggplot(agg_CurrentCompany, aes(CurrentCompanyMonths, count_prospects,label = response_rate)) + 
  geom_bar(stat = 'identity', color = "black",fill="green") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)+ xlab("Current Company Bins") +
  ylab("Number of customers in the Current Company bin")

#####################################################################################
#                       Deriving Insights of Gender Variable
#####################################################################################
#Checking NA values in Gender Variable
sum(is.na(Demographic_Data_New$Gender))  #No NA values are present
summary(Demographic_Data_New$Gender)

#Treating Missing values in Gender, replacing it with Others
Demographic_Data_New$Modified_Gender <- ifelse(Demographic_Data_New$Gender =="","F",Demographic_Data_New$Gender)

#Treating Missing values in Gender, replacing it with Others for rejected applications
Demographic_Data_Rejected$Modified_Gender <- ifelse(Demographic_Data_Rejected$Gender =="","F",Demographic_Data_Rejected$Gender)

Demographic_Data_New$Modified_Gender <- as.factor(Demographic_Data_New$Modified_Gender)

#Plotting to derive insights from gender Variable
ggplot(Demographic_Data_New,aes(x=Modified_Gender)) +geom_bar(color="black",fill="lightgreen")

agg_Gender <- merge(aggregate(Performance.Tag ~ Modified_Gender, Demographic_Data_New, mean),aggregate(Performance.Tag ~ Modified_Gender,
                                                                                                       Demographic_Data_New, sum),by = "Modified_Gender") 
# Adding No.of_prospect
count4 <- data.frame(table(Demographic_Data_New$Modified_Gender))
count4 <- count4[,-1]
agg_Gender <- cbind(agg_Gender,count4)
colnames(agg_Gender) <- c("Gender", "Default_rate", "count_prospects","No.of_prospect")

# Round Off the values
agg_Gender$Default_rate <- format(round(agg_Gender$Default_rate, 4))
agg_Gender

#Plotting to derive insights from Gender Variable
ggplot(Demographic_Data_New,aes(x=Modified_Gender, fill =(Demographic_Data_New$Performance.Tag))) +geom_bar(color="black",fill="lightgreen") + xlab("Gender") +
  ylab("Number of customers in each Gender")
summary(Demographic_Data_New$Modified_Gender)

# Let's see the response rate of each bucket in the plot
ggplot(agg_Gender, aes(Gender,count_prospects,label = Default_rate)) + 
  geom_bar(stat = 'identity', color = "black",fill="green") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)+ xlab("Gender") +
  ylab("Number of customers in each Gender")

###############################################################
#         Deriving Insights of Marital Status Variable        # 
###############################################################
#Checking NA values in Gender Variable
sum(is.na(Demographic_Data_New$`Marital_Status`))   #No NA values are present

#Treating Missing values in Marital Status, replacing it with Others
Demographic_Data_New$Modified_Marital_Status <- ifelse(Demographic_Data_New$Marital_Status =="","Other",Demographic_Data_New$Marital_Status)
Demographic_Data_New$Modified_Marital_Status <- as.factor(Demographic_Data_New$Modified_Marital_Status)

#Treating Missing values in Marital Status, replacing it with Others for rejected applications
Demographic_Data_Rejected$Modified_Marital_Status <- ifelse(Demographic_Data_Rejected$Marital_Status =="","Other",Demographic_Data_Rejected$Marital_Status)

Demographic_Data_New$Modified_Marital_Status <- as.factor(Demographic_Data_New$Modified_Marital_Status)

#Plotting to derive insights from Marital status Variable
ggplot(Demographic_Data_New,aes(x=Modified_Marital_Status)) +geom_bar(color="black",fill="lightgreen")

agg_Marital_Status <- merge(aggregate(Performance.Tag ~ Modified_Marital_Status, Demographic_Data_New, mean),aggregate(Performance.Tag ~ Modified_Marital_Status,
                                                                                                                       Demographic_Data_New, sum),by = "Modified_Marital_Status") 
# Adding No.of_prospect
count5 <- data.frame(table(Demographic_Data_New$Modified_Marital_Status))
count5 <- count5[,-1]
agg_Marital_Status <- cbind(agg_Marital_Status,count5)
colnames(agg_Marital_Status) <- c("Marital_Status", "Default_rate", "count_prospects","No.of_prospect")

# Round Off the values
agg_Marital_Status$Default_rate <- format(round(agg_Marital_Status$Default_rate, 4))
agg_Marital_Status

#Plotting to derive insights from Marital Status Variable
ggplot(Demographic_Data_New,aes(x=as.factor(Modified_Marital_Status), fill =(Demographic_Data_New$Performance.Tag))) +geom_bar(color="black",fill="lightgreen") + xlab("Marital Status") +
  ylab("Number of customers in each Category")

# Let's see the Default rate of each bucket in the plot
ggplot(agg_Marital_Status, aes(Marital_Status,count_prospects,label = Default_rate)) + 
  geom_bar(stat = 'identity', color = "black",fill="green") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)+ xlab("Marital Status") +
  ylab("Number of customers in each Category")

###################################################################
#         Deriving Insights of No_of_dependents Variable          #
###################################################################
#Checking NA values in No_of_dependents  Variable
sum(is.na(Demographic_Data_New$No_of_dependents))   #3 NA values are present
Demographic_Data_New$No_of_dependents<- as.numeric(Demographic_Data_New$No_of_dependents)

#Treating NA values In No.of.dependent variable
Demographic_Data_New$Modified_No_of_dependents <- ifelse(is.na(Demographic_Data_New$No_of_dependents),round(mean(Demographic_Data_New$No_of_dependents,na.rm = T),0),Demographic_Data_New$No_of_dependents)

#Treating NA values In No.of.dependent variable for rejected applications
Demographic_Data_Rejected$Modified_No_of_dependents <- ifelse(is.na(Demographic_Data_Rejected$No_of_dependents),round(mean(Demographic_Data_Rejected$No_of_dependents,na.rm = T),0),Demographic_Data_Rejected$No_of_dependents)

sum(is.na(Demographic_Data_New$Modified_No_of_dependents))

#Converting into factor Variable
Demographic_Data_New$Modified_No_of_dependents  <- as.factor(Demographic_Data_New$Modified_No_of_dependents)
summary(Demographic_Data_New$Modified_No_of_dependents)

#Plotting to derive insights from No.of.months.in.current.residence Variable
ggplot(Demographic_Data_New,aes(x=Modified_No_of_dependents)) +geom_bar(color="black",fill="lightgreen")

agg_No_of_dependents <- merge(aggregate(Performance.Tag ~ Modified_No_of_dependents, Demographic_Data_New, mean),aggregate(Performance.Tag ~ Modified_No_of_dependents,
                                                                                                                           Demographic_Data_New, sum),by = "Modified_No_of_dependents") 

#####################################################################
#         Deriving Insights of Education Variable                   #
#####################################################################
# #Checking NA values in Education  Variable
sum(is.na(Demographic_Data_New$Education))   #No NA values are present

#Treating Missing Values values Education variable
Demographic_Data_New$Modified_Education <- ifelse(Demographic_Data_New$Education == "","Others",Demographic_Data_New$Education)

#Treating Missing Values for Education variable for Rejected applications
Demographic_Data_Rejected$Modified_Education <- ifelse(Demographic_Data_Rejected$Education == "","Others",Demographic_Data_Rejected$Education)
 
#Converting into factor Variable
Demographic_Data_New$Modified_Education  <- as.factor(Demographic_Data_New$Modified_Education)

#Plotting to derive insights from Education Variable
ggplot(Demographic_Data_New,aes(x=Modified_Education)) +geom_bar(color="black",fill="lightgreen")

agg_Education <- merge(aggregate(Performance.Tag ~ Modified_Education, Demographic_Data_New, mean),aggregate(Performance.Tag ~ Modified_Education,
                                                                                                              Demographic_Data_New, sum),by = "Modified_Education")
# Adding No.of_prospect
count7 <- data.frame(table(Demographic_Data_New$Modified_Education))
count7 <- count7[,-1]
agg_Education <- cbind(agg_Education,count7)
colnames(agg_Education) <- c("Education", "Default_rate", "count_prospects","No.of_prospect")

#Round Off the values
agg_Education$Default_rate <- format(round(agg_Education$Default_rate, 4))
agg_Education

#Let's see the Default rate of each bucket in the plot
ggplot(agg_Education, aes(Education,count_prospects,label = Default_rate)) +
geom_bar(stat = 'identity', color = "black",fill="green") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
geom_text(size = 3, vjust = -0.5)+ xlab("Education") +
ylab("Number of customers in each Category")

##################################################################
#         Deriving Insights of Profession Variable               #
##################################################################
#Checking NA values in profession  Variable
sum(is.na(Demographic_Data_New$Profession))   #No NA values are present

#Treating Missing Values in Profession variable
Demographic_Data_New$Modified_Profession <- ifelse(Demographic_Data_New$Profession == "","Others",Demographic_Data_New$Profession)

#Treating Missing Values in Profession variable for Rejected applications
Demographic_Data_Rejected$Modified_Profession <- ifelse(Demographic_Data_Rejected$Profession == "","Others",Demographic_Data_Rejected$Profession)

#Converting into factor Variable
Demographic_Data_New$Modified_Profession  <- as.factor(Demographic_Data_New$Modified_Profession)

#Plotting to derive insights from Profession Variable
ggplot(Demographic_Data_New,aes(x=Modified_Profession)) +geom_bar(color="black",fill="lightgreen")

agg_Profession <- merge(aggregate(Performance.Tag ~ Modified_Profession, Demographic_Data_New, mean),aggregate(Performance.Tag ~ Modified_Profession,
                                                                                                               Demographic_Data_New, sum),by = "Modified_Profession") 
# Adding No.of_prospect
count8 <- data.frame(table(Demographic_Data_New$Modified_Profession))
count8 <- count8[,-1]
agg_Profession <- cbind(agg_Profession,count8)
colnames(agg_Profession) <- c("Profession", "Default_rate", "count_prospects","No.of_prospect")

# Round Off the values
agg_Profession$Default_rate <- format(round(agg_Profession$Default_rate, 4))
agg_Profession

# Let's see the Default rate of each bucket in the plot
ggplot(agg_Profession, aes(Profession,count_prospects,label = Default_rate)) + 
  geom_bar(stat = 'identity', color = "black",fill="green") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)+ xlab("Profession") +
  ylab("Number of customers in each Category")

###################################################################
#         Deriving Insights of Type of Residence Variable         #
###################################################################
#Checking NA values in Type of Residence  Variable
sum(is.na(Demographic_Data_New$Type_of_Residence)) #No NA values are present

#Treating Missing Values in Type of Residence variable
Demographic_Data_New$Modified_Residence_Type <- ifelse(Demographic_Data_New$Type_of_Residence == "","Others",Demographic_Data_New$Type_of_Residence)

#Treating Missing Values in Type of Residence variable for Rejected applications
Demographic_Data_Rejected$Modified_Residence_Type <- ifelse(Demographic_Data_Rejected$Type_of_Residence == "","Others",Demographic_Data_Rejected$Type_of_Residence)

#Converting into factor Variable
Demographic_Data_New$Modified_Residence_Type  <- as.factor(Demographic_Data_New$Modified_Residence_Type)

#Plotting to derive insights from Residence Variable
ggplot(Demographic_Data_New,aes(x=Modified_Residence_Type)) +geom_bar(color="black",fill="lightgreen")

agg_Residence <- merge(aggregate(Performance.Tag ~ Modified_Residence_Type, Demographic_Data_New, mean),aggregate(Performance.Tag ~ Modified_Residence_Type,
                                                                                                                  Demographic_Data_New, sum),by = "Modified_Residence_Type") 
# Adding No.of_prospect
count9 <- data.frame(table(Demographic_Data_New$Modified_Residence_Type))
count9 <- count9[,-1]
agg_Residence <- cbind(agg_Residence,count9)
colnames(agg_Residence) <- c("ResidenceType", "Default_rate", "count_prospects","No.of_prospect")

# Round Off the values
agg_Residence$Default_rate <- format(round(agg_Residence$Default_rate, 4))
agg_Residence

# Let's see the Default rate of each bucket in the plot
ggplot(agg_Residence, aes(ResidenceType,count_prospects,label = Default_rate)) + 
  geom_bar(stat = 'identity', color = "black",fill="green") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)+ xlab("Residence_Type") +
  ylab("Number of customers in each Category")

#Storing in Separate Dataframe
Demographic_Data_New1<- Demographic_Data

###########WOE Calculation for Demographic Vriables############
Demographic_Data_New1[sapply(Demographic_Data_New1, is.character)] <- lapply(Demographic_Data_New1[sapply(Demographic_Data_New1, is.character)], 
                                                                             as.factor)
sapply(Demographic_Data_New1, function(x) (as.factor(x)))
Demographic_Data_New1$No_of_dependents<- as.factor(Demographic_Data_New1$No_of_dependents)
str(Demographic_Data_New1)

Demographic_Data_New1 <- subset(Demographic_Data_New1,Demographic_Data_New1$Performance.Tag!= "")

####Calulating IV values for the Demographic Variables#####
IV <- create_infotables(data=Demographic_Data_New1, y="Performance.Tag", bins=10, parallel=TRUE)
IV_Value = data.frame(IV$Summary)

# Printing IV Values for Demographic Vaiables
print(IV$Tables$Education, row.names=FALSE)
print(IV$Tables$Income, row.names=FALSE)
print(IV$Tables$Age, row.names=FALSE)
print(IV$Tables$Gender, row.names=FALSE)
print(IV$Tables$Marital_Status, row.names=FALSE)
print(IV$Tables$No_of_dependents, row.names=FALSE)
print(IV$Tables$Profession, row.names=FALSE)
print(IV$Tables$Type_of_residence, row.names=FALSE)
print(IV$Tables$No_of_months_in_current_residence, row.names=FALSE)
print(IV$Tables$No_of_months_in_current_company, row.names=FALSE)

#Ploting IV values for Demographic Variables
plot_infotables(IV, "Education")
plot_infotables(IV, "Income")
plot_infotables(IV, "Age")
plot_infotables(IV, "Gender")
plot_infotables(IV, "Marital_Status")
plot_infotables(IV, "No_of_dependents")
plot_infotables(IV, "Profession")
plot_infotables(IV, "Type_of_Residence")
plot_infotables(IV, "No_of_months_in_current_residence")
plot_infotables(IV, "No_of_months_in_current_company")
str(Demographic_Data_New)

#Creating New WOE variables for Demographic dataset
outiv <- iv.mult(Demographic_Data_New,"Performance.Tag",vars=c("BinningAge","Profession","Education","Gender","Marital_Status","Modified_No_of_dependents","BinningIncome","Type_of_Residence","BinningCurentResidence","BinningCurrentCompany"))
Demographic_Data_WOE_Updated <- iv.replace.woe(Demographic_Data_New,outiv)
Demographic_Data_Categorical <-  Demographic_Data_WOE_Updated[,c(1,14,16:24)]
str(Demographic_Data_Categorical)

#Creating Dummy variables from categorical data frame
dummy_Demographic_Data <- dummy.data.frame(Demographic_Data_Categorical)

#Merging the dummy variables to existing data frame
Demographic_Data_Final <- merge(x=Demographic_Data_WOE_Updated[,c(1,12:13,15,25:34)],y=dummy_Demographic_Data,by = "Application.ID",all = F)
Demographic_Data_Final$Performance.Tag <- as.factor(ifelse(Demographic_Data_Final$Performance.Tag == 1, "Yes", "No"))
Demographic_Data_Final

######################## WOE Calcualtion for Rejected Applications Dataframe ##################
Demographic_Data_Rejected$Performance.Tag[is.na(Demographic_Data_Rejected$Performance.Tag)] <- 0
Demographic_Data_Rejected$Performance.Tag <- ifelse(Demographic_Data_Rejected$Gender == "M",0,1)
Demographic_Data_Rejected$Performance.Tag<- as.factor(Demographic_Data_Rejected$Performance.Tag)
summary(Demographic_Data_Rejected$Performance.Tag)

rownames(Demographic_Data_Rejected) <- NULL
outiv_Rejected <- iv.mult(Demographic_Data_Rejected,"Performance.Tag",vars=c("BinningAge","Profession","Education","Gender","Marital_Status","Modified_No_of_dependents","BinningIncome","Type_of_Residence","BinningCurentResidence","BinningCurrentCompany"))
Demographic_Data_Rejected_Updated <- iv.replace.woe(Demographic_Data_Rejected,outiv_Rejected)

#Creating separate dataframe of categorical variables for Rejected applicants
Demographic_Data_Rejected_Categorical <-  Demographic_Data_Rejected_Updated[,c(1,14,16:24)]
str(Demographic_Data_Rejected_Categorical)
Demographic_Data_Rejected_Categorical$Modified_No_of_dependents <- as.factor(Demographic_Data_Rejected_Categorical$Modified_No_of_dependents)

#Creating Dummy variables from categorical data frame for rejected applicants
dummy_Demographic_Data_Rejected <- dummy.data.frame(Demographic_Data_Rejected_Categorical)

#Merging the dummy variables to existing data frame
Demographic_Data_Rejected_Final <- merge(x=Demographic_Data_Rejected_Updated[,c(1,12:13,15,25:34)],y=dummy_Demographic_Data_Rejected,by = "Application.ID",all = F)

#########################################################################################
#                   Building Logistic Regression model For Demographic Data             #
#########################################################################################
set.seed(100)
split_indices <- sample.split(Demographic_Data_Final$Performance.Tag, SplitRatio = 0.70)

#Dividing data into Train & Test
train <- Demographic_Data_Final[split_indices, ]
test <- Demographic_Data_Final[!split_indices, ]
nrow(train)/nrow(Demographic_Data_Final)
nrow(test)/nrow(Demographic_Data_Final)

####Model 1: Logistic Regression
Model_1 <- glm(Performance.Tag ~ ., family = "binomial", data = train[,-1])
summary(Model_1)

#Using stepwise algorithm for removing insignificant variables 
#Model_2 <- stepAIC(Model_1, direction = "both")
#summary(Model_2)
#vif(Model_2)

#Removing Type_of_Residence_woe variable
Model_3 <- glm(formula = Performance.Tag ~ Modified_Income + BinningAge_woe + 
                 Profession_woe + Modified_No_of_dependents_woe  + 
                 BinningCurentResidence_woe + BinningCurrentCompany_woe + 
                 `BinningIncome(10,20]` + `BinningIncome(30,40]`, family = "binomial", 
               data = train[, -1])

summary(Model_3)
vif(Model_3)

#Removing Binning Income(30,40] variable
Model_4 <- glm(formula = Performance.Tag ~ Modified_Income + BinningAge_woe + 
                 Profession_woe + Modified_No_of_dependents_woe  + 
                 BinningCurentResidence_woe + BinningCurrentCompany_woe + 
                 `BinningIncome(10,20]`, family = "binomial", 
               data = train[, -1])
summary(Model_4)
vif(Model_4)

#Removing BinningAge_woe variable
Model_5 <- glm(formula = Performance.Tag ~ Modified_Income +  
                 Profession_woe + Modified_No_of_dependents_woe  + 
                 BinningCurentResidence_woe + BinningCurrentCompany_woe + 
                 `BinningIncome(10,20]`, family = "binomial", 
               data = train[, -1])

summary(Model_5)
vif(Model_5)

#Removing BinningIncome(10,20] variable
Model_6 <- glm(formula = Performance.Tag ~ Modified_Income +  
                 Profession_woe + Modified_No_of_dependents_woe  + 
                 BinningCurentResidence_woe + BinningCurrentCompany_woe,
                  family = "binomial", 
data = train[, -1])

summary(Model_6)
vif(Model_6)

#Removing Profession_woe variable
Model_7 <- glm(formula = Performance.Tag ~ Modified_Income +
                 BinningCurentResidence_woe + BinningCurrentCompany_woe,
               family = "binomial", 
               data = train[, -1])

summary(Model_7)
vif(Model_7)

#So treating Model_7 as final Model

###########################################################################
#                   Model Evaluation on test data                         #
###########################################################################
#Predicting probabilities of responding for the test data
predictions_logit <- predict(Model_7, newdata = test[,-c( 1,2)], type = "response")
summary(predictions_logit)
predictions_logit

#-------------------------------------------------------------------------------------
##------------------------- Model Evaluation - Logistic Regression--------------------
#Let's use the probability cutoff
predicted_response <- factor(ifelse(predictions_logit >= 0.05, "Yes", "No"))
predicted_response
summary(predicted_response)

# Creating confusion matrix for identifying the model evaluation.
conf <- confusionMatrix(predicted_response, test$Performance.Tag, positive = "Yes")
conf

#---------------------------------------------------------    
# Let's find out the optimal probalility cutoff 
perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_response, test$Performance.Tag, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    
# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.
s = seq(.01,.09,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 
str(test$Performance.Tag)
str(predicted_response)

#---------------------------------------------------------    
# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

#---------------------------------------------------------    
cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.03)]
cutoff

# Let's choose a cutoff value of 12% for final model
predicted_response <- factor(ifelse(predictions_logit >= 0.04232323, "Yes", "No"))
predicted_response

conf_final <- confusionMatrix(predicted_response, test$Performance.Tag, positive = "Yes")

acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

acc   #0.573187
sens  #0.5644796
spec  #0.5735704


#########################################################################################
#                                     Credit Bureau Data Set                               #
#########################################################################################
str(Credit_Bureau_Data)
#Checking Unique values
length(unique(Credit_Bureau_Data$Application.ID))

sum(is.na(Credit_Bureau_Data$Performance.Tag))  #1425 NA values

#Creating a dataframe from the demographic data which have a value for the Performance tag
Credit_Bureau_Data_New <- subset(Credit_Bureau_Data,(Credit_Bureau_Data$Performance.Tag=='0' | Credit_Bureau_Data$Performance.Tag=='1'))

#Extracting Rejected candidates from Credit Bureau dataframe
Credit_Bureau_Data_Rejected <- Credit_Bureau_Data[is.na(Credit_Bureau_Data$Performance.Tag),]

#Find the rows containing the duplicate application id
Credit_Bureau_Data_New[duplicated(Credit_Bureau_Data_New$Application.ID),]

# 3 duplicate application ids.
#Creating a dataframe enlisting the duplicate application ids.
new.data1 <- Credit_Bureau_Data_New[ which( Credit_Bureau_Data_New$Application.ID == '671989187' | Credit_Bureau_Data_New$Application.ID == '765011468' | Credit_Bureau_Data_New$Application.ID == '653287861' ) , ]

#Removing the rows with duplicate application id having performance tag= 0
Credit_Bureau_Data_New <- Credit_Bureau_Data_New[-c(5244, 24387, 48603), ]

length(unique(Credit_Bureau_Data_New$Application.ID))  #69867 rows

# *******************************************************************************************
plot_response <- function(cat_var, var_name){
  a <- aggregate(Performance.Tag~cat_var, Credit_Bureau_Data_New, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_response <- cbind(a, count)
  
  colnames(agg_response) <- c(var_name, "response_rate","No.of_Prospect")
  agg_response[, 2] <- format(round(agg_response[, 2], 4))
  
  ggplot(agg_response, aes(agg_response[, 1], count, label = response_rate)) + geom_bar(stat = 'identity', color = "black",fill="green") + 
    theme(axis.text.x = element_text(angle = 0, hjust = 1)) + geom_text(size = 3, vjust = -0.5) + xlab(var_name)
  
}

#######################################################################
#         Deriving Insights of Type of No.of.times.90DPD Variable     #
#######################################################################

#Checking NA values in No.of.times.90DPD Variable
sum(is.na(Credit_Bureau_Data_New$No_of_times_90_DPD_or_worse_in_last_6_months))  #No NA values are present

#Converting into factor Variable
Credit_Bureau_Data_New$No_of_times_90_DPD_or_worse_in_last_6_months  <- as.factor(Credit_Bureau_Data_New$No_of_times_90_DPD_or_worse_in_last_6_months)

#Plotting to derive insights from No_of_times_90_DPD_or_worse_in_last_6_months Variable
ggplot(Credit_Bureau_Data_New,aes(x=No_of_times_90_DPD_or_worse_in_last_6_months)) +geom_bar(color="black",fill="lightgreen")

agg_90DPD_last_6Month <- merge(aggregate(Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months, Credit_Bureau_Data_New, mean),aggregate(Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months,
                                                                                                                                                 Credit_Bureau_Data_New, sum),by = "No_of_times_90_DPD_or_worse_in_last_6_months") 
# Adding No.of_prospect
count10 <- data.frame(table(Credit_Bureau_Data_New$No_of_times_90_DPD_or_worse_in_last_6_months))
count10 <- count10[,-1]
agg_90DPD_last_6Month <- cbind(agg_90DPD_last_6Month,count10)
colnames(agg_90DPD_last_6Month) <- c("No.of.times.90.DPD", "Default_rate", "count_prospects","No.of_prospect")

# Round Off the values
agg_90DPD_last_6Month$Default_rate <- format(round(agg_90DPD_last_6Month$Default_rate, 4))
agg_90DPD_last_6Month

# Let's see the Default rate of each bucket in the plot
ggplot(agg_90DPD_last_6Month, aes(No.of.times.90.DPD,count_prospects,label = Default_rate)) + 
  geom_bar(stat = 'identity', color = "black",fill="green") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)+ xlab("No.of.times.90.DPD") +
  ylab("Number of customers in each Category")

#########################################################################################
#                 Deriving Insights of Type of No.of.times.60DPD Variable               #
#########################################################################################
#Checking NA values in No.of.times.60DPD Variable
sum(is.na(Credit_Bureau_Data_New$No_of_times_60_DPD_or_worse_in_last_6_months))  #No NA values are present

#Converting into factor Variable
Credit_Bureau_Data_New$No_of_times_60_DPD_or_worse_in_last_6_months  <- as.factor(Credit_Bureau_Data_New$No_of_times_60_DPD_or_worse_in_last_6_months)

#Plotting to derive insights from No_of_times_60_DPD_or_worse_in_last_6_months Variable
ggplot(Credit_Bureau_Data_New,aes(x=No_of_times_60_DPD_or_worse_in_last_6_months)) +geom_bar(color="black",fill="lightgreen")

agg_60DPD_last_6Month <- merge(aggregate(Performance.Tag ~ No_of_times_60_DPD_or_worse_in_last_6_months, Credit_Bureau_Data_New, mean),aggregate(Performance.Tag ~ No_of_times_60_DPD_or_worse_in_last_6_months,
                                                                                                                                                 Credit_Bureau_Data_New, sum),by = "No_of_times_60_DPD_or_worse_in_last_6_months") 
# Adding No.of_prospect
count11 <- data.frame(table(Credit_Bureau_Data_New$No_of_times_60_DPD_or_worse_in_last_6_months))
count11 <- count11[,-1]
agg_60DPD_last_6Month <- cbind(agg_60DPD_last_6Month,count11)
colnames(agg_60DPD_last_6Month) <- c("No.of.times.60.DPD", "Default_rate", "count_prospects","No.of_prospect")

# Round Off the values
agg_60DPD_last_6Month$Default_rate <- format(round(agg_60DPD_last_6Month$Default_rate, 4))
agg_60DPD_last_6Month

# Let's see the Default rate of each bucket in the plot
ggplot(agg_60DPD_last_6Month, aes(No.of.times.60.DPD,count_prospects,label = Default_rate)) + 
  geom_bar(stat = 'identity', color = "black",fill="green") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)+ xlab("No.of.times.60.DPD") +
  ylab("Number of customers in each Category")

#########################################################################################
#                 Deriving Insights of Type of No.of.times.30DPD Variable               #
#########################################################################################
#Checking NA values in No.of.times.30DPD Variable
sum(is.na(Credit_Bureau_Data_New$No_of_times_30_DPD_or_worse_in_last_6_months))  #No NA values are present

#Converting into factor Variable
Credit_Bureau_Data_New$No_of_times_30_DPD_or_worse_in_last_6_months  <- as.factor(Credit_Bureau_Data_New$No_of_times_30_DPD_or_worse_in_last_6_months)

#Plotting to derive insights from No_of_times_30_DPD_or_worse_in_last_6_months Variable
ggplot(Credit_Bureau_Data_New,aes(x=No_of_times_30_DPD_or_worse_in_last_6_months)) +geom_bar(color="black",fill="lightgreen")

agg_30DPD_last_6Month <- merge(aggregate(Performance.Tag ~ No_of_times_30_DPD_or_worse_in_last_6_months, Credit_Bureau_Data_New, mean),aggregate(Performance.Tag ~ No_of_times_30_DPD_or_worse_in_last_6_months,
                                                                                                                                                 Credit_Bureau_Data_New, sum),by = "No_of_times_30_DPD_or_worse_in_last_6_months") 
# Adding No.of_prospect
count12 <- data.frame(table(Credit_Bureau_Data_New$No_of_times_30_DPD_or_worse_in_last_6_months))
count12 <- count12[,-1]
agg_30DPD_last_6Month <- cbind(agg_30DPD_last_6Month,count12)
colnames(agg_30DPD_last_6Month) <- c("No.of.times.30.DPD", "Default_rate", "count_prospects","No.of_prospect")

# Round Off the values
agg_30DPD_last_6Month$Default_rate <- format(round(agg_30DPD_last_6Month$Default_rate, 4))
agg_30DPD_last_6Month

# Let's see the Default rate of each bucket in the plot
ggplot(agg_30DPD_last_6Month, aes(No.of.times.30.DPD,count_prospects,label = Default_rate)) + 
  geom_bar(stat = 'identity', color = "black",fill="green") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)+ xlab("No.of.times.30.DPD") +
  ylab("Number of customers in each Category")  


#########################################################################################
##Deriving Insights of Type of No.of.times.90DPD.or.worse.in.last.12.months Variable###
#########################################################################################
#Checking NA values in No.of.times.90DPD.or.worse.in.last.12.months Variable
sum(is.na(Credit_Bureau_Data_New$No_of_times_90_DPD_or_worse_in_last_12_months))  #No NA values are present

#Converting into factor Variable
Credit_Bureau_Data_New$No_of_times_90_DPD_or_worse_in_last_12_months  <- as.factor(Credit_Bureau_Data_New$No_of_times_90_DPD_or_worse_in_last_12_months)

ggplot(Credit_Bureau_Data_New,aes(x=No_of_times_90_DPD_or_worse_in_last_12_months)) +geom_bar(color="black",fill="lightgreen")

agg_90DPD_last_12Month <- merge(aggregate(Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_12_months, Credit_Bureau_Data_New, mean),aggregate(Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_12_months,
                                                                                                                                                   Credit_Bureau_Data_New, sum),by = "No_of_times_90_DPD_or_worse_in_last_12_months") 
# Adding No.of_prospect
count13 <- data.frame(table(Credit_Bureau_Data_New$No_of_times_90_DPD_or_worse_in_last_12_months))
count13 <- count13[,-1]
agg_90DPD_last_12Month <- cbind(agg_90DPD_last_12Month,count13)
colnames(agg_90DPD_last_12Month) <- c("No.of.times.90.DPD.last12Month", "Default_rate", "count_prospects","No.of_prospect")

# Round Off the values
agg_90DPD_last_12Month$Default_rate <- format(round(agg_90DPD_last_12Month$Default_rate, 4))
agg_90DPD_last_12Month

# Let's see the Default rate of each bucket in the plot
ggplot(agg_90DPD_last_12Month, aes(No.of.times.90.DPD.last12Month,count_prospects,label = Default_rate)) + 
  geom_bar(stat = 'identity', color = "black",fill="green") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)+ xlab("No.of.times.90.DPD.last12Month") +
  ylab("Number of customers in each Category") 

# #########################################################################################
# Next, Lets have a look at the following columns. 
# If there are any missing values then we need to process them accordingly. 
# Avg. Credit Card Utilization has 1058 Na values. 
#########################################################################################

sum(is.na(Credit_Bureau_Data_New$No_of_times_60_DPD_or_worse_in_last_12_months)) #0
sum(is.na(Credit_Bureau_Data_New$No_of_times_30_DPD_or_worse_in_last_12_months)) #0 
sum(is.na(Credit_Bureau_Data_New$Avgas_CC_Utilization_in_last_12_months)) #1058
sum(is.na(Credit_Bureau_Data_New$No_of_trades_opened_in_last_6_months)) #1
sum(is.na(Credit_Bureau_Data_New$No_of_trades_opened_in_last_12_months)) #0
sum(is.na(Credit_Bureau_Data_New$No_of_PL_trades_opened_in_last_6_months)) #0
sum(is.na(Credit_Bureau_Data_New$No_of_PL_trades_opened_in_last_6_months)) #0

#########################################################################################
#Converting into factor Variable
# No_of_times_60_DPD_or_worse_in_last_12_months 
#########################################################################################
Credit_Bureau_Data_New$No_of_times_60_DPD_or_worse_in_last_12_months  <- as.factor(Credit_Bureau_Data_New$No_of_times_60_DPD_or_worse_in_last_12_months)
summary(Credit_Bureau_Data_New$No_of_times_60_DPD_or_worse_in_last_12_months)

#Plotting to derive insights from No_of_times_60_DPD_or_worse_in_last_12_months Variable
ggplot(Credit_Bureau_Data_New,aes(x=No_of_times_60_DPD_or_worse_in_last_12_months)) +geom_bar(color="black",fill="lightgreen")

agg_60DPD_last_12Month <- merge(aggregate(Performance.Tag ~ No_of_times_60_DPD_or_worse_in_last_12_months, Credit_Bureau_Data_New, mean),aggregate(Performance.Tag ~ No_of_times_60_DPD_or_worse_in_last_12_months,
                                                                                                                                                   Credit_Bureau_Data_New, sum),by = "No_of_times_60_DPD_or_worse_in_last_12_months")
# Adding No.of_prospect
count14 <- data.frame(table(Credit_Bureau_Data_New$No_of_times_60_DPD_or_worse_in_last_12_months))
count14 <- count14[,-1]
agg_60DPD_last_12Month <- cbind(agg_60DPD_last_12Month,count14)
colnames(agg_60DPD_last_12Month) <- c("No.of.times.60.DPD.last12Month", "Default_rate", "count_prospects","No.of_prospect")

# Round Off the values
agg_60DPD_last_12Month$Default_rate <- format(round(agg_60DPD_last_12Month$Default_rate, 4))
agg_60DPD_last_12Month

plot_response(Credit_Bureau_Data_New$No_of_times_60_DPD_or_worse_in_last_12_months, "No_of_times_60_DPD_or_worse_in_last_12_months")

#########################################################################################
# Handling of No_of_times_30_DPD_or_worse_in_last_12_months
#########################################################################################
Credit_Bureau_Data_New$No_of_times_30_DPD_or_worse_in_last_12_months  <- as.factor(Credit_Bureau_Data_New$No_of_times_30_DPD_or_worse_in_last_12_months)

#Plotting to derive insights from No_of_times_60_DPD_or_worse_in_last_12_months Variable
ggplot(Credit_Bureau_Data_New,aes(x=No_of_times_30_DPD_or_worse_in_last_12_months)) +geom_bar(color="black",fill="lightgreen")

agg_30DPD_last_12Month <- merge(aggregate(Performance.Tag ~ No_of_times_30_DPD_or_worse_in_last_12_months, Credit_Bureau_Data_New, mean),aggregate(Performance.Tag ~ No_of_times_30_DPD_or_worse_in_last_12_months,
                                                                                                                                                   Credit_Bureau_Data_New, sum),by = "No_of_times_30_DPD_or_worse_in_last_12_months")
# Adding No.of_prospect
count15 <- data.frame(table(Credit_Bureau_Data_New$No_of_times_30_DPD_or_worse_in_last_12_months))
count15 <- count15[,-1]
agg_30DPD_last_12Month <- cbind(agg_30DPD_last_12Month,count15)
colnames(agg_30DPD_last_12Month) <- c("No.of.times.30.DPD.last12Month", "Default_rate", "count_prospects","No.of_prospect")

# Round Off the values
agg_30DPD_last_12Month$Default_rate <- format(round(agg_30DPD_last_12Month$Default_rate, 4))
agg_30DPD_last_12Month

plot_response(Credit_Bureau_Data_New$No_of_times_30_DPD_or_worse_in_last_12_months, "No_of_times_30_DPD_or_worse_in_last_12_months")

#########################################################################################
# aVG cREDIT cARD Utilization in the last 12 months. 
# Here we have 751 Na values, which needs to be handled. 
#########################################################################################
# Avgas_CC_Utilization_in_last_12_months
sum(is.na(Credit_Bureau_Data_New$Avgas_CC_Utilization_in_last_12_months))  #1023

Credit_Bureau_Data_New$Modified_CC_Utilization <- ifelse(is.na(Credit_Bureau_Data_New$Avgas_CC_Utilization_in_last_12_months)
                                                         , round(median(Credit_Bureau_Data_New$Avgas_CC_Utilization_in_last_12_months, na.rm = T),1),
                                                         Credit_Bureau_Data_New$Avgas_CC_Utilization_in_last_12_months)

#Handling NA values for Credit card utilization for Rejected applicants
Credit_Bureau_Data_Rejected$Modified_CC_Utilization <- ifelse(is.na(Credit_Bureau_Data_Rejected$Avgas_CC_Utilization_in_last_12_months)
                                                         , round(median(Credit_Bureau_Data_Rejected$Avgas_CC_Utilization_in_last_12_months, na.rm = T),1),
                                                         Credit_Bureau_Data_Rejected$Avgas_CC_Utilization_in_last_12_months)

#Binning of values for Modified CC ultilization
Credit_Bureau_Data_New$Binned_CC_Utilization <- as.factor(cut(Credit_Bureau_Data_New$Modified_CC_Utilization, breaks = c(0,10, 20, 30, 40, 50, 60,70,80,90,100,110,120),include.lowest = TRUE))

#Binning of values for Modified CC ultilization
Credit_Bureau_Data_Rejected$Binned_CC_Utilization <- as.factor(cut(Credit_Bureau_Data_Rejected$Modified_CC_Utilization, breaks = c(0,10, 20, 30, 40, 50, 60,70,80,90,100,110,120),include.lowest = TRUE))

ggplot(Credit_Bureau_Data_New,aes(x=Modified_CC_Utilization)) +geom_bar(color="black",fill="lightgreen")

Agg_CC_utilization <- merge(aggregate(Performance.Tag ~ Binned_CC_Utilization, Credit_Bureau_Data_New, mean),aggregate(Performance.Tag ~ Binned_CC_Utilization,
                                                                                                                  Credit_Bureau_Data_New, sum),by = "Binned_CC_Utilization")
count16 <- data.frame(table(Credit_Bureau_Data_New$Binned_CC_Utilization))
count16 <- count16[,-1]
Agg_CC_utilization <- cbind(Agg_CC_utilization,count16)
colnames(Agg_CC_utilization) <- c("Agg_CC_util", "Default_rate", "count_prospects","No.of_prospect")

# Round Off the values
Agg_CC_utilization$Default_rate <- format(round(Agg_CC_utilization$Default_rate, 4))
Agg_CC_utilization

plot_response(Credit_Bureau_Data_New$Binned_CC_Utilization, "Binned_CC_Utilization")

#########################################################################################
# No_of_trades_opened_in_last_6_months
#########################################################################################
sum(is.na(Credit_Bureau_Data_New$No_of_trades_opened_in_last_6_months)) # 1 NA value is present. 

Credit_Bureau_Data_New$Modified_No_of_trades_opened_in_last_6_months <- ifelse(is.na(Credit_Bureau_Data_New$No_of_trades_opened_in_last_6_months),round(
  mean(Credit_Bureau_Data_New$No_of_trades_opened_in_last_6_months,na.rm = T),0),Credit_Bureau_Data_New$No_of_trades_opened_in_last_6_months)

sum(is.na(Credit_Bureau_Data_New$Modified_No_of_trades_opened_in_last_6_months))

#Changing the datatype to Numeric
Credit_Bureau_Data_New$Modified_No_of_trades_opened_in_last_6_months <- as.numeric(Credit_Bureau_Data_New$Modified_No_of_trades_opened_in_last_6_months)

#Creating bins for Number of trades opened in last 6 months variable
Credit_Bureau_Data_New$Binned_No_of_trades_opened_in_last_6_months <- as.factor(cut(Credit_Bureau_Data_New$Modified_No_of_trades_opened_in_last_6_months, breaks = c(-1,2,5,14)))

#Creating bins for Number of trades opened in last 6 months variable for rejected applications
Credit_Bureau_Data_Rejected$Binned_No_of_trades_opened_in_last_6_months <- as.factor(cut(Credit_Bureau_Data_Rejected$No_of_trades_opened_in_last_6_months, breaks = c(-1,2,5,14)))

ggplot(Credit_Bureau_Data_New,aes(x=Binned_No_of_trades_opened_in_last_6_months)) +geom_bar(color="black",fill="lightgreen")


agg_Trades_last_6Month <- merge(aggregate(Performance.Tag ~ Binned_No_of_trades_opened_in_last_6_months, Credit_Bureau_Data_New, mean),aggregate(Performance.Tag ~ Binned_No_of_trades_opened_in_last_6_months,
                                                                                                                                                      Credit_Bureau_Data_New, sum),by = "Binned_No_of_trades_opened_in_last_6_months")
# Adding No.of_prospect
count17 <- data.frame(table(Credit_Bureau_Data_New$Binned_No_of_trades_opened_in_last_6_months))
count17 <- count17[,-1]
agg_Trades_last_6Month <- cbind(agg_Trades_last_6Month,count17)
colnames(agg_Trades_last_6Month) <- c("agg_Trades_last_6Month", "Default_rate", "count_prospects","No.of_prospect")

# Round Off the values
agg_Trades_last_6Month$Default_rate <- format(round(agg_Trades_last_6Month$Default_rate, 4))
agg_Trades_last_6Month

plot_response(Credit_Bureau_Data_New$Binned_No_of_trades_opened_in_last_6_months, "Binned_No_of_trades_opened_in_last_6_months")

#########################################################################################
# No_of_trades_opened_in_last_12_months
#########################################################################################
sum(is.na(Credit_Bureau_Data_New$No_of_trades_opened_in_last_12_months))

Credit_Bureau_Data_New$No_of_trades_opened_in_last_12_months  <- as.factor(Credit_Bureau_Data_New$No_of_trades_opened_in_last_12_months)

ggplot(Credit_Bureau_Data_New,aes(x=No_of_trades_opened_in_last_12_months)) +geom_bar(color="black",fill="lightgreen")

#########################################################################################
# There are a lot of levels for no. of trades opened in last 12 months. 
# We are binning those levels in a new column called categorized_No_of_trades_opened_in_last_12_months
#########################################################################################

Credit_Bureau_Data_New$No_of_trades_opened_in_last_12_months <- as.numeric(Credit_Bureau_Data_New$No_of_trades_opened_in_last_12_months)
Credit_Bureau_Data_New$Binned_No_of_trades_opened_in_last_12_months <- as.factor(cut(Credit_Bureau_Data_New$No_of_trades_opened_in_last_12_months, breaks = c(-1,4,9,14,19,29)))

#Creating bins for Number of trades opened in last 12 months variable.
Credit_Bureau_Data_Rejected$Binned_No_of_trades_opened_in_last_12_months <- as.factor(cut(Credit_Bureau_Data_Rejected$No_of_trades_opened_in_last_12_months, breaks = c(-1,4,9,14,19,29)))

levels(Credit_Bureau_Data_New$Binned_No_of_trades_opened_in_last_12_months)

ggplot(Credit_Bureau_Data_New,aes(x=Binned_No_of_trades_opened_in_last_12_months)) +geom_bar(color="black",fill="lightgreen")

agg_Trades_last_12Month <- merge(aggregate(Performance.Tag ~ Binned_No_of_trades_opened_in_last_12_months, Credit_Bureau_Data_New, mean),aggregate(Performance.Tag ~ Binned_No_of_trades_opened_in_last_12_months,
                                                                                                                                                 Credit_Bureau_Data_New, sum),by = "Binned_No_of_trades_opened_in_last_12_months")
# Adding No.of_prospect
count18 <- data.frame(table(Credit_Bureau_Data_New$Binned_No_of_trades_opened_in_last_12_months))
count18 <- count18[,-1]
agg_Trades_last_12Month <- cbind(agg_Trades_last_12Month,count18)
colnames(agg_Trades_last_12Month) <- c("agg_Trades_last_12Month", "Default_rate", "count_prospects","No.of_prospect")

# Round Off the values
agg_Trades_last_12Month$Default_rate <- format(round(agg_Trades_last_12Month$Default_rate, 4))
agg_Trades_last_12Month

plot_response(Credit_Bureau_Data_New$Binned_No_of_trades_opened_in_last_12_months, "Binned_No_of_trades_opened_in_last_12_months")

#########################################################################################
# No_of_PL_trades_opened_in_last_6_months. There are no NA values in this column either. 
#########################################################################################
Credit_Bureau_Data_New$No_of_PL_trades_opened_in_last_6_months  <- as.factor(Credit_Bureau_Data_New$No_of_PL_trades_opened_in_last_6_months)
summary(Credit_Bureau_Data_New$No_of_PL_trades_opened_in_last_6_months)

#Plotting to derive insights from No.of.times.60.DPD.or.worse.in.last.12.months Variable
ggplot(Credit_Bureau_Data_New,aes(x=No_of_PL_trades_opened_in_last_6_months)) +geom_bar(color="black",fill="lightgreen")

agg_PLTrades_last_6Month <- merge(aggregate(Performance.Tag ~ No_of_PL_trades_opened_in_last_6_months, Credit_Bureau_Data_New, mean),aggregate(Performance.Tag ~ No_of_PL_trades_opened_in_last_6_months,
                                                                                                                                               Credit_Bureau_Data_New, sum),by = "No_of_PL_trades_opened_in_last_6_months")
count19 <- data.frame(table(Credit_Bureau_Data_New$No_of_PL_trades_opened_in_last_6_months))
count19 <- count19[,-1]
agg_PLTrades_last_6Month <- cbind(agg_PLTrades_last_6Month,count19)
colnames(agg_PLTrades_last_6Month) <- c("agg_PLTrades_last_6Month", "Default_rate", "count_prospects","No.of_prospect")

# Round Off the values
agg_PLTrades_last_6Month$Default_rate <- format(round(agg_PLTrades_last_6Month$Default_rate, 4))
agg_PLTrades_last_6Month

plot_response(Credit_Bureau_Data_New$No_of_PL_trades_opened_in_last_6_months, "No_of_PL_trades_opened_in_last_6_months")

#########################################################################################
# No_of_PL_trades_opened_in_last_12_months. There are no NA values in this column. 
#########################################################################################
sum(is.na(Credit_Bureau_Data_New$No_of_PL_trades_opened_in_last_12_months))

#Credit_Bureau_Data_New$No_of_PL_trades_opened_in_last_12_months <- as.numeric(Credit_Bureau_Data_New$No_of_trades_opened_in_last_12_months)
Credit_Bureau_Data_New$Binned_No_of_PL_trades_opened_in_last_12_months <- as.factor(cut(Credit_Bureau_Data_New$No_of_PL_trades_opened_in_last_12_months, breaks = c(-1,2,6,12)))

ggplot(Credit_Bureau_Data_New,aes(x=Binned_No_of_PL_trades_opened_in_last_12_months)) +geom_bar(color="black",fill="lightgreen")

agg_PL_Trades_last_12Month <- merge(aggregate(Performance.Tag ~ Binned_No_of_PL_trades_opened_in_last_12_months, Credit_Bureau_Data_New, mean),aggregate(Performance.Tag ~ Binned_No_of_PL_trades_opened_in_last_12_months,
                                                                                                                                                   Credit_Bureau_Data_New, sum),by = "Binned_No_of_PL_trades_opened_in_last_12_months")
# Adding No.of_prospect
count20 <- data.frame(table(Credit_Bureau_Data_New$Binned_No_of_PL_trades_opened_in_last_12_months))
count20 <- count20[,-1]
agg_PL_Trades_last_12Month <- cbind(agg_PL_Trades_last_12Month,count20)
colnames(agg_PL_Trades_last_12Month) <- c("agg_PL_Trades_last_12Month", "Default_rate", "count_prospects","No.of_prospect")

# Round Off the values
agg_PL_Trades_last_12Month$Default_rate <- format(round(agg_PL_Trades_last_12Month$Default_rate, 4))
agg_PL_Trades_last_12Month

plot_response(Credit_Bureau_Data_New$Binned_No_of_PL_trades_opened_in_last_12_months, "Binned_No_of_PL_trades_opened_in_last_12_months")

#################################################################################################
# Deriving insights from No_of_Enquiries_in_last_6_months. 
#################################################################################################

#Credit_Bureau_Data_New$No_of_Inquiries_in_last_6_months_excluding_home_auto_loans<- as.factor(Credit_Bureau_Data_New$No_of_Inquiries_in_last_6_months_excluding_home_auto_loans)
Credit_Bureau_Data_New$No_of_Inquiries_in_last_6_months_excluding_home_auto_loans <- as.numeric(Credit_Bureau_Data_New$No_of_Inquiries_in_last_6_months_excluding_home_auto_loans)
Credit_Bureau_Data_New$Binned_No_of_Inquiries_in_last_6_months <- as.factor(cut(Credit_Bureau_Data_New$No_of_Inquiries_in_last_6_months_excluding_home_auto_loans, breaks = c(-1,2,5,10)))

#Creating binsfr om No_of_Enquiries_in_last_6_months for rejected applications 
Credit_Bureau_Data_Rejected$Binned_No_of_Inquiries_in_last_6_months <- as.factor(cut(Credit_Bureau_Data_Rejected$No_of_Inquiries_in_last_6_months_excluding_home_auto_loans, breaks = c(-1,2,5,10)))

Credit_Bureau_Data_New$No_of_Inquiries_in_last_6_months_excluding_home_auto_loans<- as.factor(Credit_Bureau_Data_New$No_of_Inquiries_in_last_6_months_excluding_home_auto_loans)
ggplot(Credit_Bureau_Data_New,aes(x=No_of_Inquiries_in_last_6_months_excluding_home_auto_loans)) +geom_bar(color="black",fill="lightgreen")

Inquiries_last_6Month <- merge(aggregate(Performance.Tag ~ Binned_No_of_Inquiries_in_last_6_months, Credit_Bureau_Data_New, mean),aggregate(Performance.Tag ~ Binned_No_of_Inquiries_in_last_6_months,
                                                                                                                                     Credit_Bureau_Data_New, sum),by = "Binned_No_of_Inquiries_in_last_6_months")
# Adding No.of_prospect
count21 <- data.frame(table(Credit_Bureau_Data_New$Binned_No_of_Inquiries_in_last_6_months))
count21 <- count21[,-1]
Inquiries_last_6Month <- cbind(Inquiries_last_6Month,count21)
Inquiries_last_6Month
colnames(Inquiries_last_6Month) <- c("Inquiries_last_6_months", "Default_rate", "count_prospects","No.of_prospect")

ggplot(Credit_Bureau_Data_New,aes(x=Binned_No_of_Inquiries_in_last_6_months)) +geom_bar(color="black",fill="lightgreen")
# Round Off the values
Inquiries_last_6Month$Default_rate <- format(round(Inquiries_last_6Month$Default_rate, 4))
Inquiries_last_6Month

plot_response(Credit_Bureau_Data_New$Binned_No_of_Inquiries_in_last_6_months, "Binned_No_of_Inquiries_in_last_6_months")

#################################################################################################
# Deriving insights from No_of_Enquiries_in_last_12_months. 
#################################################################################################

#Credit_Bureau_Data_New$No_of_Inquiries_in_last_6_months_excluding_home_auto_loans<- as.factor(Credit_Bureau_Data_New$No_of_Inquiries_in_last_6_months_excluding_home_auto_loans)
#summary(Credit_Bureau_Data_New$No_of_Inquiries_in_last_12_months_excluding_home_auto_loans)
#Credit_Bureau_Data_New$No_of_Inquiries_in_last_12_months_excluding_home_auto_loans <- as.numeric(Credit_Bureau_Data_New$No_of_Inquiries_in_last_12_months_excluding_home_auto_loans)

Credit_Bureau_Data_New$Binned_No_of_Inquiries_in_last_12_months <- as.factor(cut(Credit_Bureau_Data_New$No_of_Inquiries_in_last_12_months_excluding_home_auto_loans, breaks = c(-1,4,8,12,16,20)))

#Creating bins from No_of_Enquiries_in_last_12_months for rejected applications 
Credit_Bureau_Data_Rejected$Binned_No_of_Inquiries_in_last_12_months <- as.factor(cut(Credit_Bureau_Data_Rejected$No_of_Inquiries_in_last_12_months_excluding_home_auto_loans, breaks = c(-1,4,8,12,16,20)))

sum(is.na(Credit_Bureau_Data_New$Binned_No_of_Inquiries_in_last_12_months))

ggplot(Credit_Bureau_Data_New,aes(x=No_of_Inquiries_in_last_12_months_excluding_home_auto_loans)) +geom_bar(color="black",fill="lightgreen")

Inquiries_last_12Month <- merge(aggregate(Performance.Tag ~ Binned_No_of_Inquiries_in_last_12_months, Credit_Bureau_Data_New, mean),aggregate(Performance.Tag ~ Binned_No_of_Inquiries_in_last_12_months,
                                                                                                                                            Credit_Bureau_Data_New, sum),by = "Binned_No_of_Inquiries_in_last_12_months")
# Adding No.of_prospect
count22 <- data.frame(table(Credit_Bureau_Data_New$Binned_No_of_Inquiries_in_last_12_months))
count22 <- count22[,-1]
Inquiries_last_12Month <- cbind(Inquiries_last_12Month,count22)
Inquiries_last_12Month
colnames(Inquiries_last_12Month) <- c("Inquiries_last_12_months", "Default_rate", "count_prospects","No.of_prospect")

ggplot(Credit_Bureau_Data_New,aes(x=Binned_No_of_Inquiries_in_last_12_months)) +geom_bar(color="black",fill="lightgreen")
# Round Off the values
Inquiries_last_12Month$Default_rate <- format(round(Inquiries_last_12Month$Default_rate, 4))
Inquiries_last_12Month

plot_response(Credit_Bureau_Data_New$Binned_No_of_Inquiries_in_last_12_months, "Binned_No_of_Inquiries_in_last_12_months")

#########################################################################################
# Deriving Insights from presence of open home loan variable
#########################################################################################
summary(Credit_Bureau_Data_New$Presence_of_open_home_loan)   # 272 NA value is present. 

Credit_Bureau_Data_New$Modified_Presence_of_open_home_loan <- ifelse(is.na(Credit_Bureau_Data_New$Presence_of_open_home_loan),round(
  mean(Credit_Bureau_Data_New$Presence_of_open_home_loan,na.rm = T),0),Credit_Bureau_Data_New$Presence_of_open_home_loan)

sum(is.na(Credit_Bureau_Data_New$Modified_Presence_of_open_home_loan))

Credit_Bureau_Data_New$Modified_Presence_of_open_home_loan <- as.factor(Credit_Bureau_Data_New$Modified_Presence_of_open_home_loan)

ggplot(Credit_Bureau_Data_New,aes(x=Modified_Presence_of_open_home_loan)) +geom_bar(color="black",fill="lightgreen")

agg_Presence_of_open_home_loan <- merge(aggregate(Performance.Tag ~ Modified_Presence_of_open_home_loan, Credit_Bureau_Data_New, mean),aggregate(Performance.Tag ~ Modified_Presence_of_open_home_loan,
                                                                                                                                                 Credit_Bureau_Data_New, sum),by = "Modified_Presence_of_open_home_loan")
# Adding No.of_prospect
count23 <- data.frame(table(Credit_Bureau_Data_New$Modified_Presence_of_open_home_loan))
count23 <- count23[,-1]
agg_Presence_of_open_home_loan <- cbind(agg_Presence_of_open_home_loan,count23)
agg_Presence_of_open_home_loan

colnames(agg_Presence_of_open_home_loan) <- c("agg_Presence_of_open_home_loan", "Default_rate", "count_prospects","No.of_prospect")

# Round Off the values
agg_Presence_of_open_home_loan$Default_rate <- format(round(agg_Presence_of_open_home_loan$Default_rate, 4))

plot_response(Credit_Bureau_Data_New$Modified_Presence_of_open_home_loan, "Modified_Presence_of_open_home_loan")

#########################################################################################
##Deriving Insights for Outstanding balance Variable###
#########################################################################################
summary(Credit_Bureau_Data_New$Outstanding_Balance)    # 272 NA values

Credit_Bureau_Data_New$Outstanding.Balance <- as.numeric(Credit_Bureau_Data_New$Outstanding.Balance)

Credit_Bureau_Data_New$Modified_Outstanding_Balance <- ifelse(is.na(Credit_Bureau_Data_New$Outstanding_Balance),round(
   median(Credit_Bureau_Data_New$Outstanding_Balance,na.rm = T),0),Credit_Bureau_Data_New$Outstanding_Balance)
 
sum(is.na(Credit_Bureau_Data_New$Modified_Outstanding_Balance))

#Creating Outstanding balance bins
Credit_Bureau_Data_New$Binning_outstanding_Balance <- as.factor(cut(Credit_Bureau_Data_New$Modified_Outstanding_Balance, breaks = c(0, 500000, 1000000, 1500000, 2000000, 2500000,3000000,3500000,4000000,4500000,5000000,5500000),include.lowest = TRUE))

#Creating Outstanding balance bins for rejected applications
 Credit_Bureau_Data_Rejected$Binning_outstanding_Balance <- as.factor(cut(Credit_Bureau_Data_Rejected$Outstanding_Balance, breaks = c(0, 500000, 1000000, 1500000, 2000000, 2500000,3000000,3500000,4000000,4500000,5000000,5500000),include.lowest = TRUE))

#Plotting Modified outstanding balance
ggplot(Credit_Bureau_Data_New,aes(x=Binning_outstanding_Balance)) +geom_bar(color="black",fill="lightgreen")
 
agg_Binning_Outstanding_Balance <- merge(aggregate(Performance.Tag ~ Binning_outstanding_Balance, Credit_Bureau_Data_New, mean),aggregate(Performance.Tag ~ Binning_outstanding_Balance,
                                                                                                                                                 Credit_Bureau_Data_New, sum),by = "Binning_outstanding_Balance")
# Adding No.of_prospect
count24 <- data.frame(table(Credit_Bureau_Data_New$Binning_outstanding_Balance))
count24 <- count24[,-1]
agg_Binning_Outstanding_Balance <- cbind(agg_Binning_Outstanding_Balance,count24)
agg_Binning_Outstanding_Balance

colnames(agg_Binning_Outstanding_Balance) <- c("agg_Binning_Outstanding_Balance", "Default_rate", "count_prospects","No.of_prospect")

# Round Off the values
agg_Binning_Outstanding_Balance$Default_rate <- format(round(agg_Binning_Outstanding_Balance$Default_rate, 4))

plot_response(Credit_Bureau_Data_New$Binning_outstanding_Balance, "Binning_outstanding_Balance")

#################################################################################################
# Handling of total number of trades. 
#################################################################################################
sum(is.na(Credit_Bureau_Data_New$Total_No_of_Trades))

ggplot(Credit_Bureau_Data_New,aes(Total_No_of_Trades)) +geom_bar(color="black",fill="lightgreen")
quantile(Credit_Bureau_Data_New$Total_No_of_Trades,seq(0,1,0.01))

# Capping the total number of trades beyond 31 to 31.(i.e 99% percentile.) 
Credit_Bureau_Data_New[(which(Credit_Bureau_Data_New$Total_No_of_Trades>31)),]$Total_No_of_Trades <- 31

Credit_Bureau_Data_New$binning_Total_No_of_Trades <- as.factor(cut(Credit_Bureau_Data_New$Total_No_of_Trades, breaks = c(0,10,20,30,40),include.lowest = TRUE))

#Creating bins for total number of trades for rejected applications
Credit_Bureau_Data_Rejected$binning_Total_No_of_Trades <- as.factor(cut(Credit_Bureau_Data_Rejected$Total_No_of_Trades, breaks = c(0,10,20,30,40),include.lowest = TRUE))

plot_response(Credit_Bureau_Data_New$binning_Total_No_of_Trades,"binning.Total.No.of.Trades with default rate")

#########################################################################################
# Deriving Insights from presence of open auto loan variable
#########################################################################################
sum(is.na(Credit_Bureau_Data_New$Presence_of_open_auto_loan))

Credit_Bureau_Data_New$Presence_of_open_auto_loan <- as.factor(Credit_Bureau_Data_New$Presence_of_open_auto_loan)
ggplot(Credit_Bureau_Data_New,aes(x=Presence_of_open_auto_loan)) +geom_bar(color="black",fill="lightgreen")

agg_Presence_of_open_auto_loan <- merge(aggregate(Performance.Tag ~ Presence_of_open_auto_loan, Credit_Bureau_Data_New, mean),aggregate(Performance.Tag ~ Presence_of_open_auto_loan,
                                                                                                                                                 Credit_Bureau_Data_New, sum),by = "Presence_of_open_auto_loan")
# Adding No.of_prospect
count25 <- data.frame(table(Credit_Bureau_Data_New$Presence_of_open_auto_loan))
count25 <- count25[,-1]
agg_Presence_of_open_auto_loan <- cbind(agg_Presence_of_open_auto_loan,count25)
agg_Presence_of_open_auto_loan

colnames(agg_Presence_of_open_auto_loan) <- c("agg_Presence_of_open_auto_loan", "Default_rate", "count_prospects","No.of_prospect")

# Round Off the values
agg_Presence_of_open_auto_loan$Default_rate <- format(round(agg_Presence_of_open_auto_loan$Default_rate, 4))

plot_response(Credit_Bureau_Data_New$Presence_of_open_auto_loan, "Presence_of_open_auto_loan")


#WOE Calculation for Credit Bureau Dataframe
Credit_Bureau_Data_New1 <- Credit_Bureau_Data_New

sapply(Credit_Bureau_Data_New1, function(x) (as.factor(x)))
Credit_Bureau_Data_New1$No_of_trades_opened_in_last_6_months <- as.factor(Credit_Bureau_Data_New1$No_of_trades_opened_in_last_6_months)
Credit_Bureau_Data_New1$No_of_trades_opened_in_last_12_months <- as.factor(Credit_Bureau_Data_New1$No_of_trades_opened_in_last_12_months)
Credit_Bureau_Data_New1$No_of_PL_trades_opened_in_last_12_months <- as.factor(Credit_Bureau_Data_New1$No_of_PL_trades_opened_in_last_12_months)
Credit_Bureau_Data_New1$No_of_Inquiries_in_last_12_months_excluding_home_auto_loans <- as.factor(Credit_Bureau_Data_New1$No_of_Inquiries_in_last_12_months_excluding_home_auto_loans)
Credit_Bureau_Data_New1$No_of_Inquiries_in_last_6_months_excluding_home_auto_loans <- as.factor(Credit_Bureau_Data_New1$No_of_Inquiries_in_last_6_months_excluding_home_auto_loans)

#The below line is required to allign the row numbers in sequence. 
row.names(Credit_Bureau_Data_New) = seq(1,nrow(Credit_Bureau_Data_New))

# Creating WOE as new Columns for Credit Bureau Dataset
outiv1 <- iv.mult(Credit_Bureau_Data_New1, "Performance.Tag", 
                    vars = c("No_of_times_90_DPD_or_worse_in_last_6_months","No_of_times_60_DPD_or_worse_in_last_6_months","No_of_times_30_DPD_or_worse_in_last_6_months",
                             "No_of_times_90_DPD_or_worse_in_last_12_months","No_of_times_60_DPD_or_worse_in_last_12_months","No_of_times_30_DPD_or_worse_in_last_12_months",
                             "Binned_CC_Utilization","Binned_No_of_trades_opened_in_last_6_months","Binned_No_of_trades_opened_in_last_12_months","No_of_PL_trades_opened_in_last_6_months","No_of_PL_trades_opened_in_last_12_months",
                              "Binned_No_of_Inquiries_in_last_6_months","Binned_No_of_Inquiries_in_last_12_months","Modified_Presence_of_open_home_loan","Binning_outstanding_Balance","binning_Total_No_of_Trades","Presence_of_open_auto_loan"))

Credit_Bureau_Data_WOE <- iv.replace.woe(Credit_Bureau_Data_New1,outiv1)
str(Credit_Bureau_Data_WOE)

#Credit_Bureau_Data_WOE for Rejected Apllications
Credit_Bureau_Data_Rejected$Performance.Tag[is.na(Credit_Bureau_Data_Rejected$Performance.Tag)] <- 0
Credit_Bureau_Data_Rejected$Performance.Tag <- ifelse(Credit_Bureau_Data_Rejected$Presence_of_open_home_loan == "0",1,0)
Credit_Bureau_Data_Rejected$Performance.Tag <- as.factor(Credit_Bureau_Data_Rejected$Performance.Tag)
summary(Credit_Bureau_Data_Rejected$Performance.Tag)

rownames(Credit_Bureau_Data_Rejected) <- NULL

sapply(Credit_Bureau_Data_Rejected, function(x) (as.factor(x)))
Credit_Bureau_Data_Rejected$No_of_trades_opened_in_last_6_months <- as.factor(Credit_Bureau_Data_Rejected$No_of_trades_opened_in_last_6_months)
Credit_Bureau_Data_Rejected$No_of_trades_opened_in_last_12_months <- as.factor(Credit_Bureau_Data_Rejected$No_of_trades_opened_in_last_12_months)
Credit_Bureau_Data_Rejected$No_of_PL_trades_opened_in_last_12_months <- as.factor(Credit_Bureau_Data_Rejected$No_of_PL_trades_opened_in_last_12_months)
Credit_Bureau_Data_Rejected$No_of_Inquiries_in_last_12_months_excluding_home_auto_loans <- as.factor(Credit_Bureau_Data_Rejected$No_of_Inquiries_in_last_12_months_excluding_home_auto_loans)
Credit_Bureau_Data_Rejected$No_of_Inquiries_in_last_6_months_excluding_home_auto_loans <- as.factor(Credit_Bureau_Data_Rejected$No_of_Inquiries_in_last_6_months_excluding_home_auto_loans)
str(Credit_Bureau_Data_New1$Presence_of_open_auto_loan)

# The below line is required to allign the row numbers in sequence. 
row.names(Credit_Bureau_Data_Rejected) = seq(1,nrow(Credit_Bureau_Data_Rejected))

#Calculating WOE for Rejected Applicants
outiv_cb_rejected <- iv.mult(Credit_Bureau_Data_Rejected, "Performance.Tag", 
                  vars = c("No_of_times_90_DPD_or_worse_in_last_6_months","No_of_times_60_DPD_or_worse_in_last_6_months","No_of_times_30_DPD_or_worse_in_last_6_months",
                           "No_of_times_90_DPD_or_worse_in_last_12_months","No_of_times_60_DPD_or_worse_in_last_12_months","No_of_times_30_DPD_or_worse_in_last_12_months",
                           "Binned_CC_Utilization","Binned_No_of_trades_opened_in_last_6_months","Binned_No_of_trades_opened_in_last_12_months","No_of_PL_trades_opened_in_last_6_months","No_of_PL_trades_opened_in_last_12_months",
                           "Binned_No_of_Inquiries_in_last_6_months","Binned_No_of_Inquiries_in_last_12_months","binning_Total_No_of_Trades","Presence_of_open_home_loan","Binning_outstanding_Balance","Presence_of_open_auto_loan"))

Credit_Bureau_Data_Rejected_WOE <- iv.replace.woe(Credit_Bureau_Data_Rejected,outiv_cb_rejected)

#Removing unwanted columns from the WOE dataframe; will be keeping only WOE columns
Credit_Bureau_Data_Rejected_WOE <-  Credit_Bureau_Data_Rejected_WOE[,-c(2:27)]
Credit_Bureau_Data_WOE <-  Credit_Bureau_Data_WOE[,-c(2:31)]

#Combining Both Demographic and Credit bUreau dataframes
Combined_Dataframe <- merge(x=Credit_Bureau_Data_WOE,y=Demographic_Data_Final,by = "Application.ID",all = F)

#Combining Both Demographic and Credit bUreau dataframe for rejected applications
Combined_Dataframe_Rejected <- merge(x=Credit_Bureau_Data_Rejected_WOE,y=Demographic_Data_Rejected_Final,by = "Application.ID",all = F)

# converting Performance tag variable from Final Dataset from No/Yes character to factorwith levels 0/1 
Combined_Dataframe$Performance.Tag<- ifelse(Combined_Dataframe$Performance.Tag=="Yes",1,0)
summary(Combined_Dataframe)

###########################################################################################################
#         Building Combined - Logistic- Regression model on Unbalanced Dataset                            #
###########################################################################################################
set.seed(100)
split_indices_Final <- sample.split(Combined_Dataframe$Performance.Tag, SplitRatio = 0.70)

#Dividing combined Dataset into Train & Test
training <- Combined_Dataframe[split_indices_Final, ]
testing <- Combined_Dataframe[!split_indices_Final, ]
nrow(training)/nrow(Combined_Dataframe)
nrow(testing)/nrow(Combined_Dataframe)

### Model 1: Logistic Regression
Reg_Model_1 <- glm(Performance.Tag ~ ., family = "binomial", data = training[,-1])
summary(Reg_Model_1)
cor(Combined_Dataframe)

#Using stepwise algorithm for removing insignificant variables 
#Reg_Model_2 <- stepAIC(Reg_Model_1, direction = "both")
#summary(Reg_Model_2)

# Model 3: Logistic Regression
Reg_Model_3 <- glm(formula = Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
      No_of_times_60_DPD_or_worse_in_last_6_months_woe + No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
      No_of_times_90_DPD_or_worse_in_last_12_months_woe + No_of_times_60_DPD_or_worse_in_last_12_months_woe + 
      No_of_times_30_DPD_or_worse_in_last_12_months_woe + Binned_CC_Utilization_woe + 
      Binned_No_of_trades_opened_in_last_6_months_woe + Binned_No_of_trades_opened_in_last_12_months_woe + 
      No_of_PL_trades_opened_in_last_6_months_woe + No_of_PL_trades_opened_in_last_12_months_woe + 
      Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
      Modified_Presence_of_open_home_loan_woe + Binning_outstanding_Balance_woe + 
      binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
      Modified_Age + Modified_Income + BinningAge_woe + Profession_woe + 
      Education_woe + Gender_woe + Marital_Status_woe + Modified_No_of_dependents_woe + 
      BinningIncome_woe + Type_of_Residence_woe + BinningCurentResidence_woe + 
      BinningCurrentCompany_woe + `BinningAge(14,25]` + `BinningAge(25,35]` + 
      `BinningAge(35,45]` +  `BinningIncome[0,10]` + `BinningIncome(10,20]` + `BinningIncome(20,30]` + 
      `BinningIncome(30,40]` +  `BinningCurentResidence(0,20]` + `BinningCurentResidence(20,40]` + 
      `BinningCurentResidence(40,60]` + `BinningCurentResidence(60,80]` + 
      `BinningCurrentCompany(0,15]` + `BinningCurrentCompany(15,30]` + 
      `BinningCurrentCompany(30,45]` +  Modified_GenderF + 
      Modified_Marital_StatusMarried + Modified_Marital_StatusOther + 
       Modified_No_of_dependents1 +  Modified_No_of_dependents2 + Modified_No_of_dependents3 + 
      Modified_EducationBachelor + Modified_EducationMasters +  Modified_EducationOthers + Modified_EducationPhd + 
      Modified_ProfessionOthers + Modified_ProfessionSAL +  `Modified_Residence_TypeCompany provided` + 
      `Modified_Residence_TypeLiving with Parents` + Modified_Residence_TypeOwned + 
      Modified_Residence_TypeRented, family = "binomial", data = training[, 
                                                                          -1])
summary(Reg_Model_3)
vif(Reg_Model_3)

#Removing Modified_Residence_Type_Rented variable
Reg_Model_4 <- glm(formula = Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                     No_of_times_60_DPD_or_worse_in_last_6_months_woe + No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                     No_of_times_90_DPD_or_worse_in_last_12_months_woe + No_of_times_60_DPD_or_worse_in_last_12_months_woe + 
                     No_of_times_30_DPD_or_worse_in_last_12_months_woe + Binned_CC_Utilization_woe + 
                     Binned_No_of_trades_opened_in_last_6_months_woe + Binned_No_of_trades_opened_in_last_12_months_woe + 
                     No_of_PL_trades_opened_in_last_6_months_woe + No_of_PL_trades_opened_in_last_12_months_woe + 
                     Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                     Modified_Presence_of_open_home_loan_woe + Binning_outstanding_Balance_woe + 
                     binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                     Modified_Age + Modified_Income + BinningAge_woe + Profession_woe + 
                     Education_woe + Gender_woe + Marital_Status_woe + Modified_No_of_dependents_woe + 
                     BinningIncome_woe + Type_of_Residence_woe + BinningCurentResidence_woe + 
                     BinningCurrentCompany_woe + `BinningAge(14,25]` + `BinningAge(25,35]` + 
                     `BinningAge(35,45]` +  `BinningIncome[0,10]` + `BinningIncome(10,20]` + `BinningIncome(20,30]` + 
                     `BinningIncome(30,40]` + `BinningCurentResidence(0,20]` + `BinningCurentResidence(20,40]` + 
                     `BinningCurentResidence(40,60]` + `BinningCurentResidence(60,80]` + 
                     `BinningCurrentCompany(0,15]` + `BinningCurrentCompany(15,30]` + 
                     `BinningCurrentCompany(30,45]` + 
                     Modified_GenderF +  Modified_Marital_StatusMarried + Modified_Marital_StatusOther + 
                     Modified_No_of_dependents1 +  Modified_No_of_dependents2 + Modified_No_of_dependents3 + 
                     Modified_EducationBachelor + Modified_EducationMasters + 
                     Modified_EducationOthers + Modified_EducationPhd +  Modified_ProfessionOthers + Modified_ProfessionSAL + 
                     `Modified_Residence_TypeCompany provided` +  `Modified_Residence_TypeLiving with Parents` + Modified_Residence_TypeOwned 
                     , family = "binomial", data = training[, -1])
    
summary(Reg_Model_4)                                                                           
vif(Reg_Model_4)

# Removing Modified_Marital_StatusMarried variable
Reg_Model_5 <- glm(formula = Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                     No_of_times_60_DPD_or_worse_in_last_6_months_woe + No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                     No_of_times_90_DPD_or_worse_in_last_12_months_woe + No_of_times_60_DPD_or_worse_in_last_12_months_woe + 
                     No_of_times_30_DPD_or_worse_in_last_12_months_woe + Binned_CC_Utilization_woe + 
                     Binned_No_of_trades_opened_in_last_6_months_woe + Binned_No_of_trades_opened_in_last_12_months_woe + 
                     No_of_PL_trades_opened_in_last_6_months_woe + No_of_PL_trades_opened_in_last_12_months_woe + 
                     Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                     Modified_Presence_of_open_home_loan_woe + Binning_outstanding_Balance_woe + 
                     binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                     Modified_Age + Modified_Income + BinningAge_woe + Profession_woe + 
                     Education_woe + Gender_woe + Marital_Status_woe + Modified_No_of_dependents_woe + 
                     BinningIncome_woe + Type_of_Residence_woe + BinningCurentResidence_woe + 
                     BinningCurrentCompany_woe + `BinningAge(14,25]` + `BinningAge(25,35]` + 
                     `BinningAge(35,45]` + `BinningIncome[0,10]` + `BinningIncome(10,20]` + `BinningIncome(20,30]` + 
                     `BinningIncome(30,40]` +  `BinningCurentResidence(0,20]` + `BinningCurentResidence(20,40]` + 
                     `BinningCurentResidence(40,60]` + `BinningCurentResidence(60,80]` +`BinningCurrentCompany(0,15]` + `BinningCurrentCompany(15,30]` + 
                     `BinningCurrentCompany(30,45]` +  Modified_GenderF +  Modified_Marital_StatusOther + 
                     Modified_No_of_dependents1 +  Modified_No_of_dependents2 + Modified_No_of_dependents3 + Modified_EducationBachelor + Modified_EducationMasters + 
                     Modified_EducationOthers + Modified_EducationPhd +  Modified_ProfessionOthers + Modified_ProfessionSAL + 
                     `Modified_Residence_TypeCompany provided` + `Modified_Residence_TypeLiving with Parents` + Modified_Residence_TypeOwned 
                   , family = "binomial", data = training[, -1])

summary(Reg_Model_5)
vif(Reg_Model_5)

# Removing Gender_WOE and MaritalStatus_WOE variable
Reg_Model_6 <- glm(formula = Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                     No_of_times_60_DPD_or_worse_in_last_6_months_woe + No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                     No_of_times_90_DPD_or_worse_in_last_12_months_woe + No_of_times_60_DPD_or_worse_in_last_12_months_woe + 
                     No_of_times_30_DPD_or_worse_in_last_12_months_woe + Binned_CC_Utilization_woe + 
                     Binned_No_of_trades_opened_in_last_6_months_woe + Binned_No_of_trades_opened_in_last_12_months_woe + 
                     No_of_PL_trades_opened_in_last_6_months_woe + No_of_PL_trades_opened_in_last_12_months_woe + 
                     Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                     Modified_Presence_of_open_home_loan_woe + Binning_outstanding_Balance_woe + 
                     binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                     Modified_Age + Modified_Income + BinningAge_woe + Profession_woe + 
                     Education_woe +   Modified_No_of_dependents_woe + 
                     BinningIncome_woe + Type_of_Residence_woe + BinningCurentResidence_woe + 
                     BinningCurrentCompany_woe + `BinningAge(14,25]` + `BinningAge(25,35]` + 
                     `BinningAge(35,45]` + `BinningIncome[0,10]` + `BinningIncome(10,20]` + `BinningIncome(20,30]` + 
                     `BinningIncome(30,40]` +  `BinningCurentResidence(0,20]` + `BinningCurentResidence(20,40]` + 
                     `BinningCurentResidence(40,60]` + `BinningCurentResidence(60,80]` +`BinningCurrentCompany(0,15]` + `BinningCurrentCompany(15,30]` + 
                     `BinningCurrentCompany(30,45]` +  Modified_GenderF +  Modified_Marital_StatusOther + 
                     Modified_No_of_dependents1 +  Modified_No_of_dependents2 + Modified_No_of_dependents3 + Modified_EducationBachelor + Modified_EducationMasters + 
                     Modified_EducationOthers + Modified_EducationPhd + 
                     Modified_ProfessionOthers + Modified_ProfessionSAL + 
                     `Modified_Residence_TypeCompany provided` + 
                     `Modified_Residence_TypeLiving with Parents` + Modified_Residence_TypeOwned 
                   , family = "binomial", data = training[, -1])

summary(Reg_Model_6)
vif(Reg_Model_6)

# Removing Gender_WOE and MaritalStatus_WOE variable
Reg_Model_6 <- glm(formula = Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                     No_of_times_60_DPD_or_worse_in_last_6_months_woe + No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                     No_of_times_90_DPD_or_worse_in_last_12_months_woe + No_of_times_60_DPD_or_worse_in_last_12_months_woe + 
                     No_of_times_30_DPD_or_worse_in_last_12_months_woe + Binned_CC_Utilization_woe + 
                     Binned_No_of_trades_opened_in_last_6_months_woe + Binned_No_of_trades_opened_in_last_12_months_woe + 
                     No_of_PL_trades_opened_in_last_6_months_woe + No_of_PL_trades_opened_in_last_12_months_woe + 
                     Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                     Modified_Presence_of_open_home_loan_woe + Binning_outstanding_Balance_woe + 
                     binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                     Modified_Age + Modified_Income + BinningAge_woe + Profession_woe + 
                     Education_woe +   Modified_No_of_dependents_woe + 
                     BinningIncome_woe + Type_of_Residence_woe + BinningCurentResidence_woe + 
                     BinningCurrentCompany_woe + `BinningAge(14,25]` + `BinningAge(25,35]` + 
                     `BinningAge(35,45]` + `BinningIncome[0,10]` + `BinningIncome(10,20]` + `BinningIncome(20,30]` + 
                     `BinningIncome(30,40]` +  `BinningCurentResidence(0,20]` + `BinningCurentResidence(20,40]` + 
                     `BinningCurentResidence(40,60]` + `BinningCurentResidence(60,80]` +`BinningCurrentCompany(0,15]` + `BinningCurrentCompany(15,30]` + 
                     `BinningCurrentCompany(30,45]` +  Modified_GenderF +  Modified_Marital_StatusOther + 
                     Modified_No_of_dependents1 +  Modified_No_of_dependents2 + Modified_No_of_dependents3 + Modified_EducationBachelor + Modified_EducationMasters + 
                     Modified_EducationOthers + Modified_EducationPhd + 
                     Modified_ProfessionOthers + Modified_ProfessionSAL + 
                     `Modified_Residence_TypeCompany provided` + 
                     `Modified_Residence_TypeLiving with Parents` + Modified_Residence_TypeOwned 
                   , family = "binomial", data = training[, -1])

summary(Reg_Model_6)
vif(Reg_Model_6)

# Removing BinningCurrentResidence_WOE variable
Reg_Model_7 <- glm(formula = Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                     No_of_times_60_DPD_or_worse_in_last_6_months_woe + No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                     No_of_times_90_DPD_or_worse_in_last_12_months_woe + No_of_times_60_DPD_or_worse_in_last_12_months_woe + 
                     No_of_times_30_DPD_or_worse_in_last_12_months_woe + Binned_CC_Utilization_woe + 
                     Binned_No_of_trades_opened_in_last_6_months_woe + Binned_No_of_trades_opened_in_last_12_months_woe + 
                     No_of_PL_trades_opened_in_last_6_months_woe + No_of_PL_trades_opened_in_last_12_months_woe + 
                     Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                     Modified_Presence_of_open_home_loan_woe + Binning_outstanding_Balance_woe + 
                     binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                     Modified_Age + Modified_Income + BinningAge_woe + Profession_woe + 
                     Education_woe +   Modified_No_of_dependents_woe + 
                     BinningIncome_woe + Type_of_Residence_woe + 
                     BinningCurrentCompany_woe + `BinningAge(14,25]` + `BinningAge(25,35]` + 
                     `BinningAge(35,45]` + `BinningIncome[0,10]` + `BinningIncome(10,20]` + `BinningIncome(20,30]` + 
                     `BinningIncome(30,40]` +  `BinningCurentResidence(0,20]` + `BinningCurentResidence(20,40]` + 
                     `BinningCurentResidence(40,60]` + `BinningCurentResidence(60,80]` +`BinningCurrentCompany(0,15]` + `BinningCurrentCompany(15,30]` + 
                     `BinningCurrentCompany(30,45]` +  Modified_GenderF +  Modified_Marital_StatusOther + 
                     Modified_No_of_dependents1 +  Modified_No_of_dependents2 + Modified_No_of_dependents3 + Modified_EducationBachelor + Modified_EducationMasters + 
                     Modified_EducationOthers + Modified_EducationPhd + Modified_ProfessionOthers + Modified_ProfessionSAL + 
                     `Modified_Residence_TypeCompany provided` + `Modified_Residence_TypeLiving with Parents` + Modified_Residence_TypeOwned 
                   , family = "binomial", data = training[, -1])

summary(Reg_Model_7)
vif(Reg_Model_7)

# Removing BinningIncome[0,10] variable
Reg_Model_8 <- glm(formula = Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                     No_of_times_60_DPD_or_worse_in_last_6_months_woe + No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                     No_of_times_90_DPD_or_worse_in_last_12_months_woe + No_of_times_60_DPD_or_worse_in_last_12_months_woe + 
                     No_of_times_30_DPD_or_worse_in_last_12_months_woe + Binned_CC_Utilization_woe + 
                     Binned_No_of_trades_opened_in_last_6_months_woe + Binned_No_of_trades_opened_in_last_12_months_woe + 
                     No_of_PL_trades_opened_in_last_6_months_woe + No_of_PL_trades_opened_in_last_12_months_woe + 
                     Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                     Modified_Presence_of_open_home_loan_woe + Binning_outstanding_Balance_woe + 
                     binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                     Modified_Age + Modified_Income + BinningAge_woe + Profession_woe + 
                     Education_woe +   Modified_No_of_dependents_woe + 
                     BinningIncome_woe + Type_of_Residence_woe + 
                     BinningCurrentCompany_woe + `BinningAge(14,25]` + `BinningAge(25,35]` + 
                     `BinningAge(35,45]` +  `BinningIncome(10,20]` + `BinningIncome(20,30]` + 
                     `BinningIncome(30,40]` +  `BinningCurentResidence(0,20]` + `BinningCurentResidence(20,40]` + 
                     `BinningCurentResidence(40,60]` + `BinningCurentResidence(60,80]` +`BinningCurrentCompany(0,15]` + `BinningCurrentCompany(15,30]` + 
                     `BinningCurrentCompany(30,45]` +  Modified_GenderF +  Modified_Marital_StatusOther + 
                     Modified_No_of_dependents1 + Modified_No_of_dependents2 + Modified_No_of_dependents3 + Modified_EducationBachelor + Modified_EducationMasters + 
                     Modified_EducationOthers + Modified_EducationPhd + 
                     Modified_ProfessionOthers + Modified_ProfessionSAL + 
                     `Modified_Residence_TypeCompany provided` + 
                     `Modified_Residence_TypeLiving with Parents` + Modified_Residence_TypeOwned 
                   , family = "binomial", data = training[, -1])

summary(Reg_Model_8)
vif(Reg_Model_8)


# Removing Modified_No_of_dependents_woe and BinningIncome_WoE variable
Reg_Model_9 <- glm(formula = Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                     No_of_times_60_DPD_or_worse_in_last_6_months_woe + No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                     No_of_times_90_DPD_or_worse_in_last_12_months_woe + No_of_times_60_DPD_or_worse_in_last_12_months_woe + 
                     No_of_times_30_DPD_or_worse_in_last_12_months_woe + Binned_CC_Utilization_woe + 
                     Binned_No_of_trades_opened_in_last_6_months_woe + Binned_No_of_trades_opened_in_last_12_months_woe + 
                     No_of_PL_trades_opened_in_last_6_months_woe + No_of_PL_trades_opened_in_last_12_months_woe + 
                     Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                     Modified_Presence_of_open_home_loan_woe + Binning_outstanding_Balance_woe + 
                     binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                     Modified_Age + Modified_Income + BinningAge_woe + Profession_woe + 
                     Education_woe + Type_of_Residence_woe + 
                     BinningCurrentCompany_woe + `BinningAge(14,25]` + `BinningAge(25,35]` + 
                     `BinningAge(35,45]` +  `BinningIncome(10,20]` + `BinningIncome(20,30]` + 
                     `BinningIncome(30,40]` +  `BinningCurentResidence(0,20]` + `BinningCurentResidence(20,40]` + 
                     `BinningCurentResidence(40,60]` + `BinningCurentResidence(60,80]` +`BinningCurrentCompany(0,15]` + `BinningCurrentCompany(15,30]` + 
                     `BinningCurrentCompany(30,45]` +  Modified_GenderF +  Modified_Marital_StatusOther + 
                     Modified_No_of_dependents1 + Modified_No_of_dependents2 + Modified_No_of_dependents3 + Modified_EducationBachelor + Modified_EducationMasters + 
                     Modified_EducationOthers + Modified_EducationPhd + 
                     Modified_ProfessionOthers + Modified_ProfessionSAL + `Modified_Residence_TypeCompany provided` + 
                     `Modified_Residence_TypeLiving with Parents` + Modified_Residence_TypeOwned 
                   , family = "binomial", data = training[, -1])

summary(Reg_Model_9)
vif(Reg_Model_9)


# Removing `BinningAge(25,35]` variable
Reg_Model_10 <- glm(formula = Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                     No_of_times_60_DPD_or_worse_in_last_6_months_woe + No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                     No_of_times_90_DPD_or_worse_in_last_12_months_woe + No_of_times_60_DPD_or_worse_in_last_12_months_woe + 
                     No_of_times_30_DPD_or_worse_in_last_12_months_woe + Binned_CC_Utilization_woe + 
                     Binned_No_of_trades_opened_in_last_6_months_woe + Binned_No_of_trades_opened_in_last_12_months_woe + 
                     No_of_PL_trades_opened_in_last_6_months_woe + No_of_PL_trades_opened_in_last_12_months_woe + 
                     Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                     Modified_Presence_of_open_home_loan_woe + Binning_outstanding_Balance_woe + 
                     binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                     Modified_Age + Modified_Income + BinningAge_woe + Profession_woe + 
                     Education_woe + Type_of_Residence_woe + 
                     BinningCurrentCompany_woe + `BinningAge(14,25]` + 
                     `BinningAge(35,45]` +  `BinningIncome(10,20]` + `BinningIncome(20,30]` + 
                     `BinningIncome(30,40]` +  `BinningCurentResidence(0,20]` + `BinningCurentResidence(20,40]` + 
                     `BinningCurentResidence(40,60]` + `BinningCurentResidence(60,80]` +`BinningCurrentCompany(0,15]` + `BinningCurrentCompany(15,30]` + 
                     `BinningCurrentCompany(30,45]` +  Modified_GenderF +  Modified_Marital_StatusOther + 
                     Modified_No_of_dependents1 + 
                     Modified_No_of_dependents2 + Modified_No_of_dependents3 + Modified_EducationBachelor + Modified_EducationMasters + 
                     Modified_EducationOthers + Modified_EducationPhd + 
                     Modified_ProfessionOthers + Modified_ProfessionSAL + 
                     `Modified_Residence_TypeCompany provided` + 
                     `Modified_Residence_TypeLiving with Parents` + Modified_Residence_TypeOwned 
                   , family = "binomial", data = training[, -1])

summary(Reg_Model_10)
vif(Reg_Model_10)

# Removing No_of_times_60_DPD_or_worse_in_last_6_months_woe variable
Reg_Model_11 <- glm(formula = Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_90_DPD_or_worse_in_last_12_months_woe + No_of_times_60_DPD_or_worse_in_last_12_months_woe + 
                      No_of_times_30_DPD_or_worse_in_last_12_months_woe + Binned_CC_Utilization_woe + 
                      Binned_No_of_trades_opened_in_last_6_months_woe + Binned_No_of_trades_opened_in_last_12_months_woe + 
                      No_of_PL_trades_opened_in_last_6_months_woe + No_of_PL_trades_opened_in_last_12_months_woe + 
                      Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                      Modified_Presence_of_open_home_loan_woe + Binning_outstanding_Balance_woe + 
                      binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                      Modified_Age + Modified_Income + BinningAge_woe + Profession_woe + 
                      Education_woe + Type_of_Residence_woe + 
                      BinningCurrentCompany_woe + `BinningAge(14,25]` + 
                      `BinningAge(35,45]` +  `BinningIncome(10,20]` + `BinningIncome(20,30]` + 
                      `BinningIncome(30,40]` +  `BinningCurentResidence(0,20]` + `BinningCurentResidence(20,40]` + 
                      `BinningCurentResidence(40,60]` + `BinningCurentResidence(60,80]` +`BinningCurrentCompany(0,15]` + `BinningCurrentCompany(15,30]` + 
                      `BinningCurrentCompany(30,45]` +  Modified_GenderF +  Modified_Marital_StatusOther + 
                      Modified_No_of_dependents1 + Modified_No_of_dependents2 + Modified_No_of_dependents3 + Modified_EducationBachelor + Modified_EducationMasters + 
                      Modified_EducationOthers + Modified_EducationPhd + Modified_ProfessionOthers + Modified_ProfessionSAL + 
                      `Modified_Residence_TypeCompany provided` + `Modified_Residence_TypeLiving with Parents` + Modified_Residence_TypeOwned 
                    , family = "binomial", data = training[, -1])

summary(Reg_Model_11)
vif(Reg_Model_11)

# Removing No_of_times_30_DPD_or_worse_in_last_12_months_woe variable
Reg_Model_12 <- glm(formula = Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_90_DPD_or_worse_in_last_12_months_woe + No_of_times_60_DPD_or_worse_in_last_12_months_woe + 
                      Binned_CC_Utilization_woe + 
                      Binned_No_of_trades_opened_in_last_6_months_woe + Binned_No_of_trades_opened_in_last_12_months_woe + 
                      No_of_PL_trades_opened_in_last_6_months_woe + No_of_PL_trades_opened_in_last_12_months_woe + 
                      Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                      Modified_Presence_of_open_home_loan_woe + Binning_outstanding_Balance_woe + 
                      binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                      Modified_Age + Modified_Income + BinningAge_woe + Profession_woe + 
                      Education_woe + Type_of_Residence_woe + 
                      BinningCurrentCompany_woe + `BinningAge(14,25]` + 
                      `BinningAge(35,45]` +  `BinningIncome(10,20]` + `BinningIncome(20,30]` + 
                      `BinningIncome(30,40]` +  `BinningCurentResidence(0,20]` + `BinningCurentResidence(20,40]` + 
                      `BinningCurentResidence(40,60]` + `BinningCurentResidence(60,80]` +`BinningCurrentCompany(0,15]` + `BinningCurrentCompany(15,30]` + 
                      `BinningCurrentCompany(30,45]` +  Modified_GenderF +  Modified_Marital_StatusOther + 
                      Modified_No_of_dependents1 + Modified_No_of_dependents2 + Modified_No_of_dependents3 + Modified_EducationBachelor + Modified_EducationMasters + 
                      Modified_EducationOthers + Modified_EducationPhd + 
                      Modified_ProfessionOthers + Modified_ProfessionSAL + 
                      `Modified_Residence_TypeCompany provided` + 
                      `Modified_Residence_TypeLiving with Parents` + Modified_Residence_TypeOwned 
                    , family = "binomial", data = training[, -1])

summary(Reg_Model_12)
vif(Reg_Model_12)


# Removing No_of_PL_trades_opened_in_last_6_months_woe variable
Reg_Model_13 <- glm(formula = Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_90_DPD_or_worse_in_last_12_months_woe + No_of_times_60_DPD_or_worse_in_last_12_months_woe + 
                      Binned_CC_Utilization_woe + 
                      Binned_No_of_trades_opened_in_last_6_months_woe + Binned_No_of_trades_opened_in_last_12_months_woe + 
                      No_of_PL_trades_opened_in_last_12_months_woe + 
                      Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                      Modified_Presence_of_open_home_loan_woe + Binning_outstanding_Balance_woe + 
                      binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                      Modified_Age + Modified_Income + BinningAge_woe + Profession_woe + 
                      Education_woe + Type_of_Residence_woe + 
                      BinningCurrentCompany_woe + `BinningAge(14,25]` + 
                      `BinningAge(35,45]` +  `BinningIncome(10,20]` + `BinningIncome(20,30]` + 
                      `BinningIncome(30,40]` +  `BinningCurentResidence(0,20]` + `BinningCurentResidence(20,40]` + 
                      `BinningCurentResidence(40,60]` + `BinningCurentResidence(60,80]` +`BinningCurrentCompany(0,15]` + `BinningCurrentCompany(15,30]` + 
                      `BinningCurrentCompany(30,45]` +  Modified_GenderF +  Modified_Marital_StatusOther + 
                      Modified_No_of_dependents1 +  Modified_No_of_dependents2 + Modified_No_of_dependents3 + Modified_EducationBachelor + Modified_EducationMasters + 
                      Modified_EducationOthers + Modified_EducationPhd + Modified_ProfessionOthers + Modified_ProfessionSAL + 
                      `Modified_Residence_TypeCompany provided` + `Modified_Residence_TypeLiving with Parents` + Modified_Residence_TypeOwned 
                    , family = "binomial", data = training[, -1])

summary(Reg_Model_13)
vif(Reg_Model_13)

# Removing +`BinningCurrentCompany(0,15]` variable
Reg_Model_14 <- glm(formula = Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_90_DPD_or_worse_in_last_12_months_woe + No_of_times_60_DPD_or_worse_in_last_12_months_woe + 
                      Binned_CC_Utilization_woe + 
                      Binned_No_of_trades_opened_in_last_6_months_woe + Binned_No_of_trades_opened_in_last_12_months_woe + 
                      No_of_PL_trades_opened_in_last_12_months_woe + 
                      Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                      Modified_Presence_of_open_home_loan_woe + Binning_outstanding_Balance_woe + 
                      binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                      Modified_Age + Modified_Income + BinningAge_woe + Profession_woe + 
                      Education_woe + Type_of_Residence_woe + 
                      BinningCurrentCompany_woe + `BinningAge(14,25]` + 
                      `BinningAge(35,45]` +  `BinningIncome(10,20]` + `BinningIncome(20,30]` + 
                      `BinningIncome(30,40]` +  `BinningCurentResidence(0,20]` + `BinningCurentResidence(20,40]` + 
                      `BinningCurentResidence(40,60]` + `BinningCurentResidence(60,80]` + `BinningCurrentCompany(15,30]` + 
                      `BinningCurrentCompany(30,45]` +  Modified_GenderF +  Modified_Marital_StatusOther + 
                      Modified_No_of_dependents1 + Modified_No_of_dependents2 + Modified_No_of_dependents3 + Modified_EducationBachelor + Modified_EducationMasters + 
                      Modified_EducationOthers + Modified_EducationPhd + 
                      Modified_ProfessionOthers + Modified_ProfessionSAL + 
                      `Modified_Residence_TypeCompany provided` + 
                      `Modified_Residence_TypeLiving with Parents` + Modified_Residence_TypeOwned 
                    , family = "binomial", data = training[, -1])

summary(Reg_Model_14)
vif(Reg_Model_14)

# Removing No_of_times_60_DPD_or_worse_in_last_12_months_woe  variable
Reg_Model_15 <- glm(formula = Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_90_DPD_or_worse_in_last_12_months_woe + 
                      Binned_CC_Utilization_woe + 
                      Binned_No_of_trades_opened_in_last_6_months_woe + Binned_No_of_trades_opened_in_last_12_months_woe + 
                      No_of_PL_trades_opened_in_last_12_months_woe + 
                      Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                      Modified_Presence_of_open_home_loan_woe + Binning_outstanding_Balance_woe + 
                      binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                      Modified_Age + Modified_Income + BinningAge_woe + Profession_woe + 
                      Education_woe + Type_of_Residence_woe + 
                      BinningCurrentCompany_woe + `BinningAge(14,25]` + 
                      `BinningAge(35,45]` +  `BinningIncome(10,20]` + `BinningIncome(20,30]` + 
                      `BinningIncome(30,40]` +  `BinningCurentResidence(0,20]` + `BinningCurentResidence(20,40]` + 
                      `BinningCurentResidence(40,60]` + `BinningCurentResidence(60,80]` + `BinningCurrentCompany(15,30]` + 
                      `BinningCurrentCompany(30,45]` +  Modified_GenderF +  Modified_Marital_StatusOther + 
                      Modified_No_of_dependents1 +  Modified_No_of_dependents2 + Modified_No_of_dependents3 + Modified_EducationBachelor + Modified_EducationMasters + 
                      Modified_EducationOthers + Modified_EducationPhd + 
                      Modified_ProfessionOthers + Modified_ProfessionSAL + `Modified_Residence_TypeCompany provided` + 
                      `Modified_Residence_TypeLiving with Parents` + Modified_Residence_TypeOwned 
                    , family = "binomial", data = training[, -1])

summary(Reg_Model_15)
vif(Reg_Model_15)

# Removing Binning_outstanding_Balance_woe and Binned_No_of_trades_opened_in_last_12_months_woe variable

Reg_Model_16 <- glm(formula = Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_90_DPD_or_worse_in_last_12_months_woe + 
                      Binned_CC_Utilization_woe +  Binned_No_of_trades_opened_in_last_6_months_woe +  
                      No_of_PL_trades_opened_in_last_12_months_woe + 
                      Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                      Modified_Presence_of_open_home_loan_woe  + 
                      binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                      Modified_Age + Modified_Income + BinningAge_woe + Profession_woe + 
                      Education_woe + Type_of_Residence_woe + 
                      BinningCurrentCompany_woe + `BinningAge(14,25]` + 
                      `BinningAge(35,45]` +  `BinningIncome(10,20]` + `BinningIncome(20,30]` + 
                      `BinningIncome(30,40]` +  `BinningCurentResidence(0,20]` + `BinningCurentResidence(20,40]` + 
                      `BinningCurentResidence(40,60]` + `BinningCurentResidence(60,80]` + `BinningCurrentCompany(15,30]` + 
                      `BinningCurrentCompany(30,45]` +  Modified_GenderF +  Modified_Marital_StatusOther + 
                      Modified_No_of_dependents1 +  Modified_No_of_dependents2 + Modified_No_of_dependents3 + Modified_EducationBachelor + Modified_EducationMasters + 
                      Modified_EducationOthers + Modified_EducationPhd + 
                      Modified_ProfessionOthers + Modified_ProfessionSAL + 
                      `Modified_Residence_TypeCompany provided` + 
                      `Modified_Residence_TypeLiving with Parents` + Modified_Residence_TypeOwned 
                    , family = "binomial", data = training[, -1])

summary(Reg_Model_16)
vif(Reg_Model_16)

# Removing Education_woe variable
Reg_Model_17 <- glm(formula = Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_90_DPD_or_worse_in_last_12_months_woe + 
                      Binned_CC_Utilization_woe + Binned_No_of_trades_opened_in_last_6_months_woe +  
                      No_of_PL_trades_opened_in_last_12_months_woe + 
                      Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                      Modified_Presence_of_open_home_loan_woe  + 
                      binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                      Modified_Age + Modified_Income + BinningAge_woe + Profession_woe + 
                      Type_of_Residence_woe + BinningCurrentCompany_woe + `BinningAge(14,25]` + 
                      `BinningAge(35,45]` +  `BinningIncome(10,20]` + `BinningIncome(20,30]` + 
                      `BinningIncome(30,40]` +  `BinningCurentResidence(0,20]` + `BinningCurentResidence(20,40]` + 
                      `BinningCurentResidence(40,60]` + `BinningCurentResidence(60,80]` + `BinningCurrentCompany(15,30]` + 
                      `BinningCurrentCompany(30,45]` +  Modified_GenderF +  Modified_Marital_StatusOther + 
                      Modified_No_of_dependents1 + Modified_No_of_dependents2 + Modified_No_of_dependents3 + Modified_EducationBachelor + Modified_EducationMasters + 
                      Modified_EducationOthers + Modified_EducationPhd + 
                      Modified_ProfessionOthers + Modified_ProfessionSAL + 
                      `Modified_Residence_TypeCompany provided` + 
                      `Modified_Residence_TypeLiving with Parents` + Modified_Residence_TypeOwned 
                    , family = "binomial", data = training[, -1])

summary(Reg_Model_17)
vif(Reg_Model_17)

# Removing MaritalStatus_Other and ProfessionOther variable
Reg_Model_18 <- glm(formula = Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_90_DPD_or_worse_in_last_12_months_woe + 
                      Binned_CC_Utilization_woe + Binned_No_of_trades_opened_in_last_6_months_woe +  
                      No_of_PL_trades_opened_in_last_12_months_woe + 
                      Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                      Modified_Presence_of_open_home_loan_woe  + 
                      binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                      Modified_Age + Modified_Income + BinningAge_woe + Profession_woe + 
                      Type_of_Residence_woe + BinningCurrentCompany_woe + `BinningAge(14,25]` + 
                      `BinningAge(35,45]` +  `BinningIncome(10,20]` + `BinningIncome(20,30]` + 
                      `BinningIncome(30,40]` +  `BinningCurentResidence(0,20]` + `BinningCurentResidence(20,40]` + 
                      `BinningCurentResidence(40,60]` + `BinningCurentResidence(60,80]` + `BinningCurrentCompany(15,30]` + 
                      `BinningCurrentCompany(30,45]` +  Modified_GenderF +   
                      Modified_No_of_dependents1 + Modified_No_of_dependents2 + Modified_No_of_dependents3 + Modified_EducationBachelor + Modified_EducationMasters + 
                      Modified_EducationOthers + Modified_EducationPhd + 
                       Modified_ProfessionSAL +  `Modified_Residence_TypeCompany provided` + 
                      `Modified_Residence_TypeLiving with Parents` + Modified_Residence_TypeOwned 
                    , family = "binomial", data = training[, -1])

summary(Reg_Model_18)
vif(Reg_Model_18)

# Removing Modified_GenderF and Modified_Residence_TypeOwned variable
Reg_Model_19 <- glm(formula = Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_90_DPD_or_worse_in_last_12_months_woe + 
                      Binned_CC_Utilization_woe +  Binned_No_of_trades_opened_in_last_6_months_woe +  
                      No_of_PL_trades_opened_in_last_12_months_woe + 
                      Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                      Modified_Presence_of_open_home_loan_woe  + 
                      binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                      Modified_Age + Modified_Income + BinningAge_woe + Profession_woe + 
                      Type_of_Residence_woe + BinningCurrentCompany_woe + `BinningAge(14,25]` + 
                      `BinningAge(35,45]` +  `BinningIncome(10,20]` + `BinningIncome(20,30]` + 
                      `BinningIncome(30,40]` +  `BinningCurentResidence(0,20]` + `BinningCurentResidence(20,40]` + 
                      `BinningCurentResidence(40,60]` + `BinningCurentResidence(60,80]` + `BinningCurrentCompany(15,30]` + 
                      `BinningCurrentCompany(30,45]` +  Modified_No_of_dependents1 + 
                      Modified_No_of_dependents2 + Modified_No_of_dependents3 + Modified_EducationBachelor + Modified_EducationMasters + 
                      Modified_EducationOthers + Modified_EducationPhd + 
                      Modified_ProfessionSAL + `Modified_Residence_TypeCompany provided` + `Modified_Residence_TypeLiving with Parents` 
                    , family = "binomial", data = training[, -1])

summary(Reg_Model_19)
vif(Reg_Model_19)

# Removing ModifiedEducation_Others and Modified_EducationPhd   variable
Reg_Model_20 <- glm(formula = Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_90_DPD_or_worse_in_last_12_months_woe + 
                      Binned_CC_Utilization_woe + Binned_No_of_trades_opened_in_last_6_months_woe +  
                      No_of_PL_trades_opened_in_last_12_months_woe + 
                      Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                      Modified_Presence_of_open_home_loan_woe  + 
                      binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                      Modified_Age + Modified_Income + BinningAge_woe + Profession_woe + 
                      Type_of_Residence_woe + BinningCurrentCompany_woe + `BinningAge(14,25]` + 
                      `BinningAge(35,45]` +  `BinningIncome(10,20]` + `BinningIncome(20,30]` + 
                      `BinningIncome(30,40]` +  `BinningCurentResidence(0,20]` + `BinningCurentResidence(20,40]` + 
                      `BinningCurentResidence(40,60]` + `BinningCurentResidence(60,80]` + `BinningCurrentCompany(15,30]` + 
                      `BinningCurrentCompany(30,45]` + Modified_No_of_dependents1 + 
                      Modified_No_of_dependents2 + Modified_No_of_dependents3 + Modified_EducationBachelor + Modified_EducationMasters + 
                      Modified_ProfessionSAL + `Modified_Residence_TypeCompany provided` +`Modified_Residence_TypeLiving with Parents` 
                    , family = "binomial", data = training[, -1])

summary(Reg_Model_20)
vif(Reg_Model_20)


# Removing ModifiedEducation_Others and Modified_EducationPhd   variable
Reg_Model_21 <- glm(formula = Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_30_DPD_or_worse_in_last_6_months_woe + No_of_times_90_DPD_or_worse_in_last_12_months_woe + 
                      Binned_CC_Utilization_woe + Binned_No_of_trades_opened_in_last_6_months_woe +  
                      No_of_PL_trades_opened_in_last_12_months_woe + 
                      Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                      Modified_Presence_of_open_home_loan_woe  + 
                      binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                      Modified_Age + Modified_Income + BinningAge_woe + Profession_woe + 
                      Type_of_Residence_woe + BinningCurrentCompany_woe + `BinningAge(14,25]` + 
                      `BinningAge(35,45]` +  `BinningIncome(10,20]` + `BinningIncome(20,30]` + 
                      `BinningIncome(30,40]` +  `BinningCurentResidence(0,20]` + `BinningCurentResidence(20,40]` + 
                      `BinningCurentResidence(40,60]` + `BinningCurentResidence(60,80]` + `BinningCurrentCompany(15,30]` + 
                      `BinningCurrentCompany(30,45]` +  Modified_No_of_dependents1 + 
                      Modified_No_of_dependents2 + Modified_No_of_dependents3 + Modified_EducationBachelor + Modified_EducationMasters + 
                      Modified_ProfessionSAL + `Modified_Residence_TypeCompany provided` +`Modified_Residence_TypeLiving with Parents` 
                    , family = "binomial", data = training[, -1])

summary(Reg_Model_21)
vif(Reg_Model_21)


# Removing  Modified_No_of_dependents3, BinningCurentResidence(40,60]`variable
Reg_Model_22 <- glm(formula = Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_90_DPD_or_worse_in_last_12_months_woe + 
                      Binned_CC_Utilization_woe + Binned_No_of_trades_opened_in_last_6_months_woe +  
                      No_of_PL_trades_opened_in_last_12_months_woe + 
                      Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                      Modified_Presence_of_open_home_loan_woe  + 
                      binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                      Modified_Age + Modified_Income + BinningAge_woe + Profession_woe + 
                      Type_of_Residence_woe + BinningCurrentCompany_woe + `BinningAge(14,25]` + 
                      `BinningAge(35,45]` +  `BinningIncome(10,20]` + `BinningIncome(20,30]` + 
                      `BinningIncome(30,40]` +  `BinningCurentResidence(0,20]` +
                      `BinningCurentResidence(40,60]` + `BinningCurentResidence(60,80]` + `BinningCurrentCompany(15,30]` + 
                      `BinningCurrentCompany(30,45]` + Modified_No_of_dependents1 + 
                      Modified_No_of_dependents2 +  Modified_EducationBachelor + Modified_EducationMasters + 
                      Modified_ProfessionSAL + `Modified_Residence_TypeCompany provided` +`Modified_Residence_TypeLiving with Parents` 
                    , family = "binomial", data = training[, -1])

summary(Reg_Model_22)
vif(Reg_Model_22)

# Removing  Modified_EducationBachelor, `BinningAge(35,45]` variable
Reg_Model_23 <- glm(formula = Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_90_DPD_or_worse_in_last_12_months_woe + 
                      Binned_CC_Utilization_woe +  Binned_No_of_trades_opened_in_last_6_months_woe +  
                      No_of_PL_trades_opened_in_last_12_months_woe + 
                      Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                      Modified_Presence_of_open_home_loan_woe  + 
                      binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                      Modified_Age + Modified_Income + BinningAge_woe + Profession_woe + 
                      Type_of_Residence_woe +  BinningCurrentCompany_woe + `BinningAge(14,25]` + 
                       `BinningIncome(10,20]` + `BinningIncome(20,30]` + 
                      `BinningIncome(30,40]` +  `BinningCurentResidence(0,20]` +
                      `BinningCurentResidence(40,60]` + `BinningCurentResidence(60,80]` + `BinningCurrentCompany(15,30]` + 
                      `BinningCurrentCompany(30,45]` + Modified_No_of_dependents1 + 
                      Modified_No_of_dependents2 + Modified_EducationMasters + 
                      Modified_ProfessionSAL + `Modified_Residence_TypeCompany provided` +`Modified_Residence_TypeLiving with Parents` 
                    , family = "binomial", data = training[, -1])

summary(Reg_Model_23)
vif(Reg_Model_23)

# Removing BinningIncome(20,30], Modified_No_of_dependents1 variable
Reg_Model_24 <- glm(formula = Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_90_DPD_or_worse_in_last_12_months_woe + 
                      Binned_CC_Utilization_woe + Binned_No_of_trades_opened_in_last_6_months_woe +  
                      No_of_PL_trades_opened_in_last_12_months_woe + 
                      Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                      Modified_Presence_of_open_home_loan_woe  + 
                      binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                      Modified_Age + Modified_Income + BinningAge_woe + Profession_woe + 
                      Type_of_Residence_woe +  BinningCurrentCompany_woe + `BinningAge(14,25]` + 
                      `BinningIncome(10,20]` + `BinningIncome(30,40]` +  `BinningCurentResidence(0,20]` +
                       + `BinningCurentResidence(60,80]` + `BinningCurrentCompany(15,30]` + 
                      `BinningCurrentCompany(30,45]` + Modified_No_of_dependents2 + Modified_EducationMasters + 
                      Modified_ProfessionSAL + `Modified_Residence_TypeCompany provided` +`Modified_Residence_TypeLiving with Parents` 
                    , family = "binomial", data = training[, -1])

summary(Reg_Model_24)
vif(Reg_Model_24)

# Removing Binned_No_of_Inquiries_in_last_6_months_woe,Type_of_Residence_woe,Modified_EducationMasters,`BinningCurrentCompany(30,45]`  variable
Reg_Model_25 <- glm(formula = Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_90_DPD_or_worse_in_last_12_months_woe + 
                      Binned_CC_Utilization_woe + Binned_No_of_trades_opened_in_last_6_months_woe +  
                      No_of_PL_trades_opened_in_last_12_months_woe + 
                      Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                      Modified_Presence_of_open_home_loan_woe  + 
                      binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                      Modified_Age + Modified_Income + BinningAge_woe + Profession_woe + 
                      Type_of_Residence_woe + BinningCurrentCompany_woe + `BinningAge(14,25]` + 
                      `BinningIncome(10,20]` + `BinningIncome(30,40]` +  `BinningCurentResidence(0,20]` +
                      + `BinningCurentResidence(60,80]` + `BinningCurrentCompany(15,30]` + 
                      Modified_No_of_dependents2 + Modified_EducationMasters + 
                      Modified_ProfessionSAL + `Modified_Residence_TypeCompany provided` +`Modified_Residence_TypeLiving with Parents` 
                    , family = "binomial", data = training[, -1])

summary(Reg_Model_25)
vif(Reg_Model_25)

#Removing Binned_No_of_Inquiries_in_last_6_months_woe,Modified_ProfessionSAL,`BinningCurentResidence(60,80]` variable
Reg_Model_26 <- glm(formula = Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_90_DPD_or_worse_in_last_12_months_woe + 
                      Binned_CC_Utilization_woe + Binned_No_of_trades_opened_in_last_6_months_woe +  
                      No_of_PL_trades_opened_in_last_12_months_woe + 
                       Binned_No_of_Inquiries_in_last_12_months_woe + 
                      Modified_Presence_of_open_home_loan_woe  + 
                      binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                      Modified_Age + Modified_Income + BinningAge_woe + Profession_woe + 
                      Type_of_Residence_woe + BinningCurrentCompany_woe + `BinningAge(14,25]` + 
                      `BinningIncome(10,20]` +`BinningIncome(30,40]` +  `BinningCurentResidence(0,20]` +
                       `BinningCurrentCompany(15,30]` + Modified_No_of_dependents2 + Modified_EducationMasters + 
                       `Modified_Residence_TypeCompany provided` +`Modified_Residence_TypeLiving with Parents` 
                    , family = "binomial", data = training[, -1])

summary(Reg_Model_26)
vif(Reg_Model_26)

#Removing Type_of_Residence_woe, Modified_EducationMasters variable
Reg_Model_27 <- glm(formula = Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_90_DPD_or_worse_in_last_12_months_woe + 
                      Binned_CC_Utilization_woe + Binned_No_of_trades_opened_in_last_6_months_woe +  
                      No_of_PL_trades_opened_in_last_12_months_woe + 
                      Binned_No_of_Inquiries_in_last_12_months_woe + 
                      Modified_Presence_of_open_home_loan_woe  + 
                      binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                      Modified_Age + Modified_Income + BinningAge_woe + Profession_woe + 
                      BinningCurrentCompany_woe + `BinningAge(14,25]` + 
                      `BinningIncome(10,20]` +`BinningIncome(30,40]` +  `BinningCurentResidence(0,20]` +
                      `BinningCurrentCompany(15,30]` + Modified_No_of_dependents2 + 
                      `Modified_Residence_TypeCompany provided` +`Modified_Residence_TypeLiving with Parents` 
                    , family = "binomial", data = training[, -1])

summary(Reg_Model_27)
vif(Reg_Model_27)

#Removing Presence_of_open_auto_loan_woe, `BinningCurrentCompany(15,30]`, Modified_Age  variable
Reg_Model_28 <- glm(formula = Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_30_DPD_or_worse_in_last_6_months_woe + No_of_times_90_DPD_or_worse_in_last_12_months_woe + 
                      Binned_CC_Utilization_woe + Binned_No_of_trades_opened_in_last_6_months_woe +  No_of_PL_trades_opened_in_last_12_months_woe + 
                      Binned_No_of_Inquiries_in_last_12_months_woe +  Modified_Presence_of_open_home_loan_woe  + 
                      binning_Total_No_of_Trades_woe +  Modified_Income + BinningAge_woe + Profession_woe + 
                      BinningCurrentCompany_woe + `BinningAge(14,25]` + `BinningIncome(10,20]` +`BinningIncome(30,40]` +  `BinningCurentResidence(0,20]` +
                       + Modified_No_of_dependents2 + `Modified_Residence_TypeCompany provided` +`Modified_Residence_TypeLiving with Parents` 
                    , family = "binomial", data = training[, -1])

summary(Reg_Model_28)
vif(Reg_Model_28)

#Removing Modified_Presence_of_open_home_loan_woe , binning_Total_No_of_Trades_woe , Modified_Income,`Modified_Residence_TypeCompany provided` variable
Reg_Model_29 <- glm(formula = Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_30_DPD_or_worse_in_last_6_months_woe + No_of_times_90_DPD_or_worse_in_last_12_months_woe + 
                      Binned_CC_Utilization_woe + Binned_No_of_trades_opened_in_last_6_months_woe +  No_of_PL_trades_opened_in_last_12_months_woe + 
                      Binned_No_of_Inquiries_in_last_12_months_woe +BinningAge_woe + Profession_woe + BinningCurrentCompany_woe + `BinningAge(14,25]` + `BinningIncome(10,20]` +`BinningIncome(30,40]` +  `BinningCurentResidence(0,20]` +
                      + Modified_No_of_dependents2  +`Modified_Residence_TypeLiving with Parents` 
                    , family = "binomial", data = training[, -1])

summary(Reg_Model_29)
vif(Reg_Model_29)

#Removing Binned_No_of_trades_opened_in_last_6_months_woe , BinningCurrentCompany_woe , BinningAge_woe  variable
Reg_Model_30 <- glm(formula = Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_30_DPD_or_worse_in_last_6_months_woe + No_of_times_90_DPD_or_worse_in_last_12_months_woe + 
                      Binned_CC_Utilization_woe +  No_of_PL_trades_opened_in_last_12_months_woe + 
                      Binned_No_of_Inquiries_in_last_12_months_woe  + Profession_woe  + `BinningAge(14,25]` + `BinningIncome(10,20]` +`BinningIncome(30,40]` +  `BinningCurentResidence(0,20]` +
                      + Modified_No_of_dependents2  +`Modified_Residence_TypeLiving with Parents` 
                    , family = "binomial", data = training[, -1])

summary(Reg_Model_30)
vif(Reg_Model_30)

#Removing `BinningAge(14,25]` , `BinningIncome(30,40]` variable
Reg_Model_31 <- glm(formula = Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_30_DPD_or_worse_in_last_6_months_woe + No_of_times_90_DPD_or_worse_in_last_12_months_woe + 
                      Binned_CC_Utilization_woe +  No_of_PL_trades_opened_in_last_12_months_woe + 
                      Binned_No_of_Inquiries_in_last_12_months_woe  + Profession_woe  + `BinningIncome(10,20]` + `BinningCurentResidence(0,20]` +
                      + Modified_No_of_dependents2  +`Modified_Residence_TypeLiving with Parents` 
                    , family = "binomial", data = training[, -1])

summary(Reg_Model_31)
vif(Reg_Model_31)

#Removing `Modified_Residence_TypeLiving with Parents` , `BinningIncome(10,20]` variable
Reg_Model_32 <- glm(formula = Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_30_DPD_or_worse_in_last_6_months_woe + No_of_times_90_DPD_or_worse_in_last_12_months_woe + 
                      Binned_CC_Utilization_woe +  No_of_PL_trades_opened_in_last_12_months_woe + 
                      Binned_No_of_Inquiries_in_last_12_months_woe  + Profession_woe  + `BinningCurentResidence(0,20]` +
                      + Modified_No_of_dependents2
                    , family = "binomial", data = training[, -1])

summary(Reg_Model_32)
vif(Reg_Model_32)

#Removing Profession_WOE variable
Reg_Model_33 <- glm(formula = Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_30_DPD_or_worse_in_last_6_months_woe + No_of_times_90_DPD_or_worse_in_last_12_months_woe + 
                      Binned_CC_Utilization_woe +  No_of_PL_trades_opened_in_last_12_months_woe + 
                      Binned_No_of_Inquiries_in_last_12_months_woe  +  `BinningCurentResidence(0,20]` +
                      + Modified_No_of_dependents2
                    , family = "binomial", data = training[, -1])

summary(Reg_Model_33)
vif(Reg_Model_33)

#Removing No_of_times_90_DPD_or_worse_in_last_12_months_woe variable
Reg_Model_34 <- glm(formula = Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_30_DPD_or_worse_in_last_6_months_woe +  
                      Binned_CC_Utilization_woe +  No_of_PL_trades_opened_in_last_12_months_woe + 
                      Binned_No_of_Inquiries_in_last_12_months_woe  +  `BinningCurentResidence(0,20]` +
                      + Modified_No_of_dependents2
                    , family = "binomial", data = training[, -1])

summary(Reg_Model_34)
vif(Reg_Model_34)

#Removing No_of_times_90_DPD_or_worse_in_last_6_months_woe variable
Reg_Model_35 <- glm(formula = Performance.Tag ~   No_of_times_30_DPD_or_worse_in_last_6_months_woe +  
                      Binned_CC_Utilization_woe +  No_of_PL_trades_opened_in_last_12_months_woe + 
                      Binned_No_of_Inquiries_in_last_12_months_woe  +  `BinningCurentResidence(0,20]` +
                      + Modified_No_of_dependents2
                    , family = "binomial", data = training[, -1])

summary(Reg_Model_35)
vif(Reg_Model_35)

# Model Evaluation: Logistic Regression Combined without oversampling/undersampling
#-------------------------------------------------------------------------------------
##------------------------- Model Evaluation - Logistic Regression Final --------------------
# Predicting probabilities of responding for the test data
predictions_logit_Final <- predict(Reg_Model_35, newdata = testing[,-c( 1,19)], type = "response")
summary(predictions_logit_Final)

# Let's use the probability cutoff of 50%.
predicted_response_Final <- factor(ifelse(predictions_logit_Final >= 0.05, "Yes", "No"))
summary(predicted_response_Final)

# Creating confusion matrix for identifying the model evaluation.
testing$Performance.Tag <- factor(ifelse(testing$Performance.Tag == 1, "Yes","No"))

conf_1 <- confusionMatrix(predicted_response_Final, testing$Performance.Tag, positive = "Yes")
conf_1

#---------------------------------------------------------    
# Let's find out the optimal probalility cutoff 
perform_fn <- function(cutoff) 
{
  predicted_response_Final <- factor(ifelse(predictions_logit_Final >= cutoff, "Yes", "No"))
  conf_1 <- confusionMatrix(predicted_response_Final, testing$Performance.Tag, positive = "Yes")
  acc <- conf_1$overall[1]
  sens <- conf_1$byClass[1]
  spec <- conf_1$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    
# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.
s = seq(.01,.09,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 
str(testing$Performance.Tag)
str(predicted_response_Final)
#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

#---------------------------------------------------------    

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.03)]
cutoff

# Let's choose a cutoff for final model
predicted_response_Final <- factor(ifelse(predictions_logit_Final >= 0.04555556, "Yes", "No"))

conf_final_1 <- confusionMatrix(predicted_response_Final, testing$Performance.Tag, positive = "Yes")
acc_1 <- conf_final_1$overall[1]
sens_1 <- conf_final_1$byClass[1]
spec_1 <- conf_final_1$byClass[2]

acc_1   #0.6418416
sens_1  #0.6368778
spec_1  #0.6420602

##################### Balancing the dataset using oversampling and undersampling methods########3
training_Final <- training
training_Final <- training_Final[,-1]

#Changing Column names for training dataset 
colnames(training_Final)[colnames(training_Final) == "BinningAge(14,25]"] <- "BinningAge_fourteen_twentyfive"
colnames(training_Final)[colnames(training_Final) == "BinningAge(25,35]"] <- "BinningAge_Twentyfive_Thirtyfive"
colnames(training_Final)[colnames(training_Final) == "BinningAge(35,45]"] <- "BinningAge_ThirtyFive_FortyFive"
colnames(training_Final)[colnames(training_Final) == "BinningAge(45,55]"] <- "BinningAge_FortyFive_FiftyFive"
colnames(training_Final)[colnames(training_Final) == "BinningAge(55,65]"] <- "BinningAge_FiftyFive_SixtyFive"
colnames(training_Final)[colnames(training_Final) == "BinningIncome[0,10]"] <- "BinningIncome_Zero_Ten"
colnames(training_Final)[colnames(training_Final) == "BinningIncome(10,20]"] <- "BinningIncome_Ten_Twenty"
colnames(training_Final)[colnames(training_Final) == "BinningIncome(20,30]"] <- "BinningIncome_Twenty_Thirty"
colnames(training_Final)[colnames(training_Final) == "BinningIncome(30,40]"] <- "BinningIncome_Thirty_Forty"
colnames(training_Final)[colnames(training_Final) == "BinningIncome(40,50]"] <- "BinningIncome_Forty_Fifty"
colnames(training_Final)[colnames(training_Final) == "BinningIncome(50,60]"] <- "BinningIncome_Fifty_Sixty"
colnames(training_Final)[colnames(training_Final) == "BinningCurentResidence(0,20]"] <- "BinningCurentResidence_Zero_Twenty"
colnames(training_Final)[colnames(training_Final) == "BinningCurentResidence(20,40]"] <- "BinningCurentResidence_Twenty_Forty"
colnames(training_Final)[colnames(training_Final) == "BinningCurentResidence(40,60]"] <- "BinningCurentResidence_Forty_Sixty"
colnames(training_Final)[colnames(training_Final) == "BinningCurentResidence(60,80]"] <- "BinningCurentResidence_Sixty_Eighty"
colnames(training_Final)[colnames(training_Final) == "BinningCurentResidence(80,100]"] <- "BinningCurentResidence_Eighty_Hundred"
colnames(training_Final)[colnames(training_Final) == "BinningCurentResidence(100,130]"] <- "BinningCurentResidence_Hundred_oneHundredThirty"
colnames(training_Final)[colnames(training_Final) == "BinningCurrentCompany(0,15]"] <- "BinningCurrentCompanyZero_Fifteen"
colnames(training_Final)[colnames(training_Final) == "BinningCurrentCompany(15,30]"] <- "BinningCurrentCompany_fifteen_Thirty"
colnames(training_Final)[colnames(training_Final) == "BinningCurrentCompany(30,45]"] <- "BinningCurrentCompany_thirty_fortyfive"
colnames(training_Final)[colnames(training_Final) == "BinningCurrentCompany(45,60]"] <- "BinningCurrentCompany_fortyfive_sixty"
colnames(training_Final)[colnames(training_Final) == "BinningCurrentCompany(60,75]"] <- "BinningCurrentCompany_sixty_seventyfive"
colnames(training_Final)[colnames(training_Final) == "Modified_Residence_TypeCompany provided"] <- "Modified_Residence_TypeCompanyprovided"
colnames(training_Final)[colnames(training_Final) == "Modified_Residence_TypeLiving with Parents"] <- "Modified_Residence_TypeLivingwithParents"
colnames(training_Final)[colnames(training_Final) == "Performance.Tag"] <- "PerformanceTag"
colnames(training_Final)[colnames(training_Final) == "Modified_No_of_dependents1"] <- "Modified_No_of_dependents_One"
colnames(training_Final)[colnames(training_Final) == "Modified_No_of_dependents2"] <- "Modified_No_of_dependents_Two"
colnames(training_Final)[colnames(training_Final) == "Modified_No_of_dependents3"] <- "Modified_No_of_dependents_Three"
colnames(training_Final)[colnames(training_Final) == "Modified_No_of_dependents4"] <- "Modified_No_of_dependents_Four"
colnames(training_Final)[colnames(training_Final) == "Modified_No_of_dependents5"] <- "Modified_No_of_dependents_Five"

training_Final <- as.data.frame(training_Final)

#Both (Under and Over)Sampling 
#*****************************************************************************************#
# method over instructs the algorithm to perform over sampling. 
# N refers to number of observations in the resulting balanced set.
#*****************************************************************************************#
data_balanced_sampled <- ovun.sample(PerformanceTag ~ ., data = training_Final, method = "both",p=0.5,N=97814,seed=1)$data
table(data_balanced_sampled$PerformanceTag)

#########################################################
#       Performing Modelling on Balanced Data set       #
#########################################################

### Model 1: Logistic Regression
Model_1_Final <- glm(PerformanceTag ~ ., family = "binomial", data = data_balanced_sampled)
summary(Reg_Model_1)
cor(Combined_Dataframe)

#Using stepwise algorithm for removing insignificant variables 
#Model_2_Final <- stepAIC(Model_1_Final, direction = "both")
#summary(Model_2_Final)

Model_3_Final<- glm(formula = PerformanceTag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_60_DPD_or_worse_in_last_6_months_woe + No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_90_DPD_or_worse_in_last_12_months_woe + No_of_times_60_DPD_or_worse_in_last_12_months_woe + 
                      No_of_times_30_DPD_or_worse_in_last_12_months_woe + Binned_CC_Utilization_woe + 
                      Binned_No_of_trades_opened_in_last_6_months_woe + Binned_No_of_trades_opened_in_last_12_months_woe + 
                      No_of_PL_trades_opened_in_last_6_months_woe + No_of_PL_trades_opened_in_last_12_months_woe + 
                      Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                      Modified_Presence_of_open_home_loan_woe + Binning_outstanding_Balance_woe + 
                      binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                      Modified_Age + Modified_Income + BinningAge_woe + Profession_woe + 
                      Education_woe + Gender_woe + Marital_Status_woe + Modified_No_of_dependents_woe + 
                      BinningIncome_woe + Type_of_Residence_woe + BinningCurentResidence_woe + 
                      BinningCurrentCompany_woe + BinningAge_fourteen_twentyfive + 
                      BinningAge_Twentyfive_Thirtyfive + BinningAge_ThirtyFive_FortyFive + 
                      BinningIncome_Zero_Ten + BinningIncome_Ten_Twenty + BinningIncome_Twenty_Thirty + 
                      BinningIncome_Thirty_Forty + 
                      BinningCurentResidence_Zero_Twenty + 
                      BinningCurentResidence_Twenty_Forty + BinningCurentResidence_Forty_Sixty + 
                      BinningCurentResidence_Sixty_Eighty + 
                      BinningCurrentCompanyZero_Fifteen + 
                      BinningCurrentCompany_fifteen_Thirty + BinningCurrentCompany_thirty_fortyfive + 
                      Modified_GenderF +  Modified_Marital_StatusMarried + 
                      Modified_Marital_StatusOther + 
                      Modified_No_of_dependents_One + Modified_No_of_dependents_Two + 
                      Modified_No_of_dependents_Three +  
                      Modified_EducationBachelor + 
                      Modified_EducationMasters + Modified_EducationOthers + Modified_EducationPhd + 
                      Modified_ProfessionOthers + 
                      Modified_ProfessionSE + Modified_Residence_TypeCompanyprovided + 
                      Modified_Residence_TypeLivingwithParents, family = "binomial", 
                    data = data_balanced_sampled)

summary(Model_3_Final)
vif(Model_3_Final)

#  Removing BinningAge_ThirtyFive_FortyFive and   BinningIncome_Twenty_Thirty variable
Model_4_Final<- glm(formula = PerformanceTag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_60_DPD_or_worse_in_last_6_months_woe + No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_90_DPD_or_worse_in_last_12_months_woe + No_of_times_60_DPD_or_worse_in_last_12_months_woe + 
                      No_of_times_30_DPD_or_worse_in_last_12_months_woe + Binned_CC_Utilization_woe + 
                      Binned_No_of_trades_opened_in_last_6_months_woe + Binned_No_of_trades_opened_in_last_12_months_woe + 
                      No_of_PL_trades_opened_in_last_6_months_woe + No_of_PL_trades_opened_in_last_12_months_woe + 
                      Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                      Modified_Presence_of_open_home_loan_woe + Binning_outstanding_Balance_woe + 
                      binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                      Modified_Age + Modified_Income + BinningAge_woe + Profession_woe + 
                      Education_woe + Gender_woe + Marital_Status_woe + Modified_No_of_dependents_woe + 
                      BinningIncome_woe + Type_of_Residence_woe + BinningCurentResidence_woe + 
                      BinningCurrentCompany_woe + BinningAge_fourteen_twentyfive + 
                      BinningAge_Twentyfive_Thirtyfive +  
                      BinningIncome_Zero_Ten + BinningIncome_Ten_Twenty +  
                      BinningIncome_Thirty_Forty + 
                      BinningCurentResidence_Zero_Twenty + 
                      BinningCurentResidence_Twenty_Forty + BinningCurentResidence_Forty_Sixty + 
                      BinningCurentResidence_Sixty_Eighty + 
                      BinningCurrentCompanyZero_Fifteen + 
                      BinningCurrentCompany_fifteen_Thirty + BinningCurrentCompany_thirty_fortyfive + 
                      Modified_GenderF +  Modified_Marital_StatusMarried + 
                      Modified_Marital_StatusOther + 
                      Modified_No_of_dependents_One + Modified_No_of_dependents_Two + 
                      Modified_No_of_dependents_Three +  
                      Modified_EducationBachelor + 
                      Modified_EducationMasters + Modified_EducationOthers + Modified_EducationPhd + 
                      Modified_ProfessionOthers + 
                      Modified_ProfessionSE + Modified_Residence_TypeCompanyprovided + 
                      Modified_Residence_TypeLivingwithParents, family = "binomial", 
                    data = data_balanced_sampled)

summary(Model_4_Final)
vif(Model_4_Final)

#  Removing Modified_Income, Modified_GenderF,Profession_WOE variable
Model_5_Final<- glm(formula = PerformanceTag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_60_DPD_or_worse_in_last_6_months_woe + No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_90_DPD_or_worse_in_last_12_months_woe + No_of_times_60_DPD_or_worse_in_last_12_months_woe + 
                      No_of_times_30_DPD_or_worse_in_last_12_months_woe + Binned_CC_Utilization_woe + 
                      Binned_No_of_trades_opened_in_last_6_months_woe + Binned_No_of_trades_opened_in_last_12_months_woe + 
                      No_of_PL_trades_opened_in_last_6_months_woe + No_of_PL_trades_opened_in_last_12_months_woe + 
                      Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                      Modified_Presence_of_open_home_loan_woe + Binning_outstanding_Balance_woe + 
                      binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                      Modified_Age +  BinningAge_woe + 
                      Education_woe + Gender_woe + Marital_Status_woe + Modified_No_of_dependents_woe + 
                      BinningIncome_woe + Type_of_Residence_woe + BinningCurentResidence_woe + 
                      BinningCurrentCompany_woe + BinningAge_fourteen_twentyfive + 
                      BinningAge_Twentyfive_Thirtyfive +BinningIncome_Zero_Ten + BinningIncome_Ten_Twenty +  
                      BinningIncome_Thirty_Forty + BinningCurentResidence_Zero_Twenty + BinningCurentResidence_Twenty_Forty + BinningCurentResidence_Forty_Sixty + 
                      BinningCurentResidence_Sixty_Eighty + BinningCurrentCompanyZero_Fifteen + BinningCurrentCompany_fifteen_Thirty + BinningCurrentCompany_thirty_fortyfive + 
                      Modified_Marital_StatusMarried + Modified_No_of_dependents_One + Modified_No_of_dependents_Two + 
                      Modified_No_of_dependents_Three +  Modified_EducationBachelor + Modified_EducationMasters + Modified_EducationOthers + Modified_EducationPhd + 
                      Modified_ProfessionOthers + Modified_ProfessionSE + Modified_Residence_TypeCompanyprovided + 
                      Modified_Residence_TypeLivingwithParents, family = "binomial", data = data_balanced_sampled)

summary(Model_5_Final)
vif(Model_5_Final)

#  Removing MaritalStatus_WOE variable
Model_6_Final<- glm(formula = PerformanceTag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_60_DPD_or_worse_in_last_6_months_woe + No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_90_DPD_or_worse_in_last_12_months_woe + No_of_times_60_DPD_or_worse_in_last_12_months_woe + 
                      No_of_times_30_DPD_or_worse_in_last_12_months_woe + Binned_CC_Utilization_woe + 
                      Binned_No_of_trades_opened_in_last_6_months_woe + Binned_No_of_trades_opened_in_last_12_months_woe + 
                      No_of_PL_trades_opened_in_last_6_months_woe + No_of_PL_trades_opened_in_last_12_months_woe + 
                      Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                      Modified_Presence_of_open_home_loan_woe + Binning_outstanding_Balance_woe + 
                      binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                      Modified_Age +  BinningAge_woe + 
                      Education_woe + Gender_woe +  Modified_No_of_dependents_woe + 
                      BinningIncome_woe + Type_of_Residence_woe + BinningCurentResidence_woe + 
                      BinningCurrentCompany_woe + BinningAge_fourteen_twentyfive + 
                      BinningAge_Twentyfive_Thirtyfive +BinningIncome_Zero_Ten + BinningIncome_Ten_Twenty +  
                      BinningIncome_Thirty_Forty + BinningCurentResidence_Zero_Twenty + BinningCurentResidence_Twenty_Forty + BinningCurentResidence_Forty_Sixty + 
                      BinningCurentResidence_Sixty_Eighty + BinningCurrentCompanyZero_Fifteen + BinningCurrentCompany_fifteen_Thirty + BinningCurrentCompany_thirty_fortyfive + 
                      Modified_Marital_StatusMarried + Modified_No_of_dependents_One + Modified_No_of_dependents_Two + 
                      Modified_No_of_dependents_Three +  Modified_EducationBachelor + Modified_EducationMasters + Modified_EducationOthers + Modified_EducationPhd + 
                      Modified_ProfessionOthers + Modified_ProfessionSE + Modified_Residence_TypeCompanyprovided + 
                      Modified_Residence_TypeLivingwithParents, family = "binomial", data = data_balanced_sampled)

summary(Model_6_Final)
vif(Model_6_Final)

#  Removing BinningCurentResidence_woe, BinningCurentResidence_Twenty_Forty  variable
Model_7_Final<- glm(formula = PerformanceTag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_60_DPD_or_worse_in_last_6_months_woe + No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_90_DPD_or_worse_in_last_12_months_woe + No_of_times_60_DPD_or_worse_in_last_12_months_woe + 
                      No_of_times_30_DPD_or_worse_in_last_12_months_woe + Binned_CC_Utilization_woe + 
                      Binned_No_of_trades_opened_in_last_6_months_woe + Binned_No_of_trades_opened_in_last_12_months_woe + 
                      No_of_PL_trades_opened_in_last_6_months_woe + No_of_PL_trades_opened_in_last_12_months_woe + 
                      Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                      Modified_Presence_of_open_home_loan_woe + Binning_outstanding_Balance_woe + 
                      binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                      Modified_Age +  BinningAge_woe + 
                      Education_woe + Gender_woe +  Modified_No_of_dependents_woe + 
                      BinningIncome_woe + Type_of_Residence_woe +  
                      BinningCurrentCompany_woe + BinningAge_fourteen_twentyfive + 
                      BinningAge_Twentyfive_Thirtyfive +BinningIncome_Zero_Ten + BinningIncome_Ten_Twenty +  
                      BinningIncome_Thirty_Forty + BinningCurentResidence_Zero_Twenty +  BinningCurentResidence_Forty_Sixty + 
                      BinningCurentResidence_Sixty_Eighty + BinningCurrentCompanyZero_Fifteen + BinningCurrentCompany_fifteen_Thirty + BinningCurrentCompany_thirty_fortyfive + 
                      Modified_Marital_StatusMarried + Modified_No_of_dependents_One + Modified_No_of_dependents_Two + 
                      Modified_No_of_dependents_Three +  Modified_EducationBachelor + Modified_EducationMasters + Modified_EducationOthers + Modified_EducationPhd + 
                      Modified_ProfessionOthers + Modified_ProfessionSE + Modified_Residence_TypeCompanyprovided + 
                      Modified_Residence_TypeLivingwithParents, family = "binomial", data = data_balanced_sampled)

summary(Model_7_Final)
vif(Model_7_Final)

#  Removing Modified_No_of_dependents_woe, Modified_No_of_Dependents_Two, Modified_No_of_Dependents_Three variable
Model_8_Final<- glm(formula = PerformanceTag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_60_DPD_or_worse_in_last_6_months_woe + No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_90_DPD_or_worse_in_last_12_months_woe + No_of_times_60_DPD_or_worse_in_last_12_months_woe + 
                      No_of_times_30_DPD_or_worse_in_last_12_months_woe + Binned_CC_Utilization_woe + 
                      Binned_No_of_trades_opened_in_last_6_months_woe + Binned_No_of_trades_opened_in_last_12_months_woe + 
                      No_of_PL_trades_opened_in_last_6_months_woe + No_of_PL_trades_opened_in_last_12_months_woe + 
                      Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                      Modified_Presence_of_open_home_loan_woe + Binning_outstanding_Balance_woe + 
                      binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                      Modified_Age +  BinningAge_woe + 
                      Education_woe + Gender_woe +  
                      BinningIncome_woe + Type_of_Residence_woe +  
                      BinningCurrentCompany_woe + BinningAge_fourteen_twentyfive + 
                      BinningAge_Twentyfive_Thirtyfive +BinningIncome_Zero_Ten + BinningIncome_Ten_Twenty +  
                      BinningIncome_Thirty_Forty + BinningCurentResidence_Zero_Twenty +  BinningCurentResidence_Forty_Sixty + 
                      BinningCurentResidence_Sixty_Eighty + BinningCurrentCompanyZero_Fifteen + BinningCurrentCompany_fifteen_Thirty + BinningCurrentCompany_thirty_fortyfive + 
                      Modified_Marital_StatusMarried + Modified_No_of_dependents_One +  
                      Modified_EducationBachelor + Modified_EducationMasters + Modified_EducationOthers + Modified_EducationPhd + 
                      Modified_ProfessionOthers + Modified_ProfessionSE + Modified_Residence_TypeCompanyprovided + 
                      Modified_Residence_TypeLivingwithParents, family = "binomial", data = data_balanced_sampled)

summary(Model_8_Final)
vif(Model_8_Final)

#  Removing No_of_times_60_DPD_or_worse_in_last_6_months_woe, No_of_times_30_DPD_or_worse_in_last_12_months_woe variable
Model_9_Final<- glm(formula = PerformanceTag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_90_DPD_or_worse_in_last_12_months_woe + No_of_times_60_DPD_or_worse_in_last_12_months_woe + 
                      Binned_CC_Utilization_woe + 
                      Binned_No_of_trades_opened_in_last_6_months_woe + Binned_No_of_trades_opened_in_last_12_months_woe + 
                      No_of_PL_trades_opened_in_last_6_months_woe + No_of_PL_trades_opened_in_last_12_months_woe + 
                      Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                      Modified_Presence_of_open_home_loan_woe + Binning_outstanding_Balance_woe + 
                      binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                      Modified_Age +  BinningAge_woe + 
                      Education_woe + Gender_woe +  
                      BinningIncome_woe + Type_of_Residence_woe +  
                      BinningCurrentCompany_woe + BinningAge_fourteen_twentyfive + 
                      BinningAge_Twentyfive_Thirtyfive +BinningIncome_Zero_Ten + BinningIncome_Ten_Twenty +  
                      BinningIncome_Thirty_Forty + BinningCurentResidence_Zero_Twenty +  BinningCurentResidence_Forty_Sixty + 
                      BinningCurentResidence_Sixty_Eighty + BinningCurrentCompanyZero_Fifteen + BinningCurrentCompany_fifteen_Thirty + BinningCurrentCompany_thirty_fortyfive + 
                      Modified_Marital_StatusMarried + Modified_No_of_dependents_One +  
                      Modified_EducationBachelor + Modified_EducationMasters + Modified_EducationOthers + Modified_EducationPhd + 
                      Modified_ProfessionOthers + Modified_ProfessionSE + Modified_Residence_TypeCompanyprovided + 
                      Modified_Residence_TypeLivingwithParents, family = "binomial", data = data_balanced_sampled)

summary(Model_9_Final)
vif(Model_9_Final)


#  Removing Modified_ProfessionOthers, Modified_EducationPhd variable
Model_10_Final<- glm(formula = PerformanceTag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                      No_of_times_90_DPD_or_worse_in_last_12_months_woe + No_of_times_60_DPD_or_worse_in_last_12_months_woe + 
                      Binned_CC_Utilization_woe + 
                      Binned_No_of_trades_opened_in_last_6_months_woe + Binned_No_of_trades_opened_in_last_12_months_woe + 
                      No_of_PL_trades_opened_in_last_6_months_woe + No_of_PL_trades_opened_in_last_12_months_woe + 
                      Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                      Modified_Presence_of_open_home_loan_woe + Binning_outstanding_Balance_woe + 
                      binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                      Modified_Age +  BinningAge_woe + 
                      Education_woe + Gender_woe +  
                      BinningIncome_woe + Type_of_Residence_woe +  
                      BinningCurrentCompany_woe + BinningAge_fourteen_twentyfive + 
                      BinningAge_Twentyfive_Thirtyfive +BinningIncome_Zero_Ten + BinningIncome_Ten_Twenty +  
                      BinningIncome_Thirty_Forty + BinningCurentResidence_Zero_Twenty +  BinningCurentResidence_Forty_Sixty + 
                      BinningCurentResidence_Sixty_Eighty + BinningCurrentCompanyZero_Fifteen + BinningCurrentCompany_fifteen_Thirty + BinningCurrentCompany_thirty_fortyfive + 
                      Modified_Marital_StatusMarried + Modified_No_of_dependents_One +  
                      Modified_EducationBachelor + Modified_EducationMasters + Modified_EducationOthers +  
                      Modified_ProfessionSE + Modified_Residence_TypeCompanyprovided + 
                      Modified_Residence_TypeLivingwithParents, family = "binomial", data = data_balanced_sampled)

summary(Model_10_Final)
vif(Model_10_Final)


#  Removing Modified_ProfessionOthers, Modified_EducationPhd variable
Model_10_Final<- glm(formula = PerformanceTag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                       No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                       No_of_times_90_DPD_or_worse_in_last_12_months_woe + No_of_times_60_DPD_or_worse_in_last_12_months_woe + 
                       Binned_CC_Utilization_woe + 
                       Binned_No_of_trades_opened_in_last_6_months_woe + Binned_No_of_trades_opened_in_last_12_months_woe + 
                       No_of_PL_trades_opened_in_last_6_months_woe + No_of_PL_trades_opened_in_last_12_months_woe + 
                       Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                       Modified_Presence_of_open_home_loan_woe + Binning_outstanding_Balance_woe + 
                       binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                       Modified_Age +  BinningAge_woe + 
                       Education_woe + Gender_woe +  
                       BinningIncome_woe + Type_of_Residence_woe +  
                       BinningCurrentCompany_woe + BinningAge_fourteen_twentyfive + 
                       BinningAge_Twentyfive_Thirtyfive +BinningIncome_Zero_Ten + BinningIncome_Ten_Twenty +  
                       BinningIncome_Thirty_Forty + BinningCurentResidence_Zero_Twenty +  BinningCurentResidence_Forty_Sixty + 
                       BinningCurentResidence_Sixty_Eighty + BinningCurrentCompanyZero_Fifteen + BinningCurrentCompany_fifteen_Thirty + BinningCurrentCompany_thirty_fortyfive + 
                       Modified_Marital_StatusMarried + Modified_No_of_dependents_One +  
                       Modified_EducationBachelor + Modified_EducationMasters + Modified_EducationOthers +  
                       Modified_ProfessionSE + Modified_Residence_TypeCompanyprovided + 
                       Modified_Residence_TypeLivingwithParents, family = "binomial", data = data_balanced_sampled)

summary(Model_10_Final)
vif(Model_10_Final)

# Removing Type_of_Residence_woe, BinningCurentResidence_Forty_Sixty,Modified_EducationMasters variable
Model_11_Final<- glm(formula = PerformanceTag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                       No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                       No_of_times_90_DPD_or_worse_in_last_12_months_woe + No_of_times_60_DPD_or_worse_in_last_12_months_woe + 
                       Binned_CC_Utilization_woe + 
                       Binned_No_of_trades_opened_in_last_6_months_woe + Binned_No_of_trades_opened_in_last_12_months_woe + 
                       No_of_PL_trades_opened_in_last_6_months_woe + No_of_PL_trades_opened_in_last_12_months_woe + 
                       Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                       Modified_Presence_of_open_home_loan_woe + Binning_outstanding_Balance_woe + 
                       binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                       Modified_Age +  BinningAge_woe + 
                       Education_woe + Gender_woe +  
                       BinningIncome_woe +   
                       BinningCurrentCompany_woe + BinningAge_fourteen_twentyfive + 
                       BinningAge_Twentyfive_Thirtyfive +BinningIncome_Zero_Ten + BinningIncome_Ten_Twenty +  
                       BinningIncome_Thirty_Forty + BinningCurentResidence_Zero_Twenty +
                       BinningCurentResidence_Sixty_Eighty + BinningCurrentCompanyZero_Fifteen + BinningCurrentCompany_fifteen_Thirty + BinningCurrentCompany_thirty_fortyfive + 
                       Modified_Marital_StatusMarried + Modified_No_of_dependents_One +  
                       Modified_EducationBachelor + Modified_EducationOthers +  
                       Modified_ProfessionSE + Modified_Residence_TypeCompanyprovided + 
                       Modified_Residence_TypeLivingwithParents, family = "binomial", data = data_balanced_sampled)

summary(Model_11_Final)
vif(Model_11_Final)

# Removing No_of_PL_trades_opened_in_last_6_months_woe,BinningAge_Twentyfive_Thirtyfive, Gender_woe variable
Model_12_Final<- glm(formula = PerformanceTag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                       No_of_times_30_DPD_or_worse_in_last_6_months_woe + No_of_times_90_DPD_or_worse_in_last_12_months_woe + No_of_times_60_DPD_or_worse_in_last_12_months_woe + 
                       Binned_CC_Utilization_woe + Binned_No_of_trades_opened_in_last_6_months_woe + Binned_No_of_trades_opened_in_last_12_months_woe + 
                        No_of_PL_trades_opened_in_last_12_months_woe + Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                       Modified_Presence_of_open_home_loan_woe + Binning_outstanding_Balance_woe + binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                       Modified_Age +  BinningAge_woe + Education_woe + BinningIncome_woe + BinningCurrentCompany_woe + BinningAge_fourteen_twentyfive + 
                       BinningIncome_Zero_Ten + BinningIncome_Ten_Twenty +BinningIncome_Thirty_Forty + BinningCurentResidence_Zero_Twenty +
                       BinningCurentResidence_Sixty_Eighty + BinningCurrentCompanyZero_Fifteen + BinningCurrentCompany_fifteen_Thirty + BinningCurrentCompany_thirty_fortyfive + 
                       Modified_Marital_StatusMarried + Modified_No_of_dependents_One +  Modified_EducationBachelor + Modified_EducationOthers +  
                       Modified_ProfessionSE + Modified_Residence_TypeCompanyprovided + Modified_Residence_TypeLivingwithParents, family = "binomial", data = data_balanced_sampled)

summary(Model_12_Final)
vif(Model_12_Final)


# Removing Modified_Age,BinningAge_Twentyfive_Thirtyfive,Modified_Marital_StatusMarried variable
Model_13_Final<- glm(formula = PerformanceTag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                       No_of_times_30_DPD_or_worse_in_last_6_months_woe + No_of_times_90_DPD_or_worse_in_last_12_months_woe + No_of_times_60_DPD_or_worse_in_last_12_months_woe + 
                       Binned_CC_Utilization_woe + Binned_No_of_trades_opened_in_last_6_months_woe + Binned_No_of_trades_opened_in_last_12_months_woe + 
                       No_of_PL_trades_opened_in_last_12_months_woe + Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                       Modified_Presence_of_open_home_loan_woe + Binning_outstanding_Balance_woe + binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                       Education_woe + BinningIncome_woe + BinningCurrentCompany_woe + BinningAge_fourteen_twentyfive + 
                       BinningIncome_Zero_Ten + BinningIncome_Ten_Twenty +BinningIncome_Thirty_Forty + BinningCurentResidence_Zero_Twenty +
                       BinningCurentResidence_Sixty_Eighty + BinningCurrentCompanyZero_Fifteen + BinningCurrentCompany_fifteen_Thirty + BinningCurrentCompany_thirty_fortyfive + 
                        Modified_No_of_dependents_One +  Modified_EducationBachelor + Modified_EducationOthers +  
                       Modified_ProfessionSE + Modified_Residence_TypeCompanyprovided + Modified_Residence_TypeLivingwithParents, family = "binomial", data = data_balanced_sampled)

summary(Model_13_Final)
vif(Model_13_Final)

# Removing BinningCurrentCompany_thirty_fortyfive,BinningCurrentCompanyZero_Fifteen,Modified_Marital_StatusMarried variable
Model_14_Final<- glm(formula = PerformanceTag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                       No_of_times_30_DPD_or_worse_in_last_6_months_woe + No_of_times_90_DPD_or_worse_in_last_12_months_woe + No_of_times_60_DPD_or_worse_in_last_12_months_woe + 
                       Binned_CC_Utilization_woe + Binned_No_of_trades_opened_in_last_6_months_woe + Binned_No_of_trades_opened_in_last_12_months_woe + 
                       No_of_PL_trades_opened_in_last_12_months_woe + Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                       Modified_Presence_of_open_home_loan_woe + Binning_outstanding_Balance_woe + binning_Total_No_of_Trades_woe + Presence_of_open_auto_loan_woe + 
                       Education_woe + BinningIncome_woe + BinningCurrentCompany_woe + BinningAge_fourteen_twentyfive + 
                       BinningIncome_Zero_Ten + BinningIncome_Ten_Twenty +BinningIncome_Thirty_Forty + BinningCurentResidence_Zero_Twenty +
                       BinningCurentResidence_Sixty_Eighty +  BinningCurrentCompany_fifteen_Thirty +  
                       Modified_No_of_dependents_One +  Modified_EducationBachelor + Modified_EducationOthers +  
                       Modified_ProfessionSE + Modified_Residence_TypeCompanyprovided + Modified_Residence_TypeLivingwithParents, family = "binomial", data = data_balanced_sampled)

summary(Model_14_Final)
vif(Model_14_Final)


# Removing Presence_of_open_auto_loan_woe, BinningCurentResidence_Sixty_Eighty variable
Model_15_Final<- glm(formula = PerformanceTag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                       No_of_times_30_DPD_or_worse_in_last_6_months_woe + No_of_times_90_DPD_or_worse_in_last_12_months_woe + No_of_times_60_DPD_or_worse_in_last_12_months_woe + 
                       Binned_CC_Utilization_woe +  Binned_No_of_trades_opened_in_last_12_months_woe + 
                       No_of_PL_trades_opened_in_last_12_months_woe + Binned_No_of_Inquiries_in_last_6_months_woe + Binned_No_of_Inquiries_in_last_12_months_woe + 
                       Modified_Presence_of_open_home_loan_woe + Binning_outstanding_Balance_woe + binning_Total_No_of_Trades_woe + 
                       Education_woe + BinningIncome_woe + BinningCurrentCompany_woe + BinningAge_fourteen_twentyfive + 
                       BinningIncome_Zero_Ten + BinningIncome_Ten_Twenty +BinningIncome_Thirty_Forty + BinningCurentResidence_Zero_Twenty +
                      BinningCurrentCompany_fifteen_Thirty +  
                       Modified_No_of_dependents_One +  Modified_EducationBachelor + Modified_EducationOthers +  
                       Modified_ProfessionSE + Modified_Residence_TypeCompanyprovided + Modified_Residence_TypeLivingwithParents, family = "binomial", data = data_balanced_sampled)

summary(Model_15_Final)
vif(Model_15_Final)

# Removing Binned_No_of_Inquiries_in_last_6_months_woe, Modified_No_of_dependents_One, BinningAge_fourteen_twentyfive, Binning IncomeZeroTen variable
Model_16_Final<- glm(formula = PerformanceTag ~ No_of_times_90_DPD_or_worse_in_last_6_months_woe + 
                       No_of_times_30_DPD_or_worse_in_last_6_months_woe + No_of_times_90_DPD_or_worse_in_last_12_months_woe + No_of_times_60_DPD_or_worse_in_last_12_months_woe + 
                       Binned_CC_Utilization_woe +  Binned_No_of_trades_opened_in_last_12_months_woe + 
                       No_of_PL_trades_opened_in_last_12_months_woe +  Binned_No_of_Inquiries_in_last_12_months_woe + 
                       Modified_Presence_of_open_home_loan_woe + Binning_outstanding_Balance_woe + binning_Total_No_of_Trades_woe + 
                       Education_woe + BinningIncome_woe + BinningCurrentCompany_woe  + 
                        BinningIncome_Ten_Twenty +BinningIncome_Thirty_Forty + BinningCurentResidence_Zero_Twenty +
                       BinningCurrentCompany_fifteen_Thirty +  
                        Modified_EducationBachelor + Modified_EducationOthers +  
                       Modified_ProfessionSE + Modified_Residence_TypeCompanyprovided + Modified_Residence_TypeLivingwithParents, family = "binomial", data = data_balanced_sampled)

summary(Model_16_Final)
vif(Model_16_Final)

# Removing No_of_times_90_DPD_or_worse_in_last_6_months_woe Variable
Model_17_Final<- glm(formula = PerformanceTag ~ No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                       No_of_times_90_DPD_or_worse_in_last_12_months_woe + No_of_times_60_DPD_or_worse_in_last_12_months_woe + 
                       Binned_CC_Utilization_woe +  Binned_No_of_trades_opened_in_last_12_months_woe + 
                       No_of_PL_trades_opened_in_last_12_months_woe +  Binned_No_of_Inquiries_in_last_12_months_woe + 
                       Modified_Presence_of_open_home_loan_woe + Binning_outstanding_Balance_woe + binning_Total_No_of_Trades_woe + 
                       Education_woe + BinningIncome_woe + BinningCurrentCompany_woe  + 
                       BinningIncome_Ten_Twenty +BinningIncome_Thirty_Forty + BinningCurentResidence_Zero_Twenty +
                       BinningCurrentCompany_fifteen_Thirty +  
                       Modified_EducationBachelor + Modified_EducationOthers +  
                       Modified_ProfessionSE + Modified_Residence_TypeCompanyprovided + Modified_Residence_TypeLivingwithParents, family = "binomial", data = data_balanced_sampled)

summary(Model_17_Final)
vif(Model_17_Final)

# Removing No_of_times_60_DPD_or_worse_in_last_12_months_woe Variable
Model_18_Final<- glm(formula = PerformanceTag ~ No_of_times_30_DPD_or_worse_in_last_6_months_woe + 
                       No_of_times_90_DPD_or_worse_in_last_12_months_woe  + 
                       Binned_CC_Utilization_woe +  Binned_No_of_trades_opened_in_last_12_months_woe + 
                       No_of_PL_trades_opened_in_last_12_months_woe +  Binned_No_of_Inquiries_in_last_12_months_woe + 
                       Modified_Presence_of_open_home_loan_woe + Binning_outstanding_Balance_woe + binning_Total_No_of_Trades_woe + 
                       Education_woe + BinningIncome_woe + BinningCurrentCompany_woe  + 
                       BinningIncome_Ten_Twenty +BinningIncome_Thirty_Forty + BinningCurentResidence_Zero_Twenty +
                       BinningCurrentCompany_fifteen_Thirty +  
                       Modified_EducationBachelor + Modified_EducationOthers +  
                       Modified_ProfessionSE + Modified_Residence_TypeCompanyprovided + Modified_Residence_TypeLivingwithParents, family = "binomial", data = data_balanced_sampled)

summary(Model_18_Final)
vif(Model_18_Final)


#-------------------------------------------------------------------------------------
##------------------------- Model Evaluation - Logistic Regression Final --------------------
testing_Final <- testing

#training_Final <- training_Final[,-1]

colnames(testing_Final)[colnames(testing_Final) == "BinningAge(14,25]"] <- "BinningAge_fourteen_twentyfive"
colnames(testing_Final)[colnames(testing_Final) == "BinningAge(25,35]"] <- "BinningAge_Twentyfive_Thirtyfive"
colnames(testing_Final)[colnames(testing_Final) == "BinningAge(35,45]"] <- "BinningAge_ThirtyFive_FortyFive"
colnames(testing_Final)[colnames(testing_Final) == "BinningAge(45,55]"] <- "BinningAge_FortyFive_FiftyFive"
colnames(testing_Final)[colnames(testing_Final) == "BinningAge(55,65]"] <- "BinningAge_FiftyFive_SixtyFive"
colnames(testing_Final)[colnames(testing_Final) == "BinningIncome[0,10]"] <- "BinningIncome_Zero_Ten"
colnames(testing_Final)[colnames(testing_Final) == "BinningIncome(10,20]"] <- "BinningIncome_Ten_Twenty"
colnames(testing_Final)[colnames(testing_Final) == "BinningIncome(20,30]"] <- "BinningIncome_Twenty_Thirty"
colnames(testing_Final)[colnames(testing_Final) == "BinningIncome(30,40]"] <- "BinningIncome_Thirty_Forty"
colnames(testing_Final)[colnames(testing_Final) == "BinningIncome(40,50]"] <- "BinningIncome_Forty_Fifty"
colnames(testing_Final)[colnames(testing_Final) == "BinningIncome(50,60]"] <- "BinningIncome_Fifty_Sixty"
colnames(testing_Final)[colnames(testing_Final) == "BinningCurentResidence(0,20]"] <- "BinningCurentResidence_Zero_Twenty"
colnames(testing_Final)[colnames(testing_Final) == "BinningCurentResidence(20,40]"] <- "BinningCurentResidence_Twenty_Forty"
colnames(testing_Final)[colnames(testing_Final) == "BinningCurentResidence(40,60]"] <- "BinningCurentResidence_Forty_Sixty"
colnames(testing_Final)[colnames(testing_Final) == "BinningCurentResidence(60,80]"] <- "BinningCurentResidence_Sixty_Eighty"
colnames(testing_Final)[colnames(testing_Final) == "BinningCurentResidence(80,100]"] <- "BinningCurentResidence_Eighty_Hundred"
colnames(testing_Final)[colnames(testing_Final) == "BinningCurentResidence(100,130]"] <- "BinningCurentResidence_Hundred_oneHundredThirty"
colnames(testing_Final)[colnames(testing_Final) == "BinningCurrentCompany(0,15]"] <- "BinningCurrentCompanyZero_Fifteen"
colnames(testing_Final)[colnames(testing_Final) == "BinningCurrentCompany(15,30]"] <- "BinningCurrentCompany_fifteen_Thirty"
colnames(testing_Final)[colnames(testing_Final) == "BinningCurrentCompany(30,45]"] <- "BinningCurrentCompany_thirty_fortyfive"
colnames(testing_Final)[colnames(testing_Final) == "BinningCurrentCompany(45,60]"] <- "BinningCurrentCompany_fortyfive_sixty"
colnames(testing_Final)[colnames(testing_Final) == "BinningCurrentCompany(60,75]"] <- "BinningCurrentCompany_sixty_seventyfive"
colnames(testing_Final)[colnames(testing_Final) == "Modified_Residence_TypeCompany provided"] <- "Modified_Residence_TypeCompanyprovided"
colnames(testing_Final)[colnames(testing_Final) == "Modified_Residence_TypeLiving with Parents"] <- "Modified_Residence_TypeLivingwithParents"
colnames(testing_Final)[colnames(testing_Final) == "Performance.Tag"] <- "PerformanceTag"
colnames(testing_Final)[colnames(testing_Final) == "Modified_No_of_dependents1"] <- "Modified_No_of_dependents_One"
colnames(testing_Final)[colnames(testing_Final) == "Modified_No_of_dependents2"] <- "Modified_No_of_dependents_Two"
colnames(testing_Final)[colnames(testing_Final) == "Modified_No_of_dependents3"] <- "Modified_No_of_dependents_Three"
colnames(testing_Final)[colnames(testing_Final) == "Modified_No_of_dependents4"] <- "Modified_No_of_dependents_Four"
colnames(testing_Final)[colnames(testing_Final) == "Modified_No_of_dependents5"] <- "Modified_No_of_dependents_Five"


# Predicting probabilities of responding for the test data
predictions_logit_Final_balanced <- predict(Model_18_Final, newdata = testing_Final[,-c( 1,19)], type = "response")
summary(predictions_logit_Final_balanced)

# Let's use the probability cutoff.

predicted_response_Final_Balanced <- factor(ifelse(predictions_logit_Final_balanced >= 0.60, "Yes", "No"))
summary(predicted_response_Final_Balanced)

# Creating confusion matrix for identifying the model evaluation.
summary(testing_Final$PerformanceTag)

conf_1 <- confusionMatrix(predicted_response_Final_Balanced, testing_Final$PerformanceTag, positive = "Yes")
conf_1

#---------------------------------------------------------    

# Let's find out the optimal probalility cutoff 
perform_fn <- function(cutoff) 
{
  predicted_response_Final <- factor(ifelse(predictions_logit_Final_balanced >= cutoff, "Yes", "No"))
  conf_1 <- confusionMatrix(predicted_response_Final_Balanced, testing_Final$PerformanceTag, positive = "Yes")
  acc <- conf_1$overall[1]
  sens <- conf_1$byClass[1]
  spec <- conf_1$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    
# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.
s = seq(.01,.09,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 
str(testing_Final$PerformanceTag)
str(predicted_response_Final_Balanced)
#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

#---------------------------------------------------------    

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.30)]
cutoff

# Let's choose a cutoff for final model
predicted_response_Final_Balanced <- factor(ifelse(predictions_logit_Final_balanced >= 0.5319, "Yes", "No"))

conf_final_1 <- confusionMatrix(predicted_response_Final_Balanced, testing_Final$PerformanceTag, positive = "Yes")
acc_1 <- conf_final_1$overall[1]
sens_1 <- conf_final_1$byClass[1]
spec_1 <- conf_final_1$byClass[2]
f1_score <- conf_final_1$byClass[7]

f1_score  #.13
acc_1     #0.6500954 
sens_1    #0.6244344 
spec_1    #0.6512253

plot(roc(testing_Final$PerformanceTag ~ predictions_logit_Final_balanced))


##############################################################################################
#                                     RANDOM FOREST MODEL
##############################################################################################
# **********************************************************************************************************
# Lets try out random forest and check the metrices. 

# **********************************************************************************************************
Demographic_WOE <- Demographic_Data_WOE_Updated[,c(1,12,25:34)]

colnames(Credit_Bureau_Data_WOE)
Credit_WOE <- Credit_Bureau_Data_WOE
combined_WOE <- merge(x = Demographic_WOE, y = Credit_WOE, by = "Application.ID", all = F)
table(combined_WOE$Performance.Tag)

#*****************************************************************************************#
# Undersampling and oversampling data - Decision Tree
#*****************************************************************************************#

library(ROSE)
library(caret)

prop.table(table(combined_WOE$Performance.Tag))

# The results show that 95% non-defaulters and approx.5% defaulters.
# Lets try to build a decision tree with the data. 
#*****************************************************************************************#
# Since we are using caret package for spliting the data, the stratified sampling is taken care
# by the caret package itself.
#*****************************************************************************************#
set.seed(100)
combined_WOE$Performance.Tag <- as.factor(ifelse(combined_WOE$Performance.Tag == 1,"yes","no"))
train_demo.index <- createDataPartition(combined_WOE$Performance.Tag, p=0.7, list = FALSE)


train_RF <- combined_WOE[train_demo.index, ]
test_RF <- combined_WOE[-train_demo.index, ]

#*****************************************************************************************#
# Random Forest. 
#*****************************************************************************************#
library(randomForest)

randomForest.unbalanced <- randomForest(Performance.Tag ~ ., data = train_RF[,-1], proximity = FALSE, 
                                        ntree = 50, mtry = 4, replace = T, importance = T, do.trace = TRUE)
summary(randomForest.unbalanced)

testPred <- predict(randomForest.unbalanced, newdata=test_RF[,-1], type = "prob")
table(testPred[,-1], test_RF$Performance.Tag)

perform_fn_rf_demo <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(testPred[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_RF$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf_demo <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf_demo) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf_demo)
}
# **************************************************************************************************
# Hyper Parameter Optimization
# creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
# **************************************************************************************************

s_demo = seq(.01,.99,length=100)
OUT_rf_demo = matrix(0,100,3)

for(i in 1:100)
{
  OUT_rf_demo[i,] = perform_fn_rf_demo(s_demo[i])
}

# plotting cutoffs
plot(s_demo, OUT_rf_demo[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s_demo,OUT_rf_demo[,2],col="darkgreen",lwd=2)
lines(s_demo,OUT_rf_demo[,3],col=4,lwd=2)
box()

cutoff_rf <- s_demo[which(abs(OUT_rf_demo[,1]-OUT_rf_demo[,2])<0.08)]
cutoff_rf

predicted_response_demo <- factor(ifelse(testPred[, 2] >= 0.05, "yes", "no"))
conf_forest <- confusionMatrix(predicted_response_demo, test_RF[, 2], positive = "yes")

# Sensitivity -- Unbalanced data : 0.513
conf_forest$byClass[1] 
# Specificity -- Unbalanced data : 0.668
conf_forest$byClass[2]
# Accuracy -- Unbalanced data : 0.66
conf_forest$overall[1]

conf_forest$table


# For unbalanced data, with cutof probability of 0.06, we are getting the following metrices 
# For cutoff as 0.06/0.05
# Sensitivity  0.513
# Specificity  0.668 
# Accuracy  0.6617

#Prediction    no   yes
# no         13415   430
# yes         6660   454

# Final RF important variables
importance <- randomForest.unbalanced$importance 
importance <- data.frame(importance)
importance

varImpPlot(randomForest.unbalanced, type = 2)

# This plot  represents the mean decrease in node impurity (and not the mean decrease in accuracy).
# ***********************************************************************************
# Lets try out over sampling and under sampling and compare the results. 
# ***********************************************************************************
# Undersampling, oversampling , both way sampling data - Random Forest
#*****************************************************************************************#
balanced_over_sampling <- ovun.sample(Performance.Tag ~ ., data = train_RF, method = "over", 
                                      N = 2*46844)$data
table(balanced_over_sampling$Performance.Tag)

# Under Sampling
# However , we lose most of the information in this case. 
#*****************************************************************************************#

balanced_under_sampling <- ovun.sample(Performance.Tag ~ ., data = train_RF, method = "under", 
                                       N = 4128, seed = 1)$data
table(balanced_under_sampling$Performance.Tag)

#LEts try out both oversampling and undersampling together with 70-30 split.  

balanced_both_sampling <- ovun.sample(Performance.Tag ~ ., data = train_RF, 
                                      method = "both", p = 0.3 ,  seed = 1)$data
table(balanced_both_sampling$Performance.Tag)

# Another option is synthetic generation of data using the ROSE package. 
synthetic.data <- ROSE(Performance.Tag ~ ., data = train_RF, seed = 1)$data
table(synthetic.data$Performance.Tag)

#*****************************************************************************************#
# Over Sampled Data 
#*****************************************************************************************#

randomForest.oversampled <- randomForest(Performance.Tag ~ ., data = balanced_over_sampling[,-1], proximity = FALSE, 
                                         ntree = 50, mtry = 4, do.trace = TRUE)
summary(randomForest.oversampled)


testPred <- predict(randomForest.oversampled, newdata=test_RF[,-1], type = "prob")
table(testPred[,-1], test_RF$Performance.Tag)

# plotting cutoffs
plot(s_demo, OUT_rf_demo[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s_demo,OUT_rf_demo[,2],col="darkgreen",lwd=2)
lines(s_demo,OUT_rf_demo[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(1,1,1,1),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s_demo[which(abs(OUT_rf_demo[,1]-OUT_rf_demo[,2])<0.05)]

predicted_response_demo <- factor(ifelse(testPred[, 2] >= 0.12, "yes", "no"))
conf_forest <- confusionMatrix(predicted_response_demo, test_RF[, 2], positive = "yes")


# Sensitivity -- with cutof as 0.1 oversampled data : 0.442
conf_forest$byClass[1] 
# Specificity -- oversampled data : 0.633
conf_forest$byClass[2]
# Accuracy -- oversampled data : 0.625
conf_forest$overall[1]

conf_forest$table

# Sensitivity 0.4423077 
# Specificity 0.633325 
# Accuracy 0.6252684 

# Final RF important variables
importance <- randomForest.oversampled$importance 
importance <- data.frame(importance)
importance

varImpPlot(randomForest.oversampled, type = 2)

#*****************************************************************************************#
# Under Sampled Data 
#*****************************************************************************************#
randomForest.undersampled <- randomForest(Performance.Tag ~ ., data = balanced_under_sampling[,-1], proximity = FALSE, 
                                          ntree = 50, mtry = 4, do.trace = TRUE)
summary(randomForest.undersampled)

testPred <- predict(randomForest.undersampled, newdata=test_RF[,-1], type = "prob")
table(testPred[,-1], test_RF$Performance.Tag)

# plotting cutoffs
plot(s_demo, OUT_rf_demo[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s_demo,OUT_rf_demo[,2],col="darkgreen",lwd=2)
lines(s_demo,OUT_rf_demo[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(1,1,1,1),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s_demo[which(abs(OUT_rf_demo[,1]-OUT_rf_demo[,2])<0.05)]

predicted_response_demo <- factor(ifelse(testPred[, 2] >= 0.09, "yes", "no"))
conf_forest <- confusionMatrix(predicted_response_demo, test_RF[, 2], positive = "yes")


# Sensitivity -- undersampled data : 0.994
conf_forest$byClass[1] 
# Specificity -- undersampled data : 0.027
conf_forest$byClass[2]
# Accuracy -- undersampled data : 0.068
conf_forest$overall[1]

conf_forest$table

#Prediction    no   yes
# no          562    8
# yes       19513   876

# Under Sampling gives very poor results. So we can reject this strategy straight away. 
#*****************************************************************************************#
# Both Sampled Data 
#*****************************************************************************************#
randomForest.bothsampled <- randomForest(Performance.Tag ~ ., data = balanced_both_sampling[,-1], proximity = FALSE, 
                                         ntree = 50, mtry = 4, do.trace = TRUE)
summary(randomForest.bothsampled)

testPred <- predict(randomForest.bothsampled, newdata=test_RF[,-1], type = "prob")
table(testPred[,-1], test_RF$Performance.Tag)

# plotting cutoffs
plot(s_demo, OUT_rf_demo[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s_demo,OUT_rf_demo[,2],col="darkgreen",lwd=2)
lines(s_demo,OUT_rf_demo[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(1,1,1,1),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s_demo[which(abs(OUT_rf_demo[,1]-OUT_rf_demo[,2])<0.05)]

predicted_response_demo <- factor(ifelse(testPred[, 2] >= 0.15, "yes", "no"))
conf_forest <- confusionMatrix(predicted_response_demo, test_RF[, 2], positive = "yes")


# Sensitivity -- both sampled data : 0.5
conf_forest$byClass[1] 
# Specificity -- both sampled data : 0.672
conf_forest$byClass[2]
# Accuracy -- both sampled data : 0.664
conf_forest$overall[1]

conf_forest$table
#Prediction    no   yes
# no         13491   442
# yes         6584   442

# Sensitivity 0.5 
# Specificity 0.6720299 
# Accuracy 0.6647741 

# Final RF important variables
importance <- randomForest.bothsampled$importance 
importance <- data.frame(importance)
importance

varImpPlot(randomForest.bothsampled, type = 2)
#*****************************************************************************************#
# Synthetic Sampled Data 
#*****************************************************************************************#
randomForest.synthetic <- randomForest(Performance.Tag ~ ., data = synthetic.data[,-1], proximity = FALSE, 
                                       ntree = 50, mtry = 4, do.trace = TRUE)
summary(randomForest.synthetic)

testPred <- predict(randomForest.synthetic, newdata=test_RF[,-1], type = "prob")
table(testPred[,-1], test_RF$Performance.Tag)

# plotting cutoffs
plot(s_demo, OUT_rf_demo[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s_demo,OUT_rf_demo[,2],col="darkgreen",lwd=2)
lines(s_demo,OUT_rf_demo[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(1,1,1,1),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s_demo[which(abs(OUT_rf_demo[,1]-OUT_rf_demo[,2])<0.06)]

predicted_response_demo <- factor(ifelse(testPred[, 2] >= 0.2, "yes", "no"))
conf_forest <- confusionMatrix(predicted_response_demo, test_RF[, 2], positive = "yes")

# Sensitivity
conf_forest$byClass[1] 
#0.611
# Specificity
conf_forest$byClass[2]
#0.639
# Accuracy
conf_forest$overall[1]
#0.6382

conf_forest$table
varImpPlot(randomForest.synthetic, type = 2)
# With a probability cutoff of 0.2, we are getting accuracy of 0.63, sensitivity of 0.61 and specificity of 0.63

#Prediction    no     yes
# no          12836    343
# yes          7239    541

# Final RF important variables
importance <- randomForest.synthetic$importance 
importance <- data.frame(importance)
importance

rf.roc<-roc(train_RF$Performance.Tag,randomForest.unbalanced$votes[,2])
plot(rf.roc)
auc(rf.roc)

rf.roc<-roc(balanced_over_sampling$Performance.Tag,randomForest.oversampled$votes[,2])
plot(rf.roc)
auc(rf.roc)

rf.roc<-roc(balanced_under_sampling$Performance.Tag,randomForest.undersampled$votes[,2])
plot(rf.roc)
auc(rf.roc)

rf.roc<-roc(balanced_both_sampling$Performance.Tag,randomForest.bothsampled$votes[,2])
plot(rf.roc)
auc(rf.roc)

rf.roc<-roc(synthetic.data$Performance.Tag,randomForest.synthetic$votes[,2])
plot(rf.roc)
auc(rf.roc)

# The AUC value of 0.74 is seen in case of synthetic data generation , and the plot looks decent. 
# Lets continue our exploration with the synthetic.data
# We can calculate the f1 scores and compare between two different models. 

conf_forest$byClass[7]

#Logistic Regression Model is a better model with compared on different parameters and gives better results
#over Random Forest Model


#########################################################################################################
#                            Application Scorecard For Logistic Regression Model                        #
#########################################################################################################
# Creating Predict Default Column
testing_Final$perdict_default <- (predictions_logit_Final_balanced)
# Creating Predict Non Default Column
testing_Final$predict_NonDefault <- 1 - testing_Final$perdict_default
# Creating odds Column
testing_Final$odds <-  log(testing_Final$predict_NonDefault/testing_Final$perdict_default)

# Score = Offset + ( Factor * log(odds) )
# Factor = PDO/ln(2)
# Offset = Score-(Factor*log(odds))

# In our case, PDO = 20, Base Score=400 & odds = 10
Factor = 20/log(2)   #28.8539

Offset = 400 - (28.8539*log(10))

testing_Final$Score = 333.5614 + (28.8539*testing_Final$odds)
summary(testing_Final$Score)

#Min.   :289.2                   
#1st Qu.:322.6                   
#Median :338.4                   
#Mean   :338.8                   
#3rd Qu.:356.7                   
#Max.   :381.2 


########Predicting Potential Defaulters from Existing Credit Card Holders##########
Combined_Dataframe_Approved <- Combined_Dataframe
#############################################################################################################
# Column Name Changed for Approved Apllications Dataset
#############################################################################################################
colnames(Combined_Dataframe_Approved)[colnames(Combined_Dataframe_Approved) == "Presence_of_open_home_loan_woe"] <- "Modified_Presence_of_open_home_loan_woe"
colnames(Combined_Dataframe_Approved)[colnames(Combined_Dataframe_Approved) == "BinningAge(14,25]"] <- "BinningAge_fourteen_twentyfive"
colnames(Combined_Dataframe_Approved)[colnames(Combined_Dataframe_Approved) == "BinningAge(25,35]"] <- "BinningAge_Twentyfive_Thirtyfive"
colnames(Combined_Dataframe_Approved)[colnames(Combined_Dataframe_Approved) == "BinningAge(35,45]"] <- "BinningAge_ThirtyFive_FortyFive"
colnames(Combined_Dataframe_Approved)[colnames(Combined_Dataframe_Approved) == "BinningAge(45,55]"] <- "BinningAge_FortyFive_FiftyFive"
colnames(Combined_Dataframe_Approved)[colnames(Combined_Dataframe_Approved) == "BinningAge(55,65]"] <- "BinningAge_FiftyFive_SixtyFive"
colnames(Combined_Dataframe_Approved)[colnames(Combined_Dataframe_Approved) == "BinningIncome[0,10]"] <- "BinningIncome_Zero_Ten"
colnames(Combined_Dataframe_Approved)[colnames(Combined_Dataframe_Approved) == "BinningIncome(10,20]"] <- "BinningIncome_Ten_Twenty"
colnames(Combined_Dataframe_Approved)[colnames(Combined_Dataframe_Approved) == "BinningIncome(20,30]"] <- "BinningIncome_Twenty_Thirty"
colnames(Combined_Dataframe_Approved)[colnames(Combined_Dataframe_Approved) == "BinningIncome(30,40]"] <- "BinningIncome_Thirty_Forty"
colnames(Combined_Dataframe_Approved)[colnames(Combined_Dataframe_Approved) == "BinningIncome(40,50]"] <- "BinningIncome_Forty_Fifty"
colnames(Combined_Dataframe_Approved)[colnames(Combined_Dataframe_Approved) == "BinningIncome(50,60]"] <- "BinningIncome_Fifty_Sixty"
colnames(Combined_Dataframe_Approved)[colnames(Combined_Dataframe_Approved) == "BinningCurentResidence(0,20]"] <- "BinningCurentResidence_Zero_Twenty"
colnames(Combined_Dataframe_Approved)[colnames(Combined_Dataframe_Approved) == "BinningCurentResidence(20,40]"] <- "BinningCurentResidence_Twenty_Forty"
colnames(Combined_Dataframe_Approved)[colnames(Combined_Dataframe_Approved) == "BinningCurentResidence(40,60]"] <- "BinningCurentResidence_Forty_Sixty"
colnames(Combined_Dataframe_Approved)[colnames(Combined_Dataframe_Approved) == "BinningCurentResidence(60,80]"] <- "BinningCurentResidence_Sixty_Eighty"
colnames(Combined_Dataframe_Approved)[colnames(Combined_Dataframe_Approved) == "BinningCurentResidence(80,100]"] <- "BinningCurentResidence_Eighty_Hundred"
colnames(Combined_Dataframe_Approved)[colnames(Combined_Dataframe_Approved) == "BinningCurentResidence(100,130]"] <- "BinningCurentResidence_Hundred_oneHundredThirty"
colnames(Combined_Dataframe_Approved)[colnames(Combined_Dataframe_Approved) == "BinningCurrentCompany(0,15]"] <- "BinningCurrentCompanyZero_Fifteen"
colnames(Combined_Dataframe_Approved)[colnames(Combined_Dataframe_Approved) == "BinningCurrentCompany(15,30]"] <- "BinningCurrentCompany_fifteen_Thirty"
colnames(Combined_Dataframe_Approved)[colnames(Combined_Dataframe_Approved) == "BinningCurrentCompany(30,45]"] <- "BinningCurrentCompany_thirty_fortyfive"
colnames(Combined_Dataframe_Approved)[colnames(Combined_Dataframe_Approved) == "BinningCurrentCompany(45,60]"] <- "BinningCurrentCompany_fortyfive_sixty"
colnames(Combined_Dataframe_Approved)[colnames(Combined_Dataframe_Approved) == "BinningCurrentCompany(60,75]"] <- "BinningCurrentCompany_sixty_seventyfive"
colnames(Combined_Dataframe_Approved)[colnames(Combined_Dataframe_Approved) == "Modified_Residence_TypeCompany provided"] <- "Modified_Residence_TypeCompanyprovided"
colnames(Combined_Dataframe_Approved)[colnames(Combined_Dataframe_Approved) == "Modified_Residence_TypeLiving with Parents"] <- "Modified_Residence_TypeLivingwithParents"
colnames(Combined_Dataframe_Approved)[colnames(Combined_Dataframe_Approved) == "Performance.Tag"] <- "PerformanceTag"
colnames(Combined_Dataframe_Approved)[colnames(Combined_Dataframe_Approved) == "Modified_No_of_dependents1"] <- "Modified_No_of_dependents_One"
colnames(Combined_Dataframe_Approved)[colnames(Combined_Dataframe_Approved) == "Modified_No_of_dependents2"] <- "Modified_No_of_dependents_Two"
colnames(Combined_Dataframe_Approved)[colnames(Combined_Dataframe_Approved) == "Modified_No_of_dependents3"] <- "Modified_No_of_dependents_Three"
colnames(Combined_Dataframe_Approved)[colnames(Combined_Dataframe_Approved) == "Modified_No_of_dependents4"] <- "Modified_No_of_dependents_Four"
colnames(Combined_Dataframe_Approved)[colnames(Combined_Dataframe_Approved) == "Modified_No_of_dependents5"] <- "Modified_No_of_dependents_Five"

predictions_logit_Final_Approved <- predict(Model_18_Final, newdata = Combined_Dataframe_Approved[,-c( 1,19)], type = "response")
summary(predictions_logit_Final_Approved)

# Creating Predict Default Column
Combined_Dataframe_Approved$perdict_default <- (predictions_logit_Final_Approved)

# Creating Predict Non Default Column
Combined_Dataframe_Approved$predict_NonDefault <- 1 - Combined_Dataframe_Approved$perdict_default

# Creating odds Column
Combined_Dataframe_Approved$odds <-  log(Combined_Dataframe_Approved$predict_NonDefault/Combined_Dataframe_Approved$perdict_default)

# Score = Offset + ( Factor * log(odds) )
# Factor = PDO/ln(2)
# Offset = Score-(Factor*log(odds))

# In our case, PDO = 20, Base Score=400 & odds = 10
Factor = 20/log(2)   #28.8539

Offset = 400 - (28.8539*log(10))
Offset

Combined_Dataframe_Approved$Score = 333.5614 + (28.8539*Combined_Dataframe_Approved$odds)
summary(Combined_Dataframe_Approved$Score)

#Calculating Number of Customers having Credit Card below Cutoff which can be potential defaulters
Combined_Dataframe_Approved$Potential_defaulters <- ifelse(Combined_Dataframe_Approved$Score<=320,1,0)

Combined_Dataframe_Approved$Potential_defaulters <- as.factor(Combined_Dataframe_Approved$Potential_defaulters)
summary(Combined_Dataframe_Approved$Potential_defaulters)

quantile(Combined_Dataframe_Approved$Score,seq(0,1,0.01))


#Potential defaulters 
# Yes : 14980
# No : 54887
# Potential defaulters are the custumer which alrady have the credit card but their credit score is below
# 320 in this case, This value can be adjusted over a period of time by analysing default trends.

Combined_Dataframe_Approved$Actual_Defaulter <- ifelse((Combined_Dataframe_Approved$PerformanceTag==1 & Combined_Dataframe_Approved$Potential_defaulters==1),1,0)
Combined_Dataframe_Approved$Actual_Defaulter <- as.factor(Combined_Dataframe_Approved$Actual_Defaulter)
summary(Combined_Dataframe_Approved$Actual_Defaulter)
#Actual Defaulters
# Yes : 1194
# No : 68673

#Net Defauter that could be minimized using this model : 1194
#Let us assume 1 defaulter results to $10000 to the bank
#Net Loss due to defaut : $11,194,000 .i.e $11.2 million
#Lost Customer due to model : 14980-1194 = 13,786
#Let us Assume 1 lost customer results in $500 revenue loss to the bank
#Net Revenue loss due to lost customers = $6,893,000 i.e. $6.9 million
#Monetary Benifit for bank using our Model : $11.2 million - $6.9 million = $4.3 million
#########################################################################################################
#                                  Application Scorecard For Rejected Applications
########################################################################################################
#############################################################################################################
# Column Name Changed for Rejected Apllications Dataset
#############################################################################################################
colnames(Combined_Dataframe_Rejected)[colnames(Combined_Dataframe_Rejected) == "Presence_of_open_home_loan_woe"] <- "Modified_Presence_of_open_home_loan_woe"
colnames(Combined_Dataframe_Rejected)[colnames(Combined_Dataframe_Rejected) == "BinningAge(14,25]"] <- "BinningAge_fourteen_twentyfive"
colnames(Combined_Dataframe_Rejected)[colnames(Combined_Dataframe_Rejected) == "BinningAge(25,35]"] <- "BinningAge_Twentyfive_Thirtyfive"
colnames(Combined_Dataframe_Rejected)[colnames(Combined_Dataframe_Rejected) == "BinningAge(35,45]"] <- "BinningAge_ThirtyFive_FortyFive"
colnames(Combined_Dataframe_Rejected)[colnames(Combined_Dataframe_Rejected) == "BinningAge(45,55]"] <- "BinningAge_FortyFive_FiftyFive"
colnames(Combined_Dataframe_Rejected)[colnames(Combined_Dataframe_Rejected) == "BinningAge(55,65]"] <- "BinningAge_FiftyFive_SixtyFive"
colnames(Combined_Dataframe_Rejected)[colnames(Combined_Dataframe_Rejected) == "BinningIncome[0,10]"] <- "BinningIncome_Zero_Ten"
colnames(Combined_Dataframe_Rejected)[colnames(Combined_Dataframe_Rejected) == "BinningIncome(10,20]"] <- "BinningIncome_Ten_Twenty"
colnames(Combined_Dataframe_Rejected)[colnames(Combined_Dataframe_Rejected) == "BinningIncome(20,30]"] <- "BinningIncome_Twenty_Thirty"
colnames(Combined_Dataframe_Rejected)[colnames(Combined_Dataframe_Rejected) == "BinningIncome(30,40]"] <- "BinningIncome_Thirty_Forty"
colnames(Combined_Dataframe_Rejected)[colnames(Combined_Dataframe_Rejected) == "BinningIncome(40,50]"] <- "BinningIncome_Forty_Fifty"
colnames(Combined_Dataframe_Rejected)[colnames(Combined_Dataframe_Rejected) == "BinningIncome(50,60]"] <- "BinningIncome_Fifty_Sixty"
colnames(Combined_Dataframe_Rejected)[colnames(Combined_Dataframe_Rejected) == "BinningCurentResidence(0,20]"] <- "BinningCurentResidence_Zero_Twenty"
colnames(Combined_Dataframe_Rejected)[colnames(Combined_Dataframe_Rejected) == "BinningCurentResidence(20,40]"] <- "BinningCurentResidence_Twenty_Forty"
colnames(Combined_Dataframe_Rejected)[colnames(Combined_Dataframe_Rejected) == "BinningCurentResidence(40,60]"] <- "BinningCurentResidence_Forty_Sixty"
colnames(Combined_Dataframe_Rejected)[colnames(Combined_Dataframe_Rejected) == "BinningCurentResidence(60,80]"] <- "BinningCurentResidence_Sixty_Eighty"
colnames(Combined_Dataframe_Rejected)[colnames(Combined_Dataframe_Rejected) == "BinningCurentResidence(80,100]"] <- "BinningCurentResidence_Eighty_Hundred"
colnames(Combined_Dataframe_Rejected)[colnames(Combined_Dataframe_Rejected) == "BinningCurentResidence(100,130]"] <- "BinningCurentResidence_Hundred_oneHundredThirty"
colnames(Combined_Dataframe_Rejected)[colnames(Combined_Dataframe_Rejected) == "BinningCurrentCompany(0,15]"] <- "BinningCurrentCompanyZero_Fifteen"
colnames(Combined_Dataframe_Rejected)[colnames(Combined_Dataframe_Rejected) == "BinningCurrentCompany(15,30]"] <- "BinningCurrentCompany_fifteen_Thirty"
colnames(Combined_Dataframe_Rejected)[colnames(Combined_Dataframe_Rejected) == "BinningCurrentCompany(30,45]"] <- "BinningCurrentCompany_thirty_fortyfive"
colnames(Combined_Dataframe_Rejected)[colnames(Combined_Dataframe_Rejected) == "BinningCurrentCompany(45,60]"] <- "BinningCurrentCompany_fortyfive_sixty"
colnames(Combined_Dataframe_Rejected)[colnames(Combined_Dataframe_Rejected) == "BinningCurrentCompany(60,75]"] <- "BinningCurrentCompany_sixty_seventyfive"
colnames(Combined_Dataframe_Rejected)[colnames(Combined_Dataframe_Rejected) == "Modified_Residence_TypeCompany provided"] <- "Modified_Residence_TypeCompanyprovided"
colnames(Combined_Dataframe_Rejected)[colnames(Combined_Dataframe_Rejected) == "Modified_Residence_TypeLiving with Parents"] <- "Modified_Residence_TypeLivingwithParents"
colnames(Combined_Dataframe_Rejected)[colnames(Combined_Dataframe_Rejected) == "Performance.Tag"] <- "PerformanceTag"
colnames(Combined_Dataframe_Rejected)[colnames(Combined_Dataframe_Rejected) == "Modified_No_of_dependents1"] <- "Modified_No_of_dependents_One"
colnames(Combined_Dataframe_Rejected)[colnames(Combined_Dataframe_Rejected) == "Modified_No_of_dependents2"] <- "Modified_No_of_dependents_Two"
colnames(Combined_Dataframe_Rejected)[colnames(Combined_Dataframe_Rejected) == "Modified_No_of_dependents3"] <- "Modified_No_of_dependents_Three"
colnames(Combined_Dataframe_Rejected)[colnames(Combined_Dataframe_Rejected) == "Modified_No_of_dependents4"] <- "Modified_No_of_dependents_Four"
colnames(Combined_Dataframe_Rejected)[colnames(Combined_Dataframe_Rejected) == "Modified_No_of_dependents5"] <- "Modified_No_of_dependents_Five"

####################################################################################

predictions_logit_Final_rejected <- predict(Model_18_Final, newdata = Combined_Dataframe_Rejected[,-c( 1,19)], type = "response")
summary(predictions_logit_Final_rejected)

# Creating Predict Default Column
Combined_Dataframe_Rejected$perdict_default <- (predictions_logit_Final_rejected)
# Creating Predict Non Default Column
Combined_Dataframe_Rejected$predict_NonDefault <- 1 - Combined_Dataframe_Rejected$perdict_default
# Creating odds Column
Combined_Dataframe_Rejected$odds <-  log(Combined_Dataframe_Rejected$predict_NonDefault/Combined_Dataframe_Rejected$perdict_default)

# Score = Offset + ( Factor * log(odds) )
# Factor = PDO/ln(2)
# Offset = Score-(Factor*log(odds))

# In our case, PDO = 20, Base Score=400 & odds = 10
Factor = 20/log(2)  #28.8539

Offset = 400 - (28.8539*log(10))

Combined_Dataframe_Rejected$Score = 333.5614 + (28.8539*Combined_Dataframe_Rejected$odds)
summary(Combined_Dataframe_Rejected$Score)

#Min.   :299.8                   
#1st Qu.:327.3                   
#Median :334.1                   
#Mean   :333.0                   
#3rd Qu.:339.6                   
#Max.   :358.0 

Combined_Dataframe_Rejected<- as.data.frame(Combined_Dataframe_Rejected)
#colnames(Combined_Dataframe_Rejected)[colnames(Combined_Dataframe_Rejected) == "Score"] <- "Application Score"

#Calculating Revenue Loss for the Bank
Combined_Dataframe_Rejected$Revenue_Loss <- ifelse(Combined_Dataframe_Rejected$Score>=320,1,0)

Combined_Dataframe_Rejected$Revenue_Loss<- as.factor(Combined_Dataframe_Rejected$Revenue_Loss)
summary(Combined_Dataframe_Rejected$Revenue_Loss)

##Total number of rejected customers by bank : 1425
#Customer should be given Credit card based on credit score
# No : 133
# Yes : 1292
# Credit Score Cut off : 320
###Revenue Loss for bank###
#Let us assume bank makes $500 per year from 1 credit card customer.
# Bank refused 1292 potential credit card customer,amounting to $646,000 annual loss to the bank
