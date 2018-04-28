############# HR ANALYTICS CASE STUDY ###############################
# Submitted by: Team Fantastic Four - DDA1730283

################# ASSUMPTIONS ####################################
# 1. This code is developed & run on Windows 10 platform
# 2. Working directory is correctly set
# 3. All files are present in the working directory
# 4. All file names are maintained as per UpGrad website
# 5. All packages are installed if not explicitly installed in the code

# CRISP-DM framework applied for HR analytics case-study

################# CRISP-DM:1 Business Understanding #############################
# 1. Company XYZ faces average 15% attrition always, affecting operations & risk & brand  
# 2. Employment information for XYZ available for 2015
# 3. XYZ needs insights on factors that lead to attrition and recommendations to curb attrition

#### Business Objective #######
# 1. Identify main factors that lead to attrition for company XYZ
# 2. Identify changes that XYZ needs to do at their workplace for employee retention
# 3. Identify most critical variables that need to be addressed right-away

install.packages("bda")
install.packages('e1071', dependencies=TRUE)
install.packages('caret', dependencies=TRUE)

library(tidyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(MASS) # For stepAIC
library(car)
library(e1071)
library(caret)
library(cowplot)
library(caTools)
library(bda) # For binning
#library(OneR)
library(GGally) # For correlation computation
library(stringr)

################# CRISP-DM:2 Data collection & Understanding #############################
emp <- read.csv("general_data.csv", header = TRUE, na.strings = c("", "NA"), stringsAsFactors = FALSE)
emp_survey <- read.csv("employee_survey_data.csv", header = TRUE, na.strings = c("", "NA"), stringsAsFactors = FALSE)
mgr_survey <- read.csv("manager_survey_data.csv", header = TRUE, na.strings = c("", "NA"), stringsAsFactors = FALSE)
in_time <- read.csv("in_time.csv", header = TRUE, na.strings = c("", "NA"), stringsAsFactors = FALSE)
in_time <- in_time[,-2] # Removing second variable as it is NA
out_time <- read.csv("Out_time.csv", header = TRUE, na.strings = c("", "NA"), stringsAsFactors = FALSE)
out_time <- out_time[,-2] # Removing second variable as it is NA

# Update first column name to "EmployeeID"
colnames(in_time)[1] <- "EmployeeID"
colnames(out_time)[1] <- "EmployeeID"

# Update subsequent column names & convert to date-time format 
colnames(in_time)[2:ncol(in_time)] <- str_replace(colnames(in_time)[2:ncol(in_time)],"X","")
colnames(in_time)[2:ncol(in_time)] <- paste0("Day-", as.numeric(as.Date(colnames(in_time)[2:ncol(in_time)], format="%Y.%m.%d") - as.Date("2015.01.01", format="%Y.%m.%d")))
in_time[2:ncol(in_time)] <- sapply(in_time[2:ncol(in_time)], function(x) as.POSIXlt(x)) # (Time-consuming)

colnames(out_time)[2:ncol(in_time)] <- str_replace(colnames(out_time)[2:ncol(in_time)],"X","")
colnames(out_time)[2:ncol(out_time)] <- paste0("Day-", as.numeric(as.Date(colnames(out_time)[2:ncol(out_time)], format="%Y.%m.%d") - as.Date("2015.01.01", format="%Y.%m.%d")))
out_time[2:ncol(out_time)] <- sapply(out_time[2:ncol(out_time)], function(x) as.POSIXlt(x)) # (Time-consuming)

# Calculate the actual work-times from attendance dataset
work_time <- out_time[2:ncol(out_time)] - in_time[2:ncol(in_time)] # (Time-consuming)
work_time <- sapply(work_time, function(x)  (round(as.numeric(x),1)))
work_time <- as.data.frame(work_time)
work_time$EmployeeID <- emp$EmployeeID

# Remove complete-NA columns
work_time_na <- sapply(work_time, function(x) sum(is.na(x))) # Select whole NA columns. These are holidays
#work_time_na <- as.data.frame(work_time_na)
work_time <- work_time[,-which(work_time_na==4410)] # Remove holidays

# Compute mean hours spent per day & leaves from in- & out-times data
work_time$Avg_Work_Time <- round(rowMeans(work_time, na.rm = T),1)
work_time$Leaves <- rowSums(is.na(work_time))

str(emp) # 4410 observations of 24 variables, Attrition is the dependent variable
str(emp_survey) # 4410 observations of 4 variables, all ordinals except EmployeeID
str(mgr_survey) # 4410 observations of 3 variables, all ordinals except EmployeeID
str(work_time) # 4410 observations of 252 variables. 262 is number of work-days in calendar year with 10 holidays

# Employee ID duplication check
sum(duplicated(emp$EmployeeID)) # No duplicates found
sum(duplicated(emp_survey$EmployeeID)) # No duplicates found
sum(duplicated(mgr_survey$EmployeeID)) # No duplicates found

setdiff(emp$EmployeeID,emp_survey$EmployeeID) # Identical employeeID across these datasets
setdiff(emp$EmployeeID,mgr_survey$EmployeeID) # Identical employeeID across these datasets

##################### Data Cleaning #################################

# Null check
sapply(emp_survey, function(x) sum(is.na(x))) # Multiple NA observations, needs missing value imputation 
sapply(mgr_survey, function(x) sum(is.na(x))) # No NA observation

install.packages("mice") # For missing value imputation
library(mice)

# Performing missing value imputations for "emp_survey" due to NA
md.pattern(emp_survey) # Summarize missing values in employee_survey. Total 83 missing values
impute_emp_survey <- mice(emp_survey, m=1, maxit = 50, method = 'pmm', seed = 500) # Perform missing value imputation
emp_survey_imp <- complete(impute_emp_survey,1) # Imputed dataframe

# Merge the datasets
employee <- merge(emp, emp_survey_imp, by="EmployeeID")
employee <- merge(employee, mgr_survey, by="EmployeeID")
employee$Avg_Work_Time <- work_time$Avg_Work_Time
employee$Leaves <- work_time$Leaves

str(employee) # 4410 obs. of  31 variables


# Null check
sapply(employee, function(x) sum(is.na(x))) # NumCompaniesWorked=19, TotalWorkingYears=9
# Total of 0.06% (28/4410) observations have NA. Can be deleted
employee <- employee[!is.na(employee$NumCompaniesWorked),]
employee <- employee[!is.na(employee$TotalWorkingYears),]

# Blank Check
sapply(employee, function(x) length(which(x==""))) # No blank observations found

# Dimension reduction
summary(employee) # StandardHours & EmployeeCount have no variation, are redundant
employee <- within(employee, rm("StandardHours","EmployeeCount", "Over18"))


##################### Data Understanding ###############################
employee_TF <- employee # Transformed version of employee DF

# Barcharts for categorical features 
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

bar_theme1<- ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                            legend.position="none")
plot_grid(ggplot(employee_TF, aes(x=BusinessTravel,fill=Attrition))+ geom_bar(), 
          ggplot(employee_TF, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee_TF, aes(x=PercentSalaryHike,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee_TF, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee_TF, aes(x=TrainingTimesLastYear,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee_TF, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")  
# OBSERVATION-1: Specific attrition trend observed on business travel, department (R&D) & TrainingTimes (2 & 3) 

plot_grid(ggplot(employee_TF, aes(x=DistanceFromHome,fill=Attrition))+ geom_bar(), 
          ggplot(employee_TF, aes(x=Education,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee_TF, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee_TF, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee_TF, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee_TF, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h") 
# OBSERVATION-2: Specific attrition trend observed on Distance-from-home, marital-status & education-field

plot_grid(ggplot(employee_TF, aes(x=JobLevel,fill=Attrition))+ geom_bar(), 
          ggplot(employee_TF, aes(x=NumCompaniesWorked,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee_TF, aes(x=TotalWorkingYears,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee_TF, aes(x=YearsAtCompany,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee_TF, aes(x=YearsSinceLastPromotion,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee_TF, aes(x=YearsWithCurrManager,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h") 
# OBSERVATION-3: Specific attrition trend observed on NumCompaniesWorked, YearsAtCompany, TotalWorkingYears

# Outlier treatment
emp_melt <- melt(employee_TF, value.name = "value")
outlier_plots <- ggplot(data = emp_melt, aes(x=variable, y=value)) + geom_boxplot(aes(fill=value)) + facet_wrap( ~ variable, scales="free")
outlier_plots
# Outlier treatment not required as it will impact decision making

# Correlation between numeric variables
ggpairs(employee_TF[, c("Age","MonthlyIncome", "DistanceFromHome", "PercentSalaryHike")]) # No specfic correlation observed
# No correlation observed in any of the continuous variables

##################### CRISP-DM:3 EDA & Data Preparation ###############################

# Normalizing categorical variables with 2 levels
employee_TF$Attrition <- ifelse(employee_TF$Attrition=="Yes",1,0) # Attrition Yes=1, No=0
employee_TF$Gender <- ifelse(employee_TF$Gender=="Male",1,0) # Male=1, Female=0

attrition_rate <- sum(employee_TF$Attrition)/nrow(employee_TF)
attrition_rate # Attrition rate = 16%

# Feature standardization
# Variable scaling - Scale continuous variables
employee_TF$MonthlyIncome <- scale(employee_TF$MonthlyIncome)
employee_TF$Age <- scale(employee_TF$Age)
employee_TF$DistanceFromHome <- scale(employee_TF$DistanceFromHome)
employee_TF$PercentSalaryHike <- scale(employee_TF$PercentSalaryHike)
employee_TF$Avg_Work_Time <- scale(employee_TF$Avg_Work_Time)
employee_TF$Leaves <- scale(employee_TF$Leaves)

# Bin variables with high level of ordinality
employee_TF$TotalWorkingYears <- cut(as.numeric(employee_TF$TotalWorkingYears), breaks = c(-1,10,20,30,50), labels=c(1,2,3,4)) #[0-10]=1, [11-20]=2, [21-30]=3, [31-50]=4
employee_TF$YearsAtCompany <- cut(as.numeric(employee_TF$YearsAtCompany), breaks = c(-1,10,20,30,50), labels=c(1,2,3,4)) #[0-10]=1, [11-20]=2, [21-30]=3, [31-50]=4
employee_TF$YearsSinceLastPromotion <- cut(as.numeric(employee_TF$YearsSinceLastPromotion), breaks = c(-1,4,8,12,16), labels=c(1,2,3,4)) #[0-4]=1, [5-8]=2, [9-12]=3, [13-16]=4
employee_TF$YearsWithCurrManager <- cut(as.numeric(employee_TF$YearsWithCurrManager), breaks = c(-1,5,10,15,20), labels=c(1,2,3,4)) #[0-5]=1, [6-10]=2, [11-15]=3, [16-20]=4
employee_TF$NumCompaniesWorked <- cut(as.numeric(employee_TF$NumCompaniesWorked), breaks = c(-1,4,8,16), labels=c(1,2,3)) #[0-4]=1, [5-8]=2, [9-16]=3

# Dummy variable creation for categorical variables with >2 levels
employee_fact <- subset(employee_TF, select=c("BusinessTravel","Department","Education","EducationField","JobLevel","JobRole","MaritalStatus","StockOptionLevel","TotalWorkingYears","TrainingTimesLastYear","YearsAtCompany","YearsSinceLastPromotion","YearsWithCurrManager","EnvironmentSatisfaction","JobSatisfaction","WorkLifeBalance","JobInvolvement","PerformanceRating","NumCompaniesWorked"))
employee_fact<- data.frame(sapply(employee_fact, function(x) factor(x)))
dummies<- data.frame(sapply(employee_fact, function(x) data.frame(model.matrix(~x,data =employee_fact))))
employee_final <- within(employee_TF, rm("BusinessTravel","Department","Education","EducationField","JobLevel","JobRole","MaritalStatus","StockOptionLevel","TotalWorkingYears","TrainingTimesLastYear","YearsAtCompany","YearsSinceLastPromotion","YearsWithCurrManager","EnvironmentSatisfaction","JobSatisfaction","WorkLifeBalance","JobInvolvement","PerformanceRating","NumCompaniesWorked"))
employee_final <- cbind(employee_final, dummies)
str(employee_final) # 4382 obs. of  91 variables

# Remove redundant variable from workspace
remove(dummies, emp, emp_survey, employee, employee_fact, mgr_survey, work_time_na, impute_emp_survey, emp_survey_imp, in_time, out_time)

##################### CRISP-DM:4 Model Preparation ##############################
# Set seed
set.seed(100)

# Build train & test datasets
trainindices= sample(1:nrow(employee_final), 0.7*nrow(employee_final))
employee_train = employee_final[trainindices,-1]
employee_test = employee_final[-trainindices,-1]

# Logitic Regression Model Building
#Initial model
model_1 = glm(Attrition ~ ., data = employee_train, family = "binomial")
summary(model_1) #AIC 2264 with 30 coeff, Null-Dev=2671 & Res-Dev=2122

# StepAIC for dimensionality redution
model_2<- stepAIC(model_1, direction="both") # (Time-consuming)
summary(model_2)
vif(model_2)

# Removing Department.xSales for high VIF
model_3 <- glm(formula = Attrition ~ Age + Gender + MonthlyIncome + Avg_Work_Time + 
                 Leaves + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + 
                 Education.x2 + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xLaboratory.Technician + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 TotalWorkingYears.x3 + TotalWorkingYears.x4 + TrainingTimesLastYear.x1 + 
                 TrainingTimesLastYear.x4 + TrainingTimesLastYear.x6 + YearsAtCompany.x2 + 
                 YearsAtCompany.x4 + YearsSinceLastPromotion.x2 + YearsSinceLastPromotion.x3 + 
                 YearsSinceLastPromotion.x4 + YearsWithCurrManager.x2 + YearsWithCurrManager.x3 + 
                 YearsWithCurrManager.x4 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x2 + JobInvolvement.x3 + 
                 NumCompaniesWorked.x2 + NumCompaniesWorked.x3, family = "binomial", 
               data = employee_train)
summary(model_3)
vif(model_3)

# Removing WorkLifeBalance.x4 for high VIF
model_4 <- glm(formula = Attrition ~ Age + Gender + MonthlyIncome + Avg_Work_Time + 
                 Leaves + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + 
                 Education.x2 + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xLaboratory.Technician + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 TotalWorkingYears.x3 + TotalWorkingYears.x4 + TrainingTimesLastYear.x1 + 
                 TrainingTimesLastYear.x4 + TrainingTimesLastYear.x6 + YearsAtCompany.x2 + 
                 YearsAtCompany.x4 + YearsSinceLastPromotion.x2 + YearsSinceLastPromotion.x3 + 
                 YearsSinceLastPromotion.x4 + YearsWithCurrManager.x2 + YearsWithCurrManager.x3 + 
                 YearsWithCurrManager.x4 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 JobInvolvement.x2 + JobInvolvement.x3 + 
                 NumCompaniesWorked.x2 + NumCompaniesWorked.x3, family = "binomial", 
               data = employee_train)
summary(model_4)
vif(model_4)

# Removing YearsAtCompany.x4 for high insignificance
model_5 <- glm(formula = Attrition ~ Age + Gender + MonthlyIncome + Avg_Work_Time + 
                 Leaves + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + 
                 Education.x2 + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xLaboratory.Technician + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 TotalWorkingYears.x3 + TotalWorkingYears.x4 + TrainingTimesLastYear.x1 + 
                 TrainingTimesLastYear.x4 + TrainingTimesLastYear.x6 + YearsAtCompany.x2 + 
                 YearsSinceLastPromotion.x2 + YearsSinceLastPromotion.x3 + 
                 YearsSinceLastPromotion.x4 + YearsWithCurrManager.x2 + YearsWithCurrManager.x3 + 
                 YearsWithCurrManager.x4 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 JobInvolvement.x2 + JobInvolvement.x3 + 
                 NumCompaniesWorked.x2 + NumCompaniesWorked.x3, family = "binomial", 
               data = employee_train)
summary(model_5)
vif(model_5)

# Removing MaritalStatus.xMarried for high VIF
model_6 <- glm(formula = Attrition ~ Age + Gender + MonthlyIncome + Avg_Work_Time + 
                 Leaves + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + 
                 Education.x2 + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xLaboratory.Technician + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 TotalWorkingYears.x3 + TotalWorkingYears.x4 + TrainingTimesLastYear.x1 + 
                 TrainingTimesLastYear.x4 + TrainingTimesLastYear.x6 + YearsAtCompany.x2 + 
                 YearsSinceLastPromotion.x2 + YearsSinceLastPromotion.x3 + 
                 YearsSinceLastPromotion.x4 + YearsWithCurrManager.x2 + YearsWithCurrManager.x3 + 
                 YearsWithCurrManager.x4 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 JobInvolvement.x2 + JobInvolvement.x3 + 
                 NumCompaniesWorked.x2 + NumCompaniesWorked.x3, family = "binomial", 
               data = employee_train)
  summary(model_6)
vif(model_6)

# Removing BusinessTravel.xTravel_Rarely for high VIF
model_7 <- glm(formula = Attrition ~ Age + Gender + MonthlyIncome + Avg_Work_Time + 
                 Leaves + BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                 Education.x2 + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xLaboratory.Technician + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 TotalWorkingYears.x3 + TotalWorkingYears.x4 + TrainingTimesLastYear.x1 + 
                 TrainingTimesLastYear.x4 + TrainingTimesLastYear.x6 + YearsAtCompany.x2 + 
                 YearsSinceLastPromotion.x2 + YearsSinceLastPromotion.x3 + 
                 YearsSinceLastPromotion.x4 + YearsWithCurrManager.x2 + YearsWithCurrManager.x3 + 
                 YearsWithCurrManager.x4 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 JobInvolvement.x2 + JobInvolvement.x3 + 
                 NumCompaniesWorked.x2 + NumCompaniesWorked.x3, family = "binomial", 
               data = employee_train)
  summary(model_7)
vif(model_7) # All predictor variables show no multicollinearity

# Removing YearsWithCurrManager.x4 for high insignificance
model_8 <- glm(formula = Attrition ~ Age + Gender + MonthlyIncome + Avg_Work_Time + 
                 Leaves + BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                 Education.x2 + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xLaboratory.Technician + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 TotalWorkingYears.x3 + TotalWorkingYears.x4 + TrainingTimesLastYear.x1 + 
                 TrainingTimesLastYear.x4 + TrainingTimesLastYear.x6 + YearsAtCompany.x2 + 
                 YearsSinceLastPromotion.x2 + YearsSinceLastPromotion.x3 + 
                 YearsSinceLastPromotion.x4 + YearsWithCurrManager.x2 + YearsWithCurrManager.x3 + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 JobInvolvement.x2 + JobInvolvement.x3 + 
                 NumCompaniesWorked.x2 + NumCompaniesWorked.x3, family = "binomial", 
               data = employee_train)
summary(model_8)

# Removing TotalWorkingYears.x4 for high insignificance
model_9 <- glm(formula = Attrition ~ Age + Gender + MonthlyIncome + Avg_Work_Time + 
                 Leaves + BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                 Education.x2 + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xLaboratory.Technician + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle + TotalWorkingYears.x3 + TrainingTimesLastYear.x1 + 
                 TrainingTimesLastYear.x4 + TrainingTimesLastYear.x6 + YearsAtCompany.x2 + 
                 YearsSinceLastPromotion.x2 + YearsSinceLastPromotion.x3 + 
                 YearsSinceLastPromotion.x4 + YearsWithCurrManager.x2 + YearsWithCurrManager.x3 + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 JobInvolvement.x2 + JobInvolvement.x3 + 
                 NumCompaniesWorked.x2 + NumCompaniesWorked.x3, family = "binomial", 
               data = employee_train)
summary(model_9)

# Removing Department.xResearch...Development for high insignificance
model_10 <- glm(formula = Attrition ~ Age + Gender + MonthlyIncome + Avg_Work_Time + 
                  Leaves + BusinessTravel.xTravel_Frequently + Education.x2 + EducationField.xMedical + EducationField.xOther + 
                  EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xLaboratory.Technician + 
                  JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + TotalWorkingYears.x3 + TrainingTimesLastYear.x1 + 
                  TrainingTimesLastYear.x4 + TrainingTimesLastYear.x6 + YearsAtCompany.x2 + 
                  YearsSinceLastPromotion.x2 + YearsSinceLastPromotion.x3 + 
                  YearsSinceLastPromotion.x4 + YearsWithCurrManager.x2 + YearsWithCurrManager.x3 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  JobInvolvement.x2 + JobInvolvement.x3 + 
                  NumCompaniesWorked.x2 + NumCompaniesWorked.x3, family = "binomial", 
                data = employee_train)
summary(model_10)

# Removing Gender for high insignificance
model_11 <- glm(formula = Attrition ~ Age + MonthlyIncome + Avg_Work_Time + 
                  Leaves + BusinessTravel.xTravel_Frequently + Education.x2 + EducationField.xMedical + EducationField.xOther + 
                  EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xLaboratory.Technician + 
                  JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + TotalWorkingYears.x3 + TrainingTimesLastYear.x1 + 
                  TrainingTimesLastYear.x4 + TrainingTimesLastYear.x6 + YearsAtCompany.x2 + 
                  YearsSinceLastPromotion.x2 + YearsSinceLastPromotion.x3 + 
                  YearsSinceLastPromotion.x4 + YearsWithCurrManager.x2 + YearsWithCurrManager.x3 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  JobInvolvement.x2 + JobInvolvement.x3 + 
                  NumCompaniesWorked.x2 + NumCompaniesWorked.x3, family = "binomial", 
                data = employee_train)
summary(model_11)

# Removing TrainingTimesLastYear.x1 for high insignificance
model_11 <- glm(formula = Attrition ~ Age + MonthlyIncome + Avg_Work_Time + 
                  Leaves + BusinessTravel.xTravel_Frequently + Education.x2 + EducationField.xMedical + EducationField.xOther + 
                  EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xLaboratory.Technician + 
                  JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + TotalWorkingYears.x3 + 
                  TrainingTimesLastYear.x4 + TrainingTimesLastYear.x6 + YearsAtCompany.x2 + 
                  YearsSinceLastPromotion.x2 + YearsSinceLastPromotion.x3 + 
                  YearsSinceLastPromotion.x4 + YearsWithCurrManager.x2 + YearsWithCurrManager.x3 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  JobInvolvement.x2 + JobInvolvement.x3 + 
                  NumCompaniesWorked.x2 + NumCompaniesWorked.x3, family = "binomial", 
                data = employee_train)
summary(model_11)

# Removing JobLevel.x5 for high insignificance
model_12 <- glm(formula = Attrition ~ Age + MonthlyIncome + Avg_Work_Time + 
                  Leaves + BusinessTravel.xTravel_Frequently + Education.x2 + EducationField.xMedical + EducationField.xOther + 
                  EducationField.xTechnical.Degree + JobRole.xLaboratory.Technician + 
                  JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + TotalWorkingYears.x3 + 
                  TrainingTimesLastYear.x4 + TrainingTimesLastYear.x6 + YearsAtCompany.x2 + 
                  YearsSinceLastPromotion.x2 + YearsSinceLastPromotion.x3 + 
                  YearsSinceLastPromotion.x4 + YearsWithCurrManager.x2 + YearsWithCurrManager.x3 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  JobInvolvement.x2 + JobInvolvement.x3 + 
                  NumCompaniesWorked.x2 + NumCompaniesWorked.x3, family = "binomial", 
                data = employee_train)
summary(model_12)

# Removing JobRole.xLaboratory.Technician for high insignificance
model_13 <- glm(formula = Attrition ~ Age + MonthlyIncome + Avg_Work_Time + 
                  Leaves + BusinessTravel.xTravel_Frequently + Education.x2 + EducationField.xMedical + EducationField.xOther + 
                  EducationField.xTechnical.Degree + JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + TotalWorkingYears.x3 + 
                  TrainingTimesLastYear.x4 + TrainingTimesLastYear.x6 + YearsAtCompany.x2 + 
                  YearsSinceLastPromotion.x2 + YearsSinceLastPromotion.x3 + 
                  YearsSinceLastPromotion.x4 + YearsWithCurrManager.x2 + YearsWithCurrManager.x3 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  JobInvolvement.x2 + JobInvolvement.x3 + 
                  NumCompaniesWorked.x2 + NumCompaniesWorked.x3, family = "binomial", 
                data = employee_train)
summary(model_13)

# Removing EducationField.xOther for high insignificance
model_14 <- glm(formula = Attrition ~ Age + MonthlyIncome + Avg_Work_Time + 
                  Leaves + BusinessTravel.xTravel_Frequently + Education.x2 + EducationField.xMedical + 
                  EducationField.xTechnical.Degree + JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + TotalWorkingYears.x3 + 
                  TrainingTimesLastYear.x4 + TrainingTimesLastYear.x6 + YearsAtCompany.x2 + 
                  YearsSinceLastPromotion.x2 + YearsSinceLastPromotion.x3 + 
                  YearsSinceLastPromotion.x4 + YearsWithCurrManager.x2 + YearsWithCurrManager.x3 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  JobInvolvement.x2 + JobInvolvement.x3 + 
                  NumCompaniesWorked.x2 + NumCompaniesWorked.x3, family = "binomial", 
                data = employee_train)
summary(model_14)

# Removing EducationField.xMedical for high insignificance
model_15 <- glm(formula = Attrition ~ Age + MonthlyIncome + Avg_Work_Time + 
                  Leaves + BusinessTravel.xTravel_Frequently + Education.x2 + 
                  EducationField.xTechnical.Degree + JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + TotalWorkingYears.x3 + 
                  TrainingTimesLastYear.x4 + TrainingTimesLastYear.x6 + YearsAtCompany.x2 + 
                  YearsSinceLastPromotion.x2 + YearsSinceLastPromotion.x3 + 
                  YearsSinceLastPromotion.x4 + YearsWithCurrManager.x2 + YearsWithCurrManager.x3 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  JobInvolvement.x2 + JobInvolvement.x3 + 
                  NumCompaniesWorked.x2 + NumCompaniesWorked.x3, family = "binomial", 
                data = employee_train)
summary(model_15)

# Removing EducationField.xTechnical.Degree for high insignificance
model_16 <- glm(formula = Attrition ~ Age + MonthlyIncome + Avg_Work_Time + 
                  Leaves + BusinessTravel.xTravel_Frequently + Education.x2 + 
                  JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + TotalWorkingYears.x3 + 
                  TrainingTimesLastYear.x4 + TrainingTimesLastYear.x6 + YearsAtCompany.x2 + 
                  YearsSinceLastPromotion.x2 + YearsSinceLastPromotion.x3 + 
                  YearsSinceLastPromotion.x4 + YearsWithCurrManager.x2 + YearsWithCurrManager.x3 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  JobInvolvement.x2 + JobInvolvement.x3 + 
                  NumCompaniesWorked.x2 + NumCompaniesWorked.x3, family = "binomial", 
                data = employee_train)
summary(model_16)

# Removing WorkLifeBalance.x2 for high insignificance
model_17 <- glm(formula = Attrition ~ Age + MonthlyIncome + Avg_Work_Time + 
                  Leaves + BusinessTravel.xTravel_Frequently + Education.x2 + 
                  JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + TotalWorkingYears.x3 + 
                  TrainingTimesLastYear.x4 + TrainingTimesLastYear.x6 + YearsAtCompany.x2 + 
                  YearsSinceLastPromotion.x2 + YearsSinceLastPromotion.x3 + 
                  YearsSinceLastPromotion.x4 + YearsWithCurrManager.x2 + YearsWithCurrManager.x3 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + JobInvolvement.x2 + JobInvolvement.x3 + 
                  NumCompaniesWorked.x2 + NumCompaniesWorked.x3, family = "binomial", data = employee_train)
summary(model_17)

# Removing JobInvolvement.x2 for high insignificance
model_18 <- glm(formula = Attrition ~ Age + MonthlyIncome + Avg_Work_Time + 
                  Leaves + BusinessTravel.xTravel_Frequently + Education.x2 + 
                  JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + TotalWorkingYears.x3 + 
                  TrainingTimesLastYear.x4 + TrainingTimesLastYear.x6 + YearsAtCompany.x2 + 
                  YearsSinceLastPromotion.x2 + YearsSinceLastPromotion.x3 + 
                  YearsSinceLastPromotion.x4 + YearsWithCurrManager.x2 + YearsWithCurrManager.x3 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + JobInvolvement.x3 + 
                  NumCompaniesWorked.x2 + NumCompaniesWorked.x3, family = "binomial", data = employee_train)
summary(model_18)

# Removing JobInvolvement.x3 variables for high insignificance
model_19 <- glm(formula = Attrition ~ Age + MonthlyIncome + Avg_Work_Time + 
                  Leaves + BusinessTravel.xTravel_Frequently + Education.x2 + 
                  JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + TotalWorkingYears.x3 + 
                  TrainingTimesLastYear.x4 + TrainingTimesLastYear.x6 + YearsAtCompany.x2 + 
                  YearsSinceLastPromotion.x2 + YearsSinceLastPromotion.x3 + 
                  YearsSinceLastPromotion.x4 + YearsWithCurrManager.x2 + YearsWithCurrManager.x3 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + NumCompaniesWorked.x2 + 
                  NumCompaniesWorked.x3, family = "binomial", data = employee_train)
summary(model_19)

# Removing MonthlyIncome for high insignificance
model_20 <- glm(formula = Attrition ~ Age + Avg_Work_Time + Leaves + BusinessTravel.xTravel_Frequently + Education.x2 + 
                  JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + TotalWorkingYears.x3 + 
                  TrainingTimesLastYear.x4 + TrainingTimesLastYear.x6 + YearsAtCompany.x2 + 
                  YearsSinceLastPromotion.x2 + YearsSinceLastPromotion.x3 + 
                  YearsSinceLastPromotion.x4 + YearsWithCurrManager.x2 + YearsWithCurrManager.x3 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + NumCompaniesWorked.x2 + 
                  NumCompaniesWorked.x3, family = "binomial", data = employee_train)
summary(model_20)

# Removing JobRole.xResearch.Scientist for high insignificance
model_21 <- glm(formula = Attrition ~ Age + Avg_Work_Time + Leaves + BusinessTravel.xTravel_Frequently + Education.x2 + 
                  JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle + TotalWorkingYears.x3 + 
                  TrainingTimesLastYear.x4 + TrainingTimesLastYear.x6 + YearsAtCompany.x2 + 
                  YearsSinceLastPromotion.x2 + YearsSinceLastPromotion.x3 + 
                  YearsSinceLastPromotion.x4 + YearsWithCurrManager.x2 + YearsWithCurrManager.x3 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + NumCompaniesWorked.x2 + 
                  NumCompaniesWorked.x3, family = "binomial", data = employee_train)
summary(model_21)

# Removing JobRole.xSales.Executive for high insignificance
model_22 <- glm(formula = Attrition ~ Age + Avg_Work_Time + Leaves + BusinessTravel.xTravel_Frequently + Education.x2 + 
                  JobRole.xResearch.Director + MaritalStatus.xSingle + TotalWorkingYears.x3 + 
                  TrainingTimesLastYear.x4 + TrainingTimesLastYear.x6 + YearsAtCompany.x2 + 
                  YearsSinceLastPromotion.x2 + YearsSinceLastPromotion.x3 + 
                  YearsSinceLastPromotion.x4 + YearsWithCurrManager.x2 + YearsWithCurrManager.x3 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + NumCompaniesWorked.x2 + 
                  NumCompaniesWorked.x3, family = "binomial", data = employee_train)
summary(model_22)

# Removing JobRole.xResearch.Director for high insignificance
model_23 <- glm(formula = Attrition ~ Age + Avg_Work_Time + Leaves + BusinessTravel.xTravel_Frequently + Education.x2 + 
                  MaritalStatus.xSingle + TotalWorkingYears.x3 + TrainingTimesLastYear.x4 + TrainingTimesLastYear.x6 + YearsAtCompany.x2 + 
                  YearsSinceLastPromotion.x2 + YearsSinceLastPromotion.x3 + 
                  YearsSinceLastPromotion.x4 + YearsWithCurrManager.x2 + YearsWithCurrManager.x3 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + NumCompaniesWorked.x2 + 
                  NumCompaniesWorked.x3, family = "binomial", data = employee_train)
summary(model_23)

# Removing JobSatisfaction.x2 for high insignificance
model_24 <- glm(formula = Attrition ~ Age + Avg_Work_Time + Leaves + BusinessTravel.xTravel_Frequently + Education.x2 + 
                  MaritalStatus.xSingle + TotalWorkingYears.x3 + TrainingTimesLastYear.x4 + TrainingTimesLastYear.x6 + YearsAtCompany.x2 + 
                  YearsSinceLastPromotion.x2 + YearsSinceLastPromotion.x3 + 
                  YearsSinceLastPromotion.x4 + YearsWithCurrManager.x2 + YearsWithCurrManager.x3 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x3 + JobSatisfaction.x4 + WorkLifeBalance.x3 + NumCompaniesWorked.x2 + 
                  NumCompaniesWorked.x3, family = "binomial", data = employee_train)
summary(model_24)

# Removing TrainingTimesLastYear.x4 for high insignificance
model_25 <- glm(formula = Attrition ~ Age + Avg_Work_Time + Leaves + BusinessTravel.xTravel_Frequently + Education.x2 + 
                  MaritalStatus.xSingle + TotalWorkingYears.x3 + TrainingTimesLastYear.x6 + YearsAtCompany.x2 + 
                  YearsSinceLastPromotion.x2 + YearsSinceLastPromotion.x3 + 
                  YearsSinceLastPromotion.x4 + YearsWithCurrManager.x2 + YearsWithCurrManager.x3 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x3 + JobSatisfaction.x4 + WorkLifeBalance.x3 + NumCompaniesWorked.x2 + 
                  NumCompaniesWorked.x3, family = "binomial", data = employee_train)
summary(model_25)

# Removing YearsWithCurrManager.x2 for high insignificance
model_26 <- glm(formula = Attrition ~ Age + Avg_Work_Time + Leaves + BusinessTravel.xTravel_Frequently + Education.x2 + 
                  MaritalStatus.xSingle + TotalWorkingYears.x3 + TrainingTimesLastYear.x6 + YearsAtCompany.x2 + 
                  YearsSinceLastPromotion.x2 + YearsSinceLastPromotion.x3 + 
                  YearsSinceLastPromotion.x4 + YearsWithCurrManager.x3 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x3 + JobSatisfaction.x4 + WorkLifeBalance.x3 + NumCompaniesWorked.x2 + 
                  NumCompaniesWorked.x3, family = "binomial", data = employee_train)
summary(model_26)

# Removing YearsSinceLastPromotion.x2 for high insignificance
model_27 <- glm(formula = Attrition ~ Age + Avg_Work_Time + Leaves + BusinessTravel.xTravel_Frequently + Education.x2 + 
                  MaritalStatus.xSingle + TotalWorkingYears.x3 + TrainingTimesLastYear.x6 + YearsAtCompany.x2 + 
                  YearsSinceLastPromotion.x3 + YearsSinceLastPromotion.x4 + YearsWithCurrManager.x3 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x3 + JobSatisfaction.x4 + WorkLifeBalance.x3 + NumCompaniesWorked.x2 + 
                  NumCompaniesWorked.x3, family = "binomial", data = employee_train)
summary(model_27)

# Removing YearsSinceLastPromotion.x3 for high insignificance
model_28 <- glm(formula = Attrition ~ Age + Avg_Work_Time + Leaves + BusinessTravel.xTravel_Frequently + Education.x2 + 
                  MaritalStatus.xSingle + TotalWorkingYears.x3 + TrainingTimesLastYear.x6 + YearsAtCompany.x2 + 
                  YearsSinceLastPromotion.x4 + YearsWithCurrManager.x3 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x3 + JobSatisfaction.x4 + WorkLifeBalance.x3 + NumCompaniesWorked.x2 + 
                  NumCompaniesWorked.x3, family = "binomial", data = employee_train)
summary(model_28)

# Removing Leaves for high insignificance
model_29 <- glm(formula = Attrition ~ Age + Avg_Work_Time + BusinessTravel.xTravel_Frequently + Education.x2 + 
                  MaritalStatus.xSingle + TotalWorkingYears.x3 + TrainingTimesLastYear.x6 + YearsAtCompany.x2 + 
                  YearsSinceLastPromotion.x4 + YearsWithCurrManager.x3 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x3 + JobSatisfaction.x4 + WorkLifeBalance.x3 + NumCompaniesWorked.x2 + 
                  NumCompaniesWorked.x3, family = "binomial", data = employee_train)
summary(model_29)

# Removing YearsWithCurrManager.x3 for high insignificance
model_30 <- glm(formula = Attrition ~ Age + Avg_Work_Time + BusinessTravel.xTravel_Frequently + Education.x2 + 
                  MaritalStatus.xSingle + TotalWorkingYears.x3 + TrainingTimesLastYear.x6 + YearsAtCompany.x2 + 
                  YearsSinceLastPromotion.x4 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x3 + JobSatisfaction.x4 + WorkLifeBalance.x3 + NumCompaniesWorked.x2 + 
                  NumCompaniesWorked.x3, family = "binomial", data = employee_train)
summary(model_30)

# Removing Education.x2 for high insignificance
model_31 <- glm(formula = Attrition ~ Age + Avg_Work_Time + BusinessTravel.xTravel_Frequently + 
                  MaritalStatus.xSingle + TotalWorkingYears.x3 + TrainingTimesLastYear.x6 + YearsAtCompany.x2 + 
                  YearsSinceLastPromotion.x4 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x3 + JobSatisfaction.x4 + WorkLifeBalance.x3 + NumCompaniesWorked.x2 + 
                  NumCompaniesWorked.x3, family = "binomial", data = employee_train)
summary(model_31)

# Removing Avg_Work_Time for high insignificance
model_32 <- glm(formula = Attrition ~ Age + BusinessTravel.xTravel_Frequently + 
                  MaritalStatus.xSingle + TotalWorkingYears.x3 + TrainingTimesLastYear.x6 + YearsAtCompany.x2 + 
                  YearsSinceLastPromotion.x4 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x3 + JobSatisfaction.x4 + WorkLifeBalance.x3 + NumCompaniesWorked.x2 + 
                  NumCompaniesWorked.x3, family = "binomial", data = employee_train)
summary(model_32)

# Removing JobSatisfaction.x3 for high insignificance
model_33 <- glm(formula = Attrition ~ Age + BusinessTravel.xTravel_Frequently + 
                  MaritalStatus.xSingle + TotalWorkingYears.x3 + TrainingTimesLastYear.x6 + YearsAtCompany.x2 + 
                  YearsSinceLastPromotion.x4 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x4 + WorkLifeBalance.x3 + NumCompaniesWorked.x2 + 
                  NumCompaniesWorked.x3, family = "binomial", data = employee_train)
summary(model_33)

# Removing TotalWorkingYears.x3 for high insignificance
model_34 <- glm(formula = Attrition ~ Age + BusinessTravel.xTravel_Frequently + 
                  MaritalStatus.xSingle + TrainingTimesLastYear.x6 + YearsAtCompany.x2 + 
                  YearsSinceLastPromotion.x4 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x4 + WorkLifeBalance.x3 + NumCompaniesWorked.x2 + 
                  NumCompaniesWorked.x3, family = "binomial", data = employee_train)
summary(model_34)

# Removing YearsSinceLastPromotion.x4 for high insignificance
model_35 <- glm(formula = Attrition ~ Age + BusinessTravel.xTravel_Frequently + 
                  MaritalStatus.xSingle + TrainingTimesLastYear.x6 + YearsAtCompany.x2 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x4 + WorkLifeBalance.x3 + NumCompaniesWorked.x2 + 
                  NumCompaniesWorked.x3, family = "binomial", data = employee_train)
summary(model_35)

# Removing TrainingTimesLastYear.x6 for high insignificance
model_36 <- glm(formula = Attrition ~ Age + BusinessTravel.xTravel_Frequently + 
                  MaritalStatus.xSingle + YearsAtCompany.x2 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x4 + WorkLifeBalance.x3 + NumCompaniesWorked.x2 + 
                  NumCompaniesWorked.x3, family = "binomial", data = employee_train)
summary(model_36)
vif(model_36)

final_model<- model_36 # With 11 significant predictor variables, AIC=2373, Null-Dev=2671, Res-Dev=2349
# Predictor variables are: 
# 1.Age, 
# 2.BusinessTravel (Frequently), 
# 3.MaritalStatus (Single), 
# 4 & 5.NumCompaniesWorked (2 & 3), 
# 6.YearsAtCompany (2), 
# 7,8 & 9.EnvironmentSatisfaction (2 to 4 - Medium to very high), 
# 10.JobSatisfaction (4 - Very High),
# 11.WorkLifeBalance (3 - Better),


##################### CRISP-DM:5 Model Evaluation ##############################

# Predicted probabilities of attrition for test data

test_pred = predict(final_model, type = "response", employee_test)
employee_test$predict <- test_pred

summary(test_pred) # Min = 0.01070, Max = 0.75647


#######################################################################
# Assessing the final model at probability cutoff of 70%.
pred_attrition <- factor(ifelse(employee_test$predict >= 0.70, "Yes", "No"))
act_attrition <- factor(ifelse(employee_test$Attrition == 1,"Yes","No"))
table(act_attrition,pred_attrition)
test_conf_max <- confusionMatrix(pred_attrition, act_attrition, positive = "Yes")
test_conf_max
# True Positives = 1
# True Negatives = 1093
# False Positives = 221
# False Negatives = 0
# Accuracy = 83.2%, Sensitivity = 0.4%, Specificity = 100%. Equal number of positive attrition cases are incorrectly predicted

#######################################################################
# Assessing the final model at probability cutoff of 50%.
pred_attrition <- factor(ifelse(employee_test$predict >= 0.50, "Yes", "No"))
act_attrition <- factor(ifelse(employee_test$Attrition == 1,"Yes","No"))
table(act_attrition,pred_attrition)
test_conf_50 <- confusionMatrix(pred_attrition, act_attrition, positive = "Yes")
test_conf_50
# True Positives = 21
# True Negatives = 1084
# False Positives = 201
# False Negatives = 9
# Accuracy = 84%, Sensitivity = 9.5%, Specificity = 99.1%. Sensitivity is too low

#######################################################################
# Assessing the final model at probability cutoff of 10%.
pred_attrition <- factor(ifelse(employee_test$predict >= 0.10, "Yes", "No"))
table(act_attrition,pred_attrition)
test_conf_min <- confusionMatrix(pred_attrition, act_attrition, positive = "Yes")
test_conf_min
# True Positives = 185
# True Negatives = 498
# False Positives = 595
# False Negatives = 39
# Accuracy = 51%, Sensitivity = 82.4%, Specificity = 45.5%. Accuracy is low
# Need to find optimal cutoff where all three metrics are relatively high

#########################################################################################
# Finding the optimal probalility cutoff between min & max for test_pred

perform_fn <- function(cutoff) 
{
  predicted_attr <- factor(ifelse(employee_test$predict >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attr, act_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


# Creating cutoff values from Min = 0.01070, Max = 0.75647 for plotting and initiallizing a matrix of 100 X 3.
s = seq(.01,.75,length=100)
OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

# Plot the confusion matrix
out_df <- data.frame(OUT)
out_df$iter <- c(1:100)
out_df %>% gather(key,value, X1, X2, X3) %>% ggplot(aes(x=iter, y=value, colour=key)) + geom_line() +
  labs(title = "Confusion Matrix", x = "Iterations", y = "Value", color = "Legend\n") +
  scale_color_manual(labels = c("Sensitivity", "Specificity", "Accuracy"), values = c("blue", "red", "green")) +
  theme_bw()
 
# Select cutoff as the intersection point for optimal attrition probability
cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.02)] # Cutoff = 0.152 (predicted probability)

# Assessing the final model at probability cutoff of 0.152 as the final model
test_attr_cutoff <- factor(ifelse(employee_test$predict >=0.152, "Yes", "No"))
conf_final <- confusionMatrix(test_attr_cutoff, act_attrition, positive = "Yes")
conf_final
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]
acc # Accuracy = 65.4%
sens # Sensitivity = 67.1%
spec # Specificity = 65.1%

View(employee_test)

##################################################################################################
### KS-statistic - Test Data ######

test_attr_cutoff <- ifelse(test_attr_cutoff=="Yes",1,0)
act_attrition <- ifelse(act_attrition=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_attr_cutoff, act_attrition)

performance_measures_test <- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test) # KS-statistic = 0.322

####################################################################
# Model assessment through Lift & Gain Charts 

# plotting the lift chart

require(dplyr)

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

# Assess deciles for gain & lift metrics
Attr_decile = lift(act_attrition, test_pred, groups = 10)
Attr_decile$NonResp <- rep(0, each=10)
Attr_decile$NonResp <- Attr_decile$total - Attr_decile$totalresp # Calculate for non-attrition
Attr_decile$TotalNonResp <- rep(0, each=10)
Attr_decile$NonRespRatio <- rep(0, each=10)
Attr_decile$TotalNonResp[1] <- Attr_decile$NonResp[1] # Calculate for total non-attrition
for(i in 2:10)
{
  Attr_decile$TotalNonResp[i] <- Attr_decile$TotalNonResp[i-1]+Attr_decile$NonResp[i]
}
for(i in 1:10)
{
  Attr_decile$NonRespRatio[i] <- 100*Attr_decile$TotalNonResp[i]/sum(Attr_decile$NonResp)
}
Attr_decile$KS <- round(Attr_decile$Gain - Attr_decile$NonRespRatio,1)
max(Attr_decile$KS) # KS Statistic = 34.3
which.max(Attr_decile$KS) # 3rd decile

# Assess the model through Gain chart
Attr_decile %>% gather(key,value, Gain) %>% ggplot(aes(x=bucket, y=value, colour=key)) + geom_line() + geom_point() +
  scale_x_discrete(limits = c(1:10)) +
  geom_text(aes(y=value, label=paste0(round(value, digits=1), '%')), vjust = -0.8, size = 4) +
  labs(title = "Gain Chart", x = "Decile", y = "Gain (%)", color = "Legend") +
  theme_bw()
# 74% of the attrition cases lie within first 5 deciles


# Assess the model through Lift chart
Attr_decile %>% gather(key,value, Cumlift) %>% ggplot(aes(x=bucket, y=value, colour=key)) + geom_line() + geom_point() +
  scale_x_discrete(limits = c(1:10)) +
  geom_text(aes(y=value, label=paste0(round(value, digits=1), '%')), vjust = -0.8, size = 4) +
  labs(title = "Lift Chart", x = "Decile", y = "Cumulative Lift (%)", color = "Legend") +
  theme_bw()
# Final model shows better performance upto 4 deciles with random models

View(Attr_decile) # KS Statistic = 34.3 at 3rd decile


options(scipen=999) # update to ensure numeric format
write.csv(Attr_decile, file="HR_Analytics_decile.csv") # serialize decile data frame to a CSV. 
