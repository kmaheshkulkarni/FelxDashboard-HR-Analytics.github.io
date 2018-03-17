---
title: "HR Analytics"
output: 
  flexdashboard::flex_dashboard:
    storyboard: true
    social: menu
    source: embed
    theme: flatly
    runtime: shiny
---
    
    ```{r setup, include=FALSE}
library(flexdashboard)
```

```{r global, include=FALSE, warning=FALSE, message=FALSE}
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(shiny)

# Loading files
general_data <- read.csv("general_data.csv", stringsAsFactors = F)
survey_data <- read.csv("employee_survey_data.csv", stringsAsFactors = F)
manager_survey_data <- read.csv("manager_survey_data.csv", stringsAsFactors = F)
in_time <- read.csv("in_time.csv", stringsAsFactors = F)
out_time <- read.csv("out_time.csv", stringsAsFactors = F)

# fix employeeID column name of in_time and out_time
colnames(in_time)[colnames(in_time) == "X"] <- "EmployeeID"
head(colnames(in_time))
colnames(out_time)[colnames(out_time) == "X"] <- "EmployeeID"
head(colnames(out_time))

# check if employeeID is indeed a primary key or not
setdiff(survey_data$EmployeeID,general_data$EmployeeID) # Identical customerID across these datasets
setdiff(survey_data$EmployeeID,manager_survey_data$EmployeeID) # Identical customerID across these datasets
setdiff(survey_data$EmployeeID,in_time$EmployeeID) # Identical customerID across these datasets
setdiff(survey_data$EmployeeID,out_time$EmployeeID) # Identical customerID across these datasets
# convert in_time and out_time columns to date format
# the final values will contain seconds passed from 01-01-1970 till the date into consideration
# hence, expect large values
change_to_date <- function(data){
  
  data[,2:ncol(data)] <- as.data.frame(sapply(
    data[, 2:ncol(data)], 
    function(x){
      as.POSIXct(strptime(x, format = "%Y-%m-%d %H:%M:%S"))
    }
  ))
  
  return(data)
}

in_time <- change_to_date(in_time)
out_time <- change_to_date(out_time)

# create new empty data frame which is going to contain time differences in seconds
time_difference <- matrix(nrow = nrow(in_time), ncol = ncol(in_time))
time_difference <- as.data.frame(time_difference)

# calculate the time for which an employee has worked for each day (in hours)
for(col_number in 2:ncol(in_time)){  # do not include the 1st column which belongs to employeeID
  
  # get time difference in for each employee for a particular day in seconds
  time_difference[, col_number] <- out_time[, col_number] - in_time[, col_number]
  
  # convert to seconds difference to hours
  time_difference[, col_number] <- time_difference[, col_number]/3600
  
}

# average working hours of each employee
avg_time <- apply(time_difference, 1, mean, na.rm = T)
work_hours <- data.frame(EmployeeID = general_data$EmployeeID, AverageWorkTime = avg_time)

# merge all the dataframes into one
hr_analytics <- merge(work_hours, general_data, by = "EmployeeID", all = F)
hr_analytics <- merge(hr_analytics, survey_data, by = "EmployeeID", all = F)
hr_analytics <- merge(hr_analytics, manager_survey_data, by = "EmployeeID", all = F)
str(hr_analytics)

################################################################

### Data Preparation & Exploratory Data Analysis

# calculate average working time for each employee 
# i.e. average difference between out time and in time

# convert character variables to factor
FACTOR_VARIABLES <- c("Attrition","BusinessTravel","Department","Education","EducationField",
                      "Gender","JobLevel","JobRole","MaritalStatus","Over18","StockOptionLevel",
                      "EnvironmentSatisfaction", "JobSatisfaction","WorkLifeBalance","JobInvolvement",        
                      "PerformanceRating")
hr_analytics[, FACTOR_VARIABLES] <- as.data.frame(sapply(hr_analytics[, FACTOR_VARIABLES], as.factor))

# convert integer varaibles to Numeric
INTEGER_VARIABLES <- lapply(hr_analytics, class) == "integer"
hr_analytics[, INTEGER_VARIABLES] <- lapply(hr_analytics[, INTEGER_VARIABLES], as.numeric)
sapply(hr_analytics,class)

final_data <- hr_analytics
EmployeeID <- final_data$EmployeeID
final_data$EmployeeID <- NULL
Numeric_vars <- lapply(final_data, class) == "numeric"

#Missing values
sapply(final_data, function(x) sum(is.na(x)))

#Percentage of missing values
colMeans(is.na(final_data))

#Remove the missing values since percentage of number of missing values are very low
final_data<-na.omit(final_data)

#final_data<-read.csv("final_data.csv")
# Feature standardisation

# Normalising continuous features 

# creating a dataframe of numerical features
INTEGER_VARIABLES <- lapply(final_data, class) == "integer" | lapply(final_data, class) == "numeric"
attrition_data_num<- final_data[, INTEGER_VARIABLES]

# Standardising all numerical features
attrition_data_stand<- data.frame(sapply(attrition_data_num, 
                                         function(x) scale(x)))

# remove StandardHours because it contains NaN only
attrition_data_stand$StandardHours <- NULL

# creating a dataframe of categorical features
attrition_data_fact<- final_data[, FACTOR_VARIABLES]

# remove Over18 because it has only one category
attrition_data_fact$Over18<-NULL

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(attrition_data_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =attrition_data_fact))[,-1]))

# converting target variable attrition from No/Yes character to factorwith levels 0/1 
dummies$Attrition<- as.numeric(dummies$Attrition)

# Checking attrition rate of employees

attrition_rate <- sum(dummies$Attrition)/nrow(dummies)
attrition_rate

# Final dataset
attrition_final<- cbind(attrition_data_stand,dummies) 
View(attrition_final)

# remove EmployeeCount because it contains NaN only
attrition_final$EmployeeCount<-NULL

########################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(attrition_final$Attrition, SplitRatio = 0.7)

train = attrition_final[indices,]

test = attrition_final[!(indices),]

########################################################################
# Logistic Regression: 

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1)

# Stepwise selection
library("MASS")
model_2<- stepAIC(model_1, direction="both")

summary(model_2)

# Removing multicollinearity through VIF check
library(car)
vif(model_2)

#Excluding EducationField.xLife.Sciences 
model_3<- glm(formula = Attrition ~ AverageWorkTime + Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Education.x5 + EducationField.xMarketing +
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree +
                JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager +
                JobRole.xManufacturing.Director + JobRole.xResearch.Director +
                JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle +
                StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 +
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + JobInvolvement.x3,
              family = "binomial", data = train) 

summary(model_3) 

vif(model_3) 

# Excluding EducationField.xMarketing
model_4<- glm(formula = Attrition ~ AverageWorkTime + Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Education.x5 + EducationField.xMedical +
                EducationField.xOther + EducationField.xTechnical.Degree + JobLevel.x2 +
                JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager +
                JobRole.xManufacturing.Director + JobRole.xResearch.Director +
                JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle +
                StockOptionLevel.x1 + EnvironmentSatisfaction.x2 +
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                JobInvolvement.x3, family = "binomial", data = train) 

summary(model_4)

#Excluding EducationField.xMedical
model_5<- glm(formula = Attrition ~ AverageWorkTime + Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Education.x5 + 
                EducationField.xOther + 
                EducationField.xTechnical.Degree + JobLevel.x2 + JobLevel.x5 + 
                JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xMarried + 
                MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                JobInvolvement.x3, family = "binomial", data = train) 

summary(model_5) 

#Excluding EducationField.xOther
model_6<- glm(formula = Attrition ~ AverageWorkTime + Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Education.x5 + 
                EducationField.xTechnical.Degree + JobLevel.x2 + JobLevel.x5 + 
                JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xMarried + 
                MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                JobInvolvement.x3, family = "binomial", data = train)  

summary(model_6)

#Excluding EducationField.xTechnical.Degree
model_7<- glm(formula = Attrition ~ AverageWorkTime + Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Education.x5 + 
                JobLevel.x2 + JobLevel.x5 + 
                JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xMarried + 
                MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                JobInvolvement.x3, family = "binomial", data = train)    

summary(model_7) 

#Excluding JobLevel.x5
model_8<- glm(formula = Attrition ~ AverageWorkTime + Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Education.x5 + 
                JobLevel.x2 + 
                JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xMarried + 
                MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                JobInvolvement.x3, family = "binomial", data = train)     

summary(model_8) 

#Excluding MaritalStatus.xMarried
model_9<- glm(formula = Attrition ~ AverageWorkTime + Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Education.x5 + 
                JobLevel.x2 + 
                JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive + 
                MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                JobInvolvement.x3, family = "binomial", data = train)     

summary(model_9) 

#Excluding Education.x5
model_10 <- glm(formula = Attrition ~ AverageWorkTime + Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + 
                  JobLevel.x2 + 
                  JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Director + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  JobInvolvement.x3, family = "binomial", data = train)      

summary(model_10) 

# excluding JobRole.xHuman.Resources
model_11 <- glm(formula = Attrition ~ AverageWorkTime + Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + 
                  JobLevel.x2 + 
                  JobRole.xManager + JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Director + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  JobInvolvement.x3, family = "binomial", data = train)      

summary(model_11) 

# excluding StockOptionLevel.x1
model_12 <- glm(formula = Attrition ~ AverageWorkTime + Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + JobLevel.x2 + 
                  JobRole.xManager + JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Director + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  JobInvolvement.x3, family = "binomial", data = train)      

summary(model_12)

# excluding JobRole.xManager
model_13 <- glm(formula = Attrition ~ AverageWorkTime + Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + JobLevel.x2 + 
                  JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Director + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  JobInvolvement.x3, family = "binomial", data = train)      

summary(model_13)

# excluding JobLevel.x2
model_14 <- glm(formula = Attrition ~ AverageWorkTime + Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Director + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  JobInvolvement.x3, family = "binomial", data = train)      

summary(model_14)

# excluding JobInvolvement.x3
model_15 <- glm(formula = Attrition ~ AverageWorkTime + Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Director + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4, 
                family = "binomial", data = train)      

summary(model_15)

########################################################################
# Final Model With only significant variables in the model

final_model<- model_15

#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of Attrition for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test)


# Let's see the summary 

summary(test_pred)

# Let's use the probability cutoff of 50%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf
#######################################################################

#########################################################################################
# Sensitivity is very low. So let's choose a different cutoff value

# Let's find out the optimal probalility cutoff 
# First let's create a function to find the accuracy, sensitivity and specificity
# for a given cutoff

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.01 to 0.80 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

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

# Let's choose a cutoff value of 0.194 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >=0.194, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)

##################################################################################################
### Exporting Data for Excel Analysis (KS, Gain, Lift etc.) ######

myeval <- matrix(nrow = length(test_pred),ncol = 2)
myeval[,1] <- test_pred
myeval[,2] <- test_actual_attrition
colnames(myeval) <- c("Predicted_Prob","Actual_Labels")
write.csv(myeval,"myeval.csv")

##################################################################################################
### KS -statistic - Test Data ######

library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)


####################################################################
# Lift & Gain Chart 

# Loading dplyr package 
library(dplyr)

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

Attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)
Attrition_decile

Gain <- c(0,Attrition_decile$Gain)
Deciles <- c(0,Attrition_decile$bucket)
plot(y=Gain,x=Deciles,type ="l",lwd = 2,xlab="Bucket",ylab="Gain",main = "Gain Chart")

Random_Gain <- seq(from=0,to=100,by=10)
lines(y=Random_Gain,x=Deciles,type ="l",lwd = 2, col="red")

Perfect_Gain <- vector(mode = "numeric", length = 11)
for (i in 2:11){Perfect_Gain[i] <- 100*min(1,129*(i-1)/209)}
lines(y=Perfect_Gain,x=Deciles,type ="l",lwd = 2, col="darkgreen")



legend("bottomright",col=c("darkgreen","black","red"),lwd =c(2,2,2,2),c("Perfect Model","Actual Model","Random Model"), cex = 0.7)

# plotting the lift chart
Lift <- Gain/Random_Gain
Random_Lift <- Random_Gain/Random_Gain

plot(y=Lift,x=Deciles,type ="l",ylim=c(0,3.5),lwd = 2,xlab="Bucket",ylab="Lift",main = "Lift Chart",ylim<-c())
lines(y=Random_Lift,x=Deciles,type ="l",lwd = 2, col="red")

legend("topright",col=c("black","red"),lwd =c(2,2,2),c("Actual Model","Random Model"), cex = 0.7)
```

Blog {data-icon="fa-comment-o"}
=======================================================================
Column 
-----------------------------------------------------------------------
### HR in Industry 4.0

We stand at the cusp of Industry 4.0, it is almost inescapable to think how it’ll impact our professional worlds.

This latest trend of automation which involves processes and products ranging from 
**driverless car**, **smart robots**, **AI**, **IoT**, **big data analytics**, **Machine Learning**, **Predictive Models**, **Flex Dashboard (R Markdown / Shiny Web Apps)**, **cloud computing**, and **virtual reality (VR)** is simply speaking the new face of robotisation and is set to change all aspects of business management. What does this mean for us?

The pace of change that we’ve seen under Industry 4.0 has made many people ask the question **“Will technology replace people, and how will it impact our lives and the way we work?”**

Through some of my readings and the various TED talks on the subject, I’ve come across some interesting aspects of what HR could look like and am sharing some of these here. 

**It should be exciting to be a part of these changes...**

***
**Redefining sourcing and recruitment – ** 

With predictive analytics using R language and Logistic Regression Algorithm build a predictive Module with the help of Data Analyst, it can analyze the past Data and -
**Help company XYZ identify current employees that are very likely to leave**
**Recommend ways for company XYZ to decrease its attrition level in the future** and 
**Then assess the available talent pool to identify the most suitable candidates with the adequate skill sets and experience.** 
**With video based interviewing, face expressions, choice words, voice modulation, quality of responses could be reviewed to spot creativity, subject matter knowledge.**

***

**Virtual Reality (VR) in Onboarding -**
There’s an old joke, **“The new employees can’t even find a bathroom on their first day.”** Onboarding will be made more effective and consistent with VR. Each employee could get the office tour on their smart phones; they could meet their colleagues, listen to their leaders on smartphones, which would speed up the process of acculturalization. **Google cardboard** is a great example of VR onboarding tool.

***

**Removing redundancies in HR operations –**
Imagine a scenario equivalent to a **driverless car in HR?** The work related to mass documentations like new joining documents, account payable, invoices, etc., is being given to robots. Robotic process automation (RPA) learns how a worker does a repetitive task and can do the same task with zero errors. And remember, robots don’t need a bathroom, team, or lunch break!

***
**Taking HR services to different level -**
Even for HR helpdesks, routine queries related to policies, processes, etc., will be addressed with speed and accuracy with chat applications like **Slack**, **Facebook Workplace**, etc., or robots answering the queries on the phone. Only the complex or unique queries will be directed to specialized HR resources.

***
**Redefining learning and making it more relevant -**

***
**Apps and smart analytics based performance management -**
 

Introduction {data-icon="fa-comment-o"}
=======================================================================

Column {data-width=650}
-----------------------------------------------------------------------

###**HR Analytics Case Study**

**A large company named ETC, employs, at any given point of time, around 4000 employees. However, every year, around 15% of its employees leave the company. Since the attrition level is too high, the management wants to use predictive modelling to bring it down.**

**Hence, the objectives of the analysis are to:**
*Help company XYZ identify current employees that are very likely to leave*
*Recommend ways for company XYZ to decrease its attrition level in the future*

***
**CRISP-DM Framework**

**CRISP-DM,[1] is a data mining process model that describes commonly used approaches that data mining experts **
**use to tackle problems.(https://en.wikipedia.org/wiki/Cross-industry_standard_process_for_data_mining)**

**The analysis is divided into three parts:**

**Data Understanding – Source of data, patterns in the data**

**Predictive modelling of attrition**

**Recommending ways for company XYZ to decrease its level of attrition**

***
Data Understanding - 4 broad sources of data

The data received for the analysis can be divided into 4 broad categories -

**Data Looks Like**
![](Untitled.png)


Column {data-width=350}
-----------------------------------------------------------------------
### Cross-industry standard process for data mining (CRISP-DM Model)

![](CRISP-DM_Process_Diagram.png)




### **Exploratory Data Analysis (EDA)**

EDA (https://en.wikipedia.org/wiki/Exploratory_data_analysis) is the first step in every data analysis workflow. It is crucial for understanding the features and for choosing suitable analysis techniques, methods and algorithms.

Features are the variables that describe data in a data set (its “properties”). The response variable, or label, of a data set describes the output of interest. Here, for every measurement point we have  label that tells us whether the machine was broken (“1”) or not (“0”).High-level exploration of the features & how they  relate to the response variable can give us an intuitive understanding of major patterns, influencing factors and phenomena. Visualization can further help to get feel for the data and to communicate main characteristics.Here, I am Used a correlation and distribution or Bar/Box plots.



Prediction 1 {data-icon="fa-search"}
============================================================================
Column {data-width=400}
-----------------------------------------------------------------------
### **Attrition with Age**

```{r}
library(plotly)
plot1<- plot_ly(y=final_data$Age, color = final_data$Attrition, type = "box")%>% layout(xaxis= list(title= "Attrition"), yaxis= list(title= "Age"))
plot1
```

Column {data-width=400}
-----------------------------------------------------------------------
### **Attrition with Total Working Years**

```{r}
plot2<- plot_ly(x=final_data$Attrition, y=final_data$TotalWorkingYears, color = ~final_data$Attrition, type = "box")%>% layout(xaxis= list(title= "Attrition"), yaxis= list(title= "TotalWorkingYears"))
plot2
```

Column {data-width=200}
-----------------------------------------------------------------------

### **Description**


**Age**


-Employees aged 36 years and above are more likely to stay


-Employees aged 32 years and below are more likely to leave


**Experience**


-Employees that have worked for a total of 10 years or more are more likely to stay


-Employees that have worked for a total of 7 years or less are more likely to leave


**Among attritions, median age = 32 and median exp. = 7**

 
**Among non-attritions, median age = 36 and median exp. = 10**

**Coefficients of the variables Age and TotalWorkingYears are significant.**


Prediction 2 {data-icon="fa-search"}
============================================================================

Column {data-width=400}
-----------------------------------------------------------------------


### **Training Times LastYear**

```{r}
plot3<- plot_ly(x=final_data$Attrition, y=final_data$TrainingTimesLastYear, color = ~final_data$Attrition, type = "box")%>% layout(xaxis= list(title= "Attrition"), yaxis= list(title= "TrainingTimesLastYear"))
plot3
```

Column {data-width=400}
-------------------------------------
    
### **Training and Years with Current Manager**
```{r}
plot4<- plot_ly(x=final_data$Attrition, y=final_data$YearsWithCurrManager, color = ~final_data$Attrition, type = "box")%>% layout(xaxis= list(title= "Attrition"), yaxis= list(title= "YearsWithCurrManager"))
plot4
```

Column {data-width=200}
-----------------------------------------------------------------------
### **Description**


**Training**


-Employees that got 3 or more training  sessions last year are more likely to stay


-Employees that got 2 or fewer training sessions last year are more likely to leave


**Years with Current Manager**


-Employees that have spent 3 years or more under the same manager are more likely to stay


-Employees that have spent 2 years or less under the same manager are more likely to leave


-**Coefficients of the variables TrainingTimesLastYear and YearsWithCurrManager are significant.**


-**Rest of the data is based on means/medians etc.**


Prediction 3 {data-icon="fa-search"}
============================================================================

Column {data-width=400}
-----------------------------------------------------------------------

### **Attrition with Environment Satisfaction**

```{r}
library(cowplot)
library(gridExtra)
library(ggplot2)
library(plotly)
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position="none")
g1<- ggplot(final_data, aes(x=EnvironmentSatisfaction,fill=Attrition)) + geom_bar(position = "fill") + labs(y="Proportion")
ggplotly(g1)
```

Column {data-width=400}
-----------------------------------------------------------------------
### **Attrition with Job Satisfaction**
```{r}
g2<-ggplot(final_data, aes(x=JobSatisfaction,fill=Attrition)) + geom_bar(position = "fill") + labs(y="Proportion")
ggplotly(g2)
```

Column {data-width=200}
-----------------------------------------------------------------------
### **Description**

**Job Satisfaction**


-Employees that have medium, high or very high levels of job satisfaction, are more likely to stay


-Employees that have low levels of job satisfaction, are more likely to leave


**Environment Satisfaction**


-Employees that have medium, high or very high levels of environment satisfaction, are more likely to stay


-Employees that have low levels of  environment satisfaction, are more likely to leave


-**Coefficients of the variables JobSatisfaction and EnvironmentSatisfaction are significant.**


-**Employees were asked to report their job satisfaction and work environment satisfaction levels in a survey.**



Prediction 4 {data-icon="fa-search"}
============================================================================

Column {data-width=400}
-----------------------------------------------------------------------

### **Attrition with AverageWorkTime**

```{r}
p1<- plot_ly(x=final_data$Attrition, y=final_data$AverageWorkTime, color = ~final_data$Attrition, type = "box")%>% layout(xaxis= list(title= "Attrition"), yaxis= list(title= "AverageWorkTime"))
p1
```

Column {data-width=400}
-----------------------------------------------------------------------

### **Attrition with Work Life Balance**
```{r}
p2<- ggplot(final_data, aes(x=WorkLifeBalance,fill=Attrition)) + geom_bar(position = "fill") + labs(y="Proportion")
ggplotly(p2)
```

Column {data-width=200}
-----------------------------------------------------------------------

### **Description**

**Average Work Hours**


-Employees that, on average work for 7.3 hours or less, are more likely to stay


-Employees that, on average work for 8.2 hours or more, are more likely to leave


**Work Life Balance**


-Employees that rated their work life balance as good, better or best, are more likely to stay


-Employees that rated their work life balance as bad, are more likely to leave


-**Coefficients of the variables AverageWorkTime and WorkLIfeBalance are significant.** 


-**Average work hours data is based on means/medians etc.**


-**Employees were asked to report their level of work life balance in a survey.**


Prediction 5 {data-icon="fa-search"}
============================================================================

Column {data-width=300}
-----------------------------------------------------------------------

### **Monthly Income do not affect on attrition**
```{r}
plot5<- plot_ly(x=final_data$Attrition, y=final_data$MonthlyIncome, color = ~final_data$Attrition, type = "box")%>% 
layout(xaxis= list(title= "Attrition"), yaxis= list(title= "MonthlyIncome"))
plot5
```

Column {data-width=300}
-----------------------------------------------------------------------

### **Percent Salary Hike do not affect on attrition**
```{r}
plot6<- plot_ly(x=final_data$Attrition, y=final_data$PercentSalaryHike, color = ~final_data$Attrition, type = "box")%>% layout(xaxis= list(title= "Attrition"), yaxis= list(title= "PercentSalaryHike"))
plot6
```

Column {data-width=300}
-----------------------------------------------------------------------

### **Attrition with Total Working Years**
```{r}
plot7<- ggplot(final_data, aes(x=PerformanceRating,fill=Attrition)) + geom_bar(position = position_fill())+ labs(y="Proportion")
ggplotly(plot7)
```
