### RANDOM FOREST ###
#regression -> dependent variable is continuous
#classification -> dependent variable is categorical
#regression/classification ke trees banaenge aur phir mean/mode respectively lenge
#jab inn dono se accuracy kum aati hai, tab hum random forest karte hain
# RSS -> Residual Sum of Squares
getwd()
setwd("D:/R-Programming")
fraud_data <- read.csv("fraud_data.csv")

library(bit64)
library(caret)
library(sqldf)

## Data Preparation ##
View(fraud_data)
levels(fraud_data$purchase_value)
dim(fraud_data)
str(fraud_data)

class(fraud_data$user_id)
class(fraud_data$signup_time)
class(fraud_data$purchase_time)
class(fraud_data$purchase_value)
class(fraud_data$device_id)
class(fraud_data$source)  #factor
class(fraud_data$browser) #factor
class(fraud_data$sex)
class(fraud_data$age)     
class(fraud_data$ip_address)
class(fraud_data$class)    #factor
levels(fraud_data$source)

fraud_data$user_id <- as.character(fraud_data$user_id)
fraud_data$class <- as.factor(fraud_data$class)
fraud_data$age <- as.numeric(fraud_data$age)
fraud_data$ip_address <- as.character(fraud_data$ip_address)
levels(fraud_data$ip_address)
levels(fraud_data$class)
levels(fraud_data$age)

#for date time values, class is POSIXlt, so that we can add, subtract etc time.
#dont manipulate the original data, make a new column in the new class using function strp
fraud_data$ST1 <- strptime(fraud_data$signup_time,'%m/%d/%Y %H:%M')
View(fraud_data)
fraud_data$PT1 <- strptime(fraud_data$purchase_time,'%m/%d/%Y %H:%M')
View(fraud_data)

str(fraud_data)

fraud_data$time_difference <- fraud_data$ST1 - fraud_data$PT1
fraud_data$time_difference1 <- fraud_data$PT1 - fraud_data$ST1

#dummy encoding - converting data into 0 and 1
fraud_data$Gender <- ifelse(fraud_data$sex=="M",1,0)

## Modelling ##
# subsetting data for including only required variables
fraud_data1 <- fraud_data[,c("purchase_value", "source", "browser", "age", "class", "time_difference", "Gender")]

# Check Correlation
fraud_data2 <- fraud_data1[,c("purchase_value", "age", "time_difference")]

str(fraud_data1)
# sapply changes class of all columns in the data
fraud_data2 <- data.frame(unlist(sapply(fraud_data2,as.numeric)))
str(fraud_data2)
cor(fraud_data2)
#no variable has strong correlation

# splitting data into 60% training set and 40% for testing set
# predictions are made for test set using train set
ind <- as.vector(createDataPartition(fraud_data1$class, p=0.60, list = FALSE)) #this is from caret package
data_train <- as.data.frame(fraud_data1[ind,])
data_test <- as.data.frame(fraud_data1[-ind,])
dim(data_train)
dim(data_test)
str(data_train)
class(data_train$class)
# or, data_train <- as.data.frame(fraud_data1[1:95000,])
# data_test <- as.data.frame(fraud_data1[95001:151112,])

Logistic_Model <- glm(class ~ ., data = data_train, family = "binomial")
# to create logistic regression model
# family is binomial to get probability
options(scipen = 999)
summary(Logistic_Model)

# -2(likelihood) + 2k, k=no. of coefficients, (beta0),beta1, bata2, beta3...
# if k includes intercept, null deviance
# if k does not include intercept beta0, residual deviance

#setting threshold probability
fitted_prob <- predict(Logistic_Model, newdata = data_test, type = 'response')
fitted.results <- ifelse(fitted_prob > 0.5,1,0)

length(fitted.results)
length(data_test$class)

#check accuracy
error <- mean(fitted.results != data_test$class)
print(paste('Accuracy is', 1-error))
# 90.6% accuracy

# check 1 : confusion matrix
# calculation of accuracy using confusion matrix:-
# (true positive + true negative)/(true positive+true negative+false positive+false negative)
# true positive -> 1 ko 1 predict kiya
# true negative -> 0 ko 0 predict kiya
# false positive -> 0 ko 1 predict kiya
# false negative -> 1 ko 0 predict kiya
# table(data_test$class, fitted_prob>0.6)

table(data_test$class,fitted_prob>0.6)

# check 2: ROCR curve - check threshold, false positive rate kum karna hai, true positive rate badhaana hai

library(ROCR)
ROCRpred <- prediction(fitted_prob,data_test$class)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
plot(ROCRperf, colorize = TRUE)
fitted.results <- ifelse(fitted_prob>0.6,1,0)
fitted.results

#check3 : AUC
auc <- performance(ROCRpred, measure = "auc")
auc <- auc@y.values[[1]]
auc

#### Random Forest ####
#classification -> accuracy is checked using RSS value (should be minimum)
#regression -> accuracy is checked using Gini Index or Entropy
#Here, test data is called 'Out of the Bag'
#In linear Regression -> one tree can have only 3 decisions at a time

install.packages("randomForest")
library(randomForest)
rfl <- randomForest(as.factor(class) ~ ., data = data_train, ntrees = 300,
                    mtry = 4, importance = TRUE, strata = data_train$class, sampsize = 3000,
                    trace = FALSE, normalize = TRUE)
importance(rfl) #higer value of mean decrease accuracy, more the importance of that variable
varImpPlot(rfl)
table(data_test$class,fitted_prob>0.6)
