getwd()
setwd("D:/R-Programming")
test <- read.csv("Test.csv")
train <- read.csv("train.csv")

str(test)
dim(test)
summary(test)
table(is.na(test))

str(train)
dim(train)
summary(train)
table(is.na(train))

data <- rbind(test,train)
#number of columns not same
colnames(test)
colnames(train)
test$medv <- 1
colnames(test)
data <- rbind(test,train)

table(is.na(data))

str(data)
data$tax <- as.numeric(data$tax)

## checking corelation ##
cor <- cor(data[,c(2:4,6:9,11:15)])
cor
View(cor)
library(corrplot)
corrplot(cor,method = "circle")
#OR
install.packages("ppcorr")
library(ppcorr)

plot(data[,c(2:4,6:9,11:15)])

## outliers detection and treatment ##
boxplot(data$crim)
quantile1 <- quantile(data$crim, prob=c(0.25, 0.75))
range <- 1.5*IQR(data$crim)
data1 <- subset(data,data$crim>(quantile1[1]-range)&data$crim<(quantile1[2]+range))
data1
boxplot(data1$crim,col="red")$out
quantile1 <- quantile(data1$crim, prob=c(0.25, 0.75))
range <- 1.5*IQR(data1$crim)
data1.1 <- subset(data1,data1$crim>(quantile1[1]-range)&data1$crim<(quantile1[2]+range))
data1.1
boxplot(data1.1$crim,col="blue")$out
quantile1 <- quantile(data1.1$crim, prob=c(0.25, 0.75))
range <- 1.5*IQR(data1.1$crim)
data1.2 <- subset(data1.1,data1.1$crim>(quantile1[1]-range)&data1.1$crim<(quantile1[2]+range))
data1.2
boxplot(data1.2$crim,col="blue")$out
quantile1 <- quantile(data1.2$crim, prob=c(0.25, 0.75))
range <- 1.5*IQR(data1.2$crim)
data1.3 <- subset(data1.2,data1.2$crim>(quantile1[1]-range)&data1.2$crim<(quantile1[2]+range))
data1.3
boxplot(data1.3$crim,col="blue")$out
quantile1 <- quantile(data1.3$crim, prob=c(0.25, 0.75))
range <- 1.5*IQR(data1.3$crim)
data1.4 <- subset(data1.3,data1.3$crim>(quantile1[1]-range)&data1.3$crim<(quantile1[2]+range))
data1.4
boxplot(data1.4$crim,col="blue")$out
quantile1 <- quantile(data1.4$crim, prob=c(0.25, 0.75))
range <- 1.5*IQR(data1.4$crim)
data1.5 <- subset(data1.4,data1.4$crim>(quantile1[1]-range)&data1.4$crim<(quantile1[2]+range))
data1.5
boxplot(data1.5$crim,col="blue")$out
quantile1 <- quantile(data1.5$crim, prob=c(0.25, 0.75))
range <- 1.5*IQR(data1.5$crim)
data1.6 <- subset(data1.5,data1.5$crim>(quantile1[1]-range)&data1.5$crim<(quantile1[2]+range))
data1.6
boxplot(data1.6$crim,col="blue")$out
quantile1 <- quantile(data1.6$crim, prob=c(0.25, 0.75))
range <- 1.5*IQR(data1.6$crim)
data1.7 <- subset(data1.6,data1.6$crim>(quantile1[1]-range)&data1.6$crim<(quantile1[2]+range))
data1.7
boxplot(data1.7$crim,col="blue")$out
quantile1 <- quantile(data1.7$crim, prob=c(0.25, 0.75))
range <- 1.5*IQR(data1.7$crim)
data1.8 <- subset(data1.7,data1.7$crim>(quantile1[1]-range)&data1.7$crim<(quantile1[2]+range))
data1.8
boxplot(data1.8$crim,col="blue")$out
quantile1 <- quantile(data1.8$crim, prob=c(0.25, 0.75))
range <- 1.5*IQR(data1.8$crim)
data1.9 <- subset(data1.8,data1.8$crim>(quantile1[1]-range)&data1.8$crim<(quantile1[2]+range))
data1.9
boxplot(data1.9$crim,col="blue")$out
quantile1 <- quantile(data1.9$crim, prob=c(0.25, 0.75))
range <- 1.5*IQR(data1.9$crim)
data1.10 <- subset(data1.9,data1.9$crim>(quantile1[1]-range)&data1.9$crim<(quantile1[2]+range))
data1.10
boxplot(data1.10$crim,col="blue")$out

boxplot(data$zn)
quantile2 <- quantile(data$zn, probs=c(0.25,0.75))
range2 <- 1.5*IQR(data$zn)
data2 <- subset(data,data$zn>(quantile2[1]-range2)&data$zn<(quantile2[2]+range2))
data2
boxplot(data2$zn,col = "purple")$out
quantile2 <- quantile(data2$zn, probs=c(0.25,0.75))
range2 <- 1.5*IQR(data2$zn)
data2.1 <- subset(data2,data2$zn>(quantile2[1]-range2)&data2$zn<(quantile2[2]+range2))
data2.1
boxplot(data2.1$zn,col = "purple")$out

boxplot(data$indus)

boxplot(data$nox)

boxplot(data$rm)
quantile3 <- quantile(data$rm, probs = c(0.25,0.75))
range3 <- 1.5*IQR(data$rm)
data3 <- subset(data,data$rm>(quantile3[1]-range3)&data$rm<(quantile3[2]-range3))
data3
boxplot(data3$rm,col="red")$out

boxplot(data$age)

boxplot(data$dis)
quantile4 <- quantile(data$dis, probs = c(0.25,0.75))
range4 <- 1.5*IQR(data$dis)
data4 <- subset(data,data$dis>(quantile4[1]-range4)&data$dis<(quantile4[2]+range4))
data4
boxplot(data4$dis,col="red")$out

boxplot(data$tax)

boxplot(data$ptratio)
quantile6 <- quantile(data$ptratio, probs = c(0.25,0.75))
range6 <- 1.5*IQR(data$ptratio)
data6 <- subset(data, data$ptratio>(quantile6[1]-range6)&data$ptratio<(quantile6[2]+range6))
data6
boxplot(data6$ptratio,col="pink")$out

boxplot(data$black)
quantile7 <- quantile(data$black, probs = c(0.25,0.75))
range7 <- 1.5*IQR(data$black)
data7 <- subset(data, data$black>(quantile7[1]-range7)&data$black<(quantile7[2]+range7))
data7
boxplot(data7$black,col="pink")$out
quantile7 <- quantile(data7$black, probs = c(0.25,0.75))
range7 <- 1.5*IQR(data7$black)
data7.1 <- subset(data7, data7$black>(quantile7[1]-range7)&data7$black<(quantile7[2]+range7))
data7.1
boxplot(data7.1$black,col="pink")$out
quantile7 <- quantile(data7.1$black, probs = c(0.25,0.75))
range7 <- 1.5*IQR(data7.1$black)
data7.2 <- subset(data7.1, data7.1$black>(quantile7[1]-range7)&data7.1$black<(quantile7[2]+range7))
data7.2
boxplot(data7.2$black,col="pink")$out
quantile7 <- quantile(data7.2$black, probs = c(0.25,0.75))
range7 <- 1.5*IQR(data7.2$black)
data7.3 <- subset(data7.2, data7.2$black>(quantile7[1]-range7)&data7.2$black<(quantile7[2]+range7))
data7.3
boxplot(data7.3$black,col="pink")$out
quantile7 <- quantile(data7.3$black, probs = c(0.25,0.75))
range7 <- 1.5*IQR(data7.3$black)
data7.4 <- subset(data7.3, data7.3$black>(quantile7[1]-range7)&data7.3$black<(quantile7[2]+range7))
data7.4
boxplot(data7.4$black,col="pink")$out
quantile7 <- quantile(data7.4$black, probs = c(0.25,0.75))
range7 <- 1.5*IQR(data7.4$black)
data7.5 <- subset(data7.4, data7.4$black>(quantile7[1]-range7)&data7.4$black<(quantile7[2]+range7))
data7.5
boxplot(data7.5$black,col="pink")$out

boxplot(data$lstat)
quantile8 <- quantile(data$lstat, probs = c(0.25,0.75))
range8 <- 1.5*IQR(data$lstat)
data8 <- subset(data, data$lstat>(quantile8[1]-range8)&data$lstat<(quantile8[2]+range8))
data8
boxplot(data8$lstat,col="pink")$out

boxplot(data$medv)

## Visualization ##
hist(data$crim,col="red")
hist(data$zn,col="red")
hist(data$indus,col="red")
hist(data$chas,col="red")
hist(data$nox,col="red")
hist(data$rm,col="red")
hist(data$age,col="red")
hist(data$dis,col="red")
hist(data$rad,col="red")
hist(data$tax,col="red")
hist(data$ptratio,col="red")
hist(data$black,col="red")
hist(data$lstat,col="red")
hist(data$medv,col="red")

## Modelling ##
data_1 <- data[,c(2:4,6:9,11:15)]
data_1
New_Train <- data_1[1:nrow(train),]
New_Train
New_Test <- data_1[1:nrow(test),]
New_Test
dim(New_Train)
dim(New_Test)
colnames(New_Train)
colnames(New_Test)
Boston_Model <- lm(log(medv)~crim+rm+dis+tax+lstat,data = New_Train)
options(scipen = 999)
summary(Boston_Model)

Fitted_Values <- Boston_Model$fitted.values
Fitted_Values
Model_Resd. <- Boston_Model$residuals
Model_Resd.

plot(Boston_Model$fitted.values,Boston_Model$residuals)
#no heterocedacity

hist(Model_Resd.)

qqnorm(Model_Resd.)  #normal distribution
qqline(Model_Resd.)  #since close to straight line
Resd. <- Model_Resd.
KS_Test <-(Resd.-mean(Resd.))/sqrt(var(Resd.))
ks.test(KS_Test,rnorm(length(KS_Test)))

library(usdm)
vif(data_1)
#vif<5 no multicollinearity

library(lmtest)
dwtest(log(medv)~crim+rm+dis+tax+lstat,data = data)
#autocorrelation exists

## Accuracy
plot(Boston_Model)
prediction <- predict(Boston_Model,test)
prediction
install.packages("Metrics")
library(Metrics)
Boston_RMSE <- rmse(prediction,test[,15])
Boston_RMSE

install.packages("Metrics")
library(Metrics)
prediction <- predict(Boston_Model,test)
prediction
Boston_RMSE <- rmse(prediction,test[,15])
Boston_RMSE
# 0.7651 root mean square error