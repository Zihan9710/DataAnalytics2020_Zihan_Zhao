rm(list = ls())
setwd('/Users/zihan/Desktop/rpi/Data\ Analytics')
library(readxl)
library(reshape)
data <- read_excel("Project Data.xls") 
data <- data[-1, ] 
data <- data.frame(data)
#data$Time <- as.Date(data$Time, origin='91-1-1')
colnames(data) <- c("Time","ChargeOffRate","DelinquencyRate","CPI",
                    "UnemploymentRate","LaborRate", "TotalCustomerCredit",
                    "TotalPersonalIncome", "PersonalExpenditures", 
                    "PersonalIncomePerCapita", "GDP")

dim(data)
str(data)
data1 <- as.data.frame(lapply(data[,1:11],as.numeric))
str(data1)

#transform time variable to date type
data1$Time <- as.Date(data1$Time, origin='1899/12/30')
data2 <- data1
data3 <- data1

#install.packages("DMwR")
library(DMwR)

#filling missing data using KNN
data1 <- data1[,-1]
data1 <- knnImputation(data1)

#filling missing data using last quarterly data
i <- 3
data2[1, 2] = data2[i, 2]
data2[2, 2] = data2[i, 2]
data2[1, 3] = data2[i, 3]
data2[2, 3] = data2[i, 3]
data2[1, 11] = data2[i, 11]
data2[2, 11] = data2[i, 11]

while (i < 356) {
  data2[i+1, 2] = data2[i, 2]
  data2[i+2, 2] = data2[i, 2]
  data2[i+1, 3] = data2[i, 3]
  data2[i+2, 3] = data2[i, 3]
  data2[i+1, 11] = data2[i, 11]
  data2[i+2, 11] = data2[i, 11]
  i <- i + 3
}

data2 <- data2[-1]
#install.packages("corrplot")
library(corrplot)
library(caret)

#draw corrplot for data1 and data2
matrix <- cor(data1[1:10])
corrplot(corr = matrix, method = "color",  tl.col = "black",
         addrect = 4, addCoef.col = "grey")

matrix2 <- cor(data2[1:10])
corrplot(corr = matrix2, method = "color",  tl.col = "black",
         addrect = 4, addCoef.col = "grey")

#drwa the boxplots of variables
attach(data1)
par(mfrow = c(1,1))
boxplot(ChargeOffRate,DelinquencyRate, UnemploymentRate,
        names = c("ChargeOffRate","DelinquencyRate","UnemploymentRate"))

boxplot(CPI,xlab = "CPI")
boxplot(LaborRate, xlab = "LaborRate")
boxplot(TotalCustomerCredit, xlab = "TotalCustomerCredit")
boxplot(TotalPersonalIncome, PersonalExpenditures, 
        PersonalIncomePerCapita, GDP, 
        names = c("TotalPersonalIncome", "PersonalExpenditures", 
        "PersonalIncomePerCapita", "GDP"))

# Multiviriate Regression
data1_linear<- log(data1)
lm_chargeoff <- lm(ChargeOffRate~.,data = data1_linear)
summary(lm_chargeoff)

#Random forest
set.seed(77)
ind <- sample(2,nrow(data1),replace = T,prob = c(0.7,0.3))
train <- data1[ind==1,]
test <- data1[ind==2,]
library(randomForest)
set.seed(77)
rf <- randomForest(ChargeOffRate~.,data=train)
print(rf)
attributes(rf)
#rf$confusion
y_train <- predict(rf,train)
# calculate mse for training data 
mse_train = mean((train$ChargeOffRate - y_train)^2)

y_test <- predict(rf,test)
mse_test = mean((test$ChargeOffRate - y_test)^2)

plot(rf) 
# Tuning
t <- tuneRF(x=train[,2:9],
            y=train[,1],
             ntreeTry = 500) 
print(t)
#give us a idea what mtry to choose
set.seed(77)
rf <- randomForest(ChargeOffRate~.,data=train,
                   ntree=500,
                   mtry=4,
                   importance=T,
                   proximity=T)
print(rf) 
#improved OOB error
y2_test <- predict(rf,test)
mse2_test = mean((test$ChargeOffRate - y2_test)^2)

hist(treesize(rf),
     main = "# of Nodes for the trees")
varImpPlot(rf,
           sort = T,
           main = "Important Variables") 
varUsed(rf)

train2 <- data2[ind==1,]
test2 <- data2[ind==2,]
set.seed(77)
rf <- randomForest(ChargeOffRate~.,data=train2)
print(rf)
attributes(rf)
y_train2 <- predict(rf,train2)
# calculate mse for training data 
mse_train2 = mean((train2$ChargeOffRate - y_train2)^2)

y_test2 <- predict(rf,test2)
mse_test2 = mean((test2$ChargeOffRate - y_test2)^2)

plot(rf)
# Tuning
t <- tuneRF(x=train2[,2:9],
            y=train2[,1],
            ntreeTry = 500) 
print(t)
set.seed(77)
rf <- randomForest(ChargeOffRate~.,data=train2,
                   ntree=800,
                   mtry=4,
                   importance=T,
                   proximity=T)
print(rf) 
y2_test2 <- predict(rf,test2)
mse2_test2 = mean((test2$ChargeOffRate - y2_test2)^2)

hist(treesize(rf),
     main = "# of Nodes for the trees")
varImpPlot(rf,
           sort = T,
           main = "Important Variables") 
importance(rf)

#install.packages("tseries")
library(tseries)
#install.packages("forecast")
library(forecast)
str(data3)
data4 <- cbind(data3[1], data1[1])
attach(data4)
plot(data4,type = "l",xlab = "Time",ylab = "ChargeOffRate")
acf(ChargeOffRate)
# fails to decay to 0 quickly, is a non-stationary series

#smoothing
ndiffs(data4$ChargeOffRate) #1 fisrt difference is needed
ddata <- diff(data4$ChargeOffRate, 1)
ndiffs(ddata) #0，no need to continue
acf(ddata, main = "Series ChargeOffRate after Diff")

pacf(ChargeOffRate)
pacf(ddata, main = "Series ChargeOffRate after Diff")

plot(ddata,xlab = "Time")
ari <- arima(ddata,order = c(3,0,1), method = "ML") 
print(ari)
pred <- forecast(ari,h=4)
plot(pred,xlab = "Time",ylab = "ChargeOffRate")




