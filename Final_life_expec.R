# Loading the libraries

```{r}

library(dplyr)
library(dbplyr)
library(ggplot2)
library(corrplot)
library(scatterplot3d)
library(plotrix)
library(gridExtra)
library(psych)
library(lubridate)
library(BBmisc)
library(forecast)
library(randomForest)
library(ROCR)
library(MASS)
library(distr)
library(leaps)
library(bootstrap)
library(gvlma)
library(caret)
library(car)
library(lmtest)
```

# Loading the dataset

```{r}

who <- read.csv("C:/Users/shash/Desktop/Data mining/life expectancy project/Life Expectancy Data.csv")
head(who)
summary(who)
colnames(who)

```

# Replacing missing values with mean values

```{r}

who$Life.expectancy[is.na(who$Life.expectancy)] <- mean(who$Life.expectancy, na.rm = TRUE) 
summary(who$Life.expectancy)

who$Adult.Mortality[is.na(who$Adult.Mortality)] <- mean(who$Adult.Mortality, na.rm = TRUE) 
summary(who$Adult.Mortality)

who$Alcohol[is.na(who$Alcohol)] <- mean(who$Alcohol, na.rm = TRUE) 
summary(who$Alcohol)

who$Hepatitis.B[is.na(who$Hepatitis.B)] <- mean(who$Hepatitis.B, na.rm = TRUE) 
summary(who$Hepatitis.B)

who$BMI[is.na(who$BMI)] <- mean(who$BMI, na.rm = TRUE) 
summary(who$BMI)

who$Polio[is.na(who$Polio)] <- mean(who$Polio, na.rm = TRUE) 
summary(who$Polio)

who$Total.expenditure[is.na(who$Total.expenditure)] <- mean(who$Total.expenditure, na.rm = TRUE) 
summary(who$Total.expenditure)

who$Diphtheria[is.na(who$Diphtheria)] <- mean(who$Diphtheria, na.rm = TRUE) 
summary(who$Diphtheria)

who$GDP[is.na(who$GDP)] <- mean(who$GDP, na.rm = TRUE) 
summary(who$GDP)

who$Population[is.na(who$Population)] <- mean(who$Population, na.rm = TRUE) 
summary(who$Population)

who$thinness.1.19.years[is.na(who$thinness.1.19.years)] <- mean(who$thinness.1.19.years, na.rm = TRUE) 
summary(who$thinness.1.19.years)

who$thinness.5.9.years[is.na(who$thinness.5.9.years)] <- mean(who$thinness.5.9.years, na.rm = TRUE) 
summary(who$thinness.5.9.years)

who$Income.composition.of.resources[is.na(who$Income.composition.of.resources)] <-   mean(who$Income.composition.of.resources, na.rm = TRUE) 
summary(who$Income.composition.of.resources)

who$Schooling[is.na(who$Schooling)] <- mean(who$Schooling, na.rm = TRUE) 
summary(who$Schooling)

# Summary of the imputed data
summary(who)

```

# Plotting the graphs to remove the outliers from the data

```{r}

plot1 <- ggplot(data = who, aes(x = who$Year ,y=who$Life.expectancy))+geom_point()
plot2 <- ggplot(data = who, aes(x = who$Country ,y=who$Life.expectancy))+geom_point()
plot3 <- ggplot(data = who, aes(x = who$Alcohol ,y=who$Life.expectancy))+geom_point()
plot4 <- ggplot(data = who, aes(x = who$GDP ,y=who$Life.expectancy))+geom_point()
plot5 <- ggplot(data = who, aes(x = who$Population ,y=who$Life.expectancy))+geom_point()

grid.arrange(plot1,plot2,plot3,plot4,plot5, ncol=2)

```

# Converting categorical to numerical variables

```{r}

str(who)

who_1<-sapply(who,is.factor)
who_2<-sapply(who[,who_1],unclass) 
who_data<-cbind(who[,!who_1],who_2) 

who_data
str(who_data)

```

# Plot scatter plot matrix

```{r}

who_data_scatter1 <- who_data[,(1:11)]
scatterplotMatrix(who_data_scatter1, spread = FALSE, lty.smooth = 2,
                  main = "Scatter Plot Matrix for WHO Data")

who_data_scatter2 <- who_data[,(12:22)]
scatterplotMatrix(who_data_scatter2, spread = FALSE, lty.smooth = 2,
                  main = "Scatter Plot Matrix for WHO Data")

```

# Correlation plot 

```{r}

who_cor <- cor(who_data)
who_cor
corrplot(who_cor)

```

# Data partition

```{r}

set.seed(123)
ind <- sample(2, nrow(who_data),
              replace = TRUE,
              prob = c(0.8, 0.2))

who_train <- who_data[ind == 1,]
who_test <- who_data[ind == 2,]

who_train
who_test

```

# PCA - prcomp

```{r}

who_pc_train <- prcomp(who_train[,-2],
                       center = TRUE,
                       scale. = TRUE)

attributes(who_pc_train)
who_pc_train$center
who_pc_train$scale
who_pc_train
summary(who_pc_train)

who_pc_train_predict <- predict(who_pc_train, who_train)
who_pc_train_predict <- data.frame(who_pc_train_predict, who_train[2])
who_pc_train_predict

who_pc_test <- prcomp(who_test[,-2],
                      center = TRUE,
                      scale. = TRUE)

attributes(who_pc_test)
who_pc_test$center
who_pc_test$scale
who_pc_test
summary(who_pc_test)

who_pc_test_predict <- predict(who_pc_test, who_test)
who_pc_test_predict <- data.frame(who_pc_test_predict, who_test[2])
who_pc_test_predict

```

# Multiple Linear Regression and diagnostics

```{r}
M1_train <- lm(Life.expectancy ~ PC1 + PC2 + PC3 + PC4 + PC5, 
               data = who_pc_train_predict)

M1_train
summary(M1_train)

# Model prediction - training

M1_train_predict <- predict(M1_train, who_pc_train_predict)
M1_train_predict
M1_train_predict_table <- table(M1_train_predict, who_pc_train_predict$Life.expectancy)
M1_train_predict_table

# Model prediction - testing

M1_test_predict <- predict(M1_train, who_pc_test_predict)
M1_test_predict_table <- table(M1_test_predict, who_pc_test_predict$Life.expectancy)
M1_test_predict_table

# Training Accuracy 

some.residuals <- who_train$Life.expectancy - M1_train_predict
some.residuals
data.frame(M1_train_predict, who_train$Life.expectancy, some.residuals)
accuracy(M1_train_predict, who_train$Life.expectancy)

# Testing Accuracy

some.residuals <- who_test$Life.expectancy - M1_test_predict
some.residuals
data.frame(M1_test_predict, who_test$Life.expectancy, some.residuals)
accuracy(M1_test_predict, who_test$Life.expectancy)

# Diagnostics

par(mfrow = c(2,2))
plot(M1_train_predict)
plot(M1_test_predict)
durbinWatsonTest(M1_train_predict)
durbinWatsonTest(M1_test_predict)
crPlots(M1_train)
gvlma(M1_train)
outlierTest(M1_train)
hatplot <- function(fit){
  p = length(coefficients(fit))
  n = length(fitted(fit))
  plot(hatvalues(fit), ylim = c(0,3.2)*p/n, main = "Index plot of hat values")
  abline(h = c(2,3)*p/n, col = "red", Ity = 2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}

hatplot(M1_train)

```

# Random forest and diagnostics

```{r}

# For ntree = 10

randomForestAlgo <- function(x){
  rf <- randomForest(Life.expectancy ~ Country  + 
                       Year + Status + Adult.Mortality + infant.deaths + Alcohol + 
                       percentage.expenditure + Hepatitis.B  + Measles + BMI +
                       under.five.deaths + Polio + Total.expenditure + Diphtheria + 
                       HIV.AIDS + GDP + Population + thinness.1.19.years + thinness.5.9.years +
                       Income.composition.of.resources + Schooling , ntree=10, na.action = 
                       na.exclude, data=x, importance=T)
  return(rf)  
}

rfModel = randomForestAlgo(who_train)
print(rfModel)
predictedRF<-predict(rfModel,newdata = who_test)

# Accuracy for ntree = 10

accuracy(predictedRF,who_test$Life.expectancy)

# For ntree = 50

randomForestAlgo <- function(x){
  rf <- randomForest(Life.expectancy ~ Country  + 
                       Year + Status + Adult.Mortality + infant.deaths + Alcohol + 
                       percentage.expenditure + Hepatitis.B  + Measles + BMI +
                       under.five.deaths + Polio + Total.expenditure + Diphtheria + 
                       HIV.AIDS + GDP + Population + thinness.1.19.years + thinness.5.9.years +
                       Income.composition.of.resources + Schooling , ntree=50, na.action = 
                       na.exclude, data=x, importance=T)
  return(rf)  
}

rfModel = randomForestAlgo(who_train)
print(rfModel)
predictedRF<-predict(rfModel,newdata = who_test)

# Accuracy for ntree = 50

accuracy(predictedRF,who_test$Life.expectancy)

# For ntree = 100

randomForestAlgo <- function(x){
  rf <- randomForest(Life.expectancy ~ Country  + 
                       Year + Status + Adult.Mortality + infant.deaths + Alcohol + 
                       percentage.expenditure + Hepatitis.B  + Measles + BMI +
                       under.five.deaths + Polio + Total.expenditure + Diphtheria + 
                       HIV.AIDS + GDP + Population + thinness.1.19.years + thinness.5.9.years +
                       Income.composition.of.resources + Schooling , ntree=100, na.action = 
                       na.exclude, data=x, importance=T)
  return(rf)  
}

rfModel = randomForestAlgo(who_train)
print(rfModel)
predictedRF<-predict(rfModel,newdata = who_test)

# Accuracy for ntree = 100

accuracy(predictedRF,who_test$Life.expectancy)

```
