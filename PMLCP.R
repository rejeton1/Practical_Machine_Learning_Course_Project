library(caret)
library(ggplot2)
library(nnet)
library(randomForest)
data <- read.csv('pml-training.csv')
testdata <- read.csv('pml-testing.csv')

#preprocessing
#1. remove columns that contain NA value or blank value
columns <- c()
num <- 1
for(i in 1:160){
  if(sum(is.na(data[,i]))>=1 | sum(data[,i]=="")>=1){
    columns[num] <- i
    num <- num + 1
  }
}

new_data <- data[,-columns]

#2. remove another variables that is unnecessary for prediction
#(time-related columns, user-name, X columns)

new_data <- new_data[,-c(1:7)]

#randomly arrange row
set.seed(1)
sample <- sample(1:19622)
new_data <- new_data[sample,]
new_data$classe <- as.factor(new_data$classe)

#split data into training/test/validation set(80/20)
inTrain <- createDataPartition(new_data$classe, p=0.8, list=FALSE)
training <- new_data[inTrain,]
testing <- new_data[-inTrain,]


#cross-validation of PCA + MLR
set.seed(123)
cv <- createFolds(training$classe, k=5)
accuracy1 <- c()
num1 <- 1

for(i in 1:length(cv)){
  index <- cv[[i]]
  
  cv_valid <- training[index,]
  cv_train <- training[-index,]
  
  set.seed(234)
  prepro <- preProcess(cv_train[,-53], method='pca', thresh=0.9)
  cv_train_PC <- predict(prepro, cv_train[,-53])
  
  model <- multinom(cv_train$classe ~ ., data=cv_train_PC)
  
  cv_valid_PC <- predict(prepro, cv_valid[,-53])
  pred <- predict(model, cv_valid_PC)
  
  accuracy1[num1] <- sum(pred == cv_valid[,53])/dim(cv_valid)[1]
  num1 <- num1 + 1
  
}

ave_accuracy_MLR <- mean(accuracy1)


#cross validation of randomforest
accuracy2 <- c()
num2 <- 1

for(i in 1:length(cv)){
  index <- cv[[i]]
  
  cv_valid <- training[index,]
  cv_train <- training[-index,]
  
  set.seed(345)
  model_rf <- randomForest(classe ~ ., data=cv_train)
  pred2 <- predict(model_rf, cv_valid)
  
  accuracy2[num2] <- sum(pred2 == cv_valid[,53])/dim(cv_valid)[1]
  num2 <- num2 + 1
  
}

ave_accuracy_RF <- mean(accuracy2)

ave_accuracy_MLR
ave_accuracy_RF

