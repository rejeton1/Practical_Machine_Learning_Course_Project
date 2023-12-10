library(caret)
library(ggplot2)
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
sample <- sample(1:19622)
new_data <- new_data[sample,]
new_data$classe <- as.factor(new_data$classe)

#split data into training/test/validation set(80/20)
inTrain <- createDataPartition(new_data$classe, p=0.8, list=FALSE)
training <- new_data[inTrain,]
testing <- new_data[-inTrain,]

qplot(training$roll_dumbbell, training$pitch_dumbbell, colour=classe, data=training)


