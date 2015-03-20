##  Practical machine learning final project
##  Script for model building

##  Libraries
library(caret); library(ggplot2); library(dplyr)

##  Get data files
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", 
              destfile = "pml-training.csv",
              method = "curl")
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",
              destfile = "pml-testing.csv",
              method = "curl")

trainingData <- read.csv("pml-training.csv", na.strings=c("", "NA", "#DIV/0!"))
testingData <- read.csv("pml-testing.csv", na.strings=c("", "NA", "#DIV/0!"))

##  Only keep columns in which at least 50% of observations are not NA
keepColumns <- colMeans(is.na(trainingData)) < .5

training <- trainingData[,keepColumns]
training <- select(training, -c(X, user_name, raw_timestamp_part_1, raw_timestamp_part_2,
                               cvtd_timestamp, new_window, num_window))
testing <- testingData[,keepColumns]
testing <- select(testing, -c(X, user_name, raw_timestamp_part_1, raw_timestamp_part_2,
                                cvtd_timestamp, new_window, num_window))

##  Near zero variability
nsv <- nearZeroVar(training, saveMetrics = TRUE)
training <- training[,nsv$zeroVar == FALSE]
testing <- testing[,nsv$zeroVar == FALSE]

##  Create subsets of the training data for training and testing
inTrain <- createDataPartition(y = training$classe, p = .75, list = FALSE)

trainingSet <- training[inTrain,]
testingSet <- training[-inTrain,]


modelFit <- train(classe ~ ., method = "rf", data = trainingSet)

table(predict(modelFit, newdata = testingSet))

table(predict(modelFit, newdata = testing))

answers = as.character(predict(modelFit, newdata = testing))






