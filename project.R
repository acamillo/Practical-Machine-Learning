## URL of the database. Provided by the assignment
## Database file name

loadData <- function(url, name) {
  
  dataDir <- "data/"
  
  ## Create the required working folder.
  if (!file.exists(dataDir)) {
    dir.create(dataDir)
  }
  
  fileName <- paste0(dataDir, name)
  
  ##
  ## Download the database if not already present
  ##
  if (!file.exists(fileName)) {
    message("Downloading training database...")
    download.file(url, fileName)
  }
 
  ## A preliminary explorary revelead the data to contain different labels for the missing value.
  ## This variant of the read.csv will automatically replace the many different strings to the 
  ## classical NA value.
  
  data_data <- read.csv(fileName, na.strings=c("#DIV/0!"," ", "", "NA", "NAs", "NULL"))
  # data_data <- read.csv(fileName )
}



library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(corrplot)

set.seed(1948) 


message("Downloading data")
training_data <- loadData("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", "pml-training.csv")
testing_data <- loadData("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", "pml-testing.csv")

## Now let's  remove the columns tcontaining NA  values.
training_data <- training_data[, colSums(is.na(training_data)) == 0] 
testing_data <- testing_data[, colSums(is.na(testing_data)) == 0] 

## Remove unwanted columns.
training_data <- training_data[, !grepl("^X|timestamp|window|user_name", names(training_data)) ]
testing_data <- testing_data[, !grepl("^X|timestamp|window|user_name", names(testing_data)) ]

pivot <- createDataPartition(training_data$classe, p=0.6, list=FALSE )
trainingData  <- training_data[pivot,]
testingData  <- training_data[-pivot,]

## Second, we create a predictive model using the Random Forst algorithm (RFA). This choice is motivated by the fact that the RFA automatically selects important variables and is robust against highly correlated variables and outliers in general.  The number of *cross validation is set to 5*
  
ctrl  <- trainControl(method = "cv", 5)
modelRFA <- train(classe ~ ., data = trainingData, method = "rf", trControl = ctrl, ntree = 250)
modelRFA

## In this third step, we assess the performance of the prediction model testing it against the validation data set $testingData$
predictRFA <- predict(modelRFA, testingData)
confusionMatrix(testingData$classe, predictRFA)

accuracy <- postResample(predictRFA, testingData$classe)
out_of_sample_error <- 1 - as.numeric(confusionMatrix(testingData$classe, predictRFA)$overall[1])
accuracy

accuracy_percent = as.numeric( round(100 * accuracy, 3))
out_of_sample_error_percent = round(100 * out_of_sample_error, 3)

accuracy_percent[1]
out_of_sample_error_percent[1]

# The estimated model accuracy is `r 100 -out_of_sample_error`% with an estimated out of sample error of `r out_of_sample_error`%.
# In this fourth and final step step the model is eventually validated with the project's testing data set $testing_data$.
result <- predict(modelRFA, testing_data[, -length(names(testing_data))])
result

treeModel <- rpart(classe ~ ., data = training_data, method="class")
prp(treeModel) # fast plot


# corrPlot <- cor(training_data[, -length(names(training_data))])
# corrplot(corrPlot, method="color")
