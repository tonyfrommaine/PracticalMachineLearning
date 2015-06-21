## Get Packages and set parallelism
require("caret");
require("ggplot2");
require("rpart");
require("rpart.plot");
require("rattle");
require("randomForest");
require("doParallel");
cl <- makeCluster(detectCores());
registerDoParallel(cl);

## Set a seed for reproducabilty
set.seed(1964);

## Download the Data, but only if it has not been downloaded before
if (!file.exists("trainingset.csv")){
download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",destfile="trainingset.csv",method="curl");
}
if (!file.exists("testingset.csv")){    
download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",destfile="testingset.csv",method="curl");
}
 

### Data Cleansing - The data has NA's, blanks and DIV/0. They must be taken out first.
trainingData <- read.csv("trainingset.csv", na.strings = c("NA", "", "#DIV/0!"));
testingData <- read.csv("testingset.csv", na.strings = c("NA", "", "#DIV/0!")); 

### Anonymize the Data  
testingData <- testingData[,-2];
trainingData <- trainingData[,-2];
 
### Remove non-impactful data  
testingData <- testingData[,-c(1:6)];
trainingData <- trainingData[,-c(1:6)];
  
## Remove rows with all NA's and Columns with all NA's in both data sets  
nothingIn <- apply(trainingData, 1, function(x) all(is.na(x)));
trainingData <- trainingData[!nothingIn,];
nothingIn <- apply(testingData, 1, function(x) all(is.na(x)));
testingData <- testingData[!nothingIn,];
trainingData<-trainingData[,colSums(is.na(trainingData)) == 0]
testingData <-testingData[,colSums(is.na(testingData)) == 0]
 
### Check the data  
trainBaseSet <- createDataPartition(y=trainingData$classe, p=0.75, list=FALSE)
RealTrainingSet <- trainingData[trainBaseSet, ]; 
RealTestingSet <- trainingData[-trainBaseSet, ];
 
### Explore the data  
summary(RealTrainingSet$classe);

## Confirm this with a simple plot
plot(RealTrainingSet$classe,col="red",ylab="Frequency of classe",xlab="class",main="Distribution of Class for Training Data");
 
## Explore with Decision Tree  
modFit <- train(classe ~., method="rpart", data=RealTrainingSet);
fancyRpartPlot(modFit$finalModel);

## Create the Model with many variations
## Run the model for 20,50, 100 and 1000 ntree.  AGAIN, PLEASE NOTE THESE WILL RUN FOR A LONG TIME AND ARE PROVIDED TO SHOW MY PROCESS, THUS YOU MAY WANT TO COMMENT OUT SOME OF THESE.  

modFit <- train(classe ~ ., method = "rf", data = RealTrainingSet,ntree=10,do.trace=TRUE);
rf10 <- modFit$finalModel;
modFit <- train(classe ~ ., method = "rf", data = RealTrainingSet,ntree=20,do.trace=TRUE);
rf20 <- modFit$finalModel;
modFit <- train(classe ~ ., method = "rf", data = RealTrainingSet,ntree=50,do.trace=TRUE);
rf50 <- modFit$finalModel;
modFit <- train(classe ~ ., method = "rf", data = RealTrainingSet,ntree=100,do.trace=TRUE);
rf100 <- modFit$finalModel;
modFit <- train(classe ~ ., method = "rf", data = RealTrainingSet,ntree=1000,do.trace=TRUE);
rf1000 <- modFit$finalModel;
### Examine results to see impact of increase in ntree
rf10;
rf20;
rf50;
rf100;
rf1000;
 
## Results
predictrf1000 <- predict(rf1000,RealTestingSet,type="class");
confusionMatrix(predictrf1000,RealTestingSet$class);
 
##Submission of Data  
predictrf1000 <- predict(rf1000,testingData,type="class");
predictrf1000;

##Stop the parallel cluster
stopCluster(cl)

