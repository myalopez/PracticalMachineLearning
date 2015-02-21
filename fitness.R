# Weight Lifting Exercise Project

library(caret)
library(Hmisc)
library(e1071)
library(rattle)
library(pgmm)
library(randomForest)
library(caTools)

#  Read in data sets and define NA strings
train = read.csv(file ="pml-training.csv", header=TRUE,na.strings=c("NA", ""))
test = read.csv(file ="pml-testing.csv", header=TRUE, na.strings=c("NA", ""))
dim(train)
[1] 19622   160
dim(test)
[1]  20 160

## Use str, describe and summary functions
 
# Combine the two sets in preparation for data cleaning

merge1 = train
merge2 = test
merge1$type = "train"
merge2$type = "test"
merge1$classe = NULL 
merge2$problem_id = NULL 
merge = rbind(merge1, merge2)
dim(merge)
[1] 19642   160

## Delete columns that are predominantly "NA"
mergeAll = merge[,complete.cases(t(merge))]
dim(mergeAll)
[1] 19642    60

## Split the data back into training and testing sets

training = subset(mergeAll, type == "train")
dim(training)
[1] 19622    60

testing = subset(mergeAll, type == "test")
dim(testing)
[1] 20 60

training$classe = train$classe
training$type = NULL
testing$type = NULL
testing$problem_id = test$problem_id

# Create training, cross-validation and testing sets for model building
set.seed(12345)
fitIndex = createDataPartition(training$classe, p = 0.60,list=FALSE)
fitTrain = training[fitIndex,]
fitTest = training[-fitIndex,]
dim(fitTrain)
[1] 11776    60

dim(fitTest)
[1] 7846   60

# Baseline Accuracy
table(fitTest$classe)

   A    B    C    D    E 
2232 1518 1368 1286 1442 

RFModel = randomForest(classe~., data = fitTrain)
RFPredict = predict(RFModel, newdata=fitTest)
table(fitTest$classe,RFPredict)
   RFPredict
       A    B    C    D    E
  A 2232    0    0    0    0
  B    0 1518    0    0    0
  C    0    0 1368    0    0
  D    0    0    0 1286    0
  E    0    0    0    0 1442
sum(diag(table(fitTest$classe,RFPredict)))/nrow(fitTest)
[1] 1
confusionMatrix(RFPredict, fitTest$classe)$overall["Accuracy"]
Accuracy 
       1 
vu= varUsed(RFModel,count=TRUE)
vusorted = sort(vu, decreasing=FALSE,index.return=TRUE)
dotchart(vusorted$x,names(RFModel$forest$xlevel[vusorted$ix]))
varImpPlot(RFModel)

set.seed(1)
model1 = randomForest(classe~. - X - user_name, data = fitTrain)
model1P = predict(model1, newdata=fitTest)
table(fitTest$classe,model1P)
   model1P
       A    B    C    D    E
  A 2231    1    0    0    0
  B    2 1516    0    0    0
  C    0    2 1366    0    0
  D    0    0    3 1281    2
  E    0    0    0    1 1441

confusionMatrix(model1P, fitTest$classe)$overall["Accuracy"]
Accuracy 
0.998598 

vu= varUsed(model1,count=TRUE)
vusorted = sort(vu, decreasing=FALSE,index.return=TRUE)
dotchart(vusorted$x,names(model1$forest$xlevel[vusorted$ix]))
varImpPlot(model1)

set.seed(1)
model2 = randomForest(classe~. -X -user_name -raw_timestamp_part_1 -raw_timestamp_part_2
                               -cvtd_timestamp-new_window, data = fitTrain)
model2P = predict(model2, newdata=fitTest)
table(fitTest$classe,model2P)
   model2P
       A    B    C    D    E
  A 2232    0    0    0    0
  B    4 1511    3    0    0
  C    0    3 1364    1    0
  D    0    0   14 1270    2
  E    0    0    0    3 1439

confusionMatrix(model2P, fitTest$classe)$overall["Accuracy"]
 Accuracy 
0.9961764 

vu= varUsed(model2,count=TRUE)
vusorted = sort(vu, decreasing=FALSE,index.return=TRUE)
dotchart(vusorted$x,names(model2$forest$xlevel[vusorted$ix]))
varImpPlot(model2)


##Code follows:

> train = read.csv(file ="pml-training.csv", header=TRUE,na.strings=c("NA", ""))
> test = read.csv(file ="pml-testing.csv", header=TRUE, na.strings=c("NA", ""))
> dim(train)
[1] 19622   160
> dim(test)
[1]  20 160
> merge1 = train
> merge2 = test
> merge1$type = "train"
> merge2$type = "test"
> merge1$classe = NULL 
> merge2$problem_id = NULL 
> merge = rbind(merge1, merge2)
> dim(merge)
[1] 19642   160
> mergeAll = merge[,complete.cases(t(merge))]
> dim(mergeAll)
[1] 19642    60
> training = subset(mergeAll, type == "train")
> dim(training)
[1] 19622    60
> testing = subset(mergeAll, type == "test")
> dim(testing)
[1] 20 60
> training$classe = train$classe
> training$type = NULL
> testing$type = NULL
> testing$problem_id = test$problem_id
> set.seed(12345)
> fitIndex = createDataPartition(training$classe, p = 0.60,list=FALSE)
> fitTrain = training[fitIndex,]
> fitTest = training[-fitIndex,]
> dim(fitTrain)
[1] 11776    60
> dim(fitTest)
[1] 7846   60
> table(fitTest$classe)

   A    B    C    D    E 
2232 1518 1368 1286 1442 
> RFModel = randomForest(classe~., data = fitTrain)
> RFPredict = predict(RFModel, newdata=fitTest)
> table(fitTest$classe,RFPredict)
   RFPredict
       A    B    C    D    E
  A 2232    0    0    0    0
  B    0 1518    0    0    0
  C    0    0 1368    0    0
  D    0    0    0 1286    0
  E    0    0    0    0 1442
> confusionMatrix(RFPredict, fitTest$classe)$overall["Accuracy"]
Accuracy 
       1 
> vu= varUsed(RFModel,count=TRUE)
> vusorted = sort(vu, decreasing=FALSE,index.return=TRUE)
> dotchart(vusorted$x,names(RFModel$forest$xlevel[vusorted$ix])
+ )
> varImpPlot(RFModel)
> set.seed(1)
> model1 = randomForest(classe~. - X - user_name, data = fitTrain)
> model1P = predict(model1, newdata=fitTest)
> table(fitTest$classe,model1P)
   model1P
       A    B    C    D    E
  A 2231    1    0    0    0
  B    2 1516    0    0    0
  C    0    2 1366    0    0
  D    0    0    3 1281    2
  E    0    0    0    1 1441
> confusionMatrix(model1P, fitTest$classe)$overall["Accuracy"]
Accuracy 
0.998598 
> vu= varUsed(model1,count=TRUE)
> vusorted = sort(vu, decreasing=FALSE,index.return=TRUE)
> dotchart(vusorted$x,names(model1$forest$xlevel[vusorted$ix]))
> varImpPlot(model1)
> set.seed(1)
> model2 = randomForest(classe~. -X -user_name -raw_timestamp_part_1 -raw_timestamp_part_2
+                                -cvtd_timestamp-new_window, data = fitTrain)
> model2P = predict(model2, newdata=fitTest)
> table(fitTest$classe,model2P)
   model2P
       A    B    C    D    E
  A 2232    0    0    0    0
  B    4 1511    3    0    0
  C    0    3 1364    1    0
  D    0    0   14 1270    2
  E    0    0    0    3 1439
> confusionMatrix(model2P, fitTest$classe)$overall["Accuracy"]
 Accuracy 
0.9961764 
> vu= varUsed(model2,count=TRUE)
> vusorted = sort(vu, decreasing=FALSE,index.return=TRUE)
> dotchart(vusorted$x,names(model2$forest$xlevel[vusorted$ix]))
> varImpPlot(model2)


