
# Read in data
census = read.csv("census.csv")
str(census)

# A Logistic Regression Model
library(caTools)
set.seed(2000)
split = sample.split(census$over50k,SplitRatio = 0.60)
Train = subset(census,split == TRUE)
Test = subset(census,split == FALSE)

log_reg = glm(over50k ~ .,data = Train,family = "binomial")
summary(log_reg)

log_reg_pred = predict(log_reg,newdata = Test,type = "response")

table(Test$over50k,log_reg_pred >= 0.5)
(9051+1888)/nrow(Test) # 0.8552107

# Baseline accuracy
table(Test$over50k)
9713/nrow(Test) # 0.7593621

# ROCR
library(ROCR)
ROCR_pred = prediction(log_reg_pred,Test$over50k)
ROCR_perf = performance(ROCR_pred,"tpr","fpr")
# plotting
plot(ROCR_perf,colorize = TRUE,print.cutoffs.at = seq(0.1,by=0.05),text.adj=c(-0.2,1.7))

AUC = as.numeric(performance(ROCR_pred,"auc")@y.values) 
AUC # 0.9061598

# Classification tree model
library(rpart)
library(rpart.plot)
set.seed(1000)
CARTcen = rpart(over50k ~ ., data=Train, method="class")
CARTpcen = predict(CARTcen,newdata = Test,type = "class")
prp(CARTcen)

table(Test$over50k,CARTpcen)
# accuracy
(9243+1596)/nrow(Test) # 0.8478618

# Plot the ROC curve for the CART model
ROCR_pred = predict(CARTcen, newdata = Test)

CARTROCR_pred = prediction(ROCR_pred[,2],Test$over50k)
CARTROCR_perf = performance(CARTROCR_pred,"tpr","fpr")
# plotting
plot(CARTROCR_perf,colorize = TRUE,print.cutoffs.at = seq(0.1,by=0.05),text.adj=c(-0.2,1.7))

AUC = as.numeric(performance(CARTROCR_pred,"auc")@y.values)
AUC # 0.8470256



# A Random Forest Model

###################################
# how to down-sample a training set
###################################
# While some modern personal computers can build a random forest model on the 
# entire training set, others might run out of memory when trying to train the 
# model since random forests is much more computationally intensive than CART 
# or Logistic Regression. For this reason, before continuing we will define a 
# new training set to be used when building our random forest model, that 
# contains 2000 randomly selected obervations from the original training set.
###################################

set.seed(1)
TrainSmall = Train[sample(nrow(Train), 2000), ]

set.seed(1)
censusrf = randomForest(over50k ~ . , data = TrainSmall)
# And then you can make predictions on the test set by using the following command:
predictTest = predict(censusrf, newdata=Test)
# And to compute the accuracy, you can create the confusion matrix:
table(Test$over50k, predictTest)
# The accuracy of the model should be around
(9586+1093)/nrow(test) # 75.20423


# One metric that we can look at is the number of times, aggregated over all of 
# the trees in the random forest model, that a certain variable is selected for 
# a split.

vu = varUsed(censusrf, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(censusrf$forest$xlevels[vusorted$ix]))

# A different metric we can look at is related to "impurity", which measures how
# homogenous each bucket or leaf of the tree is. In each tree in the forest, 
# whenever we select a variable and perform a split, the impurity is decreased. 
# Therefore, one way to measure the importance of a variable is to average the 
# reduction in impurity, taken over all the times that variable is selected for 
# splitting in all of the trees in the forest.

varImpPlot(censusrf)

# Selecting cp by Cross-Validation
library(caret)
library(e1071)

set.seed(2)
numFolds = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid(.cp = seq(0.002,0.1,0.002))

# Perform the cross validation
train(over50k ~ ., data = Train, method = "rpart", trControl = numFolds, tuneGrid = cartGrid)

# Create a new CART model
censusCV = rpart(over50k ~ ., data = Train, method="class", cp = 0.002)
prp(censusCV)

# Make predictions
# type = "class",so that we get class predictions.
PredictCV = predict(censusCV, newdata = Test, type = "class")

# Confusion matrix
table(Test$over50k, PredictCV)
(9178+1838)/nrow(Test) # 0.8612306

# plot tree
prp(StevensTreeCV)



