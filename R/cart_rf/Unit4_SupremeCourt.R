
# Read in the data
stevens = read.csv("stevens.csv")
str(stevens)

# Split the data
library(caTools)
set.seed(3000)
spl = sample.split(stevens$Reverse, SplitRatio = 0.7)
Train = subset(stevens, spl==TRUE)
Test = subset(stevens, spl==FALSE)

# Install rpart library
# install.packages("rpart")
library(rpart)
# install.packages("rpart.plot")
library(rpart.plot)

# CART model
# method = "class". This tells rpart to build a classification tree, instead of
# a regression tree.
# minbucket = 25. This limits the tree so that it doesn't overfit to our 
# training set.
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", minbucket=25)

prp(StevensTree)

# Make predictions
PredictCART = predict(StevensTree, newdata = Test, type = "class")
table(Test$Reverse, PredictCART)
(41+71)/(41+36+22+71) # 0.6588235

#logistic regression
StevensLogReg = glm(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, family = binomial)
PredictLogReg = predict(StevensLogReg, newdata = Test, type = "response")
table(Test$Reverse, PredictLogReg > 0.50)
(47+66)/(47+66+30+27) # 0.6647059

#baseline
table(Train$Reverse)
216/(180+216) # 0.5454545

# ROC curve
library(ROCR)

PredictROC = predict(StevensTree, newdata = Test)
PredictROC
# These numbers give the percentage of training set data in that subset with 
# outcome 0 and the percentage of data in the training set in that subset with 
# outcome 1.

pred = prediction(PredictROC[,2], Test$Reverse)
perf = performance(pred, "tpr", "fpr")
plot(perf)

# AUC
as.numeric(performance(pred, "auc")@y.values)
# 0.6927105

# minbucket=5
StevensTree2 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", minbucket=5)
prp(StevensTree2)

# minbucket=100
StevensTree3 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", minbucket=100)
prp(StevensTree3)


# Install randomForest package
# install.packages("randomForest")
library(randomForest)

# Build random forest model
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25)

# Convert outcome to factor
Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)

# Try again
set.seed(200)
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25 )

# Make predictions
PredictForest = predict(StevensForest, newdata = Test)
table(Test$Reverse, PredictForest)
(44+75)/(44+33+18+75)



# Install cross-validation packages
# install.packages("caret")
library(caret)
# install.packages("e1071")
library(e1071)
# library(class)

# Define cross-validation experiment
# method = "cv", for cross validation, and then number = 10, for 10 folds.
numFolds = trainControl( method = "cv", number = 10 )
# This will define our cp parameters to test as numbers from 0.01 to 0.5, 
# in increments of 0.01.
cpGrid = expand.grid( .cp = seq(0.01,0.5,0.01)) 

# Perform the cross validation
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

# Create a new CART model
# method = "rpart", to cross validate a CART model,
# trControl = numFolds, the outputof our trainControl function 
# tuneGrid = cpGrid, the output of the expand.grid function.
StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", cp = 0.19)

# Make predictions
# type = "class",so that we get class predictions.
PredictCV = predict(StevensTreeCV, newdata = Test, type = "class")

# Confusion matrix
table(Test$Reverse, PredictCV)
(59+64)/(59+18+29+64) # 0.7235294

# plot tree
prp(StevensTreeCV)
