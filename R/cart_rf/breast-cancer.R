# Title: Wisconsin Diagnostic Breast Cancer (WDBC)


# 
# file1 = read.table("wdbc.data", sep = ",",dec = ",", na.strings = "unknown",col.names = c("id","diag",
#                                                                              "mrad","mtext","mperi","marea","msmooth","mcompact","mcon","mconpts","msymm","mfdim",
#                                                                              "radse","textse","perise","arease","smoothse","compactse","conse","conptsse","symmse","fdimse",
#                                                                              "wrad","wtext","wperi","warea","wsmooth","wcompact","wcon","wconpts","wsymm","wfdim"),stringsAsFactors = FALSE)
# 
# 
# write.csv(file1, "WDBC.csv")
# 

############################################
# Start from here:
############################################
WDBC = read.csv("WDBC.csv")

# for tab seperation use : sep = "\t"
# View(WDBC)
# WDBC$X = NULL
# Attribute information

# 1) ID number
# 2) Diagnosis (M = malignant, B = benign)
# 3-32)

# Ten real-valued features are computed for each cell nucleus:
  
# a) radius (mean of distances from center to points on the perimeter) - rad
# b) texture (standard deviation of gray-scale values) - text
# c) perimeter - peri
# d) area - area
# e) smoothness (local variation in radius lengths) - smooth
# f) compactness (perimeter^2 / area - 1.0) - compact
# g) concavity (severity of concave portions of the contour) - con
# h) concave points (number of concave portions of the contour) - conpts
# i) symmetry - symm
# j) fractal dimension ("coastline approximation" - 1) - fdim

# The mean, standard error, and "worst" or largest (mean of the three
# largest values) of these features were computed for each image,
# resulting in 30 features.  For instance, field 3 is Mean Radius, field 13 is 
# Radius SE, field 23 is Worst Radius.

# bucket (1 = benign, 2 = malignant)
cor(WDBC[3:33])

# Convert bucket variable to factor : 
WDBC$bucket = as.factor(WDBC$bucket)
str(WDBC)
class(WDBC$bucket) # "factor"

library(caTools)
set.seed(123)
split = sample.split(WDBC$bucket,SplitRatio = 0.65)
Train = subset(WDBC,split == TRUE)
Test = subset(WDBC,split == FALSE)

############# Baseline ##############
table(Test$bucket)
125/nrow(Test) # 0.6281407

#############################
# Modelling with mean values:
#############################
WDBClog = glm(diag ~ mrad + mtext + mperi + marea + msmooth + mcompact + mcon + mconpts + msymm + mfdim,data = Train,family = "binomial")
summary(WDBClog) # AIC: 119.43

WDBClog1 = glm(bucket ~ mrad + mtext + marea + msmooth + mcompact + mcon + mconpts + msymm + mfdim,data = Train,family = "binomial")
summary(WDBClog1) # AIC: 117.51

WDBClog2 = glm(bucket ~ mrad + mtext + marea + msmooth + mcon + mconpts + msymm + mfdim,data = Train,family = "binomial")
summary(WDBClog2) # AIC: 115.86

WDBClog3 = glm(bucket ~ mrad + mtext + marea + msmooth + mconpts + msymm + mfdim,data = Train,family = "binomial")
summary(WDBClog3) # AIC: 114.18

WDBClog4 = glm(bucket ~ mrad + mtext + marea + msmooth + mconpts + mfdim,data = Train,family = "binomial")
summary(WDBClog4) # AIC: 113.08

WDBClog5 = glm(bucket ~ mrad + mtext + marea + msmooth + mconpts,data = Train,family = "binomial")
summary(WDBClog5) # AIC: 113.32

# Prediction
WDBCregpred1 = predict(WDBClog4,type = "response",newdata = Test)
table(Test$bucket,WDBCregpred1>0.5)
(121+70)/nrow(Test) # 0.959799 with WDBClog4

WDBCregpred2 = predict(WDBClog5,type = "response",newdata = Test)
table(Test$bucket,WDBCregpred2>0.5)
(121+70)/nrow(Test) # 0.959799 with WDBClog5

#############################
# Modelling with large values:
#############################
WDBClog6 = glm(bucket ~ wrad + wtext + wperi + warea + wsmooth + wcompact + wcon + wconpts + wsymm + wfdim,data = Train,family = "binomial")
summary(WDBClog6) # AIC: 85.083

WDBClog7 = glm(bucket ~ wrad + wtext + warea + wsmooth + wcompact + wcon + wconpts + wsymm + wfdim,data = Train,family = "binomial")
summary(WDBClog7) # AIC: 83.284

WDBClog8 = glm(bucket ~ wtext + warea + wsmooth + wcompact + wcon + wconpts + wsymm + wfdim,data = Train,family = "binomial")
summary(WDBClog8) # AIC: 81.439

WDBClog9 = glm(bucket ~ wtext + warea + wsmooth + wcompact + wcon + wconpts + wsymm,data = Train,family = "binomial")
summary(WDBClog9) # AIC: 80.004

WDBClog10 = glm(bucket ~ wtext + warea + wsmooth + wcompact + wconpts + wsymm,data = Train,family = "binomial")
summary(WDBClog10) # AIC: 79.778

WDBClog11 = glm(bucket ~ wtext + warea + wsmooth + wconpts,data = Train,family = "binomial")
summary(WDBClog11) # AIC: 79.83

# Prediction
WDBCregpred3 = predict(WDBClog10,type = "response",newdata = Test)
table(Test$bucket,WDBCregpred3>0.5)
(121+72)/nrow(Test) # 0.9698492 with WDBClog10

WDBCregpred4 = predict(WDBClog11,type = "response",newdata = Test)
table(Test$bucket,WDBCregpred4>0.5)
(122+72)/nrow(Test) # 0.9748744 with WDBClog5

# Setting threshold:
library(ROCR)
# perdiction function:
ROCR_pred = prediction(WDBCregpred4,Test$bucket)
# performance function:
ROCR_perf = performance(ROCR_pred,"tpr","fpr")
# plot the ROC curve:
plot(ROCR_perf,colorize = TRUE, print.cutoffs.at = seq(0.2,by = 0.2),
     text.adj = c(0,1))

confusionmat = matrix(c("TN","FP","FN","TP"),byrow = TRUE,nrow = 2)
confusionmat

# Here in our case False Positive cases should be as low as possible to avoid 
# classifying Malignant cancer as Benign cancer.
# Therefore we'll choose our threshold accordingly.
table(Test$bucket,WDBCregpred4>0.1) # 0.9447236 , FP = 11
table(Test$bucket,WDBCregpred4>0.2) # 0.9648241 , FP = 7
table(Test$bucket,WDBCregpred4>0.3) # 0.9648241 , FP = 5
table(Test$bucket,WDBCregpred4>0.4) # 0.9648241 , FP = 5
table(Test$bucket,WDBCregpred4>0.5) # 0.9748744 , FP = 3
# We should choose among these thresholds :
table(Test$bucket,WDBCregpred4>0.6) # 0.9849246 , FP = 1
table(Test$bucket,WDBCregpred4>0.7) # 0.9849246 , FP = 1
table(Test$bucket,WDBCregpred4>0.8) # 0.9748744 , FP = 0

# AUC
AUC = as.numeric(performance(ROCR_pred,"auc")@y.values)
AUC # 0.9984865

#######################################
# Modelling with standard error values:
#######################################
WDBClog12 = glm(bucket ~ radse + textse + perise + arease + smoothse + compactse + conse + conptsse + symmse + fdimse,data = Train,family = "binomial")
summary(WDBClog12) # AIC: 184.17

WDBClog13 = glm(bucket ~ radse + textse + arease + smoothse + compactse + conse + conptsse + symmse + fdimse,data = Train,family = "binomial")
summary(WDBClog13) # AIC: 182.17

WDBClog14 = glm(bucket ~ radse + textse + arease + smoothse + compactse + conptsse + symmse + fdimse,data = Train,family = "binomial")
summary(WDBClog14) # AIC: 180.17

WDBClog15 = glm(bucket ~ radse + textse + arease + smoothse + compactse + conptsse + fdimse,data = Train,family = "binomial")
summary(WDBClog15) # AIC: 178.22

WDBClog16 = glm(bucket ~ radse + textse + arease + smoothse + compactse + fdimse,data = Train,family = "binomial")
summary(WDBClog16) # AIC: 176.6

WDBClog17 = glm(bucket ~ radse + arease + smoothse + compactse + fdimse,data = Train,family = "binomial")
summary(WDBClog17) # AIC: 175.95

WDBClog18 = glm(bucket ~ radse + arease + compactse + fdimse,data = Train,family = "binomial")
summary(WDBClog18) # AIC: 175.23

# Prediction
WDBCregpred5 = predict(WDBClog18,type = "response",newdata = Test)
table(Test$bucket,WDBCregpred5>0.5)
(117+60)/nrow(Test) # 0.8894472 with WDBClog18


###############################################################################
# Logistic Regression accuracy of 0.9748744 with WDBClog5
###############################################################################

###############################################################################
# CART

library(rpart)
library(rpart.plot)

#######################################
######### Classification ##############
#######################################
# Train model:
WDBCcart1 = rpart(bucket ~ mrad + mtext + mperi + marea + msmooth + mcompact + mcon + mconpts + msymm + mfdim,data = Train,method = "class",minbucket = 15)

# Plot model:
prp(WDBCcart1)

# Predict using model:
predictcart1 = predict(WDBCcart1,newdata = Test,type = "class")

# Confusion matrix:
table(Test$bucket,predictcart1)

# Accuracy:
(118+67)/nrow(Test) # 0.9296482

##########
# with CV:
##########

library(caret)
library(e1071)


# numfold:
numFolds = trainControl(method = "cv", number = 10)

# pick cp value:
cpGrig = expand.grid(.cp = seq(0.001,0.5,0.001))

# perform cv:
set.seed(1704)
train(bucket ~ mrad + mtext + mperi + marea + msmooth + mcompact + mcon + mconpts + msymm + mfdim,data = Train,
      method = "rpart", trControl = numFolds,tuneGrid = cpGrig)
# here bucket is a factor since we are performing classification.

# Train model:
WDBCcart1cv = rpart(bucket ~ mrad + mtext + mperi + marea + msmooth + mcompact + mcon + mconpts + msymm + mfdim,data = Train,method = "class",cp = 0.016)

# Plot model
prp(WDBCcart1cv)

# Predict using model:
predictTreecv1 = predict(WDBCcart1cv,newdata = Test,type = "class")

# Confusion matrix:
table(Test$bucket,predictTreecv1)

# Accuracy:
(117+69)/nrow(Test) # 0.9346734


#######################################
######### Regression ##################
#######################################
# Train model:
WDBCcart2 = rpart(bucket ~ mrad + mtext + mperi + marea + msmooth + mcompact + mcon + mconpts + msymm + mfdim,data = Train,minbucket = 15)

# Plot model:
prp(WDBCcart2)

# Predict using model:

WDBCregpred6 = predict(WDBCcart2,newdata = Test)

WDBCregpred6ROCR = prediction(WDBCregpred6[,2],Test$bucket)

WDBCregpred6ROCR_perf = performance(WDBCregpred6ROCR,"tpr","fpr")
# plotting
plot(WDBCregpred6ROCR_perf,colorize = TRUE,print.cutoffs.at = seq(0.1,by=0.05),text.adj=c(-0.2,1.7))

AUC = as.numeric(performance(WDBCregpred6ROCR,"auc")@y.values)
AUC # 0.9608108


table(Test$bucket,WDBCregpred6[,2]>= 0.40)

# Accuracy:
(118+67)/nrow(Test) # 0.9296482

##########
# with CV:
##########

# numfold:
numFolds = trainControl(method = "cv", number = 10)


# pick cp value:
cpGrig = expand.grid(.cp = seq(0.001,0.5,0.001))

set.seed(1704)
# perform cv:
train(bucket ~ wtext + warea + wsmooth + wconpts,data = Train,
      method = "rpart", trControl = numFolds,tuneGrid = cpGrig)


# Train model:
set.seed(1704)
WDBCcart1cv = rpart(bucket ~ wtext + warea + wsmooth + wconpts,data = Train,method = "class",cp = 0.08)

# Plot model
prp(WDBCcart1cv)

# Predict using model:
predictTreecv1 = predict(WDBCcart1cv,newdata = Test,type = "class")

# Confusion matrix:
table(Test$bucket,predictTreecv1)

# Accuracy:
(121+69)/nrow(Test) # 0.9547739



################
# Penalty Matrix
PenaltyMatrix = matrix(c(0,2,1,0), byrow=TRUE, nrow=2)
PenaltyMatrix

#### Penalty error for Baseline ###
# The baseline method of predicting the most frequent outcome for all observations
# This new baseline method would predict cost bucket 1 for everyone.
table(Test2$bucket)
125/nrow(Test2) # 0.6281407

# The penalty error of this baseline method be on the test set
NPenaltyMatrix = matrix(c(0,2), byrow=FALSE, nrow=2)
NPenaltyMatrix

as.matrix(table(Test$bucket)) * NPenaltyMatrix

sum(as.matrix(table(Test$bucket)) * NPenaltyMatrix)/nrow(Test) 
# 0.7437186

# Baseline model #
# At accuracy of 0.6281407 with penalty error of 0.7437186.
# Our model should be high on accuracy and low on penalty error.

set.seed(1704)
WDBCcart1cv = rpart(bucket ~ wtext + warea + wsmooth + wconpts,data = Train2,cp = 0.08)

# Plot model
prp(WDBCcart1cv)

# Predict using model:
predictTreecv1 = predict(WDBCcart1cv,newdata = Test2,type = "class")

# Confusion matrix:
table(Test2$bucket,predictTreecv1)

# Accuracy:
(121+69)/nrow(Test2) # 0.9547739

# Penalty Error
as.matrix(table(Test2$bucket, predictTreecv1))*PenaltyMatrix

sum(as.matrix(table(Test2$bucket, predictTreecv1))*PenaltyMatrix)/nrow(Test)
#0.06532663


# New CART model with loss matrix
WDBCcartcvloss = rpart(bucket ~ wtext + warea + wsmooth + wconpts,data = Train2,method = "class",cp = 0.08, parms=list(loss=PenaltyMatrix))

# Redo predictions and penalty error
predictTreecv1 = predict(WDBCcartcvloss,newdata = Test2,type = "class")

table(Test2$bucket, predictTreecv1)

(121+69)/nrow(Test2) # 0.9547739

sum(as.matrix(table(Test2$bucket,predictTreecv1))*PenaltyMatrix)/nrow(Test)
# 0.06532663





