

# Read in dataset
quality = read.csv("quality.csv")

# Look at structure
str(quality)

# Table outcome
table(quality$PoorCare)
#  0  1 
# 98 33 
# good care = 98 & poor care = 33

# Baseline accuracy
98/131 # 0.7480916

# Here basseline model is to just predict the most frequent outcome for all 
# observations. 98 out of 131 are getting good care.
# Since good care is more common than poor care
# so if we predict all are getting good care then our model will be
# of 75% accuracy.
# This is our baseline model.

# Install and load caTools package
# install.packages("caTools")
library(caTools)

# Randomly split data
set.seed(88)
split = sample.split(quality$PoorCare, SplitRatio = 0.75)
split

# Create training and testing sets
qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)

# Logistic Regression Model
QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data=qualityTrain, family=binomial)
summary(QualityLog)

# Make predictions on training set
predictTrain = predict(QualityLog, type="response")
table(qualityTrain$PoorCare, predictTrain >= 0.5)

# Analyze predictions
summary(predictTrain)
prop.table(tapply(predictTrain, qualityTrain$PoorCare, mean))



#tapply(predictTrain, qualityTrain$PoorCare, summary)

# Compute the test set predictions
predictTest = predict(QualityLog, type="response", newdata=qualityTest)
summary(predictTest)
table(qualityTest$PoorCare, predictTest >= 0.5)

# You can compute the test set AUC
library(ROCR)
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc


# After few changes
QualityLog1 = glm(PoorCare ~ StartedOnCombination  +  ProviderCount, data=qualityTrain, family=binomial)
summary(QualityLog1)


# Confusion matrix for threshold of 0.5
table(qualityTrain$PoorCare, predictTrain > 0.5)

# Sensitivity and specificity
10/(10 + 15) # Sensitivity 0.4
70/(70 + 4) # specificity 0.9459459


# Confusion matrix for threshold of 0.7
table(qualityTrain$PoorCare, predictTrain > 0.7)

# Sensitivity and specificity
8/(17 + 8) # Sensitivity 0.32
73/(73 + 1) # specificity 0.9864865

# Confusion matrix for threshold of 0.2
table(qualityTrain$PoorCare, predictTrain > 0.2)

# Sensitivity and specificity
16/(16 + 9) # Sensitivity 0.64
54/(54 + 20) # specificity 0.7297297



# Install and load ROCR package
# install.packages("ROCR")
library(ROCR)

# Prediction function
ROCRpred = prediction(predictTrain, qualityTrain$PoorCare)


# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperf)

# Add colors
plot(ROCRperf, colorize=TRUE)

# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.05),text.adj=c(-0.2,1.7))

# my
table(qualityTrain$PoorCare, predictTrain > 0.1)


# Sensitivity and specificity
54/(54 + 20) # Sensitivity 0.9864865
16/(16 + 9) # specificity 0.92
1 - (23/(23 + 2)) # fp 0.08

table(qualityTrain$PoorCare, predictTrain > 0.7)
73/(73 + 1) # Sensitivity 0.9864865
17/(17 + 8) # specificity 0.68
1 - (17/(17 + 8)) # fp 0.32


table(qualityTrain$PoorCare, predictTrain > 0.4)

# Sensitivity and specificity
67/(67 + 7) # Sensitivity 0.9054054
12/(12 + 13) # specificity 0.48
1 - (12/(12 + 13)) # fp 0.52

69/(69 + 5) # Sensitivity 0.9324324
14/(14 + 11) # specificity 0.56
1 - 14/(14 + 11) # fp 0.44
