
# 
data(state)
statedata = data.frame(state.x77)
# statedata = read.csv("statedataSimple.csv")
str(statedata)

names(statedata)
# Life.Exp+Population+Income+Illiteracy+Murder+HS.Grad+Frost+Area 

# Linear Regression Models
lin_reg = lm(Life.Exp ~ .,data =statedata)
summary(lin_reg)

# Collinearity
cor(statedata)

# the sum of squared errors (SSE)
state.sse = sum(lin_reg$residuals^2)
state.sse # 23.29714

# another method
# Predictions2 = predict(RegModel2)
# and then computing the sum of the squared difference between the actual values 
# and the predictions:
# sum((statedata$Life.Exp - Predictions2)^2).

# Linear Regression Models 2
lin_reg2 = lm(Life.Exp ~ Population + Murder + HS.Grad + Frost,data = statedata)
summary(lin_reg2)

# the sum of squared errors (SSE)
state.sse = sum(lin_reg2$residuals^2)
state.sse # 23.30804

# CART Models
library(rpart)
library(rpart.plot)
set.seed(1000)
# no method="class" argument since this is a regression tree
CARTsta = rpart(Life.Exp ~ .,data = statedata)
prp(CARTsta)

# predict
CARTreg = predict(CARTsta)

# SSE
CARTreg.sse = sum((statedata$Life.Exp - CARTreg)^2)
CARTreg.sse # 28.99848

# new model
CARTsta2 = rpart(Life.Exp ~ .,data = statedata,minbucket=5)
prp(CARTsta2)

# predict
CARTreg2 = predict(CARTsta2)

# SSE
CARTreg2.sse = sum((statedata$Life.Exp - CARTreg2)^2)
CARTreg2.sse # 23.64283

# new model 2
CARTsta3 = rpart(Life.Exp ~ Area,data = statedata,minbucket=1)
prp(CARTsta3)

# predict
CARTreg3 = predict(CARTsta3)

# SSE
CARTreg3.sse = sum((statedata$Life.Exp - CARTreg3)^2)
CARTreg3.sse # 9.312442

# Cross-validation
library(caret)
set.seed(111)
numFolds = trainControl(method = "cv", number = 10)
cartGrid = expand.grid(.cp = seq(0.01,0.5,0.01))

# Perform the cross validation
train(Life.Exp ~ ., data=statedata, method="rpart", trControl = numFolds, tuneGrid = cartGrid)

# Create a new CART model
stateCV = rpart(Life.Exp ~ ., data = statedata, cp = 0.12)
prp(stateCV)

# SSE
PredictCV = predict(stateCV)
stateCV.sse = sum((statedata$Life.Exp - PredictCV)^2)
stateCV.sse # 32.86549



# Cross-validation for CARTsta3
set.seed(111)
numFolds = trainControl(method = "cv", number = 10)
cartGrid = expand.grid(.cp = seq(0.01,0.5,0.01))

train(Life.Exp ~ Area, data=statedata, method="rpart", trControl = numFolds, tuneGrid = cartGrid)

CARTsta32 = rpart(Life.Exp ~ Area,data = statedata,method = "class", cp = 0.02)
prp(CARTsta32)

CARTreg32 = predict(CARTsta32)

# SSE
CARTreg32.sse = sum((statedata$Life.Exp - CARTreg32)^2)
CARTreg32.sse # 44.26817
