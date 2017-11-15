rm(list = ls())
gc()

wine = read.csv("wine.csv")
str(wine)
summary(wine)

hist(wine$Price)

plot(wine$Price,wine$AGST)

# one-variable linear regression equation using AGST to predict Price.
model1 = lm(Price ~ AGST, data = wine)
summary(model1)
# Multiple R-squared:  0.435,	Adjusted R-squared:  0.4105

#Sum of squared error
SSE_m1 = sum(model1$residuals^2)
SSE_m1 # 5.734875

model2 = lm(Price ~ AGST + HarvestRain, data = wine)
summary(model2)
# Multiple R-squared:  0.7074,	Adjusted R-squared:  0.6808 

#Sum of squared error
SSE_m2 = sum(model2$residuals^2)
SSE_m2 # 2.970373

# Note:
# model2 SSE is better than model1 SSE

model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data = wine)
summary(model3)
# Multiple R-squared:  0.8294,	Adjusted R-squared:  0.7845

#Sum of squared error
SSE_m3 = sum(model3$residuals^2)
SSE_m3 # 1.732113


model4 = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data = wine)
summary(model4)
# Multiple R-squared:  0.8294,	Adjusted R-squared:  0.7845

#Sum of squared error
SSE_m3 = sum(model3$residuals^2)
SSE_m3 # 1.732113

names(wine)

plot(wine$Age,wine$FrancePop)
# to find correlation
cor(wine$WinterRain,wine$Price) # 0.1366505
cor(wine)


wineTest <- read.csv("wine_test.csv")
str(wineTest)

# to make prediction
predictTest <- predict(model4,newdata = wineTest)
predictTest

# the formula for R-squared is 1 minus the sum of squared errors divided
# by the total sum of squares.

#let's start by computing the sum of squared errors

SSE_predict <- sum((wineTest$Price - predictTest)^2)
SSE_predict

# Now, let's compute the total sum of squares, which we'll call SST.
# This time, when we take the sum, we want to compute
# the differences between the actual values,
SST_predict <- sum((wineTest$Price - mean(wine$Price))^2)
SST_predict

r_square <- 1 - SSE_predict/SST_predict
r_square
# So the R-squared on our test set is 0.79


