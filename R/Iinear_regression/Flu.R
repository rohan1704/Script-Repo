

# Read in the data
FluTrain = read.csv("FluTrain.csv")
str(FluTrain)
summary(FluTrain)

# maximum number of ILI
subset(FluTrain, ILI == max(ILI))
# alt
which.max(FluTrain$ILI) # this will give index of max
FluTrain$Week[303] # use this to access that index
# compact version
FluTrain$Week[which.max(FluTrain$ILI)]

#                        Week      ILI Queries
# 303 2009-10-18 - 2009-10-24 7.618892       1


hist(FluTrain$ILI)
?hist
# Most of the ILI values are small, with a relatively small number of much
# larger values (in statistics, this sort of data is called "skew right").

# When handling a skewed dependent variable, it is often useful to predict the 
# logarithm of the dependent variable instead of the dependent variable itself -- 
# this prevents the small number of unusually large or small observations from 
# having an undue influence on the sum of squared errors of predictive models.
# In this problem, we will predict the natural log of the ILI variable, which 
# can be computed in R using the log() function.

# plot
plot(FluTrain$Queries,log(FluTrain$ILI))

names(FluTrain)
FluTrend1 <- lm(log(ILI) ~ Queries,data = FluTrain)
summary(FluTrend1)
plot(FluTrend1$model)

# For a single variable linear regression model, there is a direct relationship
# between the R-squared and the correlation between the independent and the 
# dependent variables
# R-squared = Correlation^2
# Multiple R-squared:  0.709
Correlation = cor(FluTrain$Queries, log(FluTrain$ILI))
Correlation # 0.8420333
Correlation^2 # 0.7090201
log(1/Correlation) # 0.1719357
exp(-0.5*Correlation) # 0.6563792


# Predict
FluTest = read.csv("FluTest.csv")

hist(FluTest$ILI)

PredTest1 = predict(FluTrend1, newdata=FluTest)
summary(PredTest1)

# The dependent variable in our model is log(ILI), so PredTest1 would contain 
# predictions of the log(ILI) value. We are instead interested in obtaining 
# predictions of the ILI value.
# We can convert from predictions of log(ILI) to predictions of ILI via 
# exponentiation, or the exp() function.
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))

which(FluTest$Week == "2012-03-11 - 2012-03-17") # 11
PredTest1[11]
# compact
PredTest1[which(FluTest$Week == "2012-03-11 - 2012-03-17")]
# 2.187378

# Training a Time Series Model

library(zoo)

# First, we need to decide the amount of time to lag the observations. 
# Because the ILI variable is reported with a 1- or 2-week lag, a decision maker 
# cannot rely on the previous week's ILI value to predict the current week's 
# value. Instead, the decision maker will only have data available from 2 or 
# more weeks ago. We will build a variable called ILILag2 that contains the 
# ILI value from 2 weeks before the current observation.
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
ILILag2_test = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
# In these commands, the value of -2 passed to lag means to return 2 
# observations before the current one; a positive value would have returned 
# future observations. The parameter na.pad=TRUE means to add missing values 
# for the first two weeks of our dataset, where we can't compute the data from 
# 2 weeks earlier.

FluTrain$ILILag2 = coredata(ILILag2)
FluTest$ILIag2 = coredata(ILILag2_test)
View(FluTrain)
View(FluTest)
summary(FluTrain$ILILag2)
summary(FluTest)

# skewed right
hist(FluTrain$ILILag2)
hist(FluTrain$ILI)

plot(log(FluTrain$ILILag2),log(FluTrain$ILI))

FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2),data = FluTrain)
summary(FluTrend2)

FluTest$ILILag2[1] = FluTrain$ILI[(nrow(FluTrain)-1)]
FluTest$ILILag2[2] = FluTrain$ILI[(nrow(FluTrain))]


PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
PredTest2

# And then we can compute the RMSE with the following commands:
SSE = sum((PredTest2 - FluTest$ILI)^2)
SSE
RMSE = sqrt(SSE / nrow(FluTest))
RMSE
# Alternatively, you could use the following command to compute the RMSE:
sqrt(mean((PredTest2 - FluTest$ILI)^2))
#The test-set RMSE of FluTrend2 is 0.294.

