
library(caret)
library(caTools)
library(flexclust)

stocks = read.csv("StocksCluster.csv")

str(stocks)

summary(stocks)


prop.table(table(stocks$PositiveDec))


summary(stocks)


set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

# logistic regression model
StocksModel = glm(PositiveDec ~ .,data = stocksTrain,family=binomial)
summary(StocksModel)

# the overall accuracy on the training set, using a threshold of 0.5
Stockspredtr = predict(StocksModel,newdata = stocksTrain,type = "response")
Stockspredtr

table(stocksTrain$PositiveDec,Stockspredtr >= 0.5)
(990+3640)/nrow(stocksTrain) # 0.5711818



# the overall accuracy of the model on the test, again using a threshold of 0.5
Stockspredte = predict(StocksModel,newdata = stocksTest,type = "response")
Stockspredte

table(stocksTest$PositiveDec,Stockspredte >= 0.5)
(417+1553)/nrow(stocksTest) # 0.5670697


# Baseline model
table(stocksTest$PositiveDec)
1897/nrow(stocksTest) # 0.5460564


# Clustering Stocks
# The first step in this process is to remove the dependent variable
# In cluster-then-predict, our final goal is to predict the dependent variable, 
# which is unknown to us at the time of prediction. Therefore, if we need to know 
# the outcome value to perform the clustering, the methodology is no longer useful 
# for prediction of an unknown outcome value.
# This is an important point that is sometimes mistakenly overlooked. If you use the 
# outcome value to cluster, you might conclude your method strongly outperforms a 
# non-clustering alternative. However, this is because it is using the outcome to 
# determine the clusters, which is not valid.

limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL

limitedTest = stocksTest
limitedTest$PositiveDec = NULL


# Normalization of data:
# The preProcess command from the caret package, normalizes variables by subtracting
# by the mean and dividing by the standard deviation.

preproc = preProcess(limitedTrain)

normTrain = predict(preproc, limitedTrain)

summary(normTrain)
mean(normTrain$ReturnJan)

normTest = predict(preproc, limitedTest)
summary(normTest)
mean(normTest$ReturnJan)

# Since normTest was constructed by subtracting by the mean ReturnJan value from the
# training set, this explains why the mean value of ReturnJan is slightly negative 
# in normTest.


# k-means Clustering
k = 3
set.seed(144)
km = kmeans(normTrain,centers = k)

table(km$cluster)
# Alt
# km$size


# The flexclust package to obtain training set and testing set cluster assignments 
# for our observations

km.kcca = as.kcca(km, normTrain)

clusterTrain = predict(km.kcca)

clusterTest = predict(km.kcca, newdata=normTest)


table(clusterTest)

# Cluster-Specific Predictions

stocksTrain1 = subset(stocksTrain,clusterTrain == 1)
mean(stocksTrain1$PositiveDec)

stocksTrain2 = subset(stocksTrain,clusterTrain == 2)
mean(stocksTrain2$PositiveDec)

stocksTrain3 = subset(stocksTrain,clusterTrain == 3)
mean(stocksTrain3$PositiveDec)

stocksTest1 = subset(stocksTest,clusterTest == 1)
mean(stocksTest1$PositiveDec)

stocksTest2 = subset(stocksTest,clusterTest == 2)
mean(stocksTest2$PositiveDec)

stocksTest3 = subset(stocksTest,clusterTest == 3)
mean(stocksTest3$PositiveDec)


# Cluster-Specific Predictions
StocksModel1 = glm(PositiveDec ~ .,data = stocksTrain1,family = binomial)

StocksModel2 = glm(PositiveDec ~ .,data = stocksTrain2,family = binomial)

StocksModel3 = glm(PositiveDec ~ .,data = stocksTrain3,family = binomial)


StocksModel1$coefficients
StocksModel2$coefficients
StocksModel3$coefficients
# From summary(StocksModel1), summary(StocksModel2), and summary(StocksModel3), 
# ReturnJan, ReturnFeb, ReturnMar, ReturnJune, ReturnAug, and ReturnOct differ in 
# sign between the models.

#

PredictTest1 = predict(StocksModel1,newdata = stocksTest1,type = "response")
table(stocksTest1$PositiveDec,PredictTest1 >= 0.5)
(30+774)/nrow(stocksTest1) # 0.6194145

PredictTest2 = predict(StocksModel2,newdata = stocksTest2,type = "response")
table(stocksTest2$PositiveDec,PredictTest2 >= 0.5)
(388+757)/nrow(stocksTest2) # 0.5504808

PredictTest3 = predict(StocksModel3,newdata = stocksTest3,type = "response")
table(stocksTest3$PositiveDec,PredictTest3 >= 0.5)
(49+13)/nrow(stocksTest3) # 0.6458333



# To compute the overall test-set accuracy of the cluster-then-predict approach, 
# we can combine all the test-set predictions into a single vector and all the true 
# outcomes into a single vector
AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)

AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)

table(AllOutcomes,AllPredictions >= 0.5)
(467+1544)/length(AllPredictions) # 0.5788716


