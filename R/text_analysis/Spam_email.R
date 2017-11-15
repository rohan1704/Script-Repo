
email = read.csv("emails.csv", stringsAsFactors = FALSE)

str(email)
summary(email)

table(email$spam)

email$text[1000]
email$text[1]


max(nchar(email$text))

which.min(nchar(email$text))

# build a new corpus
corpus = VCorpus(VectorSource(email$text))

# convert the text to lowercase.
corpus = tm_map(corpus, content_transformer(tolower))

# remove all punctuation from the corpus.
corpus = tm_map(corpus, removePunctuation)

# remove all English stopwords from the corpus.
corpus = tm_map(corpus, removeWords, stopwords("english"))

# stem the words in the corpus.
corpus = tm_map(corpus, stemDocument)

# build a document term matrix
dtm = DocumentTermMatrix(corpus)
dtm

spdtm = removeSparseTerms(dtm, 0.95)
spdtm

emailsSparse = as.data.frame(as.matrix(spdtm))
emailsSparse


colnames(emailsSparse) = make.names(colnames(emailsSparse))
View(emailsSparse)


sort(colSums(emailsSparse),decreasing = TRUE)
which.max(colSums(emailsSparse))

emailsSparse$spam = email$spam


sort(colSums(emailsSparse) >= 5000,decreasing = TRUE)

sort(colSums(subset(emailsSparse, spam == 0)))

sort(colSums(subset(emailsSparse, spam == 1)))

#######
emailsSparse$spam = as.factor(emailsSparse$spam)

set.seed(123)

split = sample.split(emailsSparse$spam,0.7)

train = subset(emailsSparse, split==TRUE)
test = subset(emailsSparse, split==FALSE)

# Baseline model:
table(train$spam)
3052/nrow(train) # 0.5606759

#######################
# Log Regression
# You may have noticed that training the logistic regression model yielded the 
# messages "algorithm did not converge" and "fitted probabilities numerically 
# 0 or 1 occurred". Both of these messages often indicate overfitting and the 
# first indicates particularly severe overfitting, often to the point that the 
# training set observations are fit perfectly by the model.

spamLog = glm(spam ~.,data = train,family = "binomial")
predLog = predict(spamLog,type = "response")

table(predLog < 0.00001)
table(predLog > 0.99999)
table(predLog >= 0.00001 & predLog <= 0.99999)

summary(spamLog)

table(train$spam,predLog >= 0.5)
(3052+954)/nrow(train) # 0.9990025

library(ROCR)
predROCRLog = prediction(predLog,train$spam)
perfROCRLog = performance(predROCRLog, "tpr", "fpr")
plot(perfROCRLog, colorize=TRUE)
# Compute AUC
performance(predROCRLog, "auc")@y.values # 0.9999959

##
predLogtest = predict(spamLog,newdata = test,type = "response")
table(test$spam,predLogtest >= 0.5)
(1257+376)/nrow(test) # 0.9505239
predROCRLogtest = prediction(predLogtest,test$spam)
perfROCRLogtest = performance(predROCRLogtest, "tpr", "fpr")
plot(perfROCRLogtest, colorize=TRUE)
# Compute AUC
performance(predROCRLogtest, "auc")@y.values # 0.9627517
#######################
# CART
library(rpart)
library(rpart.plot)

spamCart = rpart(spam ~., data=train, method="class")
predCart = predict(spamCart)[,2]
prp(spamCart)

table(train$spam,predCart >=0.5)
(2885+894)/nrow(train) # 0.942394

predROCRCart = prediction(predCart,train$spam)
perfROCRCart = performance(predROCRCart, "tpr", "fpr")
plot(perfROCRCart, colorize=TRUE)
# Compute AUC
performance(predROCRCart, "auc")@y.values # 0.9696044

##
predCarttest = predict(spamCart,newdata = test)[,2]
table(test$spam,predCarttest >=0.5)
(1228+386)/nrow(test) # 0.9394645

predROCRCarttest = prediction(predCarttest,test$spam)
perfROCRCarttest = performance(predROCRCarttest, "tpr", "fpr")
plot(perfROCRCarttest, colorize=TRUE)
# Compute AUC
performance(predROCRCarttest, "auc")@y.values # 0.963176

######################
# RF
library(randomForest)

set.seed(123)
spamRF = randomForest(spam ~., data=train)
predRF = predict(spamRF,type="prob")[,2]
table(train$spam,predRF > 0.5)
(3013+914)/nrow(train) # 0.9793017

predROCRRf = prediction(predRF,train$spam)
perfROCRRf = performance(predROCRRf, "tpr", "fpr")
plot(perfROCRRf, colorize=TRUE)
# Compute AUC
performance(predROCRRf, "auc")@y.values # 0.9979116

##
predRFtest = predict(spamRF,newdata = test,type="prob")[,2]
table(test$spam,predRFtest > 0.5)
(1290+385)/nrow(test) # 0.9749709

predROCRRftest = prediction(predRFtest,test$spam)
perfROCRRftest = performance(predROCRRftest, "tpr", "fpr")
plot(perfROCRRftest, colorize=TRUE)
# Compute AUC
performance(predROCRRftest, "auc")@y.values # 0.9975656


# The parameter method="class" builds a classification tree, possibly with 
# multiple classes. The parameter method="anova" would build a regression tree.

# To answer your question, you would also use the method="class" for a 
# classification problem that is not binary.


# Integrating Word Count Information

wordCount = rowSums(as.matrix(dtm))
wordCount
# or
# library(slam)
# wordCount = rollup(dtm, 2, FUN=sum)$v





hist(wordCount)

hist(log(wordCount))

logWordCount = log(wordCount)

boxplot(logWordCount[which(emailsSparse$spam == 0)])
boxplot(logWordCount[which(emailsSparse$spam == 1)])

# or
# emailsSparse$logWordCount = log(wordCount)
# boxplot(emailsSparse$logWordCount~emailsSparse$spam)


train2 = subset(emailsSparse, split==TRUE)
test2 = subset(emailsSparse, split==FALSE)

###
spam2CART = rpart(spam ~., data=train2, method="class")
prp(spam2CART)

pred2CART = predict(spam2CART,newdata = test2)[,2]
table(test2$spam,pred2CART>= 0.5)
(1214+384)/nrow(test2) # 0.9301513

pred2ROCRCART = prediction(pred2CART,test2$spam)
perf2ROCRCART = performance(pred2ROCRCART, "tpr", "fpr")
plot(perf2ROCRCART, colorize=TRUE)
# Compute AUC
performance(pred2ROCRCART, "auc")@y.values # 0.9582438

###
set.seed(123)
spam2RF = randomForest(spam ~., data=train2)
spam2RF

pred2RF = predict(spam2RF,newdata = test2,type="prob")[,2]
pred2RF
table(test2$spam,pred2RF >= 0.5)
(1296+383)/nrow(test2) # 0.9772992

pred2ROCRRf = prediction(pred2RF,test2$spam)
perf2ROCRRf = performance(pred2ROCRRf, "tpr", "fpr")
plot(perf2ROCRRf, colorize=TRUE)
# Compute AUC
performance(pred2ROCRRf, "auc")@y.values # 0.9980905


# Another source of information that might be extracted from text is the 
# frequency of various n-grams. An n-gram is a sequence of n consecutive words 
# in the document. For instance, for the document "Text analytics rocks!", 
# which we would preprocess to "text analyt rock", the 1-grams are "text", 
# "analyt", and "rock", the 2-grams are "text analyt" and "analyt rock", and 
# the only 3-gram is "text analyt rock". n-grams are order-specific, meaning 
# the 2-grams "text analyt" and "analyt text" are considered two separate 
# n-grams. We can see that so far our analysis has been extracting only 
# 1-grams.

# We do not have exercises in this class covering n-grams, but if you are 
# interested in learning more, the "RTextTools", "tau", "RWeka", and "textcat" 
# packages in R are all good resources.


