


ct = read.csv("clinical_trial.csv", stringsAsFactors = FALSE)

str(ct)
summary(ct)

max(nchar(ct$abstract))

table(nchar(ct$abstract) == 0)
sum(nchar(ct$abstract) == 0)

ct$title[which.min(nchar(ct$title))] # "A decade of letrozole: FACE."


#######
library(tm)

corpusTitle = VCorpus(VectorSource(ct$title))
corpusAbstract = VCorpus(VectorSource(ct$abstract))


corpusTitle = tm_map(corpusTitle, content_transformer(tolower))
corpusAbstract = tm_map(corpusAbstract, content_transformer(tolower))

# Remove punctuation
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)

corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))

# tm_map(corpus, removeWords, sw) with sw containing all the stopwords

# Stem document 
corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)

dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)

dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

str(dtmTitle)
str(dtmAbstract)

#ct$dtmAbstracttrial = as.factor(ct$trial)
View(dtmAbstract)

csAbstract = colSums(dtmAbstract)
which.max(csAbstract)

colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial = ct$trial
ncol(dtm)

######3
library(caTools)

set.seed(144)

split = sample.split(dtm$trial,0.7)

train = subset(dtm, split==TRUE)
test = subset(dtm, split==FALSE)

# Baseline model:
table(train$trial)
730/nrow(train) # 0.5606759

##################
library(rpart)
library(rpart.plot)

trialCART = rpart(trial~., data=train, method="class")
prp(trialCART)

predTrain = predict(trialCART)[,2]
summary(predTrain)


table(train$trial,predTrain >= 0.5)
(631+441)/nrow(train) # 0.8233487

# Sensitivity
441/(441+131) # 0.770979
# Specificity
631/(631+99) # 0.8643836

predTest = predict(trialCART,newdata = test)

table(test$trial,predTest[,2] >= 0.5)
(261+162)/nrow(test)


###########

library(ROCR)

predROCR = prediction(predTest[,2], test$trial)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)
# Compute AUC
performance(predROCR, "auc")@y.values

