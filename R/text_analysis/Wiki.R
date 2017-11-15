
wiki = read.csv("wiki.csv", stringsAsFactors = FALSE)
str(wiki)
summary(wiki)

wiki$Vandal = as.factor(wiki$Vandal)

table(wiki$Vandal)

corpusAdded = VCorpus(VectorSource(wiki$Added))

corpusAdded[[1]]$content

# remove stopwords
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
# stem documents
corpusAdded = tm_map(corpusAdded, stemDocument)

dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded

sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded

wordsAdded = as.data.frame(as.matrix(sparseAdded))
View(wordsAdded)

colnames(wordsAdded) = paste("A", colnames(wordsAdded))
View(wordsAdded)


###
corpusRemoved = VCorpus(VectorSource(wiki$Removed))

corpusRemoved[[2]]$content

# remove stopwords
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
# stem documents
corpusRemoved = tm_map(corpusRemoved, stemDocument)

dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved

sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved

wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
str(wordsRemoved)

colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
View(wordsRemoved)


wikiWords = cbind(wordsAdded, wordsRemoved)
# The cbind function combines two sets of variables for the same observations 
# into one data frame

wikiWords$Vandal = wiki$Vandal

library(caTools)

set.seed(123)

spl = sample.split(wikiWords$Vandal, 0.7)

train = subset(wikiWords, spl == TRUE)
test = subset(wikiWords, spl == FALSE)

# Baseline model:

table(test$Vandal)
618/nrow(test)
#  0.5313844


library(rpart)
library(rpart.plot)

wikiCART = rpart(Vandal ~., data=train, method="class")

prp(wikiCART)

pred = predict(wikiCART, newdata=test, type = "class")
pred
table(test$Vandal,pred)
(618+12)/nrow(test) # 0.5417025
# Although it beats the baseline, bag of words is not very predictive for this 
# problem

grepl("cat","dogs and cats",fixed=TRUE) # TRUE
grepl("cat","dogs and rats",fixed=TRUE) # FALSE

wikiWords2 = wikiWords

wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)

table(wikiWords2$HTTP)

train2 = subset(wikiWords2, spl==TRUE)

test2 = subset(wikiWords2, spl==FALSE)

wikiCART2 = rpart(Vandal ~ ., data=train2, method="class")

prp(wikiCART2)

pred = predict(wikiCART2, newdata=test2, type = "class")
pred

table(test2$Vandal,pred)
(609+57)/nrow(test2) # 0.5726569

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))

wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

mean(wikiWords2$NumWordsAdded)

######

train3 = subset(wikiWords2, spl==TRUE)

test3 = subset(wikiWords2, spl==FALSE)

wikiCART3 = rpart(Vandal ~ ., data=train3, method="class")

prp(wikiCART3)

pred = predict(wikiCART3, newdata=test3, type = "class")
pred

table(test3$Vandal,pred)
(514+248)/nrow(test3) # 0.6552021

wikiWords3 = wikiWords2

wikiWords3$Minor = wiki$Minor

wikiWords3$Loggedin = wiki$Loggedin


############
train4 = subset(wikiWords3, spl==TRUE)

test4 = subset(wikiWords3, spl==FALSE)

wikiCART4 = rpart(Vandal ~ ., data=train4, method="class")

prp(wikiCART4)

pred = predict(wikiCART4, newdata=test4, type = "class")
pred

table(test4$Vandal,pred)
(595+241)/nrow(test4) # 0.7188306















