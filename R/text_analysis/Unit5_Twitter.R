# Unit 5 - Twitter

setwd("G:/Course/Analytics edge/UNIT-5 (Text Analysis)")
# VIDEO 5

# Read in the data

# Since we're working with text data here, we need one extra argument, which is
# stringsAsFactors=FALSE.

tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)

str(tweets)


# Create dependent variable

tweets$Negative = as.factor(tweets$Avg <= -1)

table(tweets$Negative)


# Install new packages

# install.packages("tm")
library(tm)
# install.packages("SnowballC")
library(SnowballC)


# Create corpus
# A corpus is a collection of documents.
# tm can create a corpus in many different ways,
# here used is VCorpus we can also use Corpus and VectorSource.

corpus = VCorpus(VectorSource(tweets$Tweet)) 

# Look at corpus
corpus
corpus[[1]]$content


# Convert to lower-case

corpus = tm_map(corpus, content_transformer(tolower))

corpus[[1]]$content

# Remove punctuation

corpus = tm_map(corpus, removePunctuation)

corpus[[1]]$content

# Look at stop words 
stopwords("english")[1:10]
length(stopwords("english")) # 174

# Remove stopwords and apple

corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
# tm_map(corpus, removeWords, sw) with sw containing all the stopwords

corpus[[1]]$content

# Stem document 

corpus = tm_map(corpus, stemDocument)

corpus[[1]]$content




# Video 6

# Create matrix
# DocumentTermMatrix that generates a matrix where the rows correspond to 
# documents, in our case tweets, and the columns correspond to words in those 
# tweets. The values in the matrix are the number of times that word appears in 
# each document.

frequencies = DocumentTermMatrix(corpus)

frequencies

# Look at matrix 

inspect(frequencies[1000:1005,505:515])

# Check for sparsity
# This data is what we call sparse. This means that there are many zeros in our 
# matrix.

findFreqTerms(frequencies, lowfreq=20)
findFreqTerms(frequencies, lowfreq=100)

# Remove sparse terms
# So out of the 3,289 words in our matrix, only 56 words appear at least 20 
# times in our tweets. This means that we probably have a lot of terms that will
# be pretty useless for prediction model. The number of terms is an issue 
# for two main reasons. One is computational. More terms means more independent 
# variables, which usually means it takes longer to build our models. The other 
# is in building models, as we mentioned before, the ratio of independent 
# variables to observations will affect how good the model will generalize. So 
# remove some terms that don't appear very often.

sparse = removeSparseTerms(frequencies, 0.995)
sparse

# Convert to a data frame

tweetsSparse = as.data.frame(as.matrix(sparse))


################################################
# Make all variable names R-friendly
# Since R struggles with variable names that start with a number, and if you 
# probably have some words that start with a number, run the make.names function 
# to make sure all of our words are appropriate variable names.

colnames(tweetsSparse) = make.names(colnames(tweetsSparse))
# You should do this each time you've built a data frame using text analytics.

# Add dependent variable
View(tweetsSparse)
tweetsSparse$Negative = tweets$Negative

# Split the data

library(caTools)

set.seed(123)

split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)

trainSparse = subset(tweetsSparse, split==TRUE)
testSparse = subset(tweetsSparse, split==FALSE)



# Video 7

# Build a CART model

library(rpart)
library(rpart.plot)

tweetCART = rpart(Negative ~ ., data=trainSparse, method="class")

prp(tweetCART)

# Evaluate the performance of the model
# Evaluate the numerical performance of our model by making predictions on the 
# test set.
predictCART = predict(tweetCART, newdata=testSparse, type="class")

table(testSparse$Negative, predictCART)

# Compute accuracy

(294+18)/(294+6+37+18)

# Baseline accuracy 

table(testSparse$Negative)

300/(300+55)


# Random forest model

library(randomForest)
set.seed(123)

tweetRF = randomForest(Negative ~ ., data=trainSparse)

# Make predictions:
predictRF = predict(tweetRF, newdata=testSparse)

table(testSparse$Negative, predictRF)

# Accuracy:
(293+21)/(293+7+34+21)

# Logistic regression model 

tweetLog = glm(Negative ~ ., data=trainSparse, family = "binomial")

predictions = predict(tweetLog, newdata=testSparse, type="response")

table(testSparse$Negative, predictions > 0.5)

# Confusion matrix
(253+33)/nrow(testSparse) # 0.8056338





