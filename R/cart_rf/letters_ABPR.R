


# Read in data
letters = read.csv("letters_ABPR.csv")
str(letters)
letters$isB = as.factor(letters$letter == "B")

library(caTools)

set.seed(1000)
split = sample.split(letters$isB,SplitRatio = 0.50)
split

Train = subset(letters,split == TRUE)
Test = subset(letters,split == FALSE)

# Baseline
table(Test$isB)
1175/nrow(Test) # 0.754172

# Predicting B or not B CART
set.seed(1000)
CARTb = rpart(isB ~ . - letter, data=Train, method="class")
CARTp = predict(CARTb,newdata = Test,type = "class")
table(Test$isB,CARTp)

# accuracy
(1118+340)/nrow(Test) # 0.9358151

# Predicting B or not B RF
set.seed(1000)
# RFb = randomForest(isB ~ xbox + ybox + width + height + onpix + xbar + ybar + x2bar + y2bar + xybar + x2ybar + xy2bar + xedge + xedgeycor + yedge + yedgexcor, data=train)
RFb = randomForest(isB ~ . - letter, data=Train)
RFp = predict(RFb,newdata = Test,type = "class")
table(Test$isB,RFp)

# accuracy
(1165+374)/nrow(Test) # 0.9878049


# Predicting the letters A, B, P, R
# IMP

letters$letter = as.factor(letters$letter)
set.seed(2000)
split = sample.split(letters$letter,SplitRatio = 0.50)
Train = subset(letters,split == TRUE)
Test = subset(letters,split == FALSE)

# baseline accuracy on the testing set
table(Test$letter)

401/nrow(Test) # 0.2573813

# classification tree model
set.seed(2000)
CARTball = rpart(letter ~ . -isB, data=Train, method="class")
CARTpall = predict(CARTball,newdata = Test,type = "class")
table(Test$letter,CARTpall)

# accuracy
(348+318+363+340)/nrow(Test) # 0.8786906

# random forest model
set.seed(1000)
RFball = randomForest(letter ~ . - isB, data=Train)
RFpall = predict(RFball,newdata = Test,type = "class")
table(Test$letter,RFpall)

# accuracy
(390+380+393+364)/nrow(Test) # 0.9801027




