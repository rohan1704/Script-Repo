
# Read in the data
pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")

str(pisaTrain)
View(pisaTrain)

tapply(pisaTrain$readingScore,pisaTrain$male,mean)


summary(pisaTrain)

# remove NA value from training and testing set
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

str(pisaTrain)
str(pisaTest)

# To include unordered factors in a linear regression model, 
# we define one level as the "reference level" and add a binary variable for 
# each of the remaining levels.
# As an example, consider the unordered factor variable "color", 
# with levels "red", "green", and "blue". If "green" were the reference level, 
# then we would add binary variables "colorred" and "colorblue" to a 
# linear regression problem. 
# All red examples would have colorred=1 and colorblue=0. 
# All blue examples would have colorred=0 and colorblue=1. 
# All green examples would have colorred=0 and colorblue=0.

pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmScore = lm(readingScore ~ .,data = pisaTrain)
summary(lmScore)

SSE = sum(lmScore$residuals^2)
SSE
RMSE = sqrt(SSE/nrow(pisaTrain))
RMSE

(29.542707 * 11) - (29.542707 * 9)

step_lm = step(lmScore)
summary(step_lm)

predTest = predict(lmScore,newdata = pisaTest)
summary(predTest)

SSE = sum((predTest - pisaTest$readingScore)^2)
SSE
RMSE = sqrt(SSE/nrow(pisaTest))
RMSE

# shortcut
sqrt(mean((predTest-pisaTest$readingScore)^2))

baseline = mean(pisaTrain$readingScore)
baseline

SST = sum((baseline-pisaTest$readingScore)^2)
SST

R2 = 1 - SSE/SST
R2



