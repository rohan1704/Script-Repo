


# Read in data
parole = read.csv("parole.csv")
str(parole)
# While the variables male, race, state, crime, and violator are all unordered 
# factors, only state and crime have at least 3 levels in this dataset.

str(parole$state)

parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
str(parole)

set.seed(144)

library(caTools)

split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

model1 = glm(violator ~ .,family = binomial,data = train)
summary(model1) # AIC: 277.48

# male, of white race, aged 50 years at prison release, from the state of 
# Maryland, served 3 months, had a maximum sentence of 12 months, did not 
# commit multiple offenses, and committed a larceny 

(-4.2411574)+(0.3869904*1)+(0.8867192*1)+(-0.0001756*50)+(-0.1238867*3)+(0.0802954*12)+(0)+(0.6837143*1)             
# -1.700629

# According to the model, twhat are the odds this individual is a violator?
exp(-1.700629)
# 0.1825687

# According to the model, what is the probability this individual is a violator?
1/(1+exp(-(-1.700629)))
# 0.1543831

# Explanation:
# From the logistic regression equation, we have 
# log(odds) = -4.2411574 + 0.3869904*male + 0.8867192*race - 0.0001756*age + 
# 0.4433007*state2 + 0.8349797*state3 - 3.3967878*state4 - 0.1238867*time.served 
# + 0.0802954*max.sentence + 1.6119919*multiple.offenses + 0.6837143*crime2 - 
# 0.2781054*crime3 - 0.0117627*crime4. This parolee has male=1, race=1, age=50, 
# state2=0, state3=0, state4=0, time.served=3, max.sentence=12, 
# multiple.offenses=0, crime2=1, crime3=0, crime4=0. We conclude that 
# log(odds) = -1.700629.
# Therefore, the odds ratio is exp(-1.700629) = 0.183, and the predicted 
# probability of violation is 1/(1+exp(1.700629)) = 0.154.


pred_1 = predict(model1,type = "response",newdata = test)
summary(pred_1)

table(test$violator,pred_1 >= 0.5)
#What is the model's sensitivity?
12/(12+11)
#What is the model's specificity?
167/(167+12)
#What is the model's accuracy?
(167+12)/(167+12+12+11)

# The accuracy of a simple model that predicts that every parolee 
# is a non-violator
table(test$violator)
179/nrow(test)

#library(ROCR)
ROCRpred = prediction(pred_1, test$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)
# AUC
# The probability the model can correctly differentiate between a randomly 
# selected parole violator and a randomly selected parole non-violator. correct

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperf)

# Add colors
plot(ROCRperf, colorize=TRUE)

# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))





