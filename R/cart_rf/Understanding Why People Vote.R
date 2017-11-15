

# Read in data
gerber = read.csv("gerber.csv")
str(gerber)

# proportion of people in this dataset voted in this election
prop.table(table(gerber$voting))
# baseline 0.6841004
#         0         1 
# 0.6841004 0.3158996 

# Which of the four "treatment groups" had the largest percentage of people who 
# actually voted (voting = 1)
tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$neighbors, mean)
tapply(gerber$voting, gerber$self, mean)

# Exploration and Logistic Regression
log_reg = glm(voting ~ civicduty + hawthorne + self + neighbors, data = gerber,family = binomial)
summary(log_reg)

log_reg_pred = predict(log_reg,type = "response")

table(gerber$voting, log_reg_pred >= 0.3)
(134513+51966)/nrow(gerber) # 0.5419578

table(gerber$voting, log_reg_pred >= 0.5)
(235388)/nrow(gerber) # 0.6841004



library(ROCR)

ROCR_pred = prediction(log_reg_pred,gerber$voting)
ROCR_perf = performance(ROCR_pred,"tpr","fpr")
plot(ROCR_perf)
AUC = as.numeric(performance(ROCR_pred,"auc")@y.values)
AUC

# If we used method="class", CART would only split if one of the groups had a 
# probability of voting above 50% and the other had a probability of voting less 
# than 50% (since the predicted outcomes would be different). However, with 
# regression trees, CART will split even if both groups have probability less 
# than 50%.

CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2,digits = 6)

CARTmodel3 = rpart(voting ~ sex + civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel3)

# Trees can handle "nonlinear" relationships
CARTcontrol = rpart(voting ~ control, data=gerber, cp=0.0)

CARTsex = rpart(voting ~ control + sex, data=gerber, cp=0.0)

prp(CARTcontrol, digits=6)
abs(0.296638 - 0.34)

prp(CARTsex,digits=6)
abs(0.334176 - 0.345818)


#
LogModelSex = glm(voting ~ sex + control, data = gerber,family = binomial)
summary(LogModelSex)

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(LogModelSex, newdata=Possibilities, type="response")

abs(0.2908065 - 0.290456)


LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)

predict(LogModel2, newdata=Possibilities, type="response")

abs(0.2904558 - 0.290456)







