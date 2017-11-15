
# Read in data
loan = read.csv("loans.csv")
str(loan)

table(loan$not.fully.paid)
# 1533 loans were not paid, and 8045 were fully paid
1 - 8045/(nrow(loan))

summary(loan)
str(loan)

## fill missing value

missing = subset(loan, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))
View(missing)
nrow(missing)
table(missing$not.fully.paid)

library(mice)

set.seed(144)

vars.for.imputation = setdiff(names(loan), "not.fully.paid")

imputed = complete(mice(loan[vars.for.imputation]))

loan[vars.for.imputation] = imputed
# Explaination : 
# Imputation predicts missing variable values for a given observation using the 
# variable values that are reported. We called the imputation on a data frame 
# with the dependent variable not.fully.paid removed, so we predicted the 
# missing values using only other independent variables.

imputed = read.csv("loans_imputed.csv")

summary(loan)
summary(imputed)



library(caTools)
set.seed(144)
split = sample.split(imputed$not.fully.paid, SplitRatio = 0.7)
train = subset(imputed, split == TRUE)
test = subset(imputed, split == FALSE)

model2 = glm(not.fully.paid ~ .,family = binomial,data = train)
summary(model2)


a = (9.187 + (-0.009317 * 700))
a # logit of a

b = (9.187 + (-0.009317 * 710))
b # logit of b

exp(a) # odd of a 14.36939

exp(b) # odd of b 13.09107

14.36939 / 13.09107


test$predicted.risk = predict(model6,type = "response",newdata = test)

table(test$not.fully.paid,test$predicted.risk >= 0.5)
(2399+4)/(nrow(test)) # accuracy

table(train$not.fully.paid)
5632/(nrow(train)) # baseline


ROCRpred = prediction(test$predicted.risk, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

# a bivariate logistic regression model (aka a logistic regression model with a 
# single independent variable) that predicts the dependent variable 
# not.fully.paid using only the variable int.rate.

mod = glm(not.fully.paid ~ int.rate,data = train,family = binomial)
summary(mod)

cor(train$int.rate, train$fico)

pred.bivariate = predict(mod, newdata=test, type="response")
summary(pred.bivariate)
# According to the summary function, the maximum predicted probability of the 
# loan not being paid back is 0.4266, which means no loans would be flagged at 
# a logistic regression cutoff of 0.5.

ROCRpred = prediction(pred.bivariate, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)


# While the investment has value c * exp(rt) dollars after collecting interest
(10) * exp((6/100)*(3))

# the profit to the investor if the investment is paid back in full
# c * exp(rt) - c 

test$profit = exp(test$int.rate*3) - 1

test$profit[test$not.fully.paid == 1] = -1

summary(test$profit)
# max * 10

highInterest = subset(test, int.rate >= 0.15)
View(highInterest)
summary(highInterest$profit)

table(highInterest$not.fully.paid)
110/(327+110)

# we will determine the 100th smallest predicted probability of not paying in 
# full by sorting the predicted risks in increasing order and selecting the 
# 100th element of this sorted list. Find the highest predicted risk that we 
# will include by typing the following command into your R console
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]

selectedLoans = subset(highInterest,predicted.risk <= cutoff)
View(selectedLoans)

sum(selectedLoans$profit)

table(selectedLoans$not.fully.paid)


model2 = glm(not.fully.paid ~ .,family = binomial,data = train)
summary(model2)

# model2 AIC: 5523.2
# model3 AIC: 5521.2
# model4 AIC: 5519.3
# model5 AIC: 5518.1
# model6 AIC: 5517.8 (best)


model3 = glm(not.fully.paid ~ credit.policy + purpose + int.rate + installment + log.annual.inc + dti + fico + revol.bal + revol.util + inq.last.6mths + delinq.2yrs + pub.rec,family = binomial,data = train)
summary(model3)

model4 = glm(not.fully.paid ~ credit.policy + purpose + installment + log.annual.inc + dti + fico + revol.bal + revol.util + inq.last.6mths + delinq.2yrs + pub.rec,family = binomial,data = train)
summary(model4)

model5 = glm(not.fully.paid ~ credit.policy + purpose + installment + log.annual.inc + fico + revol.bal + revol.util + inq.last.6mths + delinq.2yrs + pub.rec,family = binomial,data = train)
summary(model5)

model6 = glm(not.fully.paid ~ credit.policy + purpose + installment + log.annual.inc + fico + revol.bal + revol.util + inq.last.6mths + pub.rec,family = binomial,data = train)
summary(model6)

names(loan)

cor(train$not.fully.paid,train$dti)
cor(train$revol.util,train$delinq.2yrs)


