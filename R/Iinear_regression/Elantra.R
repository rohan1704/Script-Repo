
# Read in the data
Elantra = read.csv("elantra.csv")
str(Elantra)
summary(Elantra)

elan_train = subset(Elantra, Year<= 2012)
elan_test = subset(Elantra, Year> 2012)

str(elan_train)
str(elan_test)
names(elan_train)

elan_sales = lm(ElantraSales ~ Unemployment + Queries + 
                  CPI_energy + CPI_all,data = elan_train)
summary(elan_sales)
# Multiple R-squared:  0.4282,	Adjusted R-squared:  0.3544 

# new model
elan_sales1 = lm(ElantraSales ~ Month + Unemployment + Queries + 
                  CPI_energy + CPI_all,data = elan_train)
summary(elan_sales1)
# Multiple R-squared:  0.4344,	Adjusted R-squared:  0.3402

# convert month to a factor in new variable
elan_train$MonthFac <- as.factor(elan_train$Month)
elan_test$MonthFac = as.factor(elan_test$Month)

ElantraLM = lm(ElantraSales ~ Unemployment + Queries + 
                 CPI_energy + CPI_all + MonthFac, data=elan_train)
summary(ElantraLM)

names(elan_train)

# find correlation
cor(subset(elan_train,select = Month:CPI_all))
cor(elan_train[c("Unemployment","Month","Queries","CPI_energy","CPI_all")])


# highest p-value (i.e., the least statistically significant variable)


ElantraLM = lm(ElantraSales ~ Unemployment + CPI_energy + CPI_all + MonthFac, data=elan_train)

pred_sale <- predict(ElantraLM,newdata = elan_test)
pred_sale

baseline = mean(elan_train$ElantraSales)
baseline


SSE = sum((pred_sale - elan_test$ElantraSales)^2)
SSE

SST = sum((baseline - elan_test$ElantraSales)^2)
SST

R2 = 1 - SSE/SST
R2

max(abs((pred_sale - elan_test$ElantraSales)))

which.max(abs((pred_sale - elan_test$ElantraSales)))

