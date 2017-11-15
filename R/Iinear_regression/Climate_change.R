setwd("G:/Course/Analytics edge")

# Read in the data
climate = read.csv("climate_change.csv")
View(climate)
str(climate)
names(climate)

# splitting the data into a training set and testing set
# training set <= 2006
# testing set > 2006

climate_train = subset(climate,Year <= 2006)
View(climate_train)

climate_test = subset(climate,Year > 2006)
View(climate_test)

# Building model
cli_reg = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = climate_train) 
summary(cli_reg)
# Multiple R-squared:  0.7509,	Adjusted R-squared:  0.7436

gases <- climate[c("CO2","CH4","N2O","CFC.11","CFC.12" )]
cor(gases)

cor(climate_train)

N2O_reg = lm( ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = climate_train) 
summary(cli_reg)

cli_reg_new = step(cli_reg)
summary(cli_reg_new)
# Multiple R-squared:  0.7508,	Adjusted R-squared:  0.7445

######

cli_predict <- predict(cli_reg_new,newdata = climate_test)

# Compute out-of-sample R^2
SSE = sum((cli_predict - climate_test$Temp)^2)
SST = sum((mean(climate_train$Temp) - climate_test$Temp)^2)
R2 = 1 - SSE/SST
R2

# Compute the RMSE
RMSE = sqrt(SSE/nrow(NBA_test))
RMSE



