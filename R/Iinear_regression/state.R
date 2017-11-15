data(state)
statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center,  state.division, state.name, state.region)
str(statedata)
plot(statedata$x,statedata$y)

# Which region of the US (West, North Central, South, or Northeast) has the 
# highest average high school graduation rate of all the states in the region
tapply(statedata$HS.Grad,statedata$state.region,mean)

# Murder rate by region
boxplot(statedata$Murder ~ statedata$state.region)

# outlier
outlier = subset(statedata,statedata$state.region == "Northeast" & statedata$Murder > 10)
View(outlier)

life_exp <- lm(Life.Exp ~ Population + Income + Illiteracy + 
                 Murder + HS.Grad + Frost + Area,data = statedata)
summary(life_exp)

plot(statedata$Income, statedata$Life.Exp)

# Experiment with removing independent variables from the original model. 
# Remember to use the significance of the coefficients to decide which variables 
# to remove (remove the one with the largest "p-value" first, or the one with 
# the "t value" closest to zero), and to remove them one at a time 
# (this is called "backwards variable selection"). 
# This is important due to multicollinearity issues - removing one insignificant 
# variable may make another previously insignificant variable become 
# significant.

life_exp1 <- lm(Life.Exp ~ Population + Murder + HS.Grad + Frost,data = statedata)
summary(life_exp1)


pre = predict(life_exp1,newdata = statedata)
sort(pre,decreasing = FALSE) # Alabama 68.48112
sort(pre,decreasing = TRUE) # Washington 72.68272

statedata$state.name[which.min(statedata$Life.Exp)] # South Carolina
statedata$state.name[which.max(statedata$Life.Exp)] # Hawaii

sort(abs(life_exp1$residuals))

sort(abs(statedata$Life.Exp - predict(life_exp1)))


