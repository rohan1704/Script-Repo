
# Read in data
baseball = read.csv("baseball.csv")
str(baseball)
summary(baseball)
View(baseball)

# Subset to only include moneyball years
moneyball = subset(baseball, Year < 2002)
str(moneyball)

# Compute Run Difference
moneyball$RD = moneyball$RS - moneyball$RA
str(moneyball)

# Scatterplot to check for linear relationship
cor(moneyball$RD, moneyball$W) # 0.938515
plot(moneyball$RD, moneyball$W)

# Regression model to predict wins
WinsReg = lm(W ~ RD, data=moneyball)
summary(WinsReg)


###########
# Win = 80.881375 + 0.105766(Run Diff)
# Win >= 95
# Run Diff >= (95 - 80.881375) / 0.105766
# if Run Diff is >= 133.4893 then Win is >= 95
###########


str(moneyball)

# Regression model to predict runs scored
RunsReg = lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(RunsReg)

RunsReg = lm(RS ~ OBP + SLG, data=moneyball)
summary(RunsReg)

RunsAll = lm(RA ~ OOBP + OSLG, data=moneyball)
summary(RunsAll)
###########
# Runs Scored = -804.63 + 2737.77(OBP) + 1584.91(SLG)
# RSquare = 0.91

# Runs Allowed = -837.38 + 2913.60(OOBP) + 1514.29(OSLG)
# RSquare = 0.90
###########
cor(moneyball$RS,moneyball$RA)

##########################################################

teamRank = c(1,2,3,3,4,4,4,4,5,5)
teamName = c("San Francisco Giants","Detroit Tigers","New York Yankees",
             "St. Louis Cardinals","Baltimore Orioles","Oakland A's",
             "Washington Nationals","Cincinnati Reds","Texas Rangers",
             "Atlanta Braves")
wins2012 = c(94,88,95,88,93,94,98,97,93,94)
wins2013 = c(97,97,92,93,92,96,94,96,92,90)

cor(teamRank,wins2012)
cor(teamRank,wins2013)

stat2012 = data.frame(teamRank,teamName,wins2012)
names(stat2012) <- c("Rank","Team Name","Wins")
stat2012

cor(stat2012$Rank,stat2012$Wins)
##########################################################
