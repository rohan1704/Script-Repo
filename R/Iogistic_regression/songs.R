

# Read in data
song = read.csv("songs.csv")
str(song)
table(song$year)
table(song$artistname == "Michael Jackson")
# or
MichaelJackson = subset(song, artistname == "Michael Jackson")
str(MichaelJackson)
nrow(MichaelJackson)

summary(song)
names(song)

MJTop10 = subset(MichaelJackson,Top10 == 1)
MJTop10
# or
# MJTop10 = subset(MichaelJackson[c("songtitle", "Top10")],Top10 == 1)

table(song$timesignature)


song$songtitle[which.max(song$tempo)] # Wanna Be Startin' Somethin'


SongsTrain = subset(song,song$year <= 2009)
SongsTest = subset(song,song$year > 2009)

str(SongsTrain)
str(SongsTest)


# Create model
# remove non var

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")

# colname in nonvar should not be in songstrain
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]

SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]


names(SongsTrain)
Model1 = glm(Top10 ~ .,data = SongsTrain,family = binomial)
summary(Model1) # AIC: 4827.2

cor(SongsTrain$loudness,SongsTrain$energy)


SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
# We just subtracted the variable loudness. We couldn't do this with the 
# variables "songtitle" and "artistname", because they are not numeric 
# variables, and we might get different values in the test set that the training 
# set has never seen. But this approach (subtracting the variable from the model 
# formula) will always work when you want to remove numeric variables.

summary(SongsLog2) # AIC: 4937.8



SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3) # AIC: 4848.7

pred_1 = predict(SongsLog3, type="response",newdata = SongsTest)

table(SongsTest$Top10,pred_1 >= 0.55)

((309+19)/nrow(SongsTest))

Sensitivity = 19/(40+19)
Sensitivity


Specificity = 309/(309+5)
Specificity


table(SongsTest$Top10)

314/(314+59)


ROCRpred = prediction(pred_1, SongsTest$Top10)
as.numeric(performance(ROCRpred, "auc")@y.values)

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperf)

# Add colors
plot(ROCRperf, colorize=TRUE)

# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
