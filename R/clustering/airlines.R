

library(caret)

airlines = read.csv("AirlinesCluster.csv")

str(airlines)

summary(airlines)

# The first command pre-processes the data, and 
# the second command performs the normalization.
preproc = preProcess(airlines)
preproc

airlinesNorm = predict(preproc, airlines)
airlinesNorm

summary(airlinesNorm)

# standard deviation
sd(airlinesNorm$Balance)
sd(airlinesNorm$QualMiles)


# Calculate distance
dist_air = dist(airlinesNorm, method = "euclidean")

# Hierarchical clustering
clust_air = hclust(dist_air, method = "ward.D")

# plot dendrogram
plot(clust_air)

# Select 5 clusters
rect.hclust(clust_air, k = 5, border = "red")

# Divide the data points into 5 clusters 
Air_Cl_5 = cutree(clust_air, k = 5)
table(Air_Cl_5)


Hcl1 = subset(airlinesNorm, Air_Cl_5 == "1")
nrow(Hcl1) # 776

Hcl2 = subset(airlinesNorm, Air_Cl_5 == "2")
nrow(Hcl2) # 519

Hcl3 = subset(airlinesNorm, Air_Cl_5 == "3")
nrow(Hcl3) # 494

Hcl4 = subset(airlinesNorm, Air_Cl_5 == "4")
nrow(Hcl4) # 868

Hcl5 = subset(airlinesNorm, Air_Cl_5 == "5")
nrow(Hcl5) # 1342

##
tapply(airlines$Balance,Air_Cl_5,mean)
tapply(airlines$QualMiles,Air_Cl_5,mean)
tapply(airlines$BonusMiles,Air_Cl_5,mean)
tapply(airlines$BonusTrans,Air_Cl_5,mean)
tapply(airlines$FlightMiles,Air_Cl_5,mean)
tapply(airlines$FlightTrans,Air_Cl_5,mean)
tapply(airlines$DaysSinceEnroll,Air_Cl_5,mean)


## k-means Clustering
k = 5
set.seed(88)
Clust_KMC = kmeans(airlinesNorm,centers = k,iter.max = 1000)

table(Clust_KMC$cluster)

# Extract clusters
Air_Clust = Clust_KMC$cluster

KClust1 = subset(airlinesNorm, Air_Clust == 1)

KClust2 = subset(airlinesNorm, Air_Clust == 2)

KClust3 = subset(airlinesNorm, Air_Clust == 3)

KClust4 = subset(airlinesNorm, Air_Clust == 4)

KClust5 = subset(airlinesNorm, Air_Clust == 5)


# Hierarchical Cluster best corresponds to K-Means Cluster 2
table(Air_Cl_5, Clust_KMC$cluster)

