

daily = read.csv("dailykos.csv")

str(daily)

summary(daily)

# Calculate distance
distances = dist(daily, method = "euclidean")

# Hierarchical clustering
clusterDaily = hclust(distances, method = "ward.D")

# plot dendrogram
plot(clusterDaily)

# Select 7 clusters
rect.hclust(clusterDaily, k = 7, border = "red")

DailyClust = cutree(clusterDaily, k = 7)
DailyClust

HClust1 = subset(daily, DailyClust == 1)

HClust2 = subset(daily, DailyClust == 2)

HClust3 = subset(daily, DailyClust == 3)

HClust4 = subset(daily, DailyClust == 4)

HClust5 = subset(daily, DailyClust == 5)

HClust6 = subset(daily, DailyClust == 6)

HClust7 = subset(daily, DailyClust == 7)

nrow(HClust1)
nrow(HClust2)
nrow(HClust3)
nrow(HClust4)
nrow(HClust5)
nrow(HClust6)
nrow(HClust7)

# Alt
# table(DailyClust)

# The colMeans function computes the column (word) means, 
# the sort function orders the words in increasing order of the mean values, and 
# the tail function outputs the last 6 words listed, which are the ones with the 
# largest column means.

tail(sort(colMeans(HClust1)))

tail(sort(colMeans(HClust2)))

tail(sort(colMeans(HClust3)))

tail(sort(colMeans(HClust4)))

tail(sort(colMeans(HClust5)))

tail(sort(colMeans(HClust6)))

tail(sort(colMeans(HClust7)))


# k-means custering

k = 7

# Run k-means
set.seed(1000)
daily_KMC = kmeans(daily, centers = k)
str(daily_KMC)

# Extract clusters
Dail_Clust = daily_KMC$cluster

KClust1 = subset(daily, Dail_Clust == 1)

KClust2 = subset(daily, Dail_Clust == 2)

KClust3 = subset(daily, Dail_Clust == 3)

KClust4 = subset(daily, Dail_Clust == 4)

KClust5 = subset(daily, Dail_Clust == 5)

KClust6 = subset(daily, Dail_Clust == 6)

KClust7 = subset(daily, Dail_Clust == 7)

nrow(KClust1) # 146
nrow(KClust2) # 144
nrow(KClust3) # 277
nrow(KClust4) # 2063
nrow(KClust5) # 163
nrow(KClust6) # 329
nrow(KClust7) # 308


tail(sort(colSums(KClust1)))
tail(sort(colSums(KClust2)))
tail(sort(colSums(KClust3)))
tail(sort(colSums(KClust4)))
tail(sort(colSums(KClust5)))
tail(sort(colSums(KClust6)))
tail(sort(colSums(KClust7)))

# Hierarchical Cluster best corresponds to K-Means Cluster 2
table(DailyClust, daily_KMC$cluster)

