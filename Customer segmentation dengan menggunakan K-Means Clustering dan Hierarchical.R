# Title       : Homework Machine Learning I
# Description : K-Means Clustering & Hierarchical Clustering
# Dataset     : https://github.com/arikunco/machinelearning/blob/master/dataset/online_retail_clean.csv
# Author      : Lambang Satrio Nuli Raharjo

# load library
library(DataExplorer) 

# read dataset
df_online_retail_clean = read.csv('https://raw.githubusercontent.com/arikunco/machinelearning/master/dataset/online_retail_clean.csv')

# display first 6 data
head(df_online_retail_clean)

# display summary of the data
# dapat dilihat bahwa data frequency memiliki outliar hingga nilai maksimal =  86
# dapat dilihat bahwa data monetary memiliki outliar dengan nilai minimum = -38970 dan maksimum = 10122.56
summary(df_online_retail_clean)

# check dataset dimension (column and row) -> 2375 rows and 4 columns
dim(df_online_retail_clean)

# check for missing value -> no missing value
plot_missing(df_online_retail_clean)

# cek frekuensi dari variabel frequency:
# dapat dilihat data tersebar < 20
hist(df_online_retail_clean$frequency)


# cek frekuensi dari variabel monetary:
# dapat dilihat data tersebar mayoritas 0 sampai 300
hist(df_online_retail_clean$monetary, breaks = 'FD', xlim = c(0,500))

hist(df_online_retail_clean$monetary, breaks = 'FD', xlim = c(0,400))

hist(df_online_retail_clean$monetary, breaks = 'FD', xlim = c(0,300))

str(as.numeric(df_online_retail_clean[,3]))

df_dataset = data.frame(df_online_retail_clean[,1:2],frequency = as.numeric(df_online_retail_clean[,3]),monetary = df_online_retail_clean[,4])

#membersihkan data
df_dataset = df_dataset[df_dataset$frequency < 20, ]
df_dataset = df_dataset[df_dataset$monetary > 0, ]
df_dataset = df_dataset[df_dataset$monetary < 300, ]

# cek summary data yang setelah dilakukan remove outliar
# terlihat bahwa distribusinya lebih baik
summary(df_dataset)



# Description : K-Means Clustering

# Cara menentukan K:
# 1. Scree plot
# 2. Melihat elbow
# 3. Jika dengan menambah cluster tidak mengurangi wss, 
#    itulah titik elbow (titik siku)

# Determine number of clusters
# Initialize total within sum of squares error: wss
wss = 0

# For 1 to 15 clusters centers
for (i in 1:15){
  km.out = kmeans(df_dataset[,2:4], centers = i, nstart = 20, iter.max = 10)
  # Save total within sum of squares to wss variable
  wss[i] = km.out$tot.withinss
}

# Plot total within sum of squares vs. number of clusters
# dari plot dapat disimpulkan k terbaik = 3.
# titik ini merupakan titik dimana garis mulai melandai
plot(1:15, wss, type = 'b',
     xlab = 'Number of Clusters',
     ylab = 'Within groups sum of squares')


# create the k-means model: km.out with 6 centers
# centers: number of cluster, 
# nstart: random start, 
# iter.max: Maximum number of iteration
km.out = kmeans(df_dataset[,2:4], center = 3, nstart = 10, iter.max = 10)

# inspect the result
summary(km.out)

# print the cluster membership component of the model
print(km.out$cluster)

# print the km.out object
print(km.out)

# scatter plot of df_online_retail_clean recency & frequency
plot(df_dataset[, 2:3], col = km.out$cluster, main = 'k-means with 3 clusters')

# scatter plot of df_online_retail_clean recency & monetary
plot(df_online_retail_clean[, c(2,4)], col = km.out$cluster, main = 'k-means with 3 clusters')

# scatter plot of df_online_retail_clean frequency & monetary
plot(df_online_retail_clean[, 3:4], col = km.out$cluster, main = 'k-means with 3 clusters')


plot(df_online_retail_clean[,2:4], col = km.out$cluster, main = 'k-means with 3 clusters')

km.out$centers



# Description : Hierarchical Clustering

# calculates similarity as Euclidean distance between observations
d = dist(df_dataset[, 2:4])

# returns hierarchical clustering model
hclust.out = hclust(d = d)

# show summary of hclust.out
summary(hclust.out)

# draws a dendogram
layout(1)
plot(hclust.out)
abline(h = 300, col = 'blue')

# cut by height h
cutree(hclust.out, h = 300)

# cut by number of clusters
cutree(hclust.out, k = 2)

# fitting hierarchical clustering model using differenct methods
hclust.complete = hclust(d, method = 'complete')
hclust.average = hclust(d, method = 'average')
hclust.single = hclust(d, method = 'single')

plot(hclust.complete)

# CHeck if scaling is necessary
colMeans(df_dataset[,2:4])
apply(df_dataset[,2:4], 2, sd)

# Produce new matrix with columns of mean of 0 and sd of 1
scaled_dataset = scale(df_dataset[,2:4]) # normalization scaling
df.scaled_dataset = data.frame(scaled_dataset) # convert to data frame
colMeans(scaled_dataset) # check means for each column
apply(scaled_dataset, 2, sd)
apply(scaled_dataset, 2, max)

hclust.out.scaled = hclust(dist(scaled_dataset))
df_dataset$label_scaled = cutree(hclust.out.scaled, k = 2)
df_dataset$label = cutree(hclust.out, k = 2)

plot(df_dataset[, 2:4], col = df_dataset$label,
     main = 'Hierarchical Cluster of Online Retail Dataset')

