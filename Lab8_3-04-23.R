#K Means Clustering 
library(ClusterR)
library(cluster)

df =iris[, -5]
head(df)

set.seed(240) # Setting seed
km = kmeans(df, centers = 3, nstart = 20)
km

km$cluster

cm <- table(iris$Species, km$cluster)
cm

plot(df[c("Sepal.Length", "Sepal.Width")],col = km$cluster, main = "K-means with 3 clusters")

km$centers[, c("Sepal.Length", "Sepal.Width")]

y_kmeans <- km$cluster
clusplot(iris[, c("Sepal.Length", "Sepal.Width")], y_kmeans,lines = 0,shade = TRUE,color = TRUE,labels = 2, plotchar = FALSE, span = TRUE, main = paste("Cluster iris"), xlab = 'Sepal.Length',ylab = 'Sepal.Width')
points(km$centers[, c("Sepal.Length", "Sepal.Width")], col = 1:3, pch = 8, cex = 3)


#K Medoid Clustering
library(factoextra)
df =iris[, -5]
head(df)

med <- pam(df, 3)
med

cm <- table(iris$Species, med$clustering)
cm

clusplot(iris[, c("Sepal.Length", "Sepal.Width")], med$clustering,lines = 0,shade = TRUE,color = TRUE,labels = 2, plotchar = FALSE, span = TRUE, main = paste("k medoids"), xlab = 'Sepal.Length',ylab = 'Sepal.Width')
points(med$medoids[, c("Sepal.Length", "Sepal.Width")], col = 1:3, pch = 8, cex = 3)


#Hierarchical Clustering
distance_mat <- dist(df, method = 'euclidean')
set.seed(240)  # Setting seed
Hierar_cl <- hclust(distance_mat, method = "average")
Hierar_cl

plot(Hierar_cl)
fit <- cutree(Hierar_cl, k = 3 )
fit
table(fit)
rect.hclust(Hierar_cl, k = 3, border = "red")

cm <- table(iris$Species, fit)
cm