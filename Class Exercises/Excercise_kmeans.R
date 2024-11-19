cluster_data = read.csv('~/GitHub/Data180_Fall24/Class Exercises/cluster_data.csv')
plot(cluster_data)
kmeans_3 <- kmeans(cluster_data, centers = 3)
kmeans_4 <- kmeans(cluster_data, centers = 4)

plot(x2~x1, cluster_data, pch=15+kmeans_3$cluster, col = kmeans_3$cluster)
points(x2~x1, kmeans_3$centers, pch=10, col= 'blue')




plot(x2~x1, cluster_data, pch=15+kmeans_4$cluster, col = kmeans_4$cluster)
points(x2~x1, kmeans_4$centers, pch=10, col= 'blue')


#------------------------------------------------------------------------------

kmeans_3_50 <- kmeans(cluster_data, centers = 3, nstart = 50)
kmeans_4_50 <- kmeans(cluster_data, centers = 4, nstart = 50)

plot(x2~x1, cluster_data, pch=15+kmeans_3_50$cluster, col = kmeans_3_50$cluster)
points(x2~x1, kmeans_3_50$centers, pch=10, col= 'blue')




plot(x2~x1, cluster_data, pch=15+kmeans_4_50$cluster, col = kmeans_4_50$cluster)
points(x2~x1, kmeans_4_50$centers, pch=10, col= 'red')

#------------------------------------------------------------------------------


wgss <- c()
for (i in 1:9){
  wgss[i] <- kmeans(cluster_data, centers = i+1, nstart=50)$tot.withinss
}
plot(c(2:10), wgss, type='b', pch=16)

#------------------------------------------------------------------------------
chindex <- c()
for (i in 1:9){
  chindex[i] <- (kmeans(cluster_data, centers = i+1, nstart = 50)$betweenss/(i+1-1))/
    (kmeans(cluster_data, centers = i +1, nstart = 50)$tot.withinss/(nrow(cluster_data)+1))
}
plot(c(2:10), chindex, type='b', pch=19)
  
     