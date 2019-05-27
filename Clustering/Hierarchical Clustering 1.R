rm(list=ls())
par(mfrow=c(1,1))
library(datasets)

data <- iris

head(data)

set.seed(20)

clusters <- hclust(dist(data[, 3:4]))
plot(clusters)
rect.hclust(clusters, h = 3) # use k for number of clusters

# We can see from the figure that the best choices for total number of clusters are either 3 or 4:

# To do this, we can cut off the tree at the desired number of clusters using cutree.

clusterCut <- cutree(clusters, 3)
clusterCut
table(clusterCut)

# > table(clusterCut)

# clusterCut
# 1  2  3 
# 50 71 29 

table(clusterCut, iris$Species)

# clusterCut setosa versicolor virginica
#          1     50          0         0
#          2      0         21        50
#          3      0         29         0

# It looks like the algorithm successfully classified all the 
# flowers of species setosa into cluster 1, and virginica into 
# cluster 2, but had trouble with versicolor. 

library(ggplot2)

ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

# Let us see if we can better by using a different linkage method. This time, we will use the mean linkage method:
  
clusters <- hclust(dist(iris[, 3:4]), method = 'average')
plot(clusters)
rect.hclust(clusters, h = 3)

# We can see that the two best choices for number of clusters are either 3 or 5.
# Let us use cutree to bring it down to 3 clusters.

clusterCut <- cutree(clusters, 3)
table(clusterCut)
table(clusterCut, iris$Species)

# We can see that this time, the algorithm did a much better job of 
# clustering the data, only going wrong with 6 of the data points.

# We can plot it as follows to compare it with the original data:
  
ggplot(iris, aes(Petal.Length, Petal.Width, color = iris$Species)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = clusterCut) + 
  scale_color_manual(values = c('black', 'red', 'green'))

# All the points where the inner color doesn't match the outer color are the 
# ones which were clustered incorrectly.

####################################################
rm(list = ls())
## Heirarchical clustering
## For heirarchical clustering , we are going to use the mtcars dataset

# Look at the column names
head(mtcars)
colnames(mtcars)
# Find distances

mtcars <- scale(mtcars)
head(mtcars)
mtcars[1:5]
cars.dist <- dist(mtcars)
cars.dist[1:5]

# # create distance matrix
# cars.dist <- as.matrix(cars.dist)

# use hclust function
clusters <- hclust(cars.dist)

# create dendogram
plot(clusters)

# apply rectangles at specified height
rect.hclust(clusters, h = 4)

###

# use hclust function
clusters <- hclust(cars.dist)

# create dendogram
plot(clusters)

# apply rectangles at specified height
rect.hclust(clusters, h = 5.5)

# use hclust function
clusters <- hclust(cars.dist)

# create dendogram
plot(clusters)

# apply rectangles at specified height
rect.hclust(clusters, h = 6)

# install "cluster" package to use agnes function
# install.packages("cluster")

library(cluster)

cluster.agnes <- agnes(mtcars)

plot(cluster.agnes)
# apply rectangles at specified height
rect.hclust(clusters, h = 4)

cluster.agnes <- agnes(mtcars, method = 'complete')
plot(cluster.agnes)
# apply rectangles at specified height
rect.hclust(clusters, h = 4)
