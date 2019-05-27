rm(list = ls())


par(mfrow =c(1,1))


data <- iris

head(data)

set.seed(20)

clusters <- hclust(dist(data[,3:4]))

plot(clusters)

rect.hclust(clusters , h = 3)

clustersCut <- cutree(clusters ,3)

clustersCut


table(clustersCut , iris$Species)

library(ggplot2)

ggplot(iris , aes(Petal.Length , Petal.Width , color = Species)) + geom_point()

clusters <- hclust(dist(iris[,3:4] ), method = "average")

plot(clusters)

rect.hclust(clusters , h = 3)

clustersCut <- cutree( clusters, h = 3)
table(clustersCut)

table(clustersCut , iris$Species)

ggplot(iris , aes(Petal.Length , Petal.Width , color = iris$Species)) + 
  geom_point(alpha = 0.4 , size = 3.5) +geom_point(col = clustersCut)+
  scale_color_manual(values = c('black' , 'red' ,'green'))

































