# Hierarchical Clustering

# The hclust() function implements hierarchical clustering in R. In the following example we use 
# the data from Section 10.5.1 to plot the hierarchical clustering dendrogram using complete, single, 
# and average linkage clustering, with Euclidean distance as the dissimilarity measure. We begin by 
# clustering observations using complete linkage. The dist() function is used to compute the 50×50 
# inter-observation Euclidean distance matrix. 

set.seed(2)
x=matrix(rnorm (50*2), ncol=2) 

hc.complete=hclust(dist(x), method="complete")

# We could just as easily perform hierarchical clustering with average or single linkage instead:

hc.average=hclust(dist(x), method="average")
hc.single=hclust(dist(x), method="single")

# We can now plot the dendrograms obtained using the usual plot() function. The numbers at the bottom 
# of the plot identify each observation.

par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc.average, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single, main="Single Linkage", xlab="", sub="", cex=.9)

# To determine the cluster labels for each observation associated with a given cut of the dendrogram, 
# we can use the cutree() function:

cutree(hc.complete, 2) # 2 - the desired number of groups
cutree(hc.average, 2)
cutree(hc.single, 2)

# For this data, complete and average linkage generally separate the observations into their correct groups. 
# However, single linkage identi???es one point as belonging to its own cluster. A more sensible answer is obtained 
# when four clusters are selected, although there are still two singletons.

cutree(hc.single, 4)

table(cutree(hc.complete, 2))
table(cutree(hc.average, 2))
table(cutree(hc.single, 2))
table(cutree(hc.single, 4))
