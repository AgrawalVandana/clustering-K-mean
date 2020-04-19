data <- read.csv("C:/Business Analytics/Winter/Multivariate/Assignment2/food.csv")
data

rownames(data)=data[,1]
data=data[,-1]
## Step 1 - Compute a distance matrix
D=dist(data)
D
# Step 2 - Run hclust()
hc<-hclust(D,"single")
summary(hc)

# Step 3 - Plot dendrogram
plot(hc)
# Step 4 - choose number of clusters
memb<-cutree(hc,k=4)
memb
# Step 5 - get clustser centers
cent<-NULL
for (k in 1:4){cent<-rbind(cent,colMeans(data[memb==k,,drop=FALSE]))}
cent  # same as Table 7.21

## Step 6 - Calculate sum of total SS . within SS for each cluster (compare to k-means below)

one=sum((data[memb==1,,drop=FALSE]-cent[1,])^2)
two=sum((data[memb==2,,drop=FALSE]-cent[2,])^2)
three=sum((data[memb==3,,drop=FALSE]-cent[3,])^2)
four=sum((data[memb==4,,drop=FALSE]-cent[4,])^2)

tss_single=one+two+three+four

data=scale(data)  ## should i standardize?  calories, protein, fat, calcium, iron

data

# Step 1 - Run kmeans and analyze clusters
cl=kmeans(data,4)  ## let's keep same number of clusters as before
cl
## notice the within sum of squares...should be lower than single linkage  (George Foreman GUARANTEE)

# Step 2 - Plot clusters

plot(data, col = cl$cluster)
points(cl$centers, col = 1:5, pch = 8)  ## this only does calories and protein
text(data,rownames(data),col=cl$cluster)

library(pca3d)
?pca3d
pca <- prcomp(data, scale.= TRUE )
pca2d( pca, group=cl$cluster )
?pca2d
pca2d( pca, group=cl$cluster,show.labels=TRUE )
pca3d( pca, group=cl$cluster,show.labels=TRUE )
