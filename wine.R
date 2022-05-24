wineclustering<-read.csv("wine-clustering.csv", header = TRUE)

names(wineclustering)
head(wineclustering)
tail(wineclustering)
summary(wineclustering)
str(wineclustering)
nrow(wineclustering)
ncol(wineclustering)
dim(wineclustering)

install.packages("cluster")
library(cluster) 
plot(Alcohol~Ash , data = wineclustering)
with(wineclustering,text(Alcohol ~ Ash,pos=4,cex=.6))

normalise <- function(df)
{
  return(((df- min(df)) /(max(df)-min(df))*(1-0))+0)
}

head(wineclustering)



wineclustering_n<-wineclustering[,1:13]
wineclustering_n<-as.data.frame(lapply(wineclustering_n,normalise))

head(wineclustering_n)

distance <- dist(wineclustering_n,method = "euclidean",)
print(distance)
print(distance,digits=3)
install.packages("factoextra")
library(factoextra)
fviz_dist(distance)
head(wineclustering_n)


distance <- dist(wineclustering_n,method = "euclidean")
fviz_dist(distance)
wineclustering.hclust <- hclust(distance)
wineclustering.hclust

plot(wineclustering.hclust)
plot(wineclustering.hclust,hang=-1)
plot(wineclustering.hclust,labels=wineclustering[,1])

rect.hclust(wineclustering.hclust, 3)
plot(wineclustering.hclust,labels=wineclustering[,1])
rect.hclust(wineclustering.hclust, 4)


hclust.average <- hclust(distance, method = "average")
plot(hclust.average,labels=wineclustering[,1])
rect.hclust(hclust.average, 4)

hclust.centroid<- hclust(distance, method = "centroid")
plot(hclust.centroid,labels=wineclustering[,1])
rect.hclust(hclust.centroid, 4)


hclust.complete <- hclust(distance, method = "complete")
plot(hclust.complete,labels=wineclustering[,1])
rect.hclust(hclust.complete, 4)


member.centroid <- cutree(hclust.centroid,4)
member.centroid
member.complete <- cutree(hclust.complete,4)
member.complete
table(member.centroid,member.complete)


kc<-kmeans(wineclustering[,-1],3)
kc
clusplot(wineclustering, kc$cluster, color=TRUE, shade=TRUE, lines=0)

