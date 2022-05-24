shopping<-read.csv("CC GENERAL.csv", header = TRUE)
names(shopping)
head(shopping)
tail(shopping)
summary(shopping)
str(shopping)
nrow(shopping)
ncol(shopping)
dim(shopping)
install.packages("cluster")
library(cluster) 
plot(BALANCE~PURCHASES , data = shopping)

with(shopping,text(BALANCE ~ PURCHASES, labels= CUST_ID))

plot(BALANCE~ PURCHASES, data = shopping)
with(shopping,text(BALANCE ~ PURCHASES, labels= CUST_ID,pos=4,cex=.6))

normalise <- function(df)
{
  return(((df- min(df)) /(max(df)-min(df))*(1-0))+0)
}

head(shopping)


cUSTOMERID<-shopping[,1]
shopping_n<-shopping[,2:18]
shopping_n<-as.data.frame(lapply(shopping_n,normalise))
shopping_n$CUST_ID<-cUSTOMERID
shopping_n<-shopping_n[,c(18,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]

head(shopping_n)

distance <- dist(shopping_n,method = "euclidean",)
print(distance)
print(distance,digits=3)
install.packages("factoextra")
library(factoextra)
fviz_dist(distance)
head(shopping_n)

rownames(shopping_n)<-shopping_n$CUST_ID

shopping_n$CUST_ID<-NULL

head(shopping_n)

distance <- dist(shopping_n,method = "euclidean")
fviz_dist(distance)
shopping.hclust <- hclust(distance)
shopping.hclust

plot(shopping.hclust)
plot(shopping.hclust,hang=-1)
plot(shopping.hclust,labels=shopping$CUST_ID)

rect.hclust(shopping.hclust, 3)
plot(shopping.hclust,labels=shopping$CUST_ID)
rect.hclust(shopping.hclust, 4)


hclust.average <- hclust(distance, method = "average")
plot(hclust.average,labels=shopping$CUST_ID)
rect.hclust(hclust.average, 4)

hclust.centroid<- hclust(distance, method = "centroid")
plot(hclust.centroid,labels=shopping$CUST_ID)
rect.hclust(hclust.centroid, 4)


hclust.complete <- hclust(distance, method = "complete")
plot(hclust.complete,labels=shopping$CUST_ID)
rect.hclust(hclust.complete, 4)


member.centroid <- cutree(hclust.centroid,4)
member.centroid
member.complete <- cutree(hclust.complete,4)
member.complete
table(member.centroid,member.complete)


kc<-kmeans(shopping[,-1],3)
kc
clusplot(shopping, kc$cluster, color=TRUE, shade=TRUE, lines=0)
