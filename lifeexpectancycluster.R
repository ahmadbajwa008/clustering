clustering<-read.csv("life expectancy index.csv", header = TRUE)

names(clustering)
head(clustering)
tail(clustering)
summary(clustering)
str(clustering)
nrow(clustering)
ncol(clustering)
dim(clustering)

install.packages("cluster")
library(cluster) 
plot(clustering)

normalise <- function(df)
{
  return(((df- min(df)) /(max(df)-min(df))*(1-0))+0)
}

head(clustering)




kc<-kmeans(clustering[,-1],3)
kc
clusplot(clustering, kc$cluster, color=TRUE, shade=TRUE, lines=0)

