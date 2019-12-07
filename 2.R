library(plyr)
library(ggplot2)
library(cluster)
library(lattice)
library(graphics)
library(grid) 
library(gridExtra)
summary(iris)
colors <-c("red","green","blue")
pairs(iris[1:4],pch=21,bg=colors[unclass(iris$Species)])
kmdata<-iris[,1:4]
wss<- numeric(15) 
for (k in 1:15) 
  wss[k] <- sum(kmeans(kmdata, centers=k,nstart=25)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within Sum of Squares") 
km<-kmeans(kmdata,3)
km
table(iris$Species,km$cluster)
df = as.data.frame(iris[,0:4]) 
df$cluster = factor(km$cluster) 
centers=as.data.frame(km$centers)
g1 =ggplot(data=df, aes(x=Sepal.Length, y=Petal.Length, color=cluster ))+geom_point() + geom_point(data=centers,aes(x=Sepal.Length,y=Petal.Length, color=as.factor(c(1,2,3))),size=10, alpha=.3, show_guide=FALSE)
g1
g2 =ggplot(data=df, aes(x=Sepal.Length, y=Sepal.Width, color=cluster ))+geom_point() + geom_point(data=centers,aes(x=Sepal.Length, y=Sepal.Width, color=as.factor(c(1,2,3))),size=10, alpha=.3, show_guide=FALSE)
g2
g3 =ggplot(data=df, aes(x=Sepal.Length, y=Petal.Width, color=cluster ))+geom_point() + geom_point(data=centers,aes(x=Sepal.Length,y=Petal.Width, color=as.factor(c(1,2,3))),size=10, alpha=.3, show_guide=FALSE)
g3
g4 =ggplot(data=df, aes(x=Sepal.Width, y=Petal.Length, color=cluster ))+geom_point() + geom_point(data=centers,aes(x=Sepal.Width,y=Petal.Length, color=as.factor(c(1,2,3))),size=10, alpha=.3, show_guide=FALSE)
g4
g5 =ggplot(data=df, aes(x=Sepal.Width, y=Petal.Width, color=cluster ))+geom_point() + geom_point(data=centers,aes(x=Sepal.Width,y=Petal.width, color=as.factor(c(1,2,3))),size=10, alpha=.3, show_guide=FALSE)
g5
g6 =ggplot(data=df, aes(x=Sepal.Width, y=Sepal.Length, color=cluster ))+geom_point() + geom_point(data=centers,aes(x=Sepal.Width,y=Sepal.Length, color=as.factor(c(1,2,3))),size=10, alpha=.3, show_guide=FALSE)
g6

dim(iris)

idx<-sample(1:dim(iris)[1],40)
iris3<-iris[idx,-5]
#iris3
hc<-hclust(dist(iris3),method = "ave") 

plot(hc,hang=-1,labels=iris$Species[idx])  

rect.hclust(hc,k=3)  
groups<-cutree(hc,k=3)