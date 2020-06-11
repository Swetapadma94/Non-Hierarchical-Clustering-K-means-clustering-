install.packages("plyr")
library(plyr)
install.packages("readxl")
library(readxl)
airline<-read_excel("E:\\data sets\\EastWestAirlines.xlsx")
View(airline)
normalized_data1<-scale(airline[,2:12])
View(normalized_data1)
d<-dist(normalized_data1,method="euclidean")
fit<-hclust(d,method = "complete")
plot(fit)
plot(fit,hang=-1)
rect.hclust(fit,k=3,border ="RED")
groups <- cutree(fit, k=3)
membership_1<-as.matrix(groups)
final<-data.frame(airline,membership)
View(final)

write.csv(final, file="final.csv",row.names = F)

aggregate(airline[,-1],by=list(final$groups),mean)
#### K means#######
km<-kmeans(airline, 5) #kmeans clustering
str(km)
install.packages("animation")
library(animation)

km<-kmeans.ani(airline,5)
km$centers

# elbow curve & k ~ sqrt(n/2) to decide the k value
# Determine number of clusters by scree-plot ccenters
wss = (nrow(normalized_data1)-1)*sum(apply(normalized_data1,2,var)) 
for (i in 2:12) wss[i] = sum(kmeans(normalized_data1, centers=i)$withinss)  
plot(1:12, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Screew-Plot")
### 3 Optimum clusters####
xcl <- clara(xds, 3, sample = 100)
clusplot(xcl)

#Partitioning around medoids
xpm <- pam(xds, 3)
clusplot(xpm)
 
### 3 Optimum clusters####
