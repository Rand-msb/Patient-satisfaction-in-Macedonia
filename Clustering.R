
dataset = read.csv('datasetsatisfaction.csv')
getwd()



#data preprocessing
#removing null values
is.na(dataset)
sum(is.na(dataset))
dataset = na.omit(dataset)
#data transformation ordering the data by switching the values of 2 and 4
dataset[dataset == 2]<-"not satisfied"
dataset[dataset == 4]<-"vary satisfied"
dataset[dataset =="not satisfied"]<- 4
dataset[dataset =="vary satisfied"]<- 2
dataset$satisfaction.in.RM[dataset$satisfaction.in.RM == 4] <- 2


#-------------------------------------------------------------------------------------------------------------------


#Clustering:
#K-medoid method
str(dataset)
set.seed(8953)
#preprocessing



data <- as.data.frame(apply(dataset, 2, as.numeric))
data<-scale(data[,-1])



str(data)
set.seed(8953)

library(factoextra)
library(cluster)
library(NbClust)

#------------------- 5 Clusters

#group into clusters
pam.result<- pam(data,5, metric = "euclidean", stand=FALSE)#change number of cluster here
plot(pam.result)

#cluster plot
fviz_cluster(pam.result, data = data)

require("cluster")
sil<-silhouette(pam.result$cluster,dist(data))
fviz_silhouette(sil)


#  NbClust Validation 
fviz_nbclust(data,pam,method="silhouette")+labs(subtitle="silhouette method")

fres.nbclust <- NbClust(data, distance="euclidean", min.nc =2, max.nc = 10, method="centroid", index="all")

#----------------- 3 cluster

pam.result<- pam(data,3, metric = "euclidean", stand=FALSE)#change number of cluster here
plot(pam.result)

#cluster plot
fviz_cluster(pam.result, data = data)

require("cluster")
sil<-silhouette(pam.result$cluster,dist(data))
fviz_silhouette(sil)


#  NbClust Validation 
fviz_nbclust(data,pam,method="silhouette")+labs(subtitle="silhouette method")

fres.nbclust <- NbClust(data, distance="euclidean", min.nc =2, max.nc = 10, method="centroid", index="all")

#----------------- 2 clusters

pam.result<- pam(data,2, metric = "euclidean", stand=FALSE)#change number of cluster here
plot(pam.result)

#cluster plot
fviz_cluster(pam.result, data = data)

require("cluster")
sil<-silhouette(pam.result$cluster,dist(data))
fviz_silhouette(sil)


#  NbClust Validation 
fviz_nbclust(data,pam,method="silhouette")+labs(subtitle="silhouette method")

fres.nbclust <- NbClust(data, distance="euclidean", min.nc =2, max.nc = 10, method="centroid", index="all")





#-----------------------------------------------

#Clustering: hirerichal
str(dataset)
set.seed(8953)
#preprocessing



data <- as.data.frame(apply(dataset, 2, as.numeric))
data<-scale(data[,-1])


#install.packages("factoextra")
#install.packages("cluster")
#install.packages("NbClust")
library(factoextra)
library(cluster)
library(NbClust)
set.seed(8953)


#------------------- 5 Clusters


# Hierarchical clustering statment
hc.cut<- hcut(data,k=5,hc_mehtod="complete")# we changed the clusters numbers here

# Visualize dendrogram
fviz_dend(hc.cut,rect=TRUE)
# Visualize clusters
fviz_cluster(hc.cut,ellipse.type="convex")
#Evaluation:
#Clustering Validation
################################################
#Get the optimal number of clusters 
# (a) fviz_nbclust using Silhouette method 

fviz_nbclust(data,hcut,method="silhouette")+labs(subtitle="silhouette method")
fviz_silhouette(hc.cut)

require("cluster")
sil<-silhouette(hc.cut$cluster,dist(data))
fviz_silhouette(sil)

# (b) NbClust Validation 
fres.nbclust<-NbClust(data,distance="euclidean",min.nc=2,max.nc=10,method="complete",index="all")


#------------------- 3 Clusters


library(factoextra)
library(cluster)
library(NbClust)
set.seed(8953)

# Hierarchical clustering statment
hc.cut<- hcut(data,k=3,hc_mehtod="complete")# we changed the clusters numbers here

# Visualize dendrogram
fviz_dend(hc.cut,rect=TRUE)
# Visualize clusters
fviz_cluster(hc.cut,ellipse.type="convex")
#Evaluation:
#Clustering Validation
################################################
#Get the optimal number of clusters 
# (a) fviz_nbclust using Silhouette method 

fviz_nbclust(data,hcut,method="silhouette")+labs(subtitle="silhouette method")
fviz_silhouette(hc.cut)

require("cluster")
sil<-silhouette(hc.cut$cluster,dist(data))
fviz_silhouette(sil)

# (b) NbClust Validation 
fres.nbclust<-NbClust(data,distance="euclidean",min.nc=2,max.nc=10,method="complete",index="all")


#------------------- 2 Clusters


library(factoextra)
library(cluster)
library(NbClust)
set.seed(8953)

# Hierarchical clustering statment
hc.cut<- hcut(data,k=2,hc_mehtod="complete")# we changed the clusters numbers here

# Visualize dendrogram
fviz_dend(hc.cut,rect=TRUE)
# Visualize clusters
fviz_cluster(hc.cut,ellipse.type="convex")
#Evaluation:
#Clustering Validation
################################################
#Get the optimal number of clusters 
# (a) fviz_nbclust using Silhouette method 

fviz_nbclust(data,hcut,method="silhouette")+labs(subtitle="silhouette method")
fviz_silhouette(hc.cut)

require("cluster")
sil<-silhouette(hc.cut$cluster,dist(data))
fviz_silhouette(sil)

# (b) NbClust Validation 
fres.nbclust<-NbClust(data,distance="euclidean",min.nc=2,max.nc=10,method="complete",index="all")







































