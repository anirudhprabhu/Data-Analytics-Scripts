# author : Anirudh Prabhu
# email : prabha2@rpi.edu

###########################

#U Clustering

# Clustering Data and Visualizing it

library(readxl)
library(xlsx)
Bern_Data <- read.xlsx("~/Downloads/Bern data.xlsx",sheetName = 'Aug')

Bern_Data$ID<-paste(Bern_Data$AGAT.Date,Bern_Data$AGAT.Station,sep = '_')

# CLUSTERING

#Commence PAM Clustering !!!

# Calculating Gower Distance
library(cluster)
library(factoextra)
library(NbClust)
set.seed(1234)



print("Start gower dist calc")

gower_dist <- daisy(Bern_Data[,8:21],
                    metric = "gower")

print("finished gower dist calc")
gower_mat <- as.matrix(gower_dist)

# The actual PAM Clustering
print("Starting PAM Clus")

sil_width <- c()

for(i in 2:10){
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}

plot(sil_width,xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(sil_width)


pam_fit <- pam(gower_dist, diss = T, k = 3)
#kfit<-kmeans(ChondriteDBSubset, centers = 11, nstart=25)

Bern_Data$Cluster <- as.factor(pam_fit$clustering)

#rownames(ChondriteDBSubset) <- ChondriteDB$`Meteorite Name`

pam_fit$clusinfo

clusplot(pam_fit)

write.csv(x = a,file = "~/Desktop/Uranium_With_5Clusters.csv")


#################################

