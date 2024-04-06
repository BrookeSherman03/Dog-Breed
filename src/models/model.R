#load in libraries
library(dplyr)
library(data.table)
library(caret)
library(Metrics)
library(Rtsne)
library(ClusterR)

data <- fread("./Dog-Breed/volume/data/raw/data.csv")

# keep samples in a column but remove from the table
samples <- data$id
data$id <- NULL

# do a pca
pca <- prcomp(data, scale. = TRUE) #scale it so it makes it more accurate for the rtsne

# look at the percent variance explained by each pca
screeplot(pca)

# look at the rotation of the variables on the PCs
pca

# see the values of the scree plot in a table 
summary(pca)

# see a biplot of the first 2 PCs
biplot(pca)

# use the unclass() function to get the data in PCA space
pca_dt <- data.table(unclass(pca)$x)

#run rtsne with pca data
tsne <- Rtsne(pca_dt, pca = F, perplexity = 65, check_duplicates = F)

# grab out the coordinates
tsne_dt <- data.table(tsne$Y)

ggplot(tsne_dt,aes(x = V1,y = V2)) + geom_point()

#do the 4 clusters
gmm_data <- GMM(tsne_dt[,.(V1,V2)], 4)

#make the data based off of likelihood for clusters by the values being as close to 1 as possible
l_clust <- gmm_data$Log_likelihood^10
l_clust <- data.table(l_clust)
net_lh <- apply(l_clust,1,FUN = function(x){sum(1/x)})
cluster_prob <- 1/l_clust/net_lh

tsne_dt$Cluster_1_prob <- cluster_prob$V1
tsne_dt$Cluster_2_prob <- cluster_prob$V2
tsne_dt$Cluster_3_prob <- cluster_prob$V3
tsne_dt$Cluster_4_prob <- cluster_prob$V4

ggplot(tsne_dt,aes(x = V1,y = V2,col = Cluster_2_prob)) + geom_point() #test out different cluster prob

#read the ids back into the table
cluster_prob$id <- samples
cluster_prob <- cluster_prob %>% relocate(id)

#assign the names correctly: sample_1 is breed 3, sample_5 is breed 2, sample_6 is breed 4
names(cluster_prob) <- c("id", "breed_1", "breed_2", "breed_4", "breed_3")

#create the submission file
fwrite(cluster_prob, "submission.csv")

pred <- predict(XGBm, newdata = dtest)
sub$ic50_Omicron <- pred
fwrite(sub, ".XGboost/volume/data/processed/sub.csv")
