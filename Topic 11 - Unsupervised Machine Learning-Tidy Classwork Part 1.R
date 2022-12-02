###########################################
# Title: Unsupervised Learning Models - kmeans/hierarchical
# Script Name: Unsupervised
# Name: Youngmin Lee
# Date: <your input>
# Script purpose:
# 
###########################################

### Install required packages if not installed
#install.packages("tidyverse")
#install.packages("tidyclust")
#install.packages("factoextra")

### Load the packages
library(tidyverse)      # For tidy tools
library(tidyclust)  
library(factoextra)
######################  Learning about k-means ######################
# Load dataset for analysis
data("USArrests")
df_arrest <- USArrests

# Initial Data Analysis (IDA) for arrest data
glimpse(df_arrest)
summary(df_arrest)

#Look for missing values
sapply(df_arrest, 
       function(x) sum(is.na(x)))

# Check column means and standard deviations
apply(df_arrest,2,mean) # 2 = columns, 1 = rows
apply(df_arrest,2,sd)

# Scale the data
mx_arrest_scaled <- scale(df_arrest, center = TRUE, scale = TRUE)
df_arrest_scaled <- as_tibble(mx_arrest_scaled, rownames = NA)
summary(df_arrest_scaled)

# Determine best number of clusters
  # check the ppt p. 10
# how tightly group
fviz_nbclust(df_arrest_scaled, kmeans, method = "wss") # elbow when k=2
fviz_nbclust(df_arrest_scaled, kmeans, method = "gap_stat")
# how tightly grouped + how far away from each cluster point
fviz_nbclust(df_arrest_scaled, kmeans, method = "silhouette") 
    # The graph shows that k=2 is the best choice, but there is no ANSWER.

# Create a k-means model on arrest data
set.seed(1234)  # For reproducible results

# Create a k-means model specification
  # the most optimized num_cluster according to the fviz_nbclust, but just put 4
kmeans_arrest_spec <- k_means(num_clusters = 4) %>%
  set_engine("stats") # or 'ClusterR' (has more parameters)

# Create a k-means model using the specification
kmeans_arrest_fit <- kmeans_arrest_spec %>%
  fit(~., data = df_arrest_scaled)

# Check model output
print(kmeans_arrest_fit) # K-means clustering with 4 clusters of sizes 13, 13, 8, 16
tidy(kmeans_arrest_fit)
glance(kmeans_arrest_fit)

kmeans_arrest_fit$fit
kmeans_arrest_fit$fit$totss
kmeans_arrest_fit$fit$size

# Append cluster number to each observation
df_arrest_km <- df_arrest %>% # NOT SCALED but ORIGINAL
  mutate(cluster = 
           extract_cluster_assignment(kmeans_arrest_fit)$.cluster)
df_arrest_km
# calculate average values for clusters
df_arrest_km %>% 
  group_by(cluster) %>% # NOT SCALED but ORIGINAL
  summarise(mean(Murder),mean(Assault), 
            mean(UrbanPop), mean(Rape))

# Visualize clusters 
fviz_cluster(kmeans_arrest_fit$fit, data = df_arrest_scaled,
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal())

# Create predictions from model
predict(kmeans_arrest_fit, df_arrest_scaled[1:4, ]) # first 4 rows with all columns
# classficication = supervised, 
# clustering = unsupervised


# for k menas with BaseR, use 'kmeans'
# ppt p. 14

#################### Hierarchical Clustering ########################
# For reproducible results
set.seed(1234)  

# Create the TidyClust specification
hc_arrest_spec <- 
  hier_clust(mode = "partition", engine = "stats",
             num_clusters = 4, cut_height = NULL,
             linkage_method = "complete")

# Run the model using the scaled data
hc_arrest_fit <- hc_arrest_spec %>%
  fit(~., data = df_arrest_scaled)

# Extract model details
hc_arrest_fit  %>% extract_fit_summary()

# Plot the dendrogram
plot(hc_arrest_fit$fit, labels = rownames(df_arrest))
abline(h=4, col = "blue")

# Add the cluster assignment to the dataset
df_arrest_hc <- df_arrest %>% 
  mutate(cluster = extract_cluster_assignment(hc_arrest_fit)$.cluster)

# make predictions
predict(hc_arrest_fit, df_arrest_scaled[1:4, ])

# Compare the two methods of clustering
table(df_arrest_km$cluster,df_arrest_hc$cluster)

# calculate average values for clusters
df_arrest_hc %>%
  group_by(cluster) %>%
  summarise(mean(Murder),mean(Assault),
            mean(UrbanPop), mean(Rape))

# Using NbClust to determine best number of clusters
# ppt p.32
library(NbClust)
# nc = number of clusters
NbClust(df_arrest_scaled, distance = "euclidean",
        min.nc = 2, max.nc = 5, 
        method = "complete", index = "alllong")
# 2 cluster is the best, 4 is the second best according to the result

# Using cluster::agnes() to find agglomerative coefficient
#define linkage methods
link_methods <- c( "average", "single", "complete", "ward")

#function to compute agglomerative coefficient
agg_coef <- function(x) {
  cluster::agnes(df_arrest_scaled, method = x)$ac
}

#calculate agglomerative coefficient for each clustering linkage method
# HIGHER THE VALUE IS THE BETTER : WARD!!
sapply(link_methods, agg_coef)



############################### Strait R ################
# create r model
hc_arrest_model_base <- hclust(dist(df_arrest_scaled), method = 'complete')

# plot the dendrogram
plot(hc_arrest_model_base)
abline(h = 4, col = 'blue')

# cut the tree to our linking 
cluster_nbr <- cutree(hc_arrest_model_base, h = 4)


# add the cluster numbe ro the dataset
df_arrest_hc_base <- df_arrest %>% 
  mutate(cluster = cluster_nbr)
  
# calculate average values for clusters
df_arrest_hc_base %>% 
  group_by(cluster) %>% 
  summarise(mean(Murder), mean(Assault), mean(UrbanPop), mean(Rape)) 
  


##### Extra credit; make sures to write comments to justification, WHY did I use these methods?

########################  PCA  ####################################
# Perform scaled PCA:
df_arrest_pca <- prcomp(x = df_arrest,                  
                        scale = TRUE,
                        center = TRUE)

# Inspect model output
summary(df_arrest_pca)

# Compute variance
pca_variance <- df_arrest_pca$dev ^ 2

# Proportion of variance for a scree plot
pca_variance_prop <- pca_variance / sum(pca_variance)

# Plot variance explained for each principal component
plot(pca_variance_prop, xlab = "principal component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1), type = "b",
     main = "Scree Plot")

# Plot the cumulative proportion of variance explained
plot(cumsum(pca_variance_prop),
     xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")

# see the elements in the model output
names(df_arrest_pca)

#reverse the signs of the loadings and scores
df_arrest_pca$rotation  <- -1 * df_arrest_pca$rotation
df_arrest_pca$x <- -1*df_arrest_pca$x

# Review the variable loadings
df_arrest_pca$... 

# Review the 
df_arrest_pca$...

# Create the biplot with ggbiplot
library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)


# x : PC1, y = PC2
ggbiplot(..., labels=rownames(df_arrest))

biplot(..., scale = 0)

###################################################################