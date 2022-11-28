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
