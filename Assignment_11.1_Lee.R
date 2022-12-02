###########################################
# Title: Assignment_11.1_Lee.R
# Script Name: Unsupervised
# Name: Youngmin Lee
# Date: Dec 2 2022
# Script purpose: An assignment for extra credit
##############################

# libraries
library(tidyverse)
library(tidymodels)
library(factoextra)
library(tidyclust) # for k_means
library(NbClust)
# load data
df_wine <- read_csv('./dataset/Wine.csv')

df_wine



# check missing
sapply(df_wine, function(x) sum(is.na(x)))


df_wine
glimpse(df_wine)




# column mean and std
apply(df_wine, 2, mean)
apply(df_wine, 2, sd)


# scaling
mx_wine_scaled <- scale(df_wine, center = TRUE, scale = TRUE) # scale for min max sqr err
df_wine_scaled <- as_tibble(mx_wine_scaled, rownames = NA)

## It seems like every column ExCEPT customer segment are numeric
df_wine$Customer_Segment <- as.factor(df_wine$Customer_Segment)

# column mean and std for scaled
apply(df_wine_scaled, 2, mean)
apply(df_wine_scaled, 2, sd)
summary(df_wine_scaled)


# set the seed
set.seed(1996)

######### First Trial Modeling ##############################################
########################################################################
## NOTE : The goal of the clustering is to cluster the wines by their 
# Proanthocyanins and Flavanoids.
# Therefore, it is reasonable to use these two columns for the clustering
# But just in case, I will train the model with the whole columns first 
# and see what happens
######################################################################
# Find the best number of clusters
fviz_nbclust(df_wine_scaled, kmeans, method = 'wss')
fviz_nbclust(df_wine_scaled, kmeans, method = 'gap_stat')
fviz_nbclust(df_wine_scaled, kmeans, method = 'silhouette')


# All those three results above says the best number of clustering is 3


# kmeans model specification
kmeans_wine_spec <- k_means(num_clusters = 3) %>% 
  set_engine('stats') # or ClusterR

kmeans_wine_spec


# train kmeans model with the specification
kmeans_wine_fit <- kmeans_wine_spec %>% 
  fit(~., data = df_wine_scaled)


# Check model's output
kmeans_wine_fit
tidy(kmeans_wine_fit)
glance(kmeans_wine_fit)
kmeans_wine_fit$fit$size
# somehow, the values with engine 'stats' from glance are the same with 'ClusterR'


# Append cluster result to each observation(row)
df_wine_km <- df_wine %>% 
  mutate(cluster = extract_cluster_assignment(kmeans_wine_fit)$.cluster)

df_wine_km

# make the cluster column factor
df_wine_km$cluster <- as.factor(df_wine_km$cluster)
glimpse(df_wine_km)

# Mean & SD values by the clusters
df_wine_km %>% 
  group_by(cluster) %>% 
  summarise(mean(Proanthocyanins), mean(Flavanoids),
            sd(Proanthocyanins), sd(Flavanoids))

# Visualizing
fviz_cluster(kmeans_wine_fit$fit, data = df_wine_scaled,
             ellipse.type = 'euclid',
             star.plot = TRUE,
             repel = TRUE,
             ggtheme = theme_minimal())

################### Result & Note for Model 1 #################################
## Result $ Note: I don't think this model cannot cluster the wine by 
# Proanthocyanins and Flavanoids that much.

# cluster   `mean(Proanthocyanins)` `mean(Flavanoids)` `sd(Proanthocyanins)` `sd(Flavanoids)`
# <fct>                       <dbl>              <dbl>                 <dbl>            <dbl>
# 1 Cluster_1                    1.92              2.97                  0.442            0.401
# 2 Cluster_2                    1.60              2.07                  0.579            0.712
# 3 Cluster_3                    1.16              0.798                 0.410            0.313




######### Second Trial Modeling ##############################################
#######################################################################
## NOTE : It seems like the clustering is not done by the Proanthocyanins
# and Flavanoids.
# Therefore, I will create another model trained with those two columns only
#################################################################

# Extract the columns Proanthocyanins and Flavanoids only from the dataset
df_wine_2 <- df_wine %>% select(Proanthocyanins, Flavanoids)
df_wine_2

# Scaling
# Don't have to scaled again. because it will be the same if the column's data is unchanged
df_wine_2_scaled <- df_wine_scaled %>% select(Proanthocyanins, Flavanoids)
df_wine_2_scaled

# Find the best number of clusters with Proanthocyanins and Flavanoids.
fviz_nbclust(df_wine_2_scaled, kmeans, method = 'wss')
fviz_nbclust(df_wine_2_scaled, kmeans, method = 'gap_stat')
fviz_nbclust(df_wine_2_scaled, kmeans, method = 'silhouette')

# All tree results from the above says 2 is the best clustering number
# kmeans model specification
kmeans_wine_2_spec <- k_means(num_clusters = 2) %>% 
  set_engine('stats') # or ClusterR

kmeans_wine_2_spec

# train kmeans model with the specification
kmeans_wine_2_fit <- kmeans_wine_2_spec %>% 
  fit(~., data = df_wine_2_scaled)

# Check model's output
kmeans_wine_2_fit
tidy(kmeans_wine_2_fit)
glance(kmeans_wine_2_fit)
kmeans_wine_2_fit$fit$size
# somehow, the values with engine 'stats' from glance are the same with 'ClusterR'

# Append cluster result to each observation(row)
df_wine_2_km <- df_wine_2 %>% 
  mutate(cluster = extract_cluster_assignment(kmeans_wine_2_fit)$.cluster)

df_wine_2_km

# make the cluster column factor
df_wine_2_km$cluster <- as.factor(df_wine_2_km$cluster)
glimpse(df_wine_2_km)


# Mean & SD values by the clusters
df_wine_2_km %>% 
  group_by(cluster) %>% 
  summarise(mean(Proanthocyanins), mean(Flavanoids),
            sd(Proanthocyanins), sd(Flavanoids))

# Visualizing
fviz_cluster(kmeans_wine_2_fit$fit, data = df_wine_2_scaled,
             ellipse.type = 'euclid',
             star.plot = TRUE,
             repel = TRUE,
             ggtheme = theme_minimal())

# make predictions
predict(kmeans_wine_2_fit, df_wine_2_scaled[1:4, ])
df_wine_2_km[1:4, ]

####################################################################
######### Result & Note for Model 2 - Proanthocyanins & Flavanoids only #####
# Result & Note :
# This new model trained only with two columns what I want to cluster
# can perform better and more obvious than the previous model
# The standard deviation values is more even than the case of model 1, too.

# cluster   `mean(Proanthocyanins)` `mean(Flavanoids)` `sd(Proanthocyanins)` `sd(Flavanoids)`
# <fct>                       <dbl>              <dbl>                 <dbl>            <dbl>
# 1 Cluster_1                    1.98               2.81                 0.455            0.589
# 2 Cluster_2                    1.16               1.18                 0.329            0.564




###################################################
################# Hierarchical clustering? ##########################
#######################################################################
## NOTE : By using the function 'fviz_nbclust' with three methods,
# it is quite obvious to set the number of cluster = 2
# but just in case, let's try to use Hierarchical clustering
#################################################################

# linkage methods
link_methods <- c('average', 'single', 'complete', 'ward.D')

# According to the ppt slide p. 25 of the chapter 11 from the class,
# Complete & Averageis most commly used methods.
# But just in the case, let's check the agglomerative coefficient to check 
# which is the best method


# find the best number of clust with the library NbClust
n_clust <- function(x){
  NbClust(df_wine_2_scaled, distance = 'euclidean',
          min.nc = 2, max.nc = 5,
          method = x, index = 'alllong')
}

######## This part takes too much time #######
# Please UNCOMMENT if you want to run this code

# sapply(link_methods, n_clust)



###### Result & Note  - NbClust ###
## Note: Except the case of AVERAGE, 
# The rest of methods says 2 is the best number of clusters
# 3 is the best number of the average method.


# linkage methods for agg_coef
link_methods_agg <- c('average', 'single', 'complete', 'ward')

# funcion to calculate agglomerative coef
agg_coef <- function(x) {
  cluster::agnes(df_wine_2_scaled, method = x)$ac
}

# calculate agglomerative coef for each link method
sapply(link_methods_agg, agg_coef)

##### Result & Note for aggromerlative coef ##########
# Note : The higher the coef value is the better,
# thus method ward is the best from those four methods
# However, top 3 methods only have a small difference of coef values
# So, I will do all those top 3 and compare them

# average    single  complete      ward 
# 0.9503902 0.8882437 0.9720613 0.9920683



###################################
### with method = average ##
# Create the Tidyclust specification
hc_wine_2_spec_avg <- 
  hier_clust(mode = 'partition', # partition is the only possible mode for this model
             engine = 'stats',
             num_clusters = 3, 
             cut_height = NULL, # use only if num_clusters = NULL
             linkage_method = 'average')

# Training the model with the method 'average'
hc_wine_2_fit_avg <- hc_wine_2_spec_avg %>% 
  fit(~., data = df_wine_2_scaled)

# HC Model with complete detail
hc_wine_2_fit_avg %>% extract_fit_summary()

### Result of average
#############
# $centroids
# # A tibble: 3 × 2
# Proanthocyanins Flavanoids
# <dbl>      <dbl>
# 1           0.985      0.871
# 2          -0.604     -0.559
# 3           0.488      3.05 
# 
# $n_members
# [1]  67 110   1
# 
# $sse_within_total_total
# [1] 51.58882 96.92531  0.00000
# 
# $sse_total
# [1] 221.7935


############################
### with method = complete ##
# Create the Tidyclust specification
hc_wine_2_spec_comp <- 
  hier_clust(mode = 'partition', # partition is the only possible mode for this model
             engine = 'stats',
             num_clusters = 2, 
             cut_height = NULL, # use only if num_clusters = NULL
             linkage_method = 'complete')

# Training the model with the method 'complete
hc_wine_2_fit_comp <- hc_wine_2_spec_comp %>% 
  fit(~., data = df_wine_2_scaled)

# HC Model with complete detail
hc_wine_2_fit_comp %>% extract_fit_summary()

### RESULT & NOTE of complete############
# HC model with the method complete has very similar mean and sd values 
# with the kmeans model 2
# Proanthocyanins Flavanoids
# <dbl>      <dbl>
# 1           0.697      0.798
# 2          -0.729     -0.834
# 
# $n_members
# [1] 91 87
# 
# $sse_within_total_total
# [1] 74.75201 66.72121
# 
# $sse_total
# [1] 221.7935

##



##############################
### with method = ward ####
# Create the Tidyclust specification
hc_wine_2_spec_ward <- 
  hier_clust(mode = 'partition', # partition is the only possible mode for this model
             engine = 'stats',
             num_clusters = 2, 
             cut_height = NULL, # use only if num_clusters = NULL
             linkage_method = 'ward')

# Training the model with the method 'ward'
hc_wine_2_fit_ward <- hc_wine_2_spec_ward %>% 
  fit(~., data = df_wine_2_scaled)

# HC Model with complete detail
hc_wine_2_fit_ward %>% extract_fit_summary()

### RESULT of ward
############
# $centroids
# # A tibble: 2 × 2
# Proanthocyanins Flavanoids
# <dbl>      <dbl>
# 1           0.903      0.884
# 2          -0.643     -0.629
# 
# $n_members
# [1]  74 104
# 
# $sse_within_total_total
# [1] 58.20108 88.48197
# 
# $sse_total
# [1] 221.7935

###########################################
####################################################################
################## Comparison among 3 methods ###########
## Note :
# Obviously, method 'average' is not fitted for out goal.
# Because, the HC model with average clustered the wines with 67 : 110 : 1
# Seems like there is a wine that has the superior amount of
# Proanthocyanins and Flavanoids.
#
# The Average's Centroids value is better than the WARD's Centroids.
# However, total sse of each Average and Ward are quite the same
# The gap in between sse_within_total value for Average is smaller than the
# difference between the case of Ward

# It is by the way very hard to say which is superior.
# Thus, let's get in to further investigation
##################################################################################
######################################################################


####### METHOD COMPLETE ###########
# ploting the dendrogram for complete
plot(hc_wine_2_fit_comp$fit, labels = rownames(df_wine_2))
abline(h=4, col = 'blue')

# Append cluster result to each observation(row) - complete
df_wine_2_hc_comp <- df_wine_2 %>% 
  mutate(cluster = extract_cluster_assignment(hc_wine_2_fit_comp)$.cluster)

# make predictions - complete
predict(hc_wine_2_fit_comp, df_wine_2_scaled[1:10, ])
df_wine_2_hc_comp[1:10, ]

# Mean & SD values - complete
df_wine_2_hc_comp %>% 
  group_by(cluster) %>% 
  summarise(mean(Proanthocyanins), mean(Flavanoids),
            sd(Proanthocyanins), sd(Flavanoids))

# Visualizing
fviz_cluster(hc_wine_2_fit_comp$fit, data = df_wine_2_scaled,
             ellipse.type = 'euclid',
             star.plot = TRUE,
             repel = TRUE,
             ggtheme = theme_minimal())

################### Result & Note for HC Model - Complete  #################################
# Result & Note :
# HC model with the method complete has very similar mean and sd values 
# with the kmeans model 2

# cluster   `mean(Proanthocyanins)` `mean(Flavanoids)` `sd(Proanthocyanins)` `sd(Flavanoids)`
# <fct>                       <dbl>              <dbl>                 <dbl>            <dbl>
# 1 Cluster_1                    1.99               2.83                 0.456            0.580
# 2 Cluster_2                    1.17               1.20                 0.335            0.572







####### METHOD WARD ###########
# ploting the dendrogram for ward
plot(hc_wine_2_fit_ward$fit, labels = rownames(df_wine_2))
abline(h=40, col = 'red')

# Append cluster result to each observation(row) - ward
df_wine_2_hc_ward <- df_wine_2 %>% 
  mutate(cluster = extract_cluster_assignment(hc_wine_2_fit_ward)$.cluster)

# make predictions - ward
predict(hc_wine_2_fit_ward, df_wine_2_scaled[1:10, ])
df_wine_2_hc_ward[1:10, ]

# Mean & SD values - ward
df_wine_2_hc_ward %>% 
  group_by(cluster) %>% 
  summarise(mean(Proanthocyanins), mean(Flavanoids),
            sd(Proanthocyanins), sd(Flavanoids))

################### Result & Note for HC Model - Ward  #################################
# Result & Note :
# HC model with the method ward has very similar mean and sd values 
# with the kmeans model 2 BUT A LITTLE BIT WORSE THAN COMPARING TO COMPLETE METHOD


# cluster   `mean(Proanthocyanins)` `mean(Flavanoids)` `sd(Proanthocyanins)` `sd(Flavanoids)`
# <fct>                       <dbl>              <dbl>                 <dbl>            <dbl>
# 1 Cluster_1                    2.11               2.91                 0.418            0.601
# 2 Cluster_2                    1.22               1.40                 0.332            0.707












####################################################################################

####################################################################################

####################################################################################
########### Retrieve essential Notes and Results #########################################################

######### Summary of Model 1 - trained with the ALL columns ######
## NOTE : ( number of cluster =3)
# When I see the average and sd of each clusters, this model trained with the whole columns 
# CANNOT cluster the wines by Proanthocyanins & Flavanoids

# cluster   `mean(Proanthocyanins)` `mean(Flavanoids)` `sd(Proanthocyanins)` `sd(Flavanoids)`
# <fct>                       <dbl>              <dbl>                 <dbl>            <dbl>
# 1 Cluster_1                    1.92              2.97                  0.442            0.401
# 2 Cluster_2                    1.60              2.07                  0.579            0.712
# 3 Cluster_3                    1.16              0.798                 0.410            0.313



######### Result & Note for Model 2 - Proanthocyanins & Flavanoids only #####
# Result & Note : (number of cluster = 2)
# This new model trained only with two columns which I want to set as the standard of clustering
# can perform better and more obvious than the previous model.
# The standard deviation values is more even than the case of model 1, too.
# Which are the indicator of the clustering model's performance.

# cluster   `mean(Proanthocyanins)` `mean(Flavanoids)` `sd(Proanthocyanins)` `sd(Flavanoids)`
# <fct>                       <dbl>              <dbl>                 <dbl>            <dbl>
# 1 Cluster_1                    1.98               2.81                 0.455            0.589
# 2 Cluster_2                    1.16               1.18                 0.329            0.564


########################################################
############ Hierarchical Clustering ##############


###### Result & Note  - NbClust ###
## Note: Except the case of AVERAGE, 
# The rest of methods says 2 is the best number of clusters
# 3 is the best number of the average method.

##### Result & Note for aggromerlative coef ##########
# average    single  complete      ward 
# 0.9503902 0.8882437 0.9720613 0.9920683

################## Comparison among 3 methods ###########
## Note :
# Obviously, method 'average' is not fitted for out goal.
# Because, the HC model with average clustered the wines with 67 : 110 : 1
# Seems like there is a wine that has the superior amount of
# Proanthocyanins and Flavanoids.
#
# 
# The Average's Centroids value is better than the WARD's Centroids.
# However, total sse of each Average and Ward are quite the same
# The gap in between sse_within_total value for Average is smaller than the
# difference between the case of Ward

# It is by the way very hard to say which is superior.
# Thus, let's get in to further investigation


################### Result & Note for HC Model - Complete  #################################
# Result & Note :
# HC model with the method complete has very similar mean and sd values 
# with the kmeans model 2

# cluster   `mean(Proanthocyanins)` `mean(Flavanoids)` `sd(Proanthocyanins)` `sd(Flavanoids)`
# <fct>                       <dbl>              <dbl>                 <dbl>            <dbl>
# 1 Cluster_1                    1.99               2.83                 0.456            0.580
# 2 Cluster_2                    1.17               1.20                 0.335            0.572


################### Result & Note for HC Model - Ward  #################################
# Result & Note :
# HC model with the method ward has very similar mean and sd values 
# with the kmeans model 2 BUT A LITTLE BIT WORSE THAN COMPARING TO COMPLETE METHOD


# cluster   `mean(Proanthocyanins)` `mean(Flavanoids)` `sd(Proanthocyanins)` `sd(Flavanoids)`
# <fct>                       <dbl>              <dbl>                 <dbl>            <dbl>
# 1 Cluster_1                    2.11               2.91                 0.418            0.601
# 2 Cluster_2                    1.22               1.40                 0.332            0.707



##############################################################################
###################### Conclusion ########################################
# Note : In my opinion, according to the distribution of Proanthocyanins & Flavanoids
# which is the main factors of the clustering, the model 2 trained with only 
# Proanthocyanins & Flavanoids columns with ordinary k means clustering model would perform the best.

# However, since this is the unsupervised learning model, there is no absolute way to 
# score the performance of the model.
# Therefore, the Hierarchical clustering model with 'complete' and 'ward' methods
# can be recommended to try as well.