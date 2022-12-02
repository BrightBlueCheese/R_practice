########################  PCA  ####################################
# Perform scaled PCA:
df_arrest_pca <- prcomp(x = ...,                  
                        scale = TRUE,
                        center = TRUE)

# Inspect model output
summary(...)

# Compute variance
pca_variance <- df_arrest_pca$... ^ 2

# Proportion of variance for a scree plot
pca_variance_prop <- pca_variance / sum(pca_variance)

# Plot variance explained for each principal component
plot(..., xlab = "principal component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1), type = "b",
     main = "Scree Plot")

# Plot the cumulative proportion of variance explained
plot(cumsum(...),
     xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")

# see the elements in the model output
names(...)

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

ggbiplot(..., labels=rownames(df_arrest))

biplot(..., scale = 0)

###################################################################