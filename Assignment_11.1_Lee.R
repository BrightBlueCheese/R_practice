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

# load data
df_wine <- read_csv('./dataset/Wine.csv')

df_wine


# check missing
sapply(df_wine, function(x) sum(is.na(x)))

# column mean and std
apply(df_wine, 2, mean)
apply(df_wine, 2, sd)


# scaling

mx_wine_scaled <- scale(df_wine, center = TRUE, scale = TRUE) # scale for min max sqr err
df_wine_scaled <- as_tibble(mx_wine_scaled, rownames = NA)

# column mean and std for scaled
apply(df_wine_scaled, 2, mean)
apply(df_wine_scaled, 2, sd)
summary(df_wine_scaled)


