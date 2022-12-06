
# Chapter 3 : plotting
#################
# Vline : chapter 3 ppt27
# Correlation ppt 43


# Chapter 5 : Cleaning Data
###################
# problems() : review the problem of the data
# distinct() : Duplicated
# summary( with Categorical ) : min max Q1 ~ 3, median
# Checking Uniqueness
rawData %>% 
  group_by(catalogNumber) %>%
  select(catalogNumber) %>%
  summarize(count = n()) %>%
  filter(count > 1)
# Show only rows 
rawData %>%
  filter(catalogNumber %in% c(50050, 50781, 50782, 50783, 50784)) %>%
  arrange(catalogNumber)
# str_trim() : Remove Extra spaces


# Chapter 6 : Plotting 2 + data cleaning (outlier)
##################################
# fill =
# color =
# facet_grid() : plot by group (or category)
# Invalid Row : ppt 60
# Removing Outlier : ppt 61   or HW 6.1
# heatmaps



# Chapter 7 : Linear Model + log scaling + LOGISTIC REGRESSION
#############################
# Multiple- R-squared : 0 to 1. The higher the Better, ppt 23. 

# rule of thumb that points with leverage of 3*mean(leverage)
lm_aug %>%
  filter(.hat > 3 * mean(lm_aug$.hat)) %>%
  arrange(desc(.hat)) # 내림차순

# Influence  Influencing Rows : ppt 29
# categorical linear regression : ppt 52


# Chapter 8 : Unsupervised(theory) + Multiple Regression + Logistic   (Parsnip)
######################
# train_test_split : initial_split()   ppt 26, starta: the  dependent variable
# Multiple Linear Regression : ppt 27~~

# model Evaluation : Homoscedasticity


# Multiple Logistic Regression : ppt 43

# confusion matrix, accuracy, sensitivity, pecificity : ppt 47

# ROC : ppt 52





# Chapter 10 : Classfication
#################
# Decision Tree : ppt 15

# Pruning : ppt 19

# F1 score : ppt 23

# Bagged Trees : ppt 25   - bagged is generally better

# Random Forest : ppt 27   - better if tuned

# Tunning : ppt 33    - kfold, DVfold 
# v fold for statistics : ppt 39


# Variable Importance : ppt 29   vip::vip(credit_model_forest)

# choosing cost_complexity : R file 10 Machine Learning

# R file 10 Bagged : bagged + Random Forest + TUNING


# Chapter 11 : Clustering - kmeans + Hierarchical Clustering