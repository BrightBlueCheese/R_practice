###########################################
# Title: Assignment_8.1_Lee
# Script Name: Assignment_8.1_Lee
# Name: Youngmin Lee
# Date: Nov 20 2022
# Script purpose: For Assignment 8.1
# 
###########################################
# packages
library(tidyverse)      # For tidy tools
library(tidymodels)
library(patchwork)
library(car)

###############
  ## read the file into a tibble
df <- read_csv('./dataset/Telco-Customer-Churn.csv')

df

# explore
glimpse(df)
summary(df)

#################
  ## Remove any rows that have NA's
sapply(df, function(x) sum(is.na(x)))
  # 11 NAs of 7043 rows are comparably small.
  # Seems like it can be removed.

df2 <- df %>% filter(!is.na(TotalCharges))
df2

###################
  ## Coerce vectors appropriately
# extract character types first
names(df2)
char_vec <- c()
for (i in names(df2)) {
  if (is.character(df2[[i]][1])) {
    
    char_vec <- append(char_vec, i)
  }
}

# class(df2[[2]][1])

char_vec

char_vec

sapply(df2[char_vec], function(x) unique(x))

# it seems like SeniorCitizen also can be factor
unique(df2$SeniorCitizen)

char_vec <- append(char_vec, 'SeniorCitizen')


df3 <- df2 %>% mutate()

df3[char_vec] <- lapply(df3[char_vec], factor)

glimpse(df3)


#####################
  ## Split the data into training and test datasets
# strata : variable(column) that want to distribute evenly
spliter <- initial_split(df3, prop = 0.75, strata = Churn)  # strata : dependent var

df3_train <- spliter %>% training()
df3_test <- spliter %>% testing()

nrow(df3_train)
nrow(df3_test)
nrow(df3_train) + nrow(df3_test) == nrow(df3)

#####################
  ## Build a logistic regression/binary classification
# define
lm_df3_def <- logistic_reg() %>% 
  set_engine('glm') %>% 
  set_mode('classification')

glimpse(df3)

# fit with train set - exclude customerID
lm_df3 <- lm_df3_def %>% fit(Churn ~ . - customerID,
                             data = df3_train)

# save the old model to compare
lm_df3_old <- lm_df3

# review
lm_df3 %>% pluck('fit') %>% summary()
# vif(lm_df3)


## Note_review : Looks like the p values of Gender, Partner, Dependents, and MonthlyCharges 
# are not significant to this model. 
# + customerID is just like an index value, so it is totally makes sense to exclude
# from the model.
# Column Excluded : customerID, Gender, Partner, Dependents, and MonthlyCharges


# remodel
lm_df3 <- lm_df3_def %>% fit(Churn ~ . - customerID - gender - 
                               Partner - Dependents - MonthlyCharges,
                             data = df3_train)
# review again
lm_df3 %>% pluck('fit') %>% summary()
# vif(lm_df3)

## Note_review_again : The high p value of Intercept does not matter.
# Because, in this case, the intercept can't be (or hardly) differentiate from ZERO.
# All of the columns in the new model has significant effect to the model's performance
# Interestingly, a categorical value 'PaymentMethodElectronic check' 
# from the column 'PaymentMethod' has the p value less than 0.001.
# However, the column 'PaymentMethod' can have a significant role on the model's performance
# with the categorical value 'PaymentMethodElectronic check'. 
# So this column is great enough to be in the model

# looks like the model automatically exclude ONE column from one-hot-encoded columns
unique(df3$PaymentMethod) 


###################
  ## Create predictions and probabilities using the test dataset, 
  ## then combine these with the customerID and Churn values 
  ## in the test dataset to create a "results" tibble.
# predict test data
churn_preds <- predict(lm_df3, new_data = df3_test, type = 'class')

# probability of test data
churn_prob <- predict(lm_df3, new_data = df3_test, type = 'prob')

# select some cols and bind with preds & prob
churn_results <- df3_test %>%
  select(customerID, Churn) %>% 
  bind_cols(churn_preds, churn_prob)
  
# result churn prediction - "results" tibble
churn_results


##################
  ## Use the "results" tibble to create 
  ## a confusion matrix, and calculate accuracy, sensitivity, and specificity values.
# confusion matrix
conf_mat(churn_results, truth = Churn,
         estimate = .pred_class)

# accuracy value = 0.816
churn_results %>% accuracy(truth = Churn, estimate = .pred_class)

# sensitivity value
# True Positive Rate = 0.916
churn_results %>% sens(truth = Churn, estimate = .pred_class) 

# specificity
# True Negative Rate = 0.538
churn_results %>% spec(truth = Churn, estimate = .pred_class) 



#####################
  ## Create a ROC curve and calculate the ROC AUC.
# thresholds
threshold_df <- churn_results %>% roc_curve(truth = Churn, .pred_No)
# plot ROC curve
ROC_plot <- threshold_df %>% autoplot()
ROC_plot

# calculate ROC AUC = 0.851
roc_auc(churn_results, truth = Churn, .pred_No)



####### FOR FUN ###########
# ROC curve for lm_df3_old
# predict test data
churn_preds_old <- predict(lm_df3_old, new_data = df3_test, type = 'class')

# probability of test data
churn_prob_old <- predict(lm_df3_old, new_data = df3_test, type = 'prob')

# old results tibble
churn_results_old <- df3_test %>%
  select(customerID, Churn) %>% 
  bind_cols(churn_preds_old, churn_prob_old)

# old threshold
threshold_df_old <- churn_results_old %>% roc_curve(truth = Churn, .pred_No)

# plot old ROC curve
ROC_plot_old <- threshold_df_old %>% autoplot()
ROC_plot_old
# ROC AUC for old
roc_auc(churn_results_old, truth = Churn, .pred_No)


# Comparing new vs old
# plot
ROC_plot / ROC_plot_old
# ROC AUC , new : old =  0.851 : 0.850
roc_auc(churn_results, truth = Churn, .pred_No)
roc_auc(churn_results_old, truth = Churn, .pred_No)
# Interestingly, there is no significant difference if comparing with ROC AUC
# However, it is possible to say the model's performance has been improved
# by excluding some insignificant columns from the training



#####################
  ## At the end of the script, create a write up to discuss your findings.

# Recalling the result (new)
# accuracy = 0.816
# sensitivity (True Positive Rate) = 0.916
# specificity (True Negative Rate) = 0.538
# ROC AUC = 0.851

# Write Up Note : 
# For training the model, the columns..
# customerID, Gender, Partner, Dependents, and MonthlyCharges  statistically 
# does not take significant roles.

# This model has 81.6 % accuracy to predict whether the customer churned or not. 
# According to the model's prediction with the test dataset,
# the model performs well at true positive case with 91.6% of sensitivity.
# However, the model shows that it weak comparably at predicting 
# the true negative cases with 53.8 % of specificity.
# Therefore, this model is not a very good model to predict whether the customer
# will NOT churning compare to the case of predicting whether will churning.
# The ROC AUC indicates that this model's performance is better than just guessing
# (85.1%)



# Thank you so much and have a blessed day!