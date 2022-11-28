library(ranger)
library(tidymodels)
library(tidyverse)
library(parsnip)
library(rpart)
# install.packages('baguette')
# install.packages('cvTools')
library(baguette)

library(cvTools)

######################  Learning about decision trees ######################
# Load file for analysis
credit_file <- "./dataset/credit.csv"

# Open and read the file into a tibble
tb_credit = read_csv(credit_file)

#Look for missing values
sapply(tb_credit, function(x) sum(is.na(x)))

# class(names(tb_credit))
# names(tb_credit)[1]
# # Convert categorical values to factors
# for (i in names(tb_credit)) {
#   if (class(i) == "character") {
#     print(i)
#   }
# }

names(tb_credit)
char_vec <- c()
for (i in names(tb_credit)) {
  if (is.character(tb_credit[[i]][1])) {
    
    char_vec <- append(char_vec, i)
  }
}

sapply(tb_credit[char_vec], function(x) unique(x))

char_vec <- append(char_vec, 'dependents')

tb_credit[char_vec] <- lapply(tb_credit[char_vec], factor)

glimpse(tb_credit)

unique(tb_credit$dependents)


# train test split
set.seed(1234)
spliter <- initial_split(tb_credit, prop = 0.75, strata = default)

tb_credit_training <- spliter %>% training()
tb_credit_test <- spliter %>% testing()



#######################  Bagged (Bootstrap Aggregation) Trees ##################
# Create the specification
credit_spec_bagged <- bag_tree() %>%
  set_mode("classification") %>%
  set_engine("rpart", times = 100)  # 100 trees


# Fit to the training data
credit_model_bagged <- fit(credit_spec_bagged,
                    default ~ ., 
                    data = tb_credit_training)

# Print the model
credit_model_bagged

######################  Create predictions ####################3
# Predict outcome categories
default_preds <- predict(..., new_data = ...,
                         type = 'class')

# Obtain estimated probabilities for each outcome value
default_prob <- predict(..., new_data = ...,
                        type = 'prob')

# Combine test set results
bagged_credit_results <- ... %>% 
  select(default) %>% 
  bind_cols(default_preds , default_prob)

# View results tibble
bagged_credit_results

# Calculate the confusion matrix
conf_mat(..., truth = default,
         estimate = .pred_class)

# Calculate the accuracy
accuracy(..., truth = default,
         estimate = .pred_class)

# Create and plot the ROC curve
bagged_credit_results %>%
  ...(estimate = .pred_no,
          truth = default) %>% 
  autoplot()

# Calculate the AUC
...(bagged_credit_results,
        estimate = .pred_no,
        truth = default)

#######################  Random Forest #################################
# Specify a random forest
credit_model_forest <- rand_forest() %>%
  set_mode("classification") %>%
  set_engine("ranger", importance = "impurity") %>%
  fit(default ~ ., data = tb_credit_training)

# Plot the variable importance
vip::vip(credit_model_forest)

######################  Create predictions ####################3
# Predict outcome categories
default_preds <- predict(..., new_data = tb_credit_test,
                         type = 'class')

# Obtain estimated probabilities for each outcome value
default_prob <- predict(..., new_data = tb_credit_test,
                        type = 'prob')

# Combine test set results
forest_credit_results <- ... %>% 
  select(default) %>% 
  bind_cols(default_preds , default_prob)

# View results tibble
forest_credit_results

# Calculate the confusion matrix
conf_mat(..., truth = default,
         estimate = .pred_class)

# Calculate the accuracy
accuracy(..., truth = default,
         estimate = .pred_class)

# Create and plot the ROC curve
... %>%
  roc_curve(estimate = .pred_no,
            truth = default) %>% 
  autoplot()

# Calculate the AUC
roc_auc(...,
        estimate = .pred_no,
        truth = default)

########################## Tune the model with Hyperparameters ################
# Step 1: Create a dummy specification with tuning placeholders
# Create a specification with tuning placeholders for a random forest
credit_model_forest_tune_spec <- 
  rand_forest(trees = tune(), min_n = tune()) %>% # tune() ppt. 31p
  set_mode("classification") %>%
  set_engine("ranger", importance = "impurity")

# Step 2: Create a regular grid and folds for resample
# Create a regular grid
tuning_grid <- grid_regular(trees(range = c(100, 1000)), # 500 was the default for trees, 
                          min_n(range = c(5, 20)), 
                          levels = 5) # 5 rows for each min_n - check tuning_grid

tuning_grid

# Create CV folds of the customers tibble # Kfold cross validation
CVfolds <- vfold_cv(tb_credit, v=10)

# Tune along the grid
credit_model_forest_tune_results <- 
  tune_grid(credit_model_forest_tune_spec,
            default ~ .,
            resamples = CVfolds,
            grid = tuning_grid)

# Plot the tuning results
autoplot(credit_model_forest_tune_results)

# Select the parameters that perform best
final_params <- 
  select_best(credit_model_forest_tune_results, 
              metric = 'accuracy', desc('roc_auc'))

final_params

# Finalize the specification
credit_model_forest_best_spec <- 
  finalize_model(credit_model_forest_best_spec, 
                 final_params)

# Build the final model
credit_model_forest_final_model <- 
  fit(credit_model_forest_best_spec,
      default ~ .,
      tb_credit_training)

credit_model_forest_final_model

######################  Create predictions ####################3
# Predict outcome categories
default_preds <- predict(..., new_data = tb_credit_test,
                         type = 'class')

# Obtain estimated probabilities for each outcome value
default_prob <- predict(..., new_data = tb_credit_test,
                        type = 'prob')

# Combine test set results
forest_tuned_credit_results <- ... %>% 
  select(default) %>% 
  bind_cols(default_preds , default_prob)

# View results tibble
forest_tuned_credit_results

# Calculate the confusion matrix
conf_mat(..., truth = default,
         estimate = .pred_class)

# Calculate the accuracy
accuracy(..., truth = default,
         estimate = .pred_class)

# Create and plot the ROC curve
forest_tuned_credit_results %>%
  ...(estimate = .pred_no,
            truth = default) %>% 
  autoplot()

# Calculate the AUC
...(forest_tuned_credit_results,
        estimate = .pred_no,
        truth = default)

# Using CVfolds to calculate statistics
model_stats <- fit_resamples(credit_model_forest_best_spec, 
                default ~ .,
                resamples = CVfolds,
                metrics = metric_set(accuracy, 
                                     sensitivity, 
                                     specificity, 
                                     roc_auc))

# Collect the metrics
collect_metrics(...)

# Turn off parallel cluster
stopCluster(cl)
