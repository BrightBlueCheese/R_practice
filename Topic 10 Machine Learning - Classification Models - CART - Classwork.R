###########################################
# Title: Classification Models
# Script Name: 
# Name: <your input>
# Date: <your input>
# Script purpose:
# 
###########################################

### Install required packages if not installed
#install.packages("tidyverse")
#install.packages("tidymodels")
#install.packages("rpart.plot")
#install.packages("doParallel")

### Load the packages
library(tidyverse)      # For tidy tools
library(tidymodels)
library(doParallel) # work with multiThreads
# stopCluster(cl) to stop
library(rpart.plot)
library(yardstick)

######################### Turn parallel processing capabilities on ############
registerDoParallel()
no_cores <- detectCores() - 1  # leave one core free to do other stuffs
# cl <- makeCluster(no_cores, type="FORK")  
# registerDoParallel(cl)  

######################  Learning about decision trees ######################
# Load file for analysis
credit_file <- "./dataset/credit.csv"

# Open and read the file into a tibble
tb_credit = read_csv(credit_file)

#######################  Explore the data set  ##############################3
# Initial Data Analysis (IDA) for geo data
glimpse(tb_credit)
summary(tb_credit)

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

# Look at summary statistics
 
###############  Create training and testing datasets  ########################
set.seed(1234)
# Create a data split object
credit_split <- 
  initial_split(tb_credit, prop = 0.75, starta = default)

# Create the training data
tb_credit_training <- credit_split %>%
  training()

# Create the test data
tb_credit_test <- credit_split %>% 
  testing()

# Check division of rows in each dataset
table(tb_credit_training$default)
table(tb_credit_test$default)

#####################  Apply Decision Tree Model to Data ############################
#Define the model specification with parsnip
    # https://www.tidymodels.org/find/parsnip/
lm_credit <- decision_tree() %>% 
  # Set the engine
  set_engine('rpart') %>% 
  # Set the mode
  set_mode('classification') %>% 
  # fit the model
  fit(default ~ ., # default = independant variable (y), . = fit evey rest of independant vars
      data = tb_credit_training)

#################### Review model statistics  ###############################
# Review the model statistics
print(lm_credit$fit, digits = 2)
rpart.plot(lm_credit$fit,
           type=4,
           extra=101, 
           box.palette="GnBu",
           branch.lty=3, 
           shadow.col="gray", 
           nn=TRUE)

######################  Create predictions ####################
# Predict outcome categories
default_preds <- predict(lm_credit, new_data = tb_credit_test, type = 'class')

# Obtain estimated probabilities for each outcome value
default_prob <- predict(lm_credit, new_data = tb_credit_test, type = 'prob')

default_prob
length(default_preds)

lm_credit
length(tb_credit_test$default)

# Combine test set results
credit_results <- tb_credit_test %>% 
  select(default) %>% 
  bind_cols(default_preds , default_prob)

# View results tibble
credit_results
##################3# Assessing model performance  #######################
# Calculate the confusion matrix
conf_mat(credit_results, truth = default, estimate = .pred_class)

# Calculate the accuracy
accuracy(credit_results, truth = default, estimate = .pred_class)

# Calculate the sensitivity
sens(credit_results, truth = default, estimate = .pred_class)

# Calculate the specificity
spec(credit_results, truth = default, estimate = .pred_class)
credit_results %>% spec(truth = default, estimate = .pred_class)
credit_results

###################### Visualizing model performance ##################3
# Calculate metrics across thresholds and plot ROC
credit_results %>% 
  roc_curve(truth = default, .pred_no) %>% 
  autoplot()

# Calculate ROC AUC
roc_auc(credit_results,
        truth = default, .pred_no)

#############################  Improving Model Performance ####################
# By default: 
# decision_tree(min_n = 20, tree_depth = 30, cost_complexity = 0.01)

# Print and plot the complexity parameter values
printcp(lm_credit$fit)
plotcp(lm_credit$fit)
minXerr <- lm_credit$fit$cptable[which.min(lm_credit$fit$cptable[,"xerror"]),"CP"]
minXerr
# X-val : Cross Validation -> take the parameters(i.e. Cp) that has the lowest X-Val
  # In this case, Cp = 0.024
  # @@@@@@@@@@@@@@ minXerr results keep changes. @@@@@@@@@@@@@@
# Prepruning
lm_credit_pruned <- decision_tree(cost_complexity = 0.02) %>% 
  set_engine('rpart') %>% 
  set_mode('classification') %>% 
  fit(default ~
        ., data = tb_credit_training)
rpart.plot(lm_credit_pruned$fit,
           type=4,
           extra=101, 
           box.palette="GnBu",
           branch.lty=3, 
           shadow.col="gray", 
           nn=TRUE)
# Check available engines
show_engines('decision_tree')



######################  Create predictions ####################3
# Predict outcome categories
default_preds_pruned <- predict(lm_credit_pruned,
                         new_data = tb_credit_test,
                         type ='class')

# Obtain estimated probabilities for each outcome value
default_prob_pruned <- predict(lm_credit_pruned,
                        new_data = tb_credit_test,
                        type = 'prob')
default_preds_pruned
default_prob_pruned
length(default_preds_pruned)
length(default_prob_pruned)
# Combine test set results
length(tb_credit_test$default)

pruned_credit_results <- tb_credit_test %>% 
  select(default) %>% 
  bind_cols(default_preds_pruned , default_prob_pruned)

# View results tibble
pruned_credit_results

# Calculate the confusion matrix
...

# Calculate the accuracy
...

# Calculate the sensitivity
...

# Calculate the specificity
...

# Create and plot the ROC curve
...

# Calculate the AUC
...

# Turn off parallel cluster
stopCluster(cl)
