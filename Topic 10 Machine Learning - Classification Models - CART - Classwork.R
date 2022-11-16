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

######################### Turn parallel processing capabilities on ############
registerDoParallel()
no_cores <- detectCores() - 1  # leave one core free to do other stuffs
cl <- makeCluster(no_cores, type="FORK")  
registerDoParallel(cl)  

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

class(names(tb_credit))
names(tb_credit)[1]
# Convert categorical values to factors
for (i in names(tb_credit)) {
  if (class(i) == "character") {
    print(i)
  }
}
to_convert <- c('checking_balance', 'credit_history', 'purpose', ) # check phone pic

# Look at summary statistics
 
###############  Create training and testing datasets  ########################
set.seed(1234)
# Create a data split object
credit_split <- 
  initial_split(...)

# Create the training data
tb_credit_training <- credit_split %>%
  ...()

# Create the test data
tb_credit_test <- credit_split %>% 
  ...()

# Check division of rows in each dataset
table(...$default)
table(...$default)

#####################  Apply Decision Tree Model to Data ############################
#Define the model specification with parsnip
lm_credit <- ... %>% 
  # Set the engine
  ... %>% 
  # Set the mode
  ... %>% 
  # fit the model
  fit(...)

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
default_preds <- predict(...)

# Obtain estimated probabilities for each outcome value
default_prob <- predict(...)

# Combine test set results
credit_results <- tb_credit_test %>% 
  select(default) %>% 
  bind_cols(... , ...)

# View results tibble
credit_results
##################3# Assessing model performance  #######################
# Calculate the confusion matrix
conf_mat(...)

# Calculate the accuracy
accuracy(...)

# Calculate the sensitivity
sens(...)

# Calculate the specificity
spec(...)

###################### Visualizing model performance ##################3
# Calculate metrics across thresholds and plot ROC
credit_results %>% 
  roc_curve(...) %>% 
  autoplot()

# Calculate ROC AUC
roc_auc(...)

#############################  Improving Model Performance ####################
# By default: 
# decision_tree(min_n = 20, tree_depth = 30, cost_complexity = 0.01)

# Print and plot the complexity parameter values
printcp(lm_credit$fit)
plotcp(lm_credit$fit)
minXerr <- lm_credit$fit$cptable[which.min(lm_credit$fit$cptable[,"xerror"]),"CP"]

# Prepruning
lm_credit_pruned <- decision_tree(cost_complexity = 0.02) %>% 
  set_engine('...') %>% 
  set_mode('...') %>% 
  fit(...)

rpart.plot(lm_credit_pruned$fit,
           type=4,
           extra=101, 
           box.palette="GnBu",
           branch.lty=3, 
           shadow.col="gray", 
           nn=TRUE)

# Postpruning
lm_credit_postpruned <- prune(lm_credit$fit, cp = minXerr )

######################  Create predictions ####################3
# Predict outcome categories
default_preds <- predict(...)

# Obtain estimated probabilities for each outcome value
default_prob <- predict(...)

# Combine test set results
...

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
