###########################################
# Title: Multiple Linear Regression
# Script Name: 
# Name: <your input>
# Date: <your input>
# Script purpose:
# 
###########################################

### Install required packages if not installed
#install.packages("tidyverse")
#install.packages("tidymodels")

### Load the packages
library(tidyverse)      # For tidy tools
library(tidymodels)
library(patchwork)

######################  Learning about linear regression ######################
# Load file for analysis
# Data file 
houseData_file <- "./dataset/House Sale Price Dataset.csv"
# Open and read the file into a tibble
tb_houseData = read_csv(houseData_file)
tb_houseData
#######################  Explore the data set  ##############################3
# Initial Data Analysis (IDA) for geo data
glimpse(tb_houseData)
summary(tb_houseData)

#Look for missing values
sapply(tb_houseData, function(x) sum(is.na(x)))

# remove rows with missing data
tb_houseData <- tb_houseData %>%
  filter(!is.na(StreetHouseFront))

# Quick recheck
summary(tb_houseData)

# Convert categorical values to factors
tb_houseData$Rating <- factor(tb_houseData$Rating)
tb_houseData$SaleType <- factor(tb_houseData$SaleType)
table(tb_houseData$ConnectivityType)
tb_houseData$ConnectivityType <- factor(tb_houseData$ConnectivityType)
tb_houseData$BuildingType <- factor(tb_houseData$BuildingType)
tb_houseData$EstateType <- factor(tb_houseData$EstateType)

# Look at summary statistics
summary(tb_houseData)  

# Quick plot to see what relationships may be there
tb_houseData %>%
  select_if(is.numeric) %>%
  pairs()

###############  Create training and testing datasets  ########################
# Create a data split object
set.seed(921)

houseData_split <- 
  initial_split(tb_houseData, 
                prop = 0.75, 
                strata = HousePrice)

# Create the training data
tb_houseData_training <- houseData_split %>%
  training()

# Create the test data
tb_houseData_test <- houseData_split %>% 
  testing()

# Check number of rows in each dataset
nrow(tb_houseData_training)
nrow(tb_houseData_test)

# Distribution of selling_price in training data
tb_houseData_training %>% 
  summarize(min_sell_price = min(HousePrice),
            max_sell_price = max(HousePrice),
            mean_sell_price = mean(HousePrice),
            sd_sell_price = sd(HousePrice))

# Distribution of selling_price in test data
tb_houseData_test %>% 
  summarize(min_sell_price = min(HousePrice),
            max_sell_price = max(HousePrice),
            mean_sell_price = mean(HousePrice),
            sd_sell_price = sd(HousePrice))

# Plot both training and testing datasets
hist_train <- 
  ggplot(tb_houseData_training, aes(x = HousePrice)) + 
  geom_histogram(bins= 12) +
  ggtitle("Train") 
hist_test <- 
  ggplot(tb_houseData_test, aes(x = HousePrice)) + 
  geom_histogram(bins= 12) +
  ggtitle("Test") 

hist_train + hist_test

#####################  Apply Linear Model to Data ############################
#Define the model specification with parsnip
lm_houseData_def <- linear_reg() %>% 
  # Set the model engine
  set_engine('lm') %>% 
  # Set the model mode
  set_mode('regression')

# Train the model with the training data
lm_houseData <- lm_houseData_def %>% 
  fit(HousePrice ~ StoreArea + StreetHouseFront 
      + BasementArea + LawnArea + Rating + SaleType, 
      data=tb_houseData_training)

# Review the model statistics
tidy(lm_houseData)
glance(lm_houseData)

# Retrain the model with the training data, dropping StreetHouseFront, SaleType
lm_houseData <- lm_houseData_def %>% 
  fit(HousePrice ~ StoreArea + BasementArea + LawnArea + Rating, 
      data=tb_houseData_training)

#################### Review model statistics  ###############################
# Review the model statistics
# RMSE, R-squared
tidy(lm_houseData)
glance(lm_houseData)

lm_houseData %>% 
  pluck("fit") %>%
  summary()

##################### Evaluate Model ##############################
# Linear relationships
ggplot(tb_houseData_training, aes(x = (LawnArea), y = HousePrice)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


# TASK : do some transformation for the numeric values and check the plot again


library(car) # for vif
vif(lm_houseData$fit)  # vif, 1-5 : not enough to worry, exceed 5: severe correlation


hist(tb_houseData_training$LawnArea)
cor((tb_houseData_training$LawnArea), tb_houseData_training$HousePrice)
