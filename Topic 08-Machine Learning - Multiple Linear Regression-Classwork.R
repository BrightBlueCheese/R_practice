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


############################################################################
  # TASK : do some transformation for the numeric values and check the plot again

# LawnArea
# x range 0.1 ~ 0.9, remove outliers..
ggplot(tb_houseData_training, aes(x = (LawnArea), y = HousePrice)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(se = FALSE, aes(colour = 'losses')) + 
  coord_cartesian(xlim = quantile(tb_houseData_training$LawnArea, c(0.1, 0.9)))

# Density plot for LawnArea - Right Skewed
ggplot(data = tb_houseData_training, aes(x = LawnArea)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density()

# Then log trans
ggplot(tb_houseData_training, aes(x = LawnArea, y = HousePrice)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(se = FALSE, aes(colour = 'losses')) + 
  scale_x_sqrt()

ggplot(tb_houseData_training, aes(x = LawnArea, y = HousePrice)) + 
  geom_point() +
  coord_trans(x = 'sqrt') +
  geom_smooth(se = FALSE, aes(colour = 'losses')) + 
  geom_smooth(method = "lm", se = FALSE)

ggplot(tb_houseData_training, aes(x = sqrt(LawnArea), y = HousePrice)) + 
  geom_point() +
  geom_smooth(se = FALSE, aes(colour = 'losses')) + 
  geom_smooth(method = "lm", se = FALSE)

ggplot(tb_houseData_training, aes(x = LawnArea, y = HousePrice)) + 
  geom_point() +
  coord_trans(x = 'log10') +
  geom_smooth(se = FALSE, aes(colour = 'losses')) + 
  geom_smooth(method = "lm", se = FALSE)


##################
# StoraArea
ggplot(tb_houseData_training, aes(x = (StoreArea), y = HousePrice)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(se = FALSE, aes(colour = 'losses'))

# Density Right Skewed
ggplot(data = tb_houseData_training, aes(x = StoreArea)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density()

# square root trans
ggplot(tb_houseData_training, aes(x = StoreArea, y = HousePrice)) + 
  geom_point() +
  coord_trans(x = 'sqrt') +
  geom_smooth(se = FALSE, aes(colour = 'losses')) + 
  geom_smooth(method = "lm", se = FALSE)

# log10 trans        - GOOD ############## #########
ggplot(tb_houseData_training, aes(x = log(StoreArea), y = HousePrice)) + 
  geom_point() +
  geom_smooth(se = FALSE, aes(colour = 'losses')) + 
  geom_smooth(method = "lm", se = FALSE)

# Reciprocal trans
ggplot(tb_houseData_training, aes(x = StoreArea, y = HousePrice)) + 
  geom_point() +
  coord_trans(y = 'reciprocal') +
  geom_smooth(se = FALSE, aes(colour = 'losses')) + 
  geom_smooth(method = "lm", se = FALSE)

##############################
# BasementArea
ggplot(tb_houseData_training, aes(x = (BasementArea), y = HousePrice)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(se = FALSE, aes(colour = 'losses'))

# Density Right Skewed - Super Right Skewed
ggplot(data = tb_houseData_training, aes(x = BasementArea)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density()

# square root trans
ggplot(tb_houseData_training, aes(x = BasementArea, y = HousePrice)) + 
  geom_point() +
  coord_trans(x = 'sqrt') +
  geom_smooth(se = FALSE, aes(colour = 'losses')) + 
  geom_smooth(method = "lm", se = FALSE)

# log10 trans - containing non-finite values (stat_smooth). with coord_trans
ggplot(tb_houseData_training, aes(x = log(BasementArea), y = HousePrice)) + 
  geom_point() +
  geom_smooth(se = FALSE, aes(colour = 'losses')) + 
  geom_smooth(method = "lm", se = FALSE)

# Reciprocal trans
ggplot(tb_houseData_training, aes(x = BasementArea, y = HousePrice)) + 
  geom_point() +
  coord_trans(y = 'reciprocal') +
  geom_smooth(se = FALSE, aes(colour = 'losses')) + 
  geom_smooth(method = "lm", se = FALSE)

##############################
# Rating
ggplot(tb_houseData_training, aes(x = (Rating), y = HousePrice)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(se = FALSE, aes(colour = 'losses'))

class(tb_houseData_training$Rating)

# Density - Invalid with Factor

# square root trans
ggplot(tb_houseData_training, aes(x = Rating, y = HousePrice)) + 
  geom_point() +
  coord_trans(x = 'sqrt') +
  geom_smooth(se = FALSE, aes(colour = 'losses')) + 
  geom_smooth(method = "lm", se = FALSE)

# log10 trans - Invalid with Factor


# Reciprocal trans
ggplot(tb_houseData_training, aes(x = Rating, y = HousePrice)) + 
  geom_point() +
  coord_trans(y = 'reciprocal') +
  geom_smooth(se = FALSE, aes(colour = 'losses')) + 
  geom_smooth(method = "lm", se = FALSE)




# plot applied apa format
ggplot(data = df_thomy, aes(x = log(weight), y = length)) + geom_point() + 
  geom_smooth(method='lm', se=FALSE, size = 2, color='red') +
  ggtitle('log10') +
  geom_point(data = df_thomy_pred, aes(x = log(weight), y = length_pre),
             shape = 17, size = 2, color = 'purple') +
  theme_apa()



library(car) # for vif
vif(lm_houseData$fit)  # vif, 1-5 : not enough to worry, exceed 5: severe correlation


hist(tb_houseData_training$LawnArea)
cor((tb_houseData_training$LawnArea), tb_houseData_training$HousePrice)
