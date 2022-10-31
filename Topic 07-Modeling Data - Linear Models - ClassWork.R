###########################################
# Title: Linear Models 
# Script Name: 
# Name: <your input>
# Date: <your input>
# Script purpose:
# 
###########################################

### Install required packages if not installed
#install.packages("tidyverse")
#install.packages("patchwork")
# install.packages('GGally')
### Load the packages
library(tidyverse)      # For tidy tools
library(patchwork) 
library(GGally) # for ggpairs
library(ggplot2)
library(dplyr)

######################  Learning about linear regression ######################
# Load file for analysis
# Data file 
lung_cap <- "./dataset/LungCapData.csv"
# Open and read the file into a tibble
tb_lung = read_csv(lung_cap)

#######################  Explore the data set  ##############################3
#Look for missing values
NAs_found = FALSE
for (i in names(tb_lung)){
  if(sum(is.na(tb_lung[i]))) {
    print(paste(i," has NA's"))
    NAs_found = TRUE
  }
}
if(!NAs_found) {
  print("No NAs found")
}

sapply(tb_lung, function(x) sum(is.na(x)))

# Initial Data Analysis (IDA) for geo data
glimpse(tb_lung)

# Look at summary statistics
summary(tb_lung)  


table(tb_lung$Smoke)
table(tb_lung$Gender)
table(tb_lung$Caesarean)

tb_lung$Smoke <- as_factor(tb_lung$Smoke)
tb_lung$Gender <- as_factor(tb_lung$Gender)
tb_lung$Caesarean <- as_factor(tb_lung$Caesarean)

summary(tb_lung)  


# check any relationships
tb_lung %>% select_if(is.numeric) %>% pairs() # base R


# Quick plot to see what relationships may be there
plot(tb_lung$LungCap, tb_lung$Height)
cor(tb_lung$LungCap, tb_lung$Height)

tb_lung %>% select_if(is.numeric) %>% ggpairs() # GGally


##################### Graphical Analysis #########################
## Generate a histogram for Height
ggplot(tb_lung, aes(x = Height)) + 
  geom_histogram(bins= 12) +
  ggtitle("Distribution of Height Observations") 

## Generate a histogram for LungCap
ggplot(tb_lung, aes(x = LungCap)) + 
  geom_histogram(bins= 14) +
  ggtitle("Distribution of Lung Capacity Observations") 

###################### Density Plot ##########################
p1 <- ggplot(tb_lung, aes(x = Height)) +       
  geom_density(color = "black", fill = "blue", alpha = 0.25) +
  geom_vline(tb_lung, mapping = aes(xintercept = mean(Height))) +
  ggtitle("Distribution of Height Observations") 


p1
p2 <- ggplot(tb_lung, aes(x = LungCap)) +       
  geom_density(color = "black", fill = "red", alpha = 0.25) +
  geom_vline(tb_lung, mapping = aes(xintercept = mean(LungCap))) +
  ggtitle("Distribution of Lung Capacity Observations") 

p2
# Using patchwork library
p1 / p2


p1 * p2

####################### Box Plots ##############################
p1 <- ggplot(tb_lung, aes(x = "Height", y = Height)) +
  geom_boxplot(outlier.color = "red", fill = "blue") +
  geom_point(position = position_jitter(width = 0.1), alpha = .1)

p2 <- ggplot(tb_lung, aes(x = "LungCap", y = LungCap)) +
  geom_boxplot(outlier.color = "red", fill = "red") +
  geom_point(position = position_jitter(width = 0.1), alpha = .1)

# Using patchwork library
p1 + p2


### Generate a scatter plot
ggplot(tb_lung, aes(x = Height, y = LungCap)) + 
  geom_point() +
  geom_smooth(se = FALSE) +
  geom_smooth(method =  "lm", color = "red", se = FALSE) +
  ggtitle("Lung Capacity vs. Height", 
    subtitle = "(With loess curve an standard error)")  +
  ylab("Lung Capacity")

cor(log10(tb_lung$Height), log10(tb_lung$LungCap))
#####################  Apply Linear Model to Data ############################
# Fit the model
lm_lung <- lm(formula = LungCap ~ Height, data = tb_lung)

##################### Determining Model Fit ##############################3#
#Retrieve model statistics
summary(lm_lung)

# Using glance from broom to see all variables
library(broom)
glance(lm_lung)

# tidy gives another view
tidy(lm_lung)

# Retrieve R squared, P value, RSE
glance(lm_lung) %>%
  pull(r.squared)

glance(lm_lung) %>%
  pull(p.value)

#Residual standard error
#  The "typical" diff between predicted and observed values
# In this case, in the units of lung capacity 
glance(lm_lung) %>%
  pull(sigma)

# Plot residuals
# install.packages("ggfortify")
library(ggfortify)
autoplot(lm_lung, which = 1:2, nrow = 2, ncol = 1)
autoplot(lm_lung)

#####################  Checking for Extreme Values  #########################
# Leverage 

hatvalues(lm_lung)

augment(lm_lung) %>%
  arrange(desc(.hat))
# put augment data in a data frame
lm_aug <- augment(lm_lung)
# rule of thumb that points with leverage of 3*mean(leverage)
lm_aug %>%
  filter(.hat > 3 * mean(lm_aug$.hat)) %>%
  arrange(desc(.hat))

# Influence is a measure of how much impact a point has on the model
# Cook's distance is a combination of each observation’s leverage and residual values; 
#Essentially, Cook’s Distance does one thing: it measures how much all of the fitted values in the model change when the ith data point is deleted.

lm_aug %>%
  filter(.cooksd > 3 * mean(lm_aug$.cooksd)) %>%
  arrange(desc(.cooksd))

lm_aug %>%
  filter(.cooksd > 4 / nrow(tb_lung)) %>%
  arrange(desc(.cooksd))

# Identify and remove data with high influence
cooksD <- cooks.distance(lm_lung)
influential_obs <- as.numeric(names(cooksD)[(cooksD > (4/nrow(tb_lung)))])
tb_lung_noOuts <- tb_lung[-influential_obs, ]


# check the code by myself
names(cooksD)
names(cooksD)[(cooksD > (4/nrow(tb_lung)))]


p1 <- ggplot(tb_lung, aes(x = Height, y = LungCap)) + 
  geom_point() +
  geom_smooth(method =  "lm", color = "red") +
  ggtitle("Lung Capacity vs. Height", 
          subtitle = "(All Data)")  +
  ylab("Lung Capacity")

p2 <- ggplot(tb_lung_noOuts, aes(x = Height, y = LungCap)) + 
  geom_point() +
  geom_smooth(method =  "lm", color = "red") +
  ggtitle("Lung Capacity vs. Height", 
          subtitle = "(Outliers Removed)")  +
  ylab("Lung Capacity")

p1 + p2

lm(formula = LungCap ~ Height, data = tb_lung)
lm(formula = LungCap ~ Height, data = tb_lung_noOuts)

cor(tb_lung$LungCap, tb_lung$Height)
cor(tb_lung_noOuts$LungCap, tb_lung_noOuts$Height)

tb_lung[136, ]
#####################  Using the linear model to predict  ####################
# Create a data frame (tibble) with input values 
testHeightValues <- tibble(Height = 35:90)

# Create a new dataset of predicted values
tb_lung_height_pred <- testHeightValues %>%
  mutate(lung_cap = predict(lm_lung, testHeightValues))


# Plot observations, lm plot, and predicted values
ggplot(tb_lung, aes(x = Height, y = LungCap)) + 
  geom_point() +
  geom_smooth(method =  "lm", color = "red") +
  geom_point(data = tb_lung_height_pred, 
             aes(x = Height, y = lung_cap), 
             shape = 17, color = "red", size = 3) +
  ggtitle("Lung Capacity vs. Height")  +
  ylab("Lung Capacity")

################## Regression with Transforms ############################
# Open the gapminder data
df_gapminder = read_csv("./dataset/CSC461GapMinderPlus.csv")

### Scatter plot life exp vs. income, color by region
ggplot(df_gapminder, aes(x = income, y=life_exp)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "World Life Expectancy vs. Income for 2020",
       x = "Income",
       y = "Life Expectancy")

cor(df_gapminder$income, df_gapminder$life_exp)

# install.packages("ggpubr")
library(ggpubr)

### Scatter plot life exp vs. income, color by region
ggplot(df_gapminder, aes(x = log10(income), y=life_exp)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  stat_regline_equation(label.x.npc = 0.1, label.y.npc = .9) +
  labs(title = "World Life Expectancy vs. Income for 2020",
       x = "Income",
       y = "Life Expectancy")

cor(log10(df_gapminder$income), df_gapminder$life_exp)

########################  Create the linear regression model  ###############3
lm_gapminder <- lm(life_exp ~ log10(income), data = df_gapminder)
summary(lm_gapminder)
glance(lm_gapminder)

#####################  Using the linear model to predict  ####################
# Create a data frame (tibble) with input values 
# testIncomeValues <- tibble(income = seq(1000, 100000, by = 1000))
testIncomeValues <- tibble(income = 10^seq(3, 5, by = 1000))

# Create a new dataset of predicted values
tb_income_life_pred <- testIncomeValues %>%
  mutate(life_exp = predict(lm_gapminder, testIncomeValues))

# Plot observations, lm plot, and predicted values
ggplot(df_gapminder, aes(x = log10(income), y = life_exp)) + 
  geom_point() +
  geom_smooth(method =  "lm", color = "red") +
  geom_point(data = tb_income_life_pred, 
             aes(x = log10(income), y = life_exp), 
             shape = 17, color = "red", size = 3) +
  ggtitle("World Life Expectancy vs. Income for 2020")  +
  xlab("Life Exp")+
  ylab("Log10(Income)")


### Another plot using predict () and changing the coordinate system
ggplot(df_gapminder, aes(x = income, y=life_exp)) + 
  geom_point() +
  coord_trans(x = "log10") +
  geom_line(aes(y = predict(lm_gapminder)), size = 2, color = "red") +
  labs(title = "World Life Expectancy vs. Income for 2020",
       x = "Income",
       y = "Life Expectancy")

##################  Dealing Categorical Variables  ################################
# Initial Data Analysis (IDA) for geo data
glimpse(tb_lung)

# Quick plot to see what relationships may be there
plot(tb_lung)
cor(tb_lung$LungCap, tb_lung$Smoke)

# Look at summary statistics
summary(tb_lung)  

# Consider lung capacity by gender
tb_lung %>%
  group_by(Gender) %>%
  summarize(mean_LungCap = mean(LungCap),
            sd_LungCap = sd(LungCap),
            count_LungCap = n(),
            rbind(qtiles_LungCap = quantile(LungCap)))

#####################  Apply Linear Model to Data ############################
# Fit the model
lm(formula = LungCap ~ Gender, data = tb_lung) # mean_LungCap male=8.31, female=7.41
# Gernderfemale for above indicates 7.31 - 8.31 = -0.9036 
# add + 0 for categorical variables
lm(formula = LungCap ~ Gender + 0, data = tb_lung)

lm_lungGen <- lm(formula = LungCap ~ Gender + 0, data = tb_lung)

#Retrieve model statistics
summary(lm_lungGen)

##################### Graphical Analysis #########################
## Generate a bar graph by Gender
ggplot(tb_lung, aes(x = Gender)) + 
  geom_bar(fill = c("blue","red")) +
  ggtitle("Observation Count by Gender") 

## Generate a density/histogram for LungCap by Gender
ggplot(tb_lung, aes(x = LungCap)) + 
  geom_histogram(aes(y = ..density..),  bins= 8) +
  geom_density() +
  facet_wrap(vars(Gender)) + 
  ggtitle("Distribution of Lung Capacity Observations by Gender") 

####################### Box Plots ##############################
ggplot(tb_lung, aes(x = Gender, y = LungCap)) +
  geom_boxplot(outlier.color = "red", fill = c("blue","red")) +
  geom_point(position = position_jitter(width = 0.1), alpha = .1) +
  stat_summary(fun.y = mean, shape = 15, size = 1)

### Generate a scatter plot
plot1 <- ggplot(tb_lung, aes(x = Height, y = LungCap, color = Gender)) + 
  geom_point() +
  geom_smooth(se = FALSE, color = "black") +
  geom_smooth(method =  "lm", se = FALSE) +
  scale_color_manual(values = c("blue", "red")) +
  facet_grid(Gender~.) +
  labs(title = "Lung Capacity vs. Height by Gender",
       x = "Height",
       y = "Lung Capacity")

plot1
#####################  Apply Linear Model to Data ############################
# Determine the unique values of the factor
unique(tb_lung$Gender)

# Separate the data
tb_lung_m <- tb_lung %>%
  filter(Gender == "male")
tb_lung_f <- tb_lung %>%
  filter(Gender == "female")

# Fit the models
lm_lung_m <- lm(formula = LungCap ~ Height, data = ...)
lm_lung_f <- lm(formula = LungCap ~ Height, data = ...)

summary(lm_lung_m)
summary(lm_lung_f)


# Two explanatory variables with no interaction
lm(formula = LungCap ~ Height + Gender + 0, data = tb_lung)

###################  Create and Plot Predictions  ###################
# Create prediction data
df_lung_pred <- expand_grid(
  Height = seq(45, 85, 5),
  Gender = unique(tb_lung$Gender)
)

# Calculate values for lung capacity
df_lung_pred <- df_lung_pred %>%
  mutate(
    LungCap = case_when(
      Gender == "male" ~ coefficients(...)[1] +
        coefficients(lm_lung_m)[2] * Height,
      Gender == "female" ~ coefficients(...)[1] + 
        coefficients(lm_lung_f)[2] * Height))

plot1 +
  geom_point(data = df_lung_pred, 
    aes(x = Height, y = LungCap), 
    shape = 17, size = 3, color = "black")

###################### Logistic Regression #############################
# Load file for analysis
# Data file 
titanic <- ".../TitanicPassengerData.csv"
# Open and read the file into a tibble
tb_titanic = read_csv(...)

#Look for missing values
NAs_found = FALSE
for (i in names(tb_titanic)){
  if(sum(is.na(tb_titanic[i]))) {
    print(paste(i," has NA's"))
    NAs_found = TRUE
  }
}
if(!NAs_found) {
  print("No NAs found")
}

# Initial Data Analysis (IDA) for geo data
glimpse(tb_titanic)

# Look at summary statistics
summary(tb_titanic)  

# Fit a binomial logistic regression model
glm_titanic <- glm(formula = ... ~ ..., 
                  family = ..., 
                  data = tb_titanic)

summary(glm_titanic)

# Visualize the model
plt_titanic_Fare <- ggplot(tb_titanic, aes(Fare, Survived)) +
  geom_point() +
  geom_smooth(method = "glm", se = FALSE, 
              method.args = list(family = ...)) +
  xlim(0,600)

plt_titanic_Fare

############ Making Predictions  ###########
# create values for exploratory variable
titanic_input_data <- tibble(
  Fare = seq(0, 600, 20)
)
# gather predictor values for test values above
titanic_prediction_data <- titanic_input_data %>%
  mutate(
    Survived = predict(..., ..., type = "response"))

# plot (add) the values on the graph
plt_titanic_Fare +
  geom_point(
    data = titanic_prediction_data,
    color = "blue"
  )

# Add most likely outcome, i.e., probability > .50
titanic_prediction_data <- titanic_prediction_data %>%
  mutate(
    survival_prediction = round(...)
  )
# Add the points to the plot
plt_titanic_Fare +
  geom_point(
    data = titanic_prediction_data,
    aes(y = survival_prediction),
    color = "green"
  )

# calculate the odds ratio and log odds ratio on Survival
titanic_prediction_data <- titanic_prediction_data %>%
  mutate(
    odds_ratio = ... / (1 - ...),
    log_odds_ratio = log(odds_ratio),
    log_odds_ratio2 = predict(glm_titanic, titanic_input_data)
  )

# create a (partial) graph of these
ggplot(
  titanic_prediction_data,
  aes(Fare, odds_ratio)) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dotted") +
  xlim(0, 200) +
  ylim(0, 10)

###########  Quantifying Fit #############
# get the actual and predicted outcomes
# and put in a table
actual <- tb_titanic$...
predicted <- round(fitted(...))
outcomes <- table(predicted, actual)
outcomes

# Create the Confusion matrix
#install.packages("yardstick")
library(yardstick)
confusion_matrix <- conf_mat(outcomes)
autoplot(confusion_matrix, color = "actual")

#Metrics
summary(confusion_matrix)
summary(confusion_matrix, event_level = "second")

############# Notes ############# 
# Accuracy is the proportion of correct predictions.
# Sensitivity is the proportion of true positives
# Specicity is the proportion of true negatives
