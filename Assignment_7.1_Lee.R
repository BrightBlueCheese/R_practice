#Topic 07 - Linear Models
###########################################
# Title: For the Assignment 7.1
# Name: Youngmin Lee
# Date: Nov 06 2022
# Script purpose: Assignment 7.1
# 
###########################################

# libraries
library(dplyr)
library(tidyverse)
library(ggpubr)     # For placing multiple plots in one figure
library(ggplot2)
# # install.packages('directlabels')
install.packages('papaja')
library(papaja) # For apa format plotting
library(broom) # for calling the function glance()


df <- read_csv('./dataset/rodentData_consistent.csv')

# change column class to factor as needed
# Convert institutionCode to factor
df$institutionCode <- 
  as.factor(df$institutionCode)
# Convert countryCode to factor
df$countryCode <- 
  as.factor(df$countryCode)
# Convert states to factor
df$stateProvince <- 
  as.factor(df$stateProvince)
# Convert county to factor
df$county <- 
  as.factor(df$county)
#Convert genus to a factor
df$genus <- 
  as.factor(df$genus)
# Convert specificEpithet to a factor
df$specificEpithet <- 
  as.factor(df$species)
# Convert specificEpithet to a factor
df$species <- 
  as.factor(df$species)
# Convert sex to a factor
df$sex <- 
  as.factor(df$sex)

# @@ Q1 Extract only data for genus "thomomy."
df_thomy <- df %>% filter(genus == 'thomomys')

names(df_thomy)
glimpse(df_thomy)

# @@ Q2 Create a scatterplot of length vs. weight, including a loess and linear regression model smoothing curves
ggplot(data = df_thomy, aes(x = weight, y = length)) + geom_point() + 
  geom_smooth(se = FALSE, aes(colour = 'losses')) + 
  geom_smooth(method='lm', se = FALSE, aes(colour='lm')) +
  scale_colour_manual(name = "legend", values = c("red", "blue"))

# @@ Q3 Determine the correlation between length and width data points  
# <<< seems like not 'width' but 'weight'
cor(df_thomy$length, df_thomy$weight) # 0.8019254

 
# @@ Q4 Determine the correlation between length and the base 10 log of width data points 
# <<< seems like not 'width' but 'weight'
cor(df_thomy$length, log(df_thomy$weight)) # 0.8234998




# @@ Q5 In your script, create a comment area answer and give a rationale ..
# why or why not a transform should or should not be performed on the data.

#########################################################################
# Note : I think log transformation toward column 'weight' is much better than the original

# The shape of original weight vs length plot has logarithmic curve. 
# So to get the better linear model, applying log transformation makes sense.
# The density plot of residual values from the linear model with original weight and length
# has slightly right skewed. And this means the log transformation is applicable.
# But it seems like it will not have such a dramatic difference by the log transformation.

  # original weight vs length plot
ggplot(data=df_thomy, aes(x = weight, y = length)) +
  geom_point() +
  geom_smooth(se=FALSE)

lm_origin <- lm(data = df_thomy, formula=length ~ weight)
lm_log10 <- lm(data = df_thomy, formula=length ~ log(weight))

  # densiti plot of the residual by lm_origin
residual_origin <- resid(lm_origin)
ggplot(data = df_thomy, aes(x = residual_origin)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density()

# As I check both of linear model with the original and with the logged data through summary function..
# Although P value of the intercept from log10 data has less effect on the model 
# than the original's intercept, the overall performance of the log10 model
# is better than the original model.
# Residual Standard Error_origin > RSE_log10 -> log10 win
# R-squared_origin < R-squared_log10 -> log10 win
# F-statistic_origin < F-statistic_log10 -> log19 win
### Thus, the linear model with log transformation has better performance than the origin

# Residual standard error : smaller is better
# F-statistic : bigger is better

  # Statistical Reviews
summary(lm_origin)
summary(lm_log10)


  # Graphical Reviews
p_origin <- ggplot(data = df_thomy, aes(x=weight, y=length)) + geom_point() + 
  geom_smooth(se = FALSE) + 
  geom_smooth(method = 'lm', se = FALSE, color = 'red') +
  ggtitle('origin')
p_log10 <- ggplot(data = df_thomy, aes(x = log(weight), y = length)) + geom_point() + 
  geom_smooth(se = FALSE) + 
  geom_smooth(method ='lm', se = FALSE, color='red') +
  coord_trans(x = "log10") +
  ggtitle('log10')

ggarrange(p_origin, p_log10 + rremove('ylab') + 
            theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()),
          ncol = 2, nrow = 1, align='v')

####################################################################





# @@ Q5 Create a linear model using linear regression on the model on the length 
# and weight (or log10 of weight if justified)

# Since I already created linear model at Q4, I bring the model I have created
# And Since log transformed model has the better performance, I will take the logged one
lm_log10


# @@ 6 Create a set of predicted values of length and add them, 
# along with the linear model smoothing curve, to a scatterplot of the length/width data.
summary(lm_log10) # log(weight) : 47.732, y_intercept : -15.120
df_thomy_pred <- df_thomy %>% 
  mutate(length_pre = coefficients(lm_log10)[1] + coefficients(lm_log10)[2]*log(weight))

ggplot(data = df_thomy, aes(x = log(weight), y = length)) + geom_point() + 
  geom_smooth(method='lm', se=FALSE, size = 2, color='red') +
  ggtitle('log10') +
  geom_point(data = df_thomy_pred, aes(x = log(weight), y = length_pre),
             shape = 17, size = 2, color = 'purple')


# @@ Q6 GRAD STUDENTS ONLY: Review the model statistics. 
# Research how you would report the statistics using an APA format.  
# Write a sentence or two that you would include in a research paper discussing 
# your findings regarding this most interesting research.  Place the sentences 
# at the end of the script in a commented section.
summary(lm_log10)
glance(lm_log10)

# plot applied apa format
ggplot(data = df_thomy, aes(x = log(weight), y = length)) + geom_point() + 
  geom_smooth(method='lm', se=FALSE, size = 2, color='red') +
  ggtitle('log10') +
  geom_point(data = df_thomy_pred, aes(x = log(weight), y = length_pre),
             shape = 17, size = 2, color = 'purple') +
  theme_apa()

# Comment :
# From the result of the linear model,
# I found that there is a significant proportional relationship in between
# the 10 logged weights and the lengths in thomomies, t(657) = 37.21, p < .001.


