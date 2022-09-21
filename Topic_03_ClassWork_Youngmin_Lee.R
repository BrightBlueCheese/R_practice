###########################################
# Title: Visualizing the Gapminder Data File 
# Script Name: 
# Name: <Youngmin Lee>
# Date: <Sep 16 2022>
# Script purpose:
# 
###########################################

### Install required packages if not installed
# install.packages("tidyverse")

### Load the packages
library(tidyr)      # For tidy tools
library(dplyr)      # For the dplyr functions
library(readr)      # For the importing functions
library(stringr)    # For the string manipulation
library(magrittr)   # For the pipe symbol
library(ggplot2)    # For the plotting functions

##############################################################3
# Define all the needed files 
# Define the full path and file name for the geographic data
# gapminder_file <- "./dataset/CSC461GapMinder.csv" #home
gapminder_file <- "./dataset/CSC461GapMinder.csv" #home


# Open and read the file into a tibble
tb_gapminder = read_csv(gapminder_file)

# Initial Data Analysis (IDA) for geo data
glimpse(tb_gapminder)
summary(tb_gapminder)

# calculate mean(), median(),  for all data
tb_gapminder %>%
  summarize(mean_life_exp = mean(life_exp), median_life_exp = median(life_exp), 
            mean_income = mean(income), median_income = median(income))

mean(tb_gapminder$life_exp)
median(tb_gapminder$life_exp)
mean(tb_gapminder$income)
median(tb_gapminder$income)

# calculate mean(), median(), by region
tb_gapminder %>%
  group_by(region) %>%
  summarize(mean_life_exp = mean(life_exp), median_life_exp = median(life_exp), 
            mean_income = mean(income), median_income = median(income))

# calculate variance, std. dev., and quartiles
range(tb_gapminder$life_exp)
var(tb_gapminder$life_exp)
sd(tb_gapminder$life_exp)

quantile(tb_gapminder$life_exp)

tb_gapminder %>%
  group_by(region) %>%
  summarize(qnt_life_exp_25 = quantile(life_exp, probs = .25),
            qnt_life_exp_50 = quantile(life_exp, probs = .5),
            qnt_life_exp_75 = quantile(life_exp, probs = .75))

# Factors and categorical data
summary(tb_gapminder)
table(tb_gapminder)

# make region a nominal(categorical) factor
tb_gapminder$region <- 
  as.factor(tb_gapminder$region)

# make region an nominal factor (explicit levels)
tb_gapminder$region <- 
  factor(tb_gapminder$region, ordered = FALSE, levels = c("africa", "americas", "asia", "europe"))

# make region an ordinal factor
# tb_gapminder$region <-
# factor(tb_gapminder$region, ordered = TRUE, levels = c("europe", "africa", "americas", "asia"))

#Now more data shows up with summary
summary(tb_gapminder)
levels(tb_gapminder$region)
tb_gapminder$region[1] > tb_gapminder$region[2]

#Plotting a histogram of life expectancy data
ggplot(tb_gapminder, aes(x = life_exp)) + 
  geom_histogram(binwidth = 5)

# or bins
ggplot(tb_gapminder, aes(x = life_exp)) + 
  geom_histogram(bins = 12)

#Plot a density histogram of life expectancy data with curve
ggplot(tb_gapminder, aes(x = life_exp)) + 
  geom_histogram(aes(y = ..density..), binwidth = 5) +
  geom_density(col = "red")

## Generate a histogram for income!
p <- ggplot(data=tb_gapminder, aes(x=income))
p + geom_histogram(aes(y=..density..), bins = 20) + geom_density(col="red")


#Generate density plot
  # vline = vertical line
ggplot(tb_gapminder, aes(x = life_exp)) + 
  geom_density(fill = 'lightblue') +
  geom_vline(aes(xintercept=mean(life_exp)),
     color="blue", linetype="dashed", size=1)

#Generate density plot by region
ggplot(tb_gapminder, aes(x = life_exp, fill = region)) + 
  geom_density(alpha = 0.5) #alpha = transparency

#Generate density plot by region (bonus)
# Create group means
region_means <- tb_gapminder %>%
  group_by(region) %>%
  summarize(reg_mean = mean(life_exp))

ggplot(tb_gapminder, aes(x = life_exp, fill = region)) + 
  geom_density(aes(alpha = .25)) +
  geom_vline(data = region_means, mapping = aes(xintercept = reg_mean, color = region),
              linetype="dashed", size = 1)

# Generate boxplots
ggplot(tb_gapminder, aes(x='Age', y=life_exp)) + geom_boxplot(outlier.color = "Red")
boxplot.stats(tb_gapminder$life_exp)$out


head(tb_gapminder)
#calculate quantile again
quantile(tb_gapminder$life_exp)

tb_gapminder_SKorea <- tb_gapminder %>% filter(name=="South Korea")


head(tb_gapminder)
# Generate a boxplot
ggplot(tb_gapminder, aes(x = "Age", y = life_exp)) +
  geom_boxplot(outlier.color = "red") +
  geom_point(alpha = .25)


#Finding Outliers
iqr <- quantile(tb_gapminder$life_exp, 0.75) - quantile(tb_gapminder$life_exp, 0.25)
lower_threshold <- quantile(tb_gapminder$life_exp, 0.25) - 1.5 * iqr
upper_threshold<- quantile(tb_gapminder$life_exp, 0.75) + 1.5 * iqr
tb_gapminder %>% filter(life_exp < lower_threshold | life_exp > upper_threshold ) 
IQR(tb_gapminder$life_exp)
boxplot.stats(tb_gapminder$life_exp)$out

#Boxplot by region
ggplot(tb_gapminder, aes(x = region, y = life_exp)) +
  geom_boxplot(outlier.color = "red") +
  geom_jitter(position = position_jitter(width = 0.1, height = 0), alpha = .25)

# Generate a bar chart of life exp obs counts
tb_gapminder %>% 
  ggplot(aes(x = region)) +
  geom_bar()

names(tb_gapminder)

# n() gives the current group size.
tb_gapminder %>%
  group_by(region) %>%
  summarize(obscount = n()) 

tb_gapminder %>%
  group_by(region) %>%
  summarize(obscount = n()) %>%
  ggplot(aes(x = region, y= obscount)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = obscount), vjust = -0.3) + 
  theme_classic()

###Scatterplots ####
### Generate a scatter plot
ggplot(tb_gapminder, aes(x = income, y=life_exp)) + 
  geom_point() 

tb_gapminder %>% filter(income == max(tb_gapminder$income))

### Scatter plot life exp vs. income, color by region
ggplot(tb_gapminder, aes(x = income, y=life_exp, color = region)) + 
  geom_point() 
  
### Scatter plot life exp vs. income, color by region
ggplot(tb_gapminder, aes(x = income, y=life_exp, color = region)) + 
  geom_point() +
  facet_grid(~region)

#histograms to see change in spread
ggplot(tb_gapminder, aes(x = income)) + 
  geom_histogram(bins = 12)

ggplot(tb_gapminder, aes(x = log10(income))) + 
  geom_histogram(bins = 12)

### Generate a scatter plot
ggplot(tb_gapminder, aes(x = log10(income), y=life_exp)) + 
  geom_point() +
  scale_x_log10() +
  geom_smooth(se = FALSE)

ggplot(tb_gapminder, aes(x = log10(income), y=life_exp)) + 
  geom_point() +
  geom_smooth(se = FALSE)

# Create a log-transformed income variable
tb_gapminder = tb_gapminder %>% mutate(income_log = log(income))

names(tb_gapminder)

### Generate a scatter plot
ggplot(tb_gapminder, aes(x = income_log, y=life_exp)) + 
  geom_point() +
  scale_x_log10() +
  geom_smooth(method = "lm", se = FALSE) 

### Scatter plot life exp vs. income, color by region
ggplot(tb_gapminder, aes(x = income_log, y=life_exp, color=region)) + 
  geom_point() +
  scale_x_log10() +
  geom_smooth(method = "lm", se = FALSE) 

# Find the correlation - higher correlation is better
cor(tb_gapminder$income, tb_gapminder$life_exp)
cor(tb_gapminder$income_log, tb_gapminder$life_exp)

cor(tb_gapminder$income_log, tb_gapminder$income)
