###########################################
# Title: Building the Gapminder Data File
# for Visualization Practice
# Script Name:
# Name: Youngmin Lee
# Date: Sep 07 2022
# Script purpose: Assignment for chapter 02
#
###########################################

### Install required packages if not installed
# install.packages("tidyverse")

### Load the packages
library(tidyr)      # For tidy tools
library(dplyr)      # For the dplyr functions
library(readr)      # For the data importing functions
library(stringr)    # For the string manipulation
library(magrittr)   # For the pipe symbol
library(ggplot2)    # For the plotting functions

###########################################################
library(dslabs)
data(murders)
tb_murder <- as_tibble(murders)

glimpse(tb_murder)
str(tb_murder)
tb_murder <- tb_murder %>% mutate(rate=total/population*100000)
tb_murder
## the above and the below are the same
tb_murder <- tb_murder %>% mutate(tb_murder, rate=total/population*100000)
tb_murder

tb_murder <- as_tibble(murders)
tb_murder <- tb_murder %>% transmute(state, abb, rate=total/population*100000)
tb_murder
 
tb_murder <- as_tibble(murders)
tb_murder <- tb_murder %>% 
  mutate(tb_murder, rate=total/population*100000) %>% filter(rate <= 0.6)
tb_murder

tb_murder <- as_tibble(murders)
tb_murder <- tb_murder %>% 
  mutate(tb_murder, rate=total/population*100000)
filter(tb_murder, rate <= 0.6)

filter(tb_murder, rate <= 0.6) %>% select(1:2, rate) %>% arrange(rate, abb) %>% 
  top_n(-5, rate)
select(tb_murder, -region, -population, -total)

tb_murder <- as_tibble(murders)
tb_murder <- tb_murder %>% 
  mutate(tb_murder, rate=total/population*100000)
tb_murder
tb_murder %>% arrange(rate, abb) %>% top_n(1, rate)
View(tb_murder)
#

##############################################################3
# Define all the needed files
# Define the full path and file name for the geographic data
# For Windows
#geo_file <- "C://Users//[your user name]//CSC461//gapminder//ddf--entities--geo--country.csv"
# For Mac
geo_file <- "./R_practice/dataset/Geo_Country_Data.csv"
geo_file_abs <- 'C:/Users/ymlee/OneDrive2/OneDrive/R_studio/R_practice/dataset/Geo_Country_Data.csv'


# Add life expectancy
life_exp_file <- "./R_practice/dataset/Life_Expectancy_by_Country_By_Year_Data.csv"
life_exp_file_abs <- "C:/Users/ymlee/OneDrive2/OneDrive/R_studio/R_practice/dataset/Life_Expectancy_by_Country_By_Year_Data.csv"

# Add income per person
income_file <- "./R_practice/dataset/Income_Per_Person_Per_Year_By_Country.csv"

# Open and read the file into a tibble
tb_geo = as_tibble(geo_file)
tb_geo = read_csv(geo_file)
tb_geo = read_csv(geo_file_abs) # the same

# Initial Data Analysis (IDA) for geo data
glimpse(tb_geo)
summary(tb_geo)

tb_geo

names(tb_geo)

sum(is.na(tb_geo[2]))


#Look for missing values
NAs_found = FALSE
for (i in names(tb_geo)){
  # sum_NA <- 0
  if(sum(is.na(tb_geo[i]))) {
    # sum_NA <- sum_NA + 1
    sprint(print(paste(i," has %d NA's")))
    # sprintf("%s has %d NA's", i, sum_NA)
    NAs_found = TRUE
  }
}
if(!NAs_found) {
  print("No NAs found")
}

# Custom Function
NAs_found2 = FALSE
for (i in names(tb_geo)){
  # print(i)
  if (sum(is.na(tb_geo[i]))) {
    print(sprintf("%s has %d NA's", i, sum(is.na(tb_geo[i]))))
    NAs_found2 = TRUE
  } else if (!NAs_found) {
    print(sprintf("No NAs found with column name %s", i))
  }
}


sapply(tb_geo, function(x) sum(is.na(x)))

# Extract country "name" and "world_4region"
tb_gapminder <- tb_geo %>% select(name, world_4region)
length(tb_geo)
tb_gapminder

# Open and read the life expectancy file
tb_life_exp = read_csv(life_exp_file)

# IDA for life_exp
glimpse(tb_life_exp)
View(tb_life_exp)

#Look for missing values
NAs_found = FALSE
for (i in names(tb_life_exp)){
  if(sum(is.na(tb_life_exp[i]))) {
    print(paste(i," has NA's"))
    NAs_found = TRUE
  }
}
if(!NAs_found) {
  print("No NAs found")
}

#View(tb_life_exp)

# trim down life_exp columns and observations
tb_life_exp <- tb_life_exp %>% select(name, time, 'Life expectancy') %>% 
  filter(time == 2020)

# How to join them?
glimpse(tb_life_exp)
glimpse(tb_gapminder)

# Join tb_life_exp:Life.expectancy to tb_gapminder by country name
tb_gapminder_joined <- tb_gapminder %>% inner_join(tb_life_exp, tb_gapminder, by='name')
tb_gapminder_joined2 <- tb_life_exp %>% inner_join(tb_gapminder, tb_life_exp, by='name')

tb_gapminder_joined
tb_gapminder_joined2



# What was lost?
tb_life_exp %>% anti_join(tb_geo, by='name')
tb_life_exp
tb_geo %>% anti_join(life_exp_file, by='name')
tb_geo

# Add income per person data
# Open and read the file
tb_income = read_csv(income_file)

# IDA for geo - is it tidy data?
glimpse(tb_income)
#View(tb_income)

names(tb_income)

tb_income_original <- mutate(tb_income)

#Look for missing values
NAs_found = FALSE
for (i in names(tb_income)){
  if(sum(is.na(tb_income[i]))) {
    print(paste(i," has NA's"))
    NAs_found = TRUE
  }
}
if(!NAs_found) {
  print("No NAs found")
}

#Gather the columns into rows
tb_income <- tb_income_original %>% gather(key=year, value=income, -country)
tb_income

#Filter down to just data for 2020
tb_income_2020 <- tb_income %>% filter(year==2020)
tb_income_2020

#Examine the resulting dataset
glimpse(tb_income)
head(tb_income)
tail(tb_income)

#Fix the income values!
index_vector <- grep("k", tb_income$income)
for (i in index_vector) {
  tb_income$income[i] <- str_remove(tb_income$income[i], "k")
  if (str_detect(tb_income$income[i],"\\.")) {
    tb_income$income[i] <- paste(tb_income$income[i], "00", sep = "")
    tb_income$income[i] <- str_remove(tb_income$income[i], "\\.")
  } else {
    tb_income$income[i] <- paste(tb_income$income[i], "000", sep = "")
  }
}

tb_income$income <- as.numeric(tb_income$income)

#Examine the resulting dataset
glimpse(tb_income)
head(tb_income)
tail(tb_income)

#join income data to tb_gapminder
tb_gapminder <- tb_gapminder


#what got lost again?
tb_gapminder %>%
  anti_join(tb_income, by = c("name" = "country"))
tb_income %>%
  anti_join(tb_gapminder, by = c("country" = "name"))

# rename region and life_exp
tb_gapminder <- tb_gapminder %>%
  rename(region = world_4region) %>%
  rename(life_exp = "Life expectancy") %>%
  rename(year = "time")

# A final look at the data
glimpse(tb_gapminder)

# Save the data to disk for later use
write_csv(tb_gapminder, "/Users/josborne/ORU/CSC461 Data Mining and Machine Learning/DataSets/GapMinder/CSC461GapMinder.csv")

##############################################
