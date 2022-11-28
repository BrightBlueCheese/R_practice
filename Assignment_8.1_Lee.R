###########################################
# Title: Assignment_8.1_Lee
# Script Name: 
# Name: <your input>
# Date: <your input>
# Script purpose:
# 
###########################################
# packages
library(tidyverse)      # For tidy tools
library(tidymodels)
library(patchwork)

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
spliter <- initial_split(df3, prop = 0.75, strata = default) 

df3_train <- spliter %>% training()
df3_test <- spliter %>% testing()

nrow(df3_train)
nrow(df3_test)
nrow(df3_train) + nrow(df3_test) == nrow(df3)
