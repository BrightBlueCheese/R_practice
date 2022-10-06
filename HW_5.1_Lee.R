#Topic 05-Cleaning the Data
###########################################
# Title: MW_5.1_Lee 
# Script Name:  HW_5.1_Lee

# Name: Youngmin Lee
# Date: Oct 17 2022
# Script purpose: Class Homework for 5.1
# 
###########################################


### Load the packages
library(tidyverse) # For tidy tools
library(mice)      # For the mice functions
library(assertr)   # For the assert functions
library(leaflet)   # For the leaflet functions
library(sp)        # For the sp functions
library(stringdist)

#import the full rodent data dataset
rawData <- read_csv("./dataset/rodentData_clean.csv")


# Some example code to use assert/insist to identify problems
tryCatch({rawData %>% 
    chain_start %>%
    assert(within_bounds(1,Inf), weight) %>% # assert checks individual values
    assert(within_bounds(1,Inf), length) %>%
    insist(within_n_sds(3), weight) %>% # insist checks against calculated vals
    insist(within_n_sds(3), length) %>%
    chain_end
}, warning = function(w) {
  paste("A warning was generated: ", w, sep = "")
}, error = function(e) {
  print(e)
}, finally = {
  print("this is the end of the validation check ...")
}
)


##############################################################################
## HW5 Here ##################################################################
## Copy and modify the assert/insist to find the errors in the weight data

# new tibble to put the Problematic rows
hw_tibble <- tibble(
  predicate = character(),
  column = character(),
  index = numeric(),
  value = numeric()
)


# Message
cat("It will take few secs. \nDon't worry, it will not take more than 1. XD \n")
print("============================")

# for loop from 1 to the number of rows
# if statement for each case and if i satisfied the condition,
# then add that i values into hw_tibble as a new row
for (i in 1:nrow(rawData)){
  if (!within_bounds(1,Inf)(as.numeric(rawData[i, 17]))){
    hw_tibble <- hw_tibble %>% 
      add_row(predicate='within_bounds(1,Inf)',
              column=names(rawData)[17],index=i, value=as.numeric((rawData[i, 17])))
  }
  if (!within_bounds(1,Inf)(as.numeric(rawData[i, 18]))){
    hw_tibble <- hw_tibble %>% 
      add_row(predicate='within_bounds(1,Inf)',
              column=names(rawData)[18],index=i, value=as.numeric((rawData[i, 18])))
  }
  if (!within_n_sds(3)(unlist(as.vector(rawData[17])))(as.numeric(rawData[i, 17]))){
    hw_tibble <- hw_tibble %>% 
      add_row(predicate='within_n_sds(3)', column=names(rawData)[17], index=i,
              value=as.numeric(rawData[i, 17]))
  }
  if (!within_n_sds(3)(unlist(as.vector(rawData[18])))(as.numeric(rawData[i, 18]))){
    hw_tibble <- hw_tibble %>% 
      add_row(predicate='within_n_sds(3)', column=names(rawData)[18], index=i,
              value=as.numeric(rawData[i, 18]))
  }
  
}

# Set the level of 'predicate' and 'column' columns to be able to sort them
# by the user-defined level
hw_tibble$predicate <- 
  factor(hw_tibble$predicate, levels= c('within_bounds(1,Inf)', 'within_n_sds(3)'))
hw_tibble$column <- 
  factor(hw_tibble$column, levels = c('weight', 'length'))

print(sprintf('Total Errors: %d', nrow(hw_tibble)))

# showing the result ordered by the level i sat and index num
hw_tibble[order(hw_tibble$predicate, hw_tibble$column),]

