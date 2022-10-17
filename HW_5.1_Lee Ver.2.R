#Topic 05-Cleaning the Data
###########################################
# Title: MW_5.1_Lee Ver.2
# Script Name:  HW_5.1_Lee Ver.2

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

# Yes
tryCatch({rawData %>% 
    chain_start %>%
    assert(within_bounds(1,Inf), weight) %>% # assert checks individual values
    insist(within_n_sds(3), weight) %>% # insist checks against calculated vals
    chain_end
}, warning = function(w) {
  paste("A warning was generated: ", w, sep = "")
}, error = function(e) {
  print(e)
}, finally = {
  print("this is the end of the validation check ...")
}
)


# create a new df and add a index column to track the data
new_df <- mutate(rawData)
new_df <- tibble::rowid_to_column(new_df, 'index')
names(new_df)


# Write Tidy R code (not using assert/insist) to identify records whose length
# and weight are not in a (1, Infinity) range and within 3 SDs

# weight not within 1 and inf
new_df %>% filter((weight < 1) | (weight > Inf)) %>% 
  select(index, weight)

# 3sd and mean of weight
weight_sd_3 <- sd(new_df$weight) * 3
weight_mean <- mean(new_df$weight)

# weight not within 3sd
new_df %>% 
  filter( (weight > weight_mean + weight_sd_3) 
          | (weight < weight_mean - weight_sd_3) ) %>% 
  select(index, weight)


# length not within 1 and inf
new_df %>% filter((length < 1) | (length > Inf)) %>% 
  select(index, length)

# 3sd and mean of length
length_sd_3 <- sd(new_df$length) * 3
length_mean <- mean(new_df$length)

# length not within 3sd
new_df %>% 
  filter((length > length_mean + length_sd_3) 
                  | (length < length_mean - length_sd_3)) %>% 
  select(index, length)

# calmarx <- new_df %>% filter( (weight > weight_mean + weight_sd_3) 
#                               | (weight < weight_mean - weight_sd_3) ) %>% 
#   select(index, weight) %>% mutate(column = 'weight', condition = '1, Inf',
#                                    value = weight)
# calmarx

# YES!
# Now combine those data into a new dataframe
weight_bounds <- new_df %>% filter((weight < 1) | (weight > Inf)) %>% 
  mutate(condition = 'within_(1, Inf)', column = 'weight', value = weight)

weight_3sd <- new_df %>% 
  filter( (weight > weight_mean + weight_sd_3) 
          | (weight < weight_mean - weight_sd_3)) %>% 
  mutate(condition = 'within_3sd', column = 'weight', value = weight)

length_bounds <- new_df %>% filter((length < 1) | (length > Inf)) %>% 
  mutate(condition = 'within_(1, Inf)', column = 'length', value = length)

length_3sd <- new_df %>% 
  filter((length > length_mean + length_sd_3) 
         | (length < length_mean - length_sd_3)) %>% 
  mutate(condition = 'within_3sd', column = 'length', value = length)

# Combining
result_df <- bind_rows(weight_bounds, length_bounds, weight_3sd, length_3sd)
result_df

# To present it better
# And this is the final form!!
# The same number 312 as the assert and insist code produces
final_df <- result_df[c('index', 'condition', 'column', 'value')]
final_df


# Thank you so much :)










# ###################################################################
# ################### OLD VERSION ###################################
# # new tibble to put the Problematic rows
# hw_tibble <- tibble(
#   predicate = character(),
#   column = character(),
#   index = numeric(),
#   value = numeric()
# )
# 
# 
# # Message
# cat("It will take few secs. \nDon't worry, it will not take more than 1. XD \n")
# print("============================")
# 
# # for loop from 1 to the number of rows
# # if statement for each case and if i satisfied the condition,
# # then add that i values into hw_tibble as a new row
# for (i in 1:nrow(rawData)){
#   if (!within_bounds(1,Inf)(as.numeric(rawData[i, 17]))){
#     hw_tibble <- hw_tibble %>%
#       add_row(predicate='within_bounds(1,Inf)',
#               column=names(rawData)[17],index=i, value=as.numeric((rawData[i, 17])))
#   }
#   if (!within_bounds(1,Inf)(as.numeric(rawData[i, 18]))){
#     hw_tibble <- hw_tibble %>%
#       add_row(predicate='within_bounds(1,Inf)',
#               column=names(rawData)[18],index=i, value=as.numeric((rawData[i, 18])))
#   }
#   if (!within_n_sds(3)(unlist(as.vector(rawData[17])))(as.numeric(rawData[i, 17]))){
#     hw_tibble <- hw_tibble %>%
#       add_row(predicate='within_n_sds(3)', column=names(rawData)[17], index=i,
#               value=as.numeric(rawData[i, 17]))
#   }
#   if (!within_n_sds(3)(unlist(as.vector(rawData[18])))(as.numeric(rawData[i, 18]))){
#     hw_tibble <- hw_tibble %>%
#       add_row(predicate='within_n_sds(3)', column=names(rawData)[18], index=i,
#               value=as.numeric(rawData[i, 18]))
#   }
# 
# }
# 
# # Set the level of 'predicate' and 'column' columns to be able to sort them
# # by the user-defined level
# hw_tibble$predicate <-
#   factor(hw_tibble$predicate, levels= c('within_bounds(1,Inf)', 'within_n_sds(3)'))
# hw_tibble$column <-
#   factor(hw_tibble$column, levels = c('weight', 'length'))
# 
# print(sprintf('Total Errors: %d', nrow(hw_tibble)))
# 
# # showing the result ordered by the level i sat and index num
# hw_tibble[order(hw_tibble$predicate, hw_tibble$column),]
# 
# ################### OLD VERSION ###################################
# ###################################################################