###########################################
# Title: Merging the GapMinder and Child Mortality
# for Visualization Practice
# Script Name:
# Name: Youngmin Lee
# Date: Sep 17 2022
# Script purpose: Assignment for chapter 02
#
###########################################

# package
library(tidyr)      # For tidy tools
library(dplyr)      # For the dplyr functions
library(readr)      # For the data importing functions
library(stringr)    # For the string manipulation
library(magrittr)   # For the pipe symbol
library(ggplot2)    # For the plotting functions
library(dslabs)

# file location
gapMinder_loc = './dataset/CSC461GapMinder.csv'
childMortality_loc = './dataset/Child Mortality_0_5_YO_Dying_Per_1000_Born_By_Country_By_Year.csv'

# read files
tb_gapMinder_2020 = read_csv(gapMinder_loc)
tb_childMortality = read_csv(childMortality_loc)

head(tb_gapMinder_2020)
head(tb_childMortality)


# save just in case
tb_childMortality_save <- mutate(tb_childMortality) 

# check CM
names(tb_childMortality)
str(tb_childMortality)


# filter child mortality year == 2020
tb_childMortality_2020 <- tb_childMortality %>%  filter(time == 2020)
tb_childMortality_2020

# remove ``
tb_childMortality_2020 <- tb_childMortality_2020 %>% rename(child_mort = "Child mortality")
tb_childMortality_2020

# rows comparison
tb_gapMinder_2020 # 184
tb_childMortality_2020 # 197

# check NA
sapply(tb_gapMinder_2020, function(x) sum(is.na(x))) # No
sapply(tb_childMortality_2020, function(x) sum(is.na(x))) # No

# Left join to GM
tb_gapMimnder_plus <- tb_gapMinder_2020 %>% left_join(tb_childMortality_2020 %>%
                                                        select(-time, -geo), by = c('name'='name'))
tb_gapMimnder_plus

