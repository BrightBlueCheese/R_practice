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
library(data.table) # For finding intersect between tables

# file location
gapMinder_loc = './dataset/CSC461GapMinder.csv'
childMortality_loc = './dataset/Child Mortality_0_5_YO_Dying_Per_1000_Born_By_Country_By_Year.csv'
totalFertility_loc = './dataset/tfr-by-gapminder.csv' 
    # https://www.gapminder.org/data/documentation/gd008/
worldPopulation = './dataset/GM-Population - Dataset - v6 - data-for-countries-etc-by-year.csv' 
    # https://www.gapminder.org/data/documentation/gd003/

# read files
tb_gapMinder_2020 = read_csv(gapMinder_loc)
tb_childMortality = read_csv(childMortality_loc)
tb_totalFertility = read_csv(totalFertility_loc)
tb_population = read_csv(worldPopulation)

head(tb_gapMinder_2020)
head(tb_childMortality)
head(tb_totalFertility)
head(tb_population)


# save just in case
tb_childMortality_save <- mutate(tb_childMortality) 

# check CM
names(tb_childMortality)
str(tb_childMortality)


  # The Mortality
# filter child mortality year == 2020
tb_childMortality_2020 <- tb_childMortality %>%  filter(time == 2020)
tb_childMortality_2020

# change column name - child mortality
tb_childMortality_2020 <- tb_childMortality_2020 %>% rename(child_mort = "Child mortality")
tb_childMortality_2020

  # The Fertility
# select country names and fertility in 2020
tb_totalFertility_2020 <- tb_totalFertility %>% select(geo.name, '2020')

# change column names - country names, fertility in 2020
tb_totalFertility_2020 <- tb_totalFertility_2020 %>% rename(c(name="geo.name",
                                                              fertility='2020'))
tb_totalFertility_2020


  # The Population
# filter population time == 2020 and select the columns country names and population
tb_population_2020 <- tb_population %>% filter(time == 2020) %>% select(name, Population)
tb_population_2020

# change column name Population to population
tb_population_2020 <- tb_population_2020 %>% rename(population=Population)
tb_population_2020


# rows comparison
tb_gapMinder_2020 # 184
tb_childMortality_2020 # 197
tb_totalFertility_2020 # 275
tb_population_2020 # 197

# check NA
sapply(tb_gapMinder_2020, function(x) sum(is.na(x))) # No
sapply(tb_childMortality_2020, function(x) sum(is.na(x))) # No
sapply(tb_totalFertility_2020, function(x) sum(is.na(x))) # 74 NAs out of 275 rows
sapply(tb_population_2020, function(x) sum(is.na(x))) # No


# Delete the rows contain NA
tb_totalFertility_2020 <- na.omit(tb_totalFertility_2020)
tb_totalFertility_2020

  # Check intersect among the tibbles
# check how many common rows between the gapminder and the mortality
length(intersect(tb_gapMinder_2020$name, tb_childMortality_2020$name)) # total 184 intersects
      # Which means the the mortality table contains the whole countries that the gapminder has

# check how many common rows between the gapminder and the fertility
length(intersect(tb_gapMinder_2020$name, tb_totalFertility_2020$name)) # total 184
      # Good to go

# check how many common rows between the gapminder and the population
length(intersect(tb_gapMinder_2020$name, tb_population_2020$name)) # total 184
      # Good to go

# check how many common rows between the mortality and the population
length(intersect(tb_childMortality_2020$name, tb_population_2020$name)) # total 197
      # Awesome!
      ## Since the fertility and population in 2020 share the whole countries as intersect,
      ## It is reasonable to keep these whole 197 data into the gapminder.
      ## We could produce some meaning full result from the mortality and the population.
      ## While the fertility dataset has too many rows which are not in common to the others.
      ## So, we don't need the rest of the data except those in common
      
 


  # Merging tibbles
# Right join GM and CM
tb_gapMinder_plus <- tb_gapMinder_2020 %>% right_join(tb_childMortality_2020 %>%
                                                        select(-time, -geo), by = c('name'='name'))
tb_gapMinder_plus

# save the gapminder before adding the fertility
tb_gapMinder_plus_save <- tb_gapMinder_plus %>% mutate(tb_gapMinder_plus)
tb_gapMinder_plus_save

# Left join GM and TF(total fertility)
tb_gapMinder_plus <- tb_gapMinder_plus %>% left_join(tb_totalFertility_2020, by=c('name'='name'))
tb_gapMinder_plus

# save the gapminder before adding the population
tb_gapMinder_plus_save2 <- mutate(tb_gapMinder_plus)
tb_gapMinder_plus_save2

# Inner join GM and Pp(Population) - (actually, either left or right or inner are good)
tb_gapMinder_plus <- tb_gapMinder_plus %>% inner_join(tb_population_2020, by=('name'='name'))
tb_gapMinder_plus

# save the gapminder ultimate 
tb_gapMinder_plus_save_ultimate <- mutate(tb_gapMinder_plus)



# write gapminder plus csv file
write_csv(tb_gapMinder_plus, './dataset/CSC461GapMinderPlus.csv')



  # For fun
p <- ggplot(data = tb_gapMinder_plus)

# plotting the population and the mortality
p + geom_point(aes(population/10^5, child_mort)) # very less correlation between two

p + geom_boxplot(aes(population/10^5, child_mort))


# plotting the fertility and the mortality
p + geom_point(aes(fertility, child_mort)) # stronger correlation compare to Popul vs Mortal

# Income vs life exp
p + geom_point(aes(income, life_exp, colour = factor(region))) # inversely exponential

tb_gapMinder_plus

# Income vs Fertility
p + geom_point(aes(income, fertility, colour = factor(region))) # negatively exponential

# Income vs Mort
p + geom_point(aes(income, child_mort, colour = factor(region))) # negatively exponential

# Popul vs life exp
p + geom_point(aes(population, life_exp, colour = factor(region))) # very less correlation
