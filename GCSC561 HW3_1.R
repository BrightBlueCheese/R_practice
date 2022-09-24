###########################################
# Title: Digging into the Data
# Script Name: GCSC561 HW3.1
# Name: Youngmin Lee
# Date: Sep 27 2022
# Script purpose: Assignment for chapter 03
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
# install.packages('reshape')
library(reshape)    # For melting tibble to be able to draw multiple columns density plot

  # @@Loads the CSC461GapMinderPlus.csv data
file_path = './dataset/CSC461GapMinderPlus.csv'
tb_gapMinderPlus = read_csv(file_path)

names(tb_gapMinderPlus)
sapply(tb_gapMinderPlus, function(x) sum(is.na(x)))

  # @@Calculate the mean, median, and standard deviation for the data - A
tb_gapMinderPlus %>% 
  summarize(mean_life_exp = mean(life_exp, na.rm=TRUE), median_life_exp = median(life_exp, na.rm=TRUE), 
            std_life_exp = sd(life_exp, na.rm=TRUE), mean_income = mean(income, na.rm=TRUE),
            median_income = median(income, na.rm=TRUE), std_income = sd(income, na.rm=TRUE),
            mean_child_mort = mean(child_mort), median_child_mort = median(child_mort),
            std_child_mort = sd(child_mort), mean_fertility = mean(fertility, na.rm=TRUE),
            median_fertility = median(fertility, na.rm=TRUE), std_fertility = sd(fertility, na.rm=TRUE),
            mean_population = mean(population), median_population = median(population),
            std_population = sd(population))


# To group by region, the gapMinderPlus has some rows which contains NA in region and some other
# columns. So drop NA for grouping

  # @@Calculate the mean, median, and standard deviation for the data - B
tb_gapMinderPlus %>% filter(! is.na(region)) %>% 
  summarize_at(.vars = names(.)[4:8], .funs = c(mean="mean", median="median", std="sd"))

  # @@Calculate the mean, median, and standard deviation by region for the data
tb_gapMinderPlus_summary_region <- tb_gapMinderPlus %>% filter(! is.na(region)) %>% group_by(region) %>% 
  summarize_at(.vars = names(.)[4:8], .funs = c(mean="mean", median="median", std="sd"))

tb_gapMinderPlus_summary_region


# Transpose the tibble summarized by region to see better
tb_gapMinderPlus_summary_region_T <- tb_gapMinderPlus_summary_region %>% 
  pivot_longer(cols = -region, names_to = 'Region') %>% 
  pivot_wider(names_from = region, values_from = value)

tb_gapMinderPlus_summary_region_T

  # @@Place a comment in the script summarizing what these values indicate

# The mean and the median of life_exp of each region does not have huge gaps
# The mean and the median in between Asia's child_mort is about 5,
#  and this is a significant gap comparing to the other region
# Bigger the income, then longer the life_exp, lower the child_mort, and lower the fertility
# Seems like population does not affect those four factors significantly


  #  @@Generate a density plot for the data that includes a vertical line marking 
  # the mean and another vertical line marking the media

# since I already compute mean, median, std with droping NAs, I will keep this
tb_gapMinderPlus <- drop_na(tb_gapMinderPlus)
tb_gapMinderPlus

# define the basement for the plot
p <- ggplot(data = tb_gapMinderPlus)

# Function to create the plot
density_plotter <- function(list_index){
  
  # variable to put into the geom_density x parameter - character
  selected_col <- names(tb_gapMinderPlus)[list_index]
  # variable to put into mean vline - numeric
  selected_col_mean <- tb_gapMinderPlus %>% pull(selected_col) %>% mean()
  # variable to put into median vline - numeric
  selected_col_median <- tb_gapMinderPlus %>% pull(selected_col) %>% median()
  
  # plot the density function
  my_plot <- p + geom_density(aes(x = !!sym(selected_col)), color='black', size=1) + 
    geom_vline(aes(xintercept = selected_col_mean), color = 'red', size = 1) +
    geom_vline(aes(xintercept = selected_col_median), color = 'blue', 
               size = 1, linetype = 'dashed') +
    scale_color_manual(values = c("mean" = "red", "median" = "blue"))
  
  # y location setter for annotation
  mean_range_y <- mean(ggplot_build(my_plot)$layout$panel_scales_y[[1]]$range$range)
  
  
  my_plot + annotate("text", label = paste0('mean ', round(selected_col_mean, 1)), angle = 90,
                     x = selected_col_mean * 1.03, y = mean_range_y * 0.6, color = 'red') +
    annotate('text', label=paste0('median ', round(selected_col_median, 1)), angle = 90,
             x = selected_col_median * 1.03, y = mean_range_y * 1.5, color = 'blue')
  
  
}

names(tb_gapMinderPlus)

  # @life_exp
density_plotter(4)

  # @income
density_plotter(5)

  # @child_mort
density_plotter(6)

  # @fertility
density_plotter(7)

  # @population
density_plotter(8)


  # @@Place a comment in the script discussing where these values lie in relation to the shape of the curve

# When the density curve is right-skewed like the case of income, child_mort, fertility, and population :
# the median is smaller than the mean value.
# When the curve is left-skewed in the case of life_exp : the median is greater than the mean value.
# This trend is only valid for this case. Not for every case.


  # @@Create a density plot of the chosen vector by region
density_plotter_region <- function(list_index){
  
  # variable to put into the geom_density x parameter - character
  selected_col <- names(tb_gapMinderPlus)[list_index]
  # variable to put into mean vline - numeric
  selected_col_mean <- tb_gapMinderPlus %>% pull(selected_col) %>% mean()
  # variable to put into median vline - numeric
  selected_col_median <- tb_gapMinderPlus %>% pull(selected_col) %>% median()
  # variable to put into colour - group : character ; it does not have to start with selected_
  # but due to the consistency..
  selected_region <- tb_gapMinderPlus %>% pull(region)
  # mean value vectors of each column
  selected_col_mean_vec <- tb_gapMinderPlus %>% group_by(region) %>% 
    summarize(mean = mean(!!sym(selected_col)))
  # median value vectors of each colmn
  selected_col_median_vec <- tb_gapMinderPlus %>% group_by(region) %>% 
    summarize(median = median(!!sym(selected_col)))
  
  # plot the density function
  my_plot <- p + geom_density(aes(x = !!sym(selected_col), colour = selected_region), size = 1) +
                                      # color -- get the unique value of region and sort by alphabetic order
    geom_vline(data = selected_col_mean_vec, aes(xintercept = mean, color = sort(unique(selected_region)))) +
    geom_vline(data = selected_col_median_vec, aes(xintercept = median, color = sort(unique(selected_region))), 
               linetype = 'dashed')

  
  # y location setter for annotation
  mean_range_y <- mean(ggplot_build(my_plot)$layout$panel_scales_y[[1]]$range$range)
  
  
  my_plot
  
}

# debug(density_plotter_region)


  # @life_exp_region
density_plotter_region(4)

  # @income_region
density_plotter_region(5)

  # @child_mort_region
density_plotter_region(6)

  # @fertility_region
density_plotter_region(7)

  # @population_region
density_plotter_region(8)


  # @@ Create a plot of boxplots by region which includes data points and highlighted outliers
box_plotter <- function(list_index){
  
  # variable to put into the geom_density x parameter - character
  selected_col <- names(tb_gapMinderPlus)[list_index]
  # variable to put into colour - group : character ; it does not have to start with selected_
  # but due to the consistency..
  selected_region <- tb_gapMinderPlus %>% pull(region)
  
  p + geom_boxplot(aes(x = selected_region, y = !!sym(selected_col), fill = selected_region),
                   outlier.color = "red") +
    scale_fill_brewer(palette = "Spectral")
}

# 
box_plotter(4)
