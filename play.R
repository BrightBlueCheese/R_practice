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

tb_gapMinderPlus <- drop_na(tb_gapMinderPlus)


p <- ggplot(data = tb_gapMinderPlus)


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
  
  
  my_plot + annotate("text", label = 'mean', angle = 90,
                     x = selected_col_mean * 1.01, y = mean_range_y * 0.8, color = 'red') +
    annotate('text', label='median', angle = 90,
             x = selected_col_median * 1.01, y = mean_range_y * 1.3, color = 'blue')

  
}


density_plotter(4)

density_plotter(5)

