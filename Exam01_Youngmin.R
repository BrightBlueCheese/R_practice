###########################################
# Title: Exam 1 R Script 
# Name: Youngmin Lee
# Date: Oct 07, 2022
###########################################

### Install required packages if not installed


### Load the required libraries
library(readr)
library(tidyverse)
library(ggplot2)
library(tidyr)

##############################################################
# Define all the needed files 
# Define the full path and file name for data being used
student_df = read_csv('./dataset/StudentsPerformance.csv')

# Using the variable just defined, open and read the StudentsPerformance.csv 
# file into a tibble
student_df

# Perform Initial Data Analysis (IDA) - what command(s) can you use to 
# take an initial look at the data
summary(student_df)
glimpse(student_df)
head(student_df)
sum(is.na(student_df))
# view(student_df)

# Make a note of any thing you see in the data that might be of concern
# NOTES: There is a record that a student got 0 on the math score
# And there is no NA value in this data
# In a column name 'parental level of education', values named 'some college'
# is very ambiguous

# Is the data you are working with considered "tidy"?  Why or why not?
# This data is a tidy because 1) Each variable is saved into single column
# 2) Each observation of variable is save into each individual rows


# Check for NAs in the dataset
print(sprintf('There are %s columns', length(names(student_df))))
for (i in names(student_df)) {
  print(sprintf('The column %s has %d NA(s)', i, sum(is.na(i))))
}

# NOTES: There are 8 columns in this data, and there is no NA at all
#
#

# Review/evaluate/modify each vector in dataset as needed
# view(student_df)

# Rename all vectors to change spaces to underscores
# gender
  # key : forgot to factor the gender and race/asdsad
student_df$gender <- as.factor(student_df$gender)
# race/ethnicity

# parental education

# etc., etc.
names(student_df)
student_df <- student_df %>% 
  rename("parantal_level_of_education" = "parental level of education") %>% 
  rename("test_preparation_course" = "test preparation course") %>% 
  rename("math_score" = "math score") %>% 
  rename('reading_score' = 'reading score') %>% 
  rename('writing_score' = 'writing score')
names(student_df)



# Add a new vector called "average_score" that holds the average test score,
# defined as the simple average of the math, writing, and reading scores
student_df <- student_df %>% 
  mutate(average_score = (math_score+reading_score+writing_score)/3)
head(student_df)
# Review dataset again with summary()
summary(student_df)


# Plotting a histograms of math, reading, and writing scores
# Comment on actions taken and create NOTES: section for each
# to record your analysis of what you find
p <- ggplot(data = student_df)
p + geom_histogram(aes(x = math_score), bins = 10)

p + geom_histogram(aes(x = reading_score), bins = 10)

p + geom_histogram(aes(x = writing_score), bins = 10)


# NOTES:
# The math, and reading scores are normally distributed.
# While writing score has a little rightly skewed 

# The data provider has let us know that if a student was
# absent for the exams, scores of zero were placed in observations
# Write code to REMOVE any observations from the dataset
# that reflect scores of ZERO for ALL tests for the student
# and determine how many records were lost due to this

# Remove rows that have all zeros for tests
student_df
student_df <- student_df %>% 
  filter(math_score != 0) %>% filter(reading_score != 0) %>% filter(writing_score != 0)

student_df

#Note : There was one row that contained 0 and it was removed
# 1000 rows -> 999 rows

# Create a bar graph to reflect the number of males/females
# since you will need it for your report later
student_df %>% 
  ggplot(aes(x=gender)) + geom_bar()
p + geom_bar(aes(x=gender))

# Generate a boxplot of the average scores with outliers shown on the plot
p + geom_boxplot((aes(y = average_score)), outlier.color = 'red')


# Use boxplot.stats() to find the outlier values
boxplot.stats(student_df$average_score)
quantile(student_df$average_score)

# Use the NOTES: section for each and discuss your findings
# NOTES: ALl the outliers are not not from the top, which is Q3 + 1.5 * IQR,
# But under Q1 + 1.5 * IQR

# Generate a scatterplot of math (y) vs. reading scores (x) and include
# a smoothing curve.  Do the scores appear to be correlated?
p + geom_point(aes(x=reading_score, y=math_score))

# NOTES: the reading score is proportional to the math score
# Looks like they have high correlation
#

# Calculate Pearson's correlation for the points and note the value
cor(student_df$reading_score, student_df$math_score)

# NOTES: 0.8149478
#
#

#Extra Credit
# Use tidyr:gather to gather the *_score columns into rows.
names(student_df)
student_df %>% gather('math_score', 'reading_score', 'writing_score')

#key
tb_student_tidy <- student_df %>% gather(-(1:5), key = subject, value = score)
tb_student_tidy

# have a wonderful fall break!