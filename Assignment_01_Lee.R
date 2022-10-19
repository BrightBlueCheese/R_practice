#Student/assignment info here
# Name : Youngmin Lee
# File Name : Assignment_01_Lee.R
# Due date : Sept 11 (Extended, Thank you so much)
# Purpose of the script : Assignment for chapter 1

#Load the "datasets" library
library(datasets)
library(dplyr)
#Load the "chickwts" dataset
data(chickwts)
df <- chickwts
df
#Look at the structure of the dataset
str(df)
  # another ways to investigate data
summary(df)
?chickwts
attributes(df)

#List the levels of the feed factor
levels(df$feed)

#Find the mean and standard deviation of the weights
mean(df$weight)
sd(df$weight)

#Print/list all the values of the chicken's weights
  # If the instruction is talking about the unique value of the weight
unique(df$weight)
# length(unique(df$weight))

  # If the instruction is talking about just printing all the values of the weight
sapply(df$weight, function(x) print(x))
  # or
df$weight


#Find the minimum and maximum values of chicken weight
min(df$weight) # minimum
max(df$weight) # maximum
#Extract all chicken weights from the frame and place in
#a vector variable named "chickWeights"
chickweights <- df['weight']

#Increase the values of all the extracted weights by 15 percent
chickweights <- chickweights * 1.15
chickweights

#Extract rows of chickens who were fed casein and place in
#a new dataframe called caseinChicks
caseinChicks <- df %>% filter(feed == 'casein')
caseinChicks

#Extract rows of chickens who were fed horsebean and place in
#a new dataframe called horsebeanChicks
horsebeanChicks <- df %>% filter(feed == 'horsebean')
horsebeanChicks

#Using a relational operator, answer the question, 
#"True or false: Do casein-eating chicks weigh more than horsebean eaters?"
mean(caseinChicks$weight) > mean(horsebeanChicks$weight)
  # True. The mean weight of casein-eatong chicks are greater than the horsebeans'
