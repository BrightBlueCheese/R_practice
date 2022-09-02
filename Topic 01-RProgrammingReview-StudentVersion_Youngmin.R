# An addition
123 + 456

# A subtraction
123 - 456

# A multiplication
123 * 456

# A division
123 / 456

# Exponentiation
123^2

# Modulo
123 %% 5

#Assigning Values to Variables
int_a = 123
int_b = 456

#To "see" a variable, just type its name
int_a
int_b

#There is also a print() function
print(int_a * int_b)

# Storing data in R
my_age <- 26
my_age
class(my_age)
my_age <- 26 + 22/365
my_age
class(my_age)
my_age <- TRUE
my_age
class(my_age)
my_age <- "I am 26 years old"
my_age
class(my_age)

#Comparison Operators
# concatennation
daily_tips_for_the_week <- c(10, 5, 0, 6, 7, 13, 12)
daily_tips_for_the_week <= 8

length(daily_tips_for_the_week)

# Logics
c(FALSE, FALSE, TRUE, TRUE) & c(FALSE, TRUE, FALSE, TRUE)
c(FALSE, FALSE, TRUE, TRUE) | c(FALSE, TRUE, FALSE, TRUE)
c(FALSE, FALSE, TRUE, TRUE) && c(FALSE, TRUE, FALSE, TRUE)
c(FALSE, FALSE, TRUE, TRUE) || c(FALSE, TRUE, FALSE, TRUE)


# Functions
cube_it <- function(arg_1){
  return(arg_1 ^ 3)
}

# Call the function
cube_it(5)

# More Data Types
data("gwstates")
state_listing = c("Oklahoma", "Texas", "New York")
print(state_listing) 
hot_states <- state_listing[1:2]  # A vector with two objects, OK & TX
hot_states
road_trip <- state_listing[-2]  # A vector with two objects, OK & NY
road_trip
lower_tax <- state_listing[c(1,3)]# A vector with two objects, OK & NY
lower_tax
class(lower_tax)

num_seq <- seq(3, 19, 4)
num_seq
class(num_seq)

# altered
num_seq2 <- seq(3, 19, 3.5)
num_seq2
class(num_seq2)



my_numbers <- c(seq(77, 44, -2))
my_numbers
class(my_numbers)

my_numbers <- c(4, 'five', -1.5, 77)
my_numbers

class(my_numbers)
my_numbers <- as.numeric(my_numbers)
my_numbers

class(my_numbers)

# Naming Vectors
unemployment_rates = c(2.0, 4.5, 5.5)
state_listing = c('Oklahoma', 'Texas', 'New York')
names(unemployment_rates) <- state_listing
unemployment_rates
unemployment_rates["Oklahoma"]

# Comparisons with Vectors
low_unemployment <- unemployment_rates <= 4.0
low_unemployment
low_unemployment_states <- unemployment_rates[low_unemployment]
low_unemployment_states

#Computation on Vectors
unemployment_rates <- 
unemployment_rates

sum(unemployment_rates)
mean(unemployment_rates)
sd(unemployment_rates)

# Lists
my_int <- 3
my_chars <- "Me!"
my_vect <- c(1,2,3)

the_list <- list("ints" = my_int, "chars" = my_chars, "vect" = my_vect)
the_list
the_list[2]
the_list$ints

record <- list(name = "Youngmin Lee",
               student_id = 7777,
               grades = c(runif(5, 90, 100)),
               final_grade = "A")
record$name
record[2]
record
# Factors as a Data Type
survey_vector <- c("M", "F", "M", "F", "M")
factor_survey_vector <- factor(survey_vector)
factor_survey_vector

levels(factor_survey_vector) <- c("Female", "Male")
factor_survey_vector

levels(factor_survey_vector) <- c("woman", "Man" )
factor_survey_vector
summary(factor_survey_vector)
str(factor_survey_vector)

# Matrices
my_matrix <- matrix(1:15, 3, 5, byrow = TRUE)
my_matrix

point <- my_matrix[3, 2]
point
class(point)

arow <- my_matrix[2, ]
arow
class(arow)

acol <- my_matrix[, 3]
acol
class(acol)

rowSums(my_matrix)
colSums(my_matrix)

smaller_matrix <- my_matrix[1:3, 2:4]
smaller_matrix

# operations on a matrix
sum(my_matrix)
mean(my_matrix)
sd(my_matrix)

# Data Frames
name <- c("Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus",
          "Neptune")
type <- c("Terrestrial planet", "Terrestrial planet", "Terrestrial planet",
          "Terrestrial planet", "Gas giant", "Gas giant", "Gas giant", "Gas giant")
diameter <- c(0.382, 0.949, 1, 0.532, 11.209, 9.449, 4.007, 3.883)
rotation <- c(58.64, -243.02, 1, 1.03, 0.41, 0.43, -0.72, 0.67)
rings <- c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)

planets_df <-data.frame(name,type,diameter,rotation,rings)


planets_df

# Loading libraries
library("datasets")
library(help = "datasets")
install.packages("dslab")
library()

# Loading dataset from library
data(murders)
murders
class(murders)
attributes(murders)
str(murders)
head(murders)

murders[37, ]
murders[37, 4]
murders[37, "region"]
body_count <- murders[1:10, "total"]
body_count

murders[ ,2]
murders[ ,"abb"]
murders$abb

murders[2, ]
murders[ ,2]
murders[2]
murders["abb"]

murdersSouth <- murders[murders$region == 'South']
class(murdersSouth)
mean(murdersSouth$total)
# abs() is absolute value

sum(murders$total)
murders[37, "total"]/sum(murders$total)*100
mean(murders$total)
sd(murders$total)
sort(murders$total)
murders$total/sum(murders$total)*100

x <- c(31, 4, 15, 92, 65)
sort(x)
order_x <- order(x)
order_x
class(order_x)
rank(x)


# The _apply family!
data1 <- c(1, 2, 3)
data1
data2 <- lapply(data1, cube_it) # loop apply
data2

data3 <- sapply(data1, cube_it) # 
data3

data4 <- vapply(data1, cube_it, numeric(1))
data4

data4 <- vapply(data1, cube_it, character(1))
data4


