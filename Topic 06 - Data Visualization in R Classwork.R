#Topic 06 - Data Visualization
###########################################
# Title: Visualizing Data with ggplot2
# Script Name: 
# Name: <your input>
# Date: <your input>
# Script purpose:
# 
###########################################
### Load the packages
library(tidyverse) # For tidy tools
library(assertr)

# Read the data file with a structural problem
cleanData <- read_csv("./dataset/rodentData_clean.csv")

# Display the column definitions for the imported dataset
glimpse(cleanData)

################ factorize categorical values  #################
# Convert institutionCode to factor
cleanData$institutionCode <- 
  as.factor(cleanData$institutionCode)
# Convert countryCode to factor
cleanData$countryCode <- 
  as.factor(cleanData$countryCode)
# Convert states to factor
cleanData$stateProvince <- 
  as.factor(cleanData$stateProvince)
# Convert county to factor
cleanData$county <- 
  as.factor(cleanData$county)
#Convert genus to a factor
cleanData$genus <- 
  as.factor(cleanData$genus)
# Convert specificEpithet to a factor
cleanData$specificEpithet <- 
  as.factor(cleanData$species)
# Convert specificEpithet to a factor
cleanData$species <- 
  as.factor(cleanData$species)
# Convert sex to a factor
cleanData$sex <- 
  as.factor(cleanData$sex)

######################## Basic setup ############################
ggplot(cleanData)                         #Create a blank canvas

ggplot(cleanData, aes(x = genus))         #Axis and grid bg create


######################## Bar Charts #############################
#Bar chart of genus counts, stat = "count" by default
ggplot(cleanData, aes(x = genus)) +       
  geom_bar()

#Note we get the same plot since these are default
ggplot(cleanData, aes(x = genus)) +       
  geom_bar(stat = "count", position = "stack")

#introduce a new factor in stacked bar chart
ggplot(cleanData, aes(x = genus, fill = sex)) +       
  geom_bar() # position : fill, dodge

#Fill changes it to a proportion and is a filled bar chart
ggplot(cleanData, aes(x = genus, fill = sex)) +       
  geom_bar(position = "fill")

#Dodge sets the values next to each other for comparison
ggplot(cleanData, aes(x = genus, fill = sex)) +       
  geom_bar(position = "dodge")

#introduce a new continuous variable in fill
ggplot(cleanData, aes(x = genus, fill = length)) +       
  geom_bar()

#group the variable to get shades
ggplot(cleanData, aes(x = genus, fill = length, group = length)) +       
  geom_bar()

#A bar plot with a continuous variable
ggplot(cleanData, aes(x = length)) +       
  geom_bar()

#But with the appropriate stat trans
ggplot(cleanData, aes(x = length)) +       
  geom_bar(stat = "bin")

#So we see histogram is the above
ggplot(cleanData, aes(x = length)) +       
  geom_histogram(bins=15)

#Stacked Histogram with fill
ggplot(cleanData, aes(x = length, fill = sex)) +       
  geom_histogram(binwidth=20)

###################### Density Plot ##########################
ggplot(cleanData, aes(x = length)) +       
  geom_density(color = "black", fill = "lightblue") 

ggplot(cleanData, aes(x = length, fill = sex)) +       
  geom_density(alpha = .25)

####################### Box Plots ##############################
ggplot(cleanData, aes(x = genus, y = length, fill = genus)) +
  geom_boxplot(outlier.color = "red") +
  geom_point(position = position_jitter(width = 0.1), alpha = .025)

######################## Scatterplots ###########################
# See ppt p.28
#Data with a curve
cleanData %>%
  filter(genus == "thomomys") %>%
  ggplot(aes(x = weight, y = length)) +
  geom_point() +
  geom_smooth(se = FALSE)

#Modify the scale to a log10
cleanData %>%
  filter(genus == "thomomys") %>%
  ggplot(aes(x = weight, y = length)) +
  geom_point() +
  scale_x_continuous() +
  geom_smooth(se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

cleanData %>%
  filter(genus == "thomomys") %>%
  ggplot(aes(x = weight, y = length)) +
  geom_point() +
  scale_x_continuous(trans = "log10") +
  geom_smooth(se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "red")



cleanData %>%
  filter(genus == "thomomys") %>%
  ggplot(aes(x = weight, y = length)) +
  geom_point() +
  geom_smooth(se = FALSE) + #smoothing1
  scale_x_continuous() + #smoothing2
  scale_y_continuous() #smoothing3

###### Check chapter 6 ppt p.32 for xlim vs limits

###################### Coordinate Layer  ########################
#The previous bar chart used cartesian coord
ggplot(cleanData, aes(x = genus, fill = genus)) + 
  geom_bar() 

#Another way to view is a stacked bar
ggplot(cleanData, aes(x = "", fill = genus)) + 
  geom_bar() 

#A change in the coord system and we have a pie
ggplot(cleanData, aes(x = "", fill = genus)) + 
  geom_bar() +
  coord_polar(theta = "y")

#More often than not, data is prepped/modified for plot creation
#To create a pie, usually percentages are used
cleanData %>%
  group_by(genus) %>%
  summarize(Percent = n()/nrow(.) * 100) 

#These can be presented as a stacked bar graph   # percent bar
cleanData %>%
  group_by(genus) %>%
  summarise(Percent = n()/nrow(.) * 100) %>%
  ggplot(aes(x = 2, y = Percent, fill = genus)) +
  geom_bar(stat = "identity")

#With a quick change of coordinates, a pie chart!
cleanData %>%
  group_by(genus) %>%
  summarise(Percent = n()/nrow(.) * 100) %>%
  ggplot(aes(x = 2, y = Percent, fill = genus)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y", start = 0) 

#Or a donut if you prefer
cleanData %>%
  group_by(genus) %>%
  summarise(Percent = n()/nrow(.) * 100) %>%
  ggplot(aes(x = 2, y = Percent, fill = genus)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y", start = 0) +
  xlim(c(0.5, 2.5)) 

# A prettied up version
cleanData %>%
  group_by(genus) %>%
  summarise(Percent = n()/nrow(.) * 100) %>%
  mutate(genus = fct_reorder(genus, Percent, .fun='identity')) %>%
  ggplot(aes(x = 2, y = Percent, fill = genus)) +
  geom_bar(stat = "identity") +
  geom_col(col="black") +
  geom_text(aes(label = as.integer(Percent)),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y", start = 0) +
  theme_void() +
  xlim(c(0.5, 2.5)) 

    
####################3  Coordinate Transformation #################
#Original data, unmodified
cleanData %>%
  filter(genus == "thomomys") %>%
  ggplot(aes(x = weight, y = length)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(color = "yellow", se = FALSE) 

#Transform the variables
cleanData %>%
  filter(genus == "thomomys") %>%
  ggplot(aes(x = log10(weight), y = length)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(color = "yellow", se = FALSE) 
  # X axis is not logarithmic scale, but a regular continuous scale
  # x for x = log10(x_origin)


#Transform the scale to a log10 
cleanData %>%
  filter(genus == "thomomys") %>%
  ggplot(aes(x = weight, y = length)) +
  geom_point() +
  scale_x_log10() + # x axis scaled into log thing
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(color = "yellow", se = FALSE)

#Transform the coordinate system to a log10
cleanData %>%
  filter(genus == "thomomys") %>%
  ggplot(aes(x = weight, y = length)) +
  geom_point() +
  coord_trans(x = "log10") +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(color = "yellow", se = FALSE)

########## Plotting to Create More Consistent Data ###############
# Starting with length
# Using assert/insist to look for problems
tryCatch({cleanData %>% 
    chain_start %>%
    assert(within_bounds(1,Inf), length) %>%
    insist(within_n_sds(3), length) %>%
    chain_end
}, warning = function(w) {
  paste("A warning was generated: ", w, sep = "")
}, error = function(e) {
  print(e)
}, finally = {
  print("this is the end of the validation check ...")
})

# Histogram and Density plots
cleanData %>%
ggplot(aes(x = length)) + 
  geom_histogram(aes(y = ..density..), binwidth = 20) +
  geom_density(col = "red")

# Create summary of just length vector
summary(cleanData$length)

# Box plot
# Generate a boxplot
cleanData %>%
ggplot(aes(x = "All", y = length)) +
  geom_boxplot(outlier.color = "red") +
  geom_point(alpha = .25)

# density by genus
cleanData %>%
  ggplot(aes(x = length, fill = genus, alpha = .5)) +
  geom_density()

# boxplot by genus
cleanData %>%
  ggplot(aes(x = genus, y = length, fill = genus)) +
  geom_boxplot(outlier.color = "red") +
  geom_jitter(position = 
    position_jitter(width = 0.1, height = 0), alpha = .075)

# density by species
cleanData %>%
  ggplot(aes(x = length, fill = genus, alpha = .5)) +
  geom_density()

# boxplot by species
cleanData %>%
  ggplot(aes(x = species, y = length, fill = genus)) +
  geom_boxplot(outlier.color = "red") +
  geom_jitter(position = position_jitter(width = 0.1, height = 0), alpha = .25)

# scatterplot
cleanData %>% 
  ggplot(aes(x = weight, y = length)) +
  geom_point()

# scatterplot by genus
cleanData %>% 
  ggplot(aes(x = weight, y = length, color = genus)) +
  geom_point()

# scatterplot - faceted by genus with smoothing curves
cleanData %>%
  ggplot(aes(x = weight, y = length, color = genus)) +
  geom_point() +
  facet_grid(~genus) +
  geom_smooth(color = "blue", se = FALSE) +
  geom_smooth(method = "lm", color = "red", se = FALSE)

###################  Making the data more consistent ##############
# Instead of simply removing the data, we can "mark" the data as
# inconsistent and then decide later how to deal with it as some
# problems may be "fixable"

# Mark data with equal weight/length's or zero values as invalid
cleanData <- cleanData %>%
  mutate(isInvalid = (length == weight | weight == 0 | length == 0))

summary(cleanData$isInvalid)

# Use zero-free/non-equal data for IQR and quantile calculations
cleanDataN0 <- cleanData %>% filter(!isInvalid)

##########################  IQR for length ###################################
# Create a dataframe to hold the lower/upper values for outlier identification
genusLimit = data.frame(genus = unique(cleanDataN0$genus))
genusLimit
# Calculate the lower/upper values for length
genusLimit$lower = sapply(genusLimit$genus, 
    function(x) quantile(cleanDataN0$length[cleanDataN0$genus==x],0.25) 
    - 1.5 * (quantile(cleanDataN0$length[cleanDataN0$genus==x],0.75) - quantile(cleanDataN0$length[cleanDataN0$genus==x],0.25)) )
genusLimit$upper = sapply(genusLimit$genus, 
    function(x) quantile(cleanDataN0$length[cleanDataN0$genus==x],0.75) 
    + 1.5 * (quantile(cleanDataN0$length[cleanDataN0$genus==x],0.75) - quantile(cleanDataN0$length[cleanDataN0$genus==x],0.25)) )
genusLimit

# join lower/upper values  for length to cleanData
cleanData <- cleanData %>% left_join(genusLimit, by="genus")

# set isInvalid based on lower/upper values for length
cleanData$isInvalid <- 
  ifelse(cleanData$length < cleanData$lower | cleanData$length > cleanData$upper, TRUE, cleanData$isInvalid)

# remove lower/upper values for length
cleanData <- cleanData %>% select(-lower, -upper)

# check on how many rows were marked invalid
summary(cleanData$isInvalid)

##########################  IQR for weight #############################
# Create a dataframe to hold the lower/upper values for outlier identification

# Calculate the lower/upper values for weight

# join lower/upper values  for weight to cleanData

# set isInvalid based on lower/upper values for weight

# remove lower/upper values for weight

# check on how many rows were marked invalid

#####################  Graphically review the cleaner data  ##########################
cleanData %>%
  filter(!isInvalid) %>%
  group_by (genus) %>%
  summarize(min(length), max(length),min(weight), max(weight))

cleanData %>%
  filter(!isInvalid) %>%
  ggplot(aes(x = length, fill = genus, alpha = .5)) +
  xlim(0, 400) +
  geom_density()

# boxplot by genus
cleanData %>%
  filter(!isInvalid) %>%
  ggplot(aes(x = genus, y = length, fill = genus)) +
  geom_boxplot(outlier.color = "red") +
  ylim(0,400)+
  geom_jitter(position = position_jitter(width = 0.1, height = 0), alpha = .025)

# scatterplot - faceted by genus with smoothing curves
cleanData %>%
  filter(!isInvalid) %>%
  ggplot(aes(x = weight, y = length, color = genus)) +
  geom_point() +
  facet_grid(~genus) +
  geom_smooth(color = "blue", se = FALSE) +
  geom_smooth(method = "lm", color = "red", se = FALSE)


######################### save the cleaned data to a file #######
#export the full rodent data dataset
cleanData %>%
  filter(!isInvalid) %>%
  write_csv("./dataset/rodentData_for_assignment.csv")

# HW
# length has been done, but not weight. fix it
# plot for before and after.




# check the additional work for graduate students


