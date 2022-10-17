#Topic 05-Cleaning the Data
###########################################
# Title: Visualizing the Gapminder Data File 
# Script Name:  Cleaning the data classwork

# Name: Youngmin Lee
# Date: <your input>
# Script purpose: Class
# 
###########################################

### Install required packages if not installed
#install.packages("tidyverse")
#install.packages("mice")
#install.packages("assertr")
#install.packages("leaflet")
#install.packages("sp")
#install.packages("stringdist")

### Load the packages
library(tidyverse) # For tidy tools
library(mice)      # For the mice functions
library(assertr)   # For the assert functions
library(leaflet)   # For the leaflet functions
library(sp)        # For the sp functions
library(stringdist)

#import the full rodent data dataset
rawData <- read_csv("./dataset/rodentData.csv")

# Display the column definitions for the imported dataset
glimpse(rawData)

# Report the problems that were encountered when the data were imported.
problems(rawData)

# Display the entire table
# View(rawData)

# Display the entire table and summary data
rawData
summary(rawData)

####################### Create Technically Correct Data ######################
# Quick removal for entirely duplicated rows
rawData <- rawData %>% distinct()

############################ institutionCode ###################################
#Check the existing values
table(rawData$institutionCode)

# Check for NA's
sum(is.na(rawData$institutionCode))

#Convert to a factor
rawData$institutionCode <- 
  as.factor(rawData$institutionCode)

class(rawData$institutionCode)

# Getting a peek at NA's in the dataset with mice - md from the library 'mice'
md.pattern(rawData,rotate.names = TRUE)

############################ collectionCode ###################################
# Check the existing values
table(rawData$collectionCode)

# See the two rows
rawData %>%
  filter(collectionCode == 'host (of parasite) specimens')

# Look at the two rows in context of other rows
rawData %>%
  filter(scientificName == "microtus californicus" | scientificName == 'thomomys talpoides') %>%
  arrange(eventDate, scientificName)

# view(rawData)

# Drop the two "host..." rows as they are superfluous to rodent data
rawData <- rawData %>%
  filter(collectionCode != 'host (of parasite) specimens')

# Check the drop
table(rawData$collectionCode)

# Just a different coding, so set all values to the same value
rawData$collectionCode <- "mammal specimens"

############################ catalogNumber ###################################
# Check the existing values
summary(rawData$catalogNumber)

# Check for uniqueness
rawData %>% 
  group_by(catalogNumber) %>%
  select(catalogNumber) %>%
  summarize(count = n()) %>%
  filter(count > 1)

# Examine rows with same number
rawData %>%
  filter(catalogNumber %in% c(50050, 50781, 50782, 50783, 50784)) %>%
  arrange(catalogNumber)
rawData

names(rawData)
# since those vectors are not duplicated except the catalogNumber, no ways to deal with them..
# just leave them alone
# and not use the catalogNumber as a unique valued column

############################ countryCode ###################################
# countryCode
table(rawData$countryCode)

# Convert states to factor
rawData$countryCode <- as.factor(rawData$countryCode)

############################ stateProvince ###################################
# Check the existing values
table(rawData$stateProvince)
# Correct spelling issues (this should be more rigorous, don't change if no match)
rawData <- rawData %>%
  mutate(stateProvince = 
    tolower(state.name[amatch(stateProvince,state.name,method='osa',maxDist=4)]), 
      .after = stateProvince)
# maxDist = level of letter difference b/w two strings

# Convert states to factor
rawData$stateProvince <- as.factor(rawData$stateProvince)

# Check the fixed values of stateProvince
table(rawData$stateProvince)
levels(rawData$stateProvince)
############################ county ###################################
# Check the existing values
table(rawData$county)

# view(rawData)

# class(rawData$lonDMS)

# Remove "county" from all records and trim whitespace
  # str_trim : remove spaces
rawData <- rawData %>%
  mutate(county = str_remove(county, "county")) %>%
  mutate(county = str_trim(county, side = c("both"))) 
table(rawData$county)

# view(rawData)

# Convert county to factor
rawData$county <- as.factor(rawData$county)
############################################################# 
##  function:  dms2decDeg
#############################################################

dms2decDeg <- function(dmsString) { 
  # Define a regular expression and use it to extract pieces from a DMS string
  dmsExtract <- "\\s*(-*[:digit:]+)?\\s*([:digit:]+)\\'\\s*([:digit:]+\\.*[:digit:]*)"
  # Extract the substrings from the dataset
  dmsSubstring <- str_match(dmsString, dmsExtract)
  # Convert each substring to numeric
  degrees <- as.numeric(dmsSubstring[,2])
  minutes <- as.numeric(dmsSubstring[,3])
  seconds <- as.numeric(dmsSubstring[,4])
  # Appropriately add up the lat/lon as a numeric
  decDeg <- degrees
  decDeg <- ifelse(decDeg >= 0.0,  
    decDeg + minutes/60.0 + seconds/3600.0,
    decDeg - minutes/60.0 - seconds/3600.0)
  # return the value
  return(decDeg) 
}

# add new cols with dms converted to deg for lat/lon
rawData <- rawData %>%
  mutate(latDeg = dms2decDeg(latDMS), .after = latDMS) %>%
  mutate(lonDeg = dms2decDeg(lonDMS), .after = lonDMS)

# debug(dms2decDeg)

# glimpse(rawData)
# view(rawData)

# drop dms cols
rawData <- rawData %>%
  select(-latDMS, -lonDMS)

# Check descriptive stats
summary(rawData)

#Find the questionable value
rawData %>%
  filter(lonDeg > 0)

# Look just at San Miguel values
san_miguel <- rawData %>%
  select(county, latDeg, lonDeg) %>%
  filter(county == "san miguel") %>%
  arrange(latDeg, lonDeg)

# Replace the questionable value
rawData$lonDeg <- replace(rawData$lonDeg, rawData$lonDeg > 0, -106.34050)

# Make a quick plot cuz its fun
leaflet(select(rawData, latDeg, lonDeg)) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  setView(lng = -96.0, lat = 36.0, zoom = 4) %>%
  addCircleMarkers(lng = ~lonDeg, lat = ~latDeg)
                  
############################ recordedBy ###################################
# Check the existing values
head(rawData$recordedBy)

names(rawData)
# view(rawData)

# Define regular expressions for extracting text from recordedBy column
collectorExtract <- "collector\\(s\\):\\s(.*;|.*$)"
preparatorExtract <- "preparator\\(s\\):\\s(.*;|.*$)"

# Extract the strings into list( ,2)
collector_string <- str_match(rawData$recordedBy, collectorExtract)
preparator_string <- str_match(rawData$recordedBy, preparatorExtract)

# Peel off just the names and put in a new vector in dataset
rawData$collectors <- collector_string[,2]
rawData$preparators <- preparator_string[,2]

class(collector_string)

# If "collector" tag wasn't present, just copy what was there
rawData <- rawData %>% 
  mutate(collectors = if_else(is.na(collectors), recordedBy, collectors))

# Move columns to where recordedBy is and delete recordedBy column
rawData <- rawData %>% 
  relocate(collectors, .after = catalogNumber) %>%
  relocate(preparators, .after = collectors) %>%
  select(-recordedBy)

######################### eventDate ###########################
# POSIX format
class(rawData$eventDate)
str(rawData$eventDate)
######################### genus ###########################
# Check the existing values
table(rawData$genus)

#Convert to a factor
rawData$genus <- 
  as.factor(rawData$genus)

# See what rodents are where!
table(rawData$stateProvince, rawData$genus)

######################### Specific epithet ###########################
# Check the existing values
table(rawData$specificEpithet)

# check how many NAs
sum(is.na(rawData$specificEpithet))

# Convert to categorical
rawData$specificEpithet <- 
  as.factor(rawData$specificEpithet)

# Check the levels created
levels(rawData$specificEpithet)

# Create a new "other" level and put NAs in it
rawData <- rawData %>%
  mutate(specificEpithet = fct_explicit_na(specificEpithet, na_level = "other"))

# Check after effects
levels(rawData$specificEpithet)
sum(is.na(rawData$specificEpithet))
summary(rawData$specificEpithet)

######################### Scientific Name ###########################
# Check the existing values
table(rawData$scientificName)

# Trim whitespace and remove double spaces
rawData <- rawData %>%
  mutate(scientificName = str_trim(scientificName, side = c("both")))  %>%
  mutate(scientificName = str_replace_all(scientificName, "  ", " "))  %>%
  mutate(scientificName = str_replace_all(scientificName, "  ", " "))

rawData %>%
  select(genus, specificEpithet, scientificName) %>%
  filter(specificEpithet == "other") %>%
  group_by(genus, specificEpithet, scientificName) %>%
  summarize(count = n())

######################### Species Name ##############################
# Since Scientific Name is a mess, create a species name as categorical
# Create a new vectors that is a combo of genus and specific epithet
# str_c : string concatenation
rawData <- rawData %>% 
  mutate(species = str_c(rawData$genus, sep=" ", rawData$specificEpithet), .after = specificEpithet)



# Clean up "other" as it is not an epithet
rawData$species <- str_remove(rawData$species, " other")

# Convert to categorical
rawData$species <- as.factor(rawData$species)

# Check after effects
summary(rawData$species)

# check how many NAs
sum(is.na(rawData$species))

############################ sex ###################################
# Check the existing values
table(rawData$sex)

# check how many NAs
sum(is.na(rawData$sex))

# Enter "Unknown" if no sex is listed.
rawData$sex <- ifelse(is.na(rawData$sex), "Unknown", rawData$sex)

# Convert to categorical
rawData$sex <- 
  as.factor(rawData$sex)

table(rawData$sex)
############################ length and weight #####################
# Check the existing values
summary(rawData$length)
summary(rawData$weight)

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

# HW5
# Copy and modify the assert/insist to find the errors in the weight data


############### save the cleaned data to a file #######
#export the full rodent data dataset
write_csv(rawData, "./dataset/rodentData_clean.csv")

