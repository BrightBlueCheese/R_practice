###########################################
# Title: VoterRegistration.r 
# Name: J. Bryan Osborne
# Date: 10/24/2022
###########################################

### Load the required libraries
library(tidyverse) 
library(tidymodels) 
library(lubridate)
library(stringr)
library(doParallel)

######################### Turn parallel processing capabilities on ############
registerDoParallel()
no_cores <- detectCores() - 1  
# cl <- makeCluster(no_cores, type="FORK")  
# registerDoParallel(cl)  

##############################################################
# Define all the needed files 
# Define the full path and file name for data being used

# For Lee
voterReg <- "./CTY72_vr.csv"
voterReg <- "/Users/josborne/ORU/Research/VoterRolls/OKElectionData/CTY72_VR_20221024015236/CTY72_vr.csv"


# Using the variable just defined, open and read the StudentsPerformance.csv 
# file into a tibble


tb_voterReg = read.csv(voterReg)

# Perform Initial Data Analysis (IDA)
summary(tb_voterReg)

# Check for NAs in the dataset
sapply(tb_voterReg, function(x) sum(is.na(x)))

# Review/evaluate/modify each vector in dataset as needed
tb_voterReg$Precinct <- as.factor(tb_voterReg$Precinct)
tb_voterReg$PolitalAff <- as.factor(tb_voterReg$PolitalAff)
tb_voterReg$Status <- as.factor(tb_voterReg$Status)

# Zip - trim down to 5 numbers and make a factor
tb_voterReg <- tb_voterReg %>%
  mutate(Zip = str_extract(Zip,'\\d{5}'))
tb_voterReg$Zip <- as.factor(tb_voterReg$Zip)

# Change date values to Dates
tb_voterReg$DateOfBirth <- mdy(tb_voterReg$DateOfBirth)

# Create a column for birth year only
tb_voterReg <- tb_voterReg %>%
  mutate(YearOfBirth = year(DateOfBirth), .after = DateOfBirth)

# Create a column for age
# tb_voterReg <- tb_voterReg %>%
#   mutate(age=as.period(interval(start = DateOfBirth, end = today())), .after = DateOfBirth) %>% 
#   mutate(age=if_else(!is.na(age),age$year,age))

# Make RegistrationDate a date
tb_voterReg$RegistrationDate <- mdy(tb_voterReg$RegistrationDate)

# Review dataset again with summary()
summary(tb_voterReg)

# Create a bar graph to reflect the number of voters by affiliation
tb_voterReg %>% 
  ggplot(aes(x = PolitalAff, fill = PolitalAff)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label =..count..), vjust = -1) +
  labs(x="Political Affiliation", y="Number of Registered Voters", 
       title="Registered Voters in Tulsa County - Oct 2022") +
  scale_fill_manual(values = c("blue","purple", "yellow","red"))

# Create a bar graph to reflect the number of voters by zip/affiliation
tb_voterReg %>% 
  ggplot(aes(x = Zip, fill = PolitalAff)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="Zip Code", y="Number of Registered Voters", 
       title="Registered Voters in Tulsa County by Zip Code - Oct 2022") +
  scale_fill_manual(values = c("blue","purple", "yellow","red"))

# Plotting a histograms 
tb_voterReg %>% 
  ggplot(aes(x = YearOfBirth, fill = PolitalAff)) +
  geom_bar(stat="bin", binwidth=5,position = "stack") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="Voter's Year of Birth", y="Number of Registered Voters", 
       title="Registered Voters in Tulsa County by Party and Year of Birth - Oct 2022") +
  scale_fill_manual(values = c("blue","purple", "yellow","red"))

tb_voterReg %>% 
  ggplot(aes(x = YearOfBirth, fill = PolitalAff)) +
  geom_bar(stat="bin", binwidth=5,position = "fill") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="Voter's Year of Birth", y="Number of Registered Voters", 
       title="Registered Voters in Tulsa County by Party and Year of Birth (by Percent) - Oct 2022") +
  scale_fill_manual(values = c("blue","purple", "yellow","red"))


# Plotting a histograms 
tb_voterReg %>% 
  filter(YearOfBirth <= 1922) %>%
  ggplot(aes(x = YearOfBirth, fill = PolitalAff)) + 
  geom_histogram(binwidth = 1) +
  labs(x="YearOfBirth", y="Number of Registered Voters", 
       title="Registered Voters in Tulsa County Born Before 1923 - Oct 2022") +
  scale_fill_manual(values = c("blue","purple", "yellow","red"))




# Age at registration
# function to calculate the voters' age when the event happened
age_calculator <- function(date_of_birth, object_date) {
  # if either left or right parameter is NA, then the function return NA
  # https://datacornering.com/how-to-calculate-age-in-r/
  age <- trunc(as.numeric(difftime(object_date, date_of_birth, units='weeks')) / 52.25)
  return(age)
}
# # check if the input is NA
# age_calculator('1960-10-24', NA)
# age_calculator(NA, '1960-10-24')


tb_voterReg_ver2 <- tb_voterReg
tb_voterReg_sample_50_rows <- tb_voterReg[1:50, ]

tb_voterReg_ver2 <- tb_voterReg_ver2 %>% 
  mutate(AgeAtRegistration = age_calculator(DateOfBirth, RegistrationDate), .after = RegistrationDate)

tb_voterReg_ver2

names(tb_voterReg)

# # For Test
# for (i in names(tb_voterReg_ver2)) {
#   if (grepl('VoterHist', i,)) {
#     tb_voterReg_ver2[i] <- mdy(tb_voterReg_ver2[[i]])
#   }
# }

tb_voterReg_ver2

# # tricky..
# class(tb_voterReg_ver2[['VoterHist10']])
# class(tb_voterReg_ver2['VoterHist10'])
# mdy(tb_voterReg_ver2['VoterHist10'])

# Calculate the age at the moment of the Vote
for (i in names(tb_voterReg_ver2)) {
  if (grepl('VoterHist', i,)) {
    # print(class(i))
    # print(class(tb_voterReg_ver2$DateOfBirth))
    # print(new_col)
    new_col <- paste('AgeAt', i, sep='')
    tb_voterReg_ver2 <-  tb_voterReg_ver2 %>%
      # https://stackoverflow.com/questions/26003574/use-dynamic-name-for-new-column-variable-in-dplyr
      mutate("AgeAt{i}" := age_calculator(DateOfBirth, mdy(tb_voterReg_ver2[[i]])), .after = i)
    # Just simply putting mdy(i) does not work because i is just the NAME of the column, not column data
  }
}
tb_voterReg_ver2
# Seems like the VoterHist1 is the latest one
class(tb_voterReg_ver2$AgeAtVoterHist1)
ggplot(tb_voterReg_ver2, aes(x = PolitalAff, y = AgeAtVoterHist1))

# Plotting a histograms 
tb_voterReg_ver2 %>% 
  ggplot(aes(x = AgeAtVoterHist1, fill = PolitalAff)) + 
  geom_histogram(binwidth = 1) +
  labs(x="Age When Vote 1", y="Number of Registered Voters", 
       title="Age Distribution when the Vote 1 Happened") +
  scale_fill_manual(values = c("blue","purple", "yellow","red"))

ggplot(data = tb_voterReg_ver2, aes(x = AgeAtVoterHist1, fill = PolitalAff)) +
  geom_histogram((aes(y = ..density..))) 
