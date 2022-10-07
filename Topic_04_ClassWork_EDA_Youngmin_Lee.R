###########################################
# Title: Visualizing the Gapminder Data File 
# Script Name: 
# Name: <your input>
# Date: <your input>
# Script purpose:
# 
###########################################

### Install required packages if not installed
# install.packages("tidyverse")

### Load the packages
library(tidyr)      # For tidy tools
library(dplyr)      # For the dplyr functions
library(readr)      # For the importing functions
library(stringr)    # For the string manipulation
library(magrittr)   # For the pipe symbol
library(ggplot2)    # For the plotting functions
library(lubridate)  # For extended dateTime from tidyverse

##############################################################3
# Define all the needed files 
# Define the full path and file name for the geographic data

theDate = "2022-09-23"
class(theDate)

theDateObj <- as.Date(theDate)
class(theDateObj)
theDateObj

date <- dmy("01-01-2022")
month(date)

mdy("12-01-2021")
dmy("12:01:2021")

parse_date_time(c("2021 Sep 1", "09:01:2021"), order = c("ymd", "dmy", "mdy"))


SepDay <- make_date(year = 2022, month = 9, day = 1)
SepDay

# leap_year(), am(), pm(), dst(), quarter(),semester()

today <- today()
today
str(today)
now <- now()
now
str(now)

startTime <- now()
ranSec <- runif(1, 3.0, 7.0)

timeDiffTest <- function() {
  startTime <- now()
  ranSec <- runif(1, 3.0, 7.0)
  print(ranSec)
  Sys.sleep(ranSec)
  endTime <- now()
  timeDiff <- difftime(endTime, startTime, units='secs')
  print(timeDiff)
}

timeDiffTest()

aDate <- ymd_hms("2020-12-01 00:00:00", tz="America/Chicago")
aDateSeq <- 1:12 * months(1)
aDateSeq
aYearOfFirsts <- aDate + aDateSeq
aYearOfFirsts

# or
aYearOfFirsts <- seq(as.Date("2021/01/01"), as.Date("2021/12/01"), by = "months") 
aYearOfFirsts

Sys.timezone()
OlsonNames() # show all possible timezones

# force_tz() # change timezone
with_tz(now(),"ROK") # convert into a certain timezone


date_stamp <- stamp_date("Monday, November 11, 2021")


time1 <- ymd_h("2022:11:05 12", tz = "America/Chicago")
time2 <- ymd_h("2022:11:06 12", tz = "America/Chicago")
time3 <- ymd_h("2022:11:07 12", tz = "America/Chicago")

difftime(time2, time1, units='secs')
difftime(time3, time2, units="secs")


terval <- interval(time1, time2)
int_start(terval)
int_end(terval)
int_length(terval) # in seconds
as.period(terval)
as.duration(terval)
