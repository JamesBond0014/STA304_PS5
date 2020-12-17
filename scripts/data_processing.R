#import required libraries
library(plyr)
library(dplyr)
library(tidyverse)

#read data, download link in the readme
#edit the directory to be where you save the downloaded files. 
canada_raw <- read.csv("data/canada_fatalities.csv")
usa_raw <- read.csv("data/usa_fatalities.csv")

#Select just the columns of interest from both the Canadian and USA data set
canada <- canada_raw %>% select(DATE, AGE, GENDER, RACE, PROV, POLICE.SERVICE, 
                                CAUSE.DEATH)


usa <- usa_raw %>% 
  select(Age, Gender, Race, Date.of.injury.resulting.in.death..month.day.year., 
         State, Agency.or.agencies.involved, Cause.of.death, 
         Intended.use.of.force..Developing.) %>%
  filter(Intended.use.of.force..Developing. == "Deadly force" |
           Intended.use.of.force..Developing. == "Less-than-lethal force" ) %>%
  select(-Intended.use.of.force..Developing.)

#Array to rename our columns to match from both the data sets
canada_rename = c(DATE = "Date", AGE = "Age", GENDER = "Gender",  RACE = "Race",
                  PROV= "Prov/State", POLICE.SERVICE = "Police Department", 
                  CAUSE.DEATH = "Cause of Death")

usa_rename = c(Date.of.injury.resulting.in.death..month.day.year. = "Date", 
               State= "Prov/State", Agency.or.agencies.involved = "Police Department",      
               Cause.of.death = "Cause of Death")

#Rename the columns
canada <- canada %>% plyr::rename(canada_rename)
usa <- usa %>% plyr::rename(usa_rename)

#Make our data into data frames.
canada <- data.frame(canada)
usa <- data.frame(usa)


# The following steps is to ensure our Fields are consistent 
# (Black == African-American/Black etc)

#Make the date formats consistent
usa$Date <- as.character(as.Date(as.character(usa$Date),"%m/%d/%Y"))
canada$Date <- as.character(as.Date(as.character(canada$Date), "%Y-%m-%d"))

#Remove the data with dates in 2020, since the data collection is not up to date


canada <- canada %>% subset(canada$Date < as.Date("2020-01-01"))
usa <- usa %>% subset(usa$Date < as.Date("2020-01-01"))


#Replace blank race info with Unknown or unspecified
canada$Race[canada$Race==""] <- "Unknown"
usa$Race[usa$Race==""] <- "Race unspecified"

canada_unique_race <- sort(unique(canada$Race))
usa_unique_race <- sort(unique(usa$Race))

#Map the values of Race in each data set to be consistent
canada$Race <- canada$Race %>% 
  plyr::mapvalues(c("Other", "Arab", "South Asian"), 
                  c("Unknown", "Middle Eastern", "Asian"))
usa$Race <- usa$Race %>% plyr::mapvalues(sort(usa_unique_race), 
                               c("Black", "Asian", "White", "Latin American", 
                                 "Middle Eastern", "Indigenous", "Unknown"))

#Filter the empty fields
usa$Police.Department <- usa$Police.Department %>% replace(usa$Police.Department
                                                           == "", "Unknown")

usa$Cause.of.Death <- usa$Cause.of.Death %>% replace(usa$Cause.of.Death == "", 
                                                     "Unknown")

usa$Gender <- usa$Gender %>% replace(usa$Gender == "", "Unknown")

usa$Age <- as.numeric(usa$Age)
canada$Age <- as.numeric(canada$Age)

# Write to a file to be used later
write.csv(canada, "data/canada_parsed_fatalities.csv")

write.csv(usa, "data/usa_parsed_fatalities.csv")



# find out death per million in each year in each country

#canadian population per year
# data pulled from 
# https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1710000501&pickMembers%5B0%5D=1.1&pickMembers%5B1%5D=2.1&cubeTimeFrame.startYear=2000&cubeTimeFrame.endYear=2020&referencePeriods=20000101%2C20200101
# 2020: 38005238
canada_population_year <- c(30685730,	31020902,	31360079,	31644028,	31940655,	
                            32243753,	32571174,	32889025,	33247118,	33628895,	
                            34004889,	34339328,	34714222,	35082954,	35437435,	
                            35702908,	36109487,	36545295,	37065178,	37593384)
#usa population every year
usa_population_year <- c(282162411, 284968955, 287625193, 290107933, 292805298,
                         295516599, 298379912, 301231207, 304093966, 306771529,
                         309326085, 311580009, 313874218, 316057727, 318386421,
                         320742673, 323071342, 325147121, 327167434, 328239523)
#years corresponding the previous arrays
years <- c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 
           2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)

# function to get year from data
get_year <- function(data){
  return(data %>% mutate(year = format(as.Date(Date, '%Y-%m-%d'), "%Y")) %>% 
           select(year) %>% count(year))
}

# function to calculate the per million population given a sepecifc data set
# and population
per_million <- function(data, pop) {
  data$pop = pop
  return (data %>% mutate(dpm = n/pop *1000000 ))
}


canada_fatalities_year <- get_year(canada)
usa_fatalities_year <- get_year(usa)

canada_fatalities_year <- per_million(canada_fatalities_year, 
                                          canada_population_year)
usa_fatalities_year <- per_million(usa_fatalities_year, usa_population_year)

write.csv(canada_fatalities_year, "data/canada_yearly_fatalities.csv")

write.csv(usa_fatalities_year, "data/usa_yearly_fatalities.csv")

