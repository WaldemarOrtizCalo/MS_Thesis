#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2023-01-27 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####
#      Library                                                              ####
library(tidyverse)
library(readxl)
library(lubridate)

#      Functions                                                            ####

#      Data                                                                 ####

# Importing Mortality table
table_mortality_original <- read_xlsx("1.DataManagement\\ch2_data\\dbo_tblMortality.xlsx") %>% 
  filter(StudyArea %in% c("N","S")) %>% 
  filter(EstAge %in% c("F","Y","A")) %>% 
  mutate(StudyArea = factor(StudyArea, levels = c("N","S"))) %>%
  mutate(Sex = factor(Sex, levels = c("M","F"))) %>% 
  mutate(EstAge = factor(EstAge, levels = c("F","Y","A"))) %>% 
  rename(individual.local.identifier = DeerID)

# Deer Locations
locs_deer <- read_csv("1.DataManagement\\CleanData\\deer_all_revised.csv") %>% 
  filter(site %in% c("North", "South")) %>% 
  filter(age %in% c("F","Y","A")) %>% 
  mutate(site = factor(site, levels = c("North","South"))) %>%
  mutate(sex = factor(sex, levels = c("M","F"))) %>% 
  mutate(age = factor(age, levels = c("F","Y","A"))) 

###############################################################################
#   Summary of Data                                                         ####
#      Mortality table                                                      ####

# Summary of mortality table
summary_mortality <-   
  table_mortality_original %>% 
  group_by(StudyArea,Sex,EstAge) %>% 
  summarise(n.indiv = n(),
            alive = sum(!is.na(MortDate)),
            dead = sum(is.na(MortDate))) %>% 
  arrange(StudyArea,Sex, desc(EstAge),.by_group = T)

summary_mortality

#      Collar locs                                                          ####

# Summary of relevant collar information
summary_locs <- locs_deer %>% 
  group_by(site,sex,age) %>% 
  summarise(n.indiv = n_distinct(individual.local.identifier)) %>% 
  arrange(desc(age),.by_group = TRUE)

summary_locs

#      Inclusion Tables                                                     ####

ids_mort <- unique(table_mortality_original$individual.local.identifier)
ids_locs <- unique(locs_deer$individual.local.identifier)

inclusion_table_locs <- data.frame(individual.local.identifier = ids_locs) %>% 
  mutate(inclusion_locs = ifelse(ids_locs %in% ids_mort,1,0))

inclusion_table_mort <- data.frame(individual.local.identifier = ids_mort) %>% 
  mutate(inclusion_mort = ifelse(ids_mort %in% ids_locs,1,0))

inclusion_table_final <- merge(x=inclusion_table_locs,
                   y=inclusion_table_mort,
                   by="individual.local.identifier",
                   all=TRUE) %>% 
  mutate(valid_data = ifelse(inclusion_locs == 1 & inclusion_mort == 1,
                             T,F))

###############################################################################
#   Developing Timetable                                                    ####
#      Proofing Time Calculation                                            ####
# Extracting the first date of the data and creating an anchor day to the Sunday
# of that week. 
first_date <-  min(locs_deer$timestamp)- days(wday(min(locs_deer$timestamp))-1) 
anchor_date <- floor_date(first_date, unit = "day")

# Proofing difftime function
difftime(first_date, anchor_date, units = "weeks") %>% 
  as.numeric() %>% 
  floor() + 1

#      Creating a function for studyweek calculation                        ####

# Function
studyweek_calc <- function(date,anchor_date){
  
  study_week <- difftime(date, anchor_date, units = "weeks") %>% 
    as.numeric() %>% 
    floor() + 1
  
  return(study_week)
}

# Test
studyweek_calc(first_date,
               anchor_date = anchor_date)

###############################################################################
#   Data Prep                                                               ####
#      Mortality table                                                      ####

# Mortality table
mortality_table <- table_mortality_original %>% 
  mutate(status = ifelse(!(is.na(MortDate)),1,0)) %>% 
  select(individual.local.identifier,MortDate,Harvest,status) %>% 
  mutate(Harvest = ifelse(Harvest == "Y",1,0)) %>% 
  mutate(studyweek = studyweek_calc(MortDate,anchor_date = anchor_date)) %>% 
  mutate(t_start =  studyweek,t_end = studyweek+1) %>% 
  drop_na(MortDate)

# Location Data
final_loc_data <- locs_deer %>% 
  filter(individual.local.identifier %in% unique(mortality_table$individual.local.identifier)) %>% 
  mutate(studyweek = studyweek_calc(timestamp,anchor_date = anchor_date)) %>% 
  mutate(t_start =  studyweek,t_end = studyweek+1) %>%
  left_join(mortality_table, by=c("individual.local.identifier","studyweek","t_start","t_end"))

locs_summary <- final_loc_data %>% 
  group_by(individual.local.identifier,studyweek) %>% 
  summarize(n())
  
###############################################################################
#   [DEV]                                                                 ####

# Filtering deer
loc_subset <- locs_deer %>% 
  filter(individual.local.identifier == "N15013")

###############################################################################