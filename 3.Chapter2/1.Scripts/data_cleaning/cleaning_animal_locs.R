#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2023-01-27 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####
#      Library                                                              ####
library(tidyverse)
library(readxl)

#      Functions                                                            ####

#      Data                                                                 ####

# Importing Mortality table
table_mortality_original <- read_xlsx("1.DataManagement\\ch2_data\\dbo_tblMortality.xlsx")

# Deer Locations
locs_deer <- read_csv("1.DataManagement\\CleanData\\deer_all_revised.csv")

###############################################################################
#   Summary of Data                                                         ####
summary_mortality <- table_mortality_original %>% 
  group_by(StudyArea,Sex,EstAge) %>% 
  summarise(n()) %>% 
  na.omit() %>%
  filter(StudyArea %in% c("N","S")) %>% 
  filter(EstAge %in% c("F","Y","A")) %>% 
  mutate(StudyArea = factor(StudyArea, levels = c("N","S"))) %>%
  mutate(Sex = factor(Sex, levels = c("M","F"))) %>% 
  mutate(EstAge = factor(EstAge, levels = c("F","Y","A"))) %>% 
  arrange(StudyArea,Sex, desc(EstAge),.by_group = T)

summary_mortality

###############################################################################
#   Data Prep                                                               ####

# Cleaning Mortality table
mortality_table <- table_mortality_original %>% 
  na.omit() %>%
  filter(StudyArea %in% c("N","S")) %>% 
  filter(EstAge %in% c("F","Y","A")) %>% 
  mutate(StudyArea = factor(StudyArea, levels = c("N","S"))) %>%
  mutate(Sex = factor(Sex, levels = c("M","F"))) %>% 
  mutate(EstAge = factor(EstAge, levels = c("F","Y","A"))) %>% 
  arrange(StudyArea,Sex, desc(EstAge),.by_group = T)

