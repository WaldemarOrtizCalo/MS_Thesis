#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2023-02-14 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(tidyverse)
library(readxl)
library(stringr)
library(lubridate)

#      Functions                                                            ####

#      Data                                                                 ####

# Main Directory
jon_data_dir <- "D:/Drive/Research/UMontana/2.INPROGRESS/1.Masters/MS_Thesis_Analysis/1.DataManagement/jon_data"

# Uploaded

adult_capture_hist <- list.files(jon_data_dir,
                        full.names = T) %>% 
  str_subset(pattern = "AdultCapture") %>% 
  read_excel()

collar_hist <- list.files(jon_data_dir,
                                 full.names = T) %>% 
  str_subset(pattern = "CollarHistory") %>% 
  read_excel()

deer_hist <- list.files(jon_data_dir,
                          full.names = T) %>% 
  str_subset(pattern = "Deer") %>% 
  read_excel()

fawn_hist <- list.files(jon_data_dir,
                        full.names = T) %>% 
  str_subset(pattern = "FawnCapture") %>% 
  read_excel()

mortality_hist <- list.files(jon_data_dir,
                        full.names = T) %>% 
  str_subset(pattern = "Mortality") %>% 
  read_excel()

locs_deer <- read_csv("1.DataManagement\\CleanData\\deer_all_revised.csv") %>% 
  filter(site %in% c("North", "South")) %>% 
  filter(age %in% c("F","Y","A")) %>% 
  mutate(site = factor(site, levels = c("North","South"))) %>%
  mutate(sex = factor(sex, levels = c("M","F"))) %>% 
  mutate(age = factor(age, levels = c("F","Y","A"))) %>% 
  mutate(date = floor_date(timestamp,unit = "day"),.after= timestamp) %>% 
  mutate(interval_id = NA)

age_class_transitions <- locs_deer %>% 
  group_by(individual.local.identifier) %>% 
  summarise(age_class_transitions = n_distinct(age))

###############################################################################
#   Relevancy table                                                         ####

relevancy_table <- data.frame(individual.local.identifier = unique(collar_hist$DeerID)) %>% 
  arrange(individual.local.identifier) %>% 
  mutate(LocData = ifelse(individual.local.identifier %in% unique(locs_deer$individual.local.identifier),1,0)) %>% 
  mutate(MortData = ifelse(individual.local.identifier %in% unique(mortality_hist$DeerID),1,0)) %>% 
  left_join(age_class_transitions)

north_indivs_2019 <- str_subset(relevancy_table$individual.local.identifier,
                                pattern = "N19")

north_relevancy_2019 <- relevancy_table %>% 
  filter(individual.local.identifier %in% north_indivs_2019)

south_indivs_2019 <- str_subset(relevancy_table$individual.local.identifier,
                                pattern = "S19")

south_relevancy_2019 <- relevancy_table %>% 
  filter(individual.local.identifier %in% south_indivs_2019)






