#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2023-02-24 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(tidyverse)
library(readxl)
library(lubridate)
library(foreach)

#      Functions                                                            ####

#      Data                                                                 ####

# Deer Locations 
locs_deer_raw <- "1.DataManagement\\loc_data\\deer_nsse_final_clean.csv" %>% 
  read_csv()

###############################################################################
#   1. Renaming Columns                                                     ####

locs_deer_cleaning <- locs_deer_raw %>% 
  rename(individual.local.identifier = id,
         location.long = x,
         location.lat = y,
         timestamp = t)

#   2. Removing Unnecessary Columns                                         ####

locs_deer_cleaning <- locs_deer_cleaning %>% 
  select(-c(year,id_yr))

#   3. Adding new time covariates                                           ####

locs_deer_cleaning <- locs_deer_cleaning %>% 
  mutate(year = year(timestamp),
         month = month(timestamp))
  
#   4. Adding Study Site                                                    ####

locs_deer_cleaning <- locs_deer_cleaning %>% 
  mutate(site = ifelse(str_sub(locs_deer_cleaning$individual.local.identifier,1,1) == "N","North",
                      ifelse(str_sub(locs_deer_cleaning$individual.local.identifier,1,1) == "S",
                      "South",NA))) %>% 
  mutate(site = ifelse(str_sub(locs_deer_cleaning$individual.local.identifier,1,3) == "SCF","Southeast",site))
  
###############################################################################
#   Export                                                                  #### 

write_csv(locs_deer_cleaning,
          file = "1.DataManagement/loc_data/deer_all_final_clean.csv")

###############################################################################
