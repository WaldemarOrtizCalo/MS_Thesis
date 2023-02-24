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
locs_deer <- "1.DataManagement\\loc_data\\deer_all_NS_clean.csv" %>% 
  read_csv()

###############################################################################