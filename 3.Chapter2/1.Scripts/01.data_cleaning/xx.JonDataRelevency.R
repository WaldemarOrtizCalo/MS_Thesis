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

###############################################################################