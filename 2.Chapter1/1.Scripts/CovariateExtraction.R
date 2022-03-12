#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-03-12 

# Purpose:This script is to extract covariates. 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(sf)
library(raster)
library(tidyverse)
library(adehabitatHR)
library(mapview)
library(FedData)
library(lubridate)
library(foreach)
library(stringr)


#      Functions                                                            ####

#      Data                                                                 ####
#        [Deer Data]                                                        ####
df_deer <- read_csv("1.DataManagement/CleanData/deer_all_clean.csv")

#        [NLCD Data]                                                        ####
#        [DEM Data]                                                         ####
#        [Road Data]                                                        ####



###############################################################################