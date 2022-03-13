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
Missouri_NLCD <- raster("1.DataManagement\\CleanData\\NLCD_Missouri.tif")

#        [DEM Data]                                                         ####
Missouri_topo <- stackOpen("1.DataManagement/CleanData/Topo_Missouri.stk")

#        [Road Data]                                                        ####
Missouri_roads <- st_read("1.DataManagement\\CleanData\\Missouri_Roads.shp")

#        [County Boundaries Data]                                           ####
Missouri_boundaries <- st_read("1.DataManagement\\CleanData\\shp_Missouri.shp")


###############################################################################
