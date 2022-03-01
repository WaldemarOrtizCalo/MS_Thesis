#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-02-23 

# Purpose: Used to Available Analysis

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

#      Functions                                                            ####

# Simulation Function
source("2.Chapter1\\2.Functions\\Used2Available_sim.R")

#      Data                                                                 ####

#        [Deer]                                                             ####

df_deer <- read_csv("1.DataManagement/CleanData/deer_all_clean.csv")

df_deer$quarteryear <- quarter(df_deer$t,
                               type = "year.quarter")

#        [NLCD]                                                             ####

NLCD_Missouri <- raster("1.DataManagement\\CleanData\\NLCD_Missouri.tif")
NLCD_available_north <- raster("1.DataManagement\\CleanData\\NLCD_available_north.tif")
NLCD_available_south <- raster("1.DataManagement\\CleanData\\NLCD_available_south.tif")
NLCD_available_southeast <- raster("1.DataManagement\\CleanData\\NLCD_available_southeast.tif")

#        [DEM and derivative layers]                                        ####

Topo_Missouri <- stackOpen("1.DataManagement/CleanData/Topo_Missouri.stk")
Topo_available_north <- stackOpen("1.DataManagement/CleanData/Topo_available_north.stk")
Topo_available_south <- stackOpen("1.DataManagement/CleanData/Topo_available_south.stk")
Topo_available_southeast <- stackOpen("1.DataManagement/CleanData/Topo_available_southeast.stk")

#        [Missouri Shapefile]                                               ####

shp_Missouri <- st_read("1.DataManagement\\CleanData\\shp_Missouri.shp")

#        [Covariate Stack]                                                              ####

covariate_stack <- stack(NLCD_Missouri,Topo_Missouri)

###############################################################################

