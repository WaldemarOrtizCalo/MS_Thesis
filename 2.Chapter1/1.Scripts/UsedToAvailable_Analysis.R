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
library(doParallel)

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

#        [Covariate Stack]                                                  ####

covariate_stack <- stack(NLCD_Missouri,Topo_Missouri)

###############################################################################
#   [Running Sims]                                                          ####
#      [North]                                                              ####

# Subset North
North_subset <- df_deer %>% 
  filter(site == "North")

time_periods <- unique(North_subset$quarteryear)

for (i in 1:length(time_periods)) {
  
Used2Available_sim(pop_df = df_deer,
                          year_quarter = time_periods[i],
                          site.name = "North",
                          raster_stack = covariate_stack,
                          n.avail.range = 1:5,
                          n.sim = 25,
                          export_sim = T,
                          export_dir = "2.Chapter1/3.Output/UsedToAvailable_Analysis/Sim_Data")
  print(i)
}

#      [South]                                                              ####

# Subset South
South_subset <- df_deer %>% 
  filter(site == "South")

time_periods <- unique(South_subset$quarteryear)

for (i in 1:length(time_periods)) {
  
  Used2Available_sim(pop_df = df_deer,
                     year_quarter = time_periods[i],
                     site.name = "South",
                     raster_stack = covariate_stack,
                     n.avail.range = 1:5,
                     n.sim = 25,
                     export_sim = T,
                     export_dir = "2.Chapter1/3.Output/UsedToAvailable_Analysis/Sim_Data")
  print(i)
}

#      [Southeast]                                                          ####

# Subset Southeast
Southeast_subset <- df_deer %>% 
  filter(site == "Southeast")

time_periods <- unique(Southeast_subset$quarteryear)

for (i in 1:length(time_periods)) {
  
  Used2Available_sim(pop_df = df_deer,
                     year_quarter = time_periods[i],
                     site.name = "Southeast",
                     raster_stack = covariate_stack,
                     n.avail.range = 1:5,
                     n.sim = 25,
                     export_sim = T,
                     export_dir = "2.Chapter1/3.Output/UsedToAvailable_Analysis/Sim_Data")
  print(i)
}
###############################################################################