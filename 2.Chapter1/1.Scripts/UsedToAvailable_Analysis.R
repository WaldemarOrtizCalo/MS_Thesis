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

#      Functions                                                            ####

#      Data                                                                 ####
#        [Deer]                                                             ####

df_deer <- read_csv("1.DataManagement/CleanData/deer_all_clean.csv")

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

###############################################################################
#   [Making spatial objects of telemetry data]                              ####
#      [North]                                                              ####
# Subsetting only South locations
df_deer_north <- df_deer %>% 
  filter(site == "North")

# Making SpatialPoints Object
spatialpoints_deer_north <-SpatialPointsDataFrame(coords = cbind(df_deer_north$x,df_deer_north$y),
                                         data = df_deer_north,
                                         proj4string = crs(NLCD_Missouri))

#      [South]                                                              ####

# Subsetting only South locations
df_deer_south <- df_deer %>% 
  filter(site == "South")

# Making SpatialPoints Object
spatialpoints_deer_south <-SpatialPointsDataFrame(coords = cbind(df_deer_south$x,df_deer_south$y),
                                                  data = df_deer_south,
                                                  proj4string = crs(NLCD_Missouri))
#      [Southeast]                                                          ####

# Subsetting only South locations
df_deer_southeast <- df_deer %>% 
  filter(site == "Southeast")

# Making SpatialPoints Object
spatialpoints_deer_southeast <-SpatialPointsDataFrame(coords = cbind(df_deer_southeast$x,df_deer_southeast$y),
                                                  data = df_deer_southeast,
                                                  proj4string = crs(NLCD_Missouri))

###############################################################################
#   [Subsetting by Quarter]                                              ####



#      [North]                                                              ####
North_2015_Q1 <- spatialpoints_deer_north 

spatialpoints_deer_north$t %>% quarter(type = "year.quarter") %>% unique()

#      [South]                                                              ####
#      [Southeast]                                                              ####


###############################################################################
#   [Sampling Available Locations]                                          ####

#      [Example]                                                            ####

# Sample Code

sim 
samp <- sampleRandom(Topo_available_north, size = 2000, na.rm = TRUE, sp = TRUE)


samp %>% as.data.frame()

AvailabilitySim <- function(raster2sample,n.points2sample){
  
  raster <- raster2sample
  
  samp <- sampleRandom(raster, size = n.points2sample, na.rm = TRUE, sp = TRUE) %>% as.data.frame()
  
  return(samp)
}

AvailabilitySim(Topo_available_north,
                n.points2sample = 2000)

sim2 <- NA 

for (i in 1:5) {
  sim2[i] <- sampleRandom(Topo_available_north, size = 10, na.rm = TRUE, sp = TRUE) %>% as.data.frame()
}
