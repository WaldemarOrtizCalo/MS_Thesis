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

Topo_available_north <- stack(lapply( c("1.DataManagement/CleanData/Topo_available_north_DEM_Missouri.tif",
                                        "1.DataManagement/CleanData/Topo_available_north_DEM_Slope.tif",
                                        "1.DataManagement/CleanData/Topo_available_north_DEM_Aspect.tif",
                                        "1.DataManagement/CleanData/Topo_available_north_DEM_TRI.tif"),
                                      raster))

names(Topo_available_north) <- c("North_Elevation",
                                 "North_Slope",
                                 "North_Aspect",
                                 "North_Elevation")

Topo_available_south <- raster("1.DataManagement/CleanData/Topo_available_south.tif")
Topo_available_southeast <- raster("1.DataManagement/CleanData/Topo_available_southeast.tif")

###############################################################################
#   [Making spatial objects]                                                ####
#      [North]                                                              ####
# Subsetting only South locations
df_deer_north <- df_deer %>% 
  filter(site == "North")

# Making SpatialPoints Object
spatialpoints_deer_north <-SpatialPoints(coords = cbind(df_deer_north$x,df_deer_north$y),
                                         proj4string = crs(NLCD_Missouri))

#      [South]                                                              ####

# Subsetting only South locations
df_deer_south <- df_deer %>% 
  filter(site == "South")

# Making SpatialPoints Object
spatialpoints_deer_south <-SpatialPoints(coords = cbind(df_deer_south$x,df_deer_south$y),
                                         proj4string = crs(NLCD_Missouri))


#      [Southeast]                                                          ####

# Subsetting only South locations
df_deer_southeast <- df_deer %>% 
  filter(site == "Southeast")

# Making SpatialPoints Object
spatialpoints_deer_southeast <-SpatialPoints(coords = cbind(df_deer_southeast$x,df_deer_southeast$y),
                                         proj4string = crs(NLCD_Missouri))

###############################################################################
#   [Sampling Used Locations]                                               ####
#      [North - All]                                                        ####

###############################################################################
#   [Sampling Available Locations]                                          ####

#      [Example]                                                            ####

# Sample Code
samp <- sampleRandom(NLCD_available_north, size = 2000, na.rm = TRUE, sp = TRUE)

mapview(NLCD_available_north)+mapview(samp)

samp %>% as.data.frame()
