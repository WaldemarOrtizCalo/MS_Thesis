#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-02-18 

# Purpose: To clean and organize Rasters for the project

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####

library(raster)
library(tidyverse)
library(here)
library(sf)
library(mapview)

#      Functions                                                            ####

#      Data                                                                 ####

# NLCD Raster

NLCD_raw <- raster("1.DataManagement\\RawData\\RasterData\\NLCD_2016_Land_Cover_L48_20190424.img")

# Deer Data 

df_deer <- read_csv("1.DataManagement/CleanData/deer_all_clean.csv")

# Missouri Shapefiles

Missouri_shp <- st_read("1.DataManagement\\RawData\\Shapefiles\\Missouri_Counties.shp")

###############################################################################
#   [Standardizing Projections]                                             ####
#      [NLCD]                                                               ####

# Current CRS 
crs(NLCD_raw)

#      [Missouri Shp]                                                       ####

# Current CRS 
crs(Missouri_shp)

#      [Deer Data]                                                          ####

###############################################################################

#   [Cropping Raster to Missouri]                                           ####
Missouri_NLCD <- crop(NLCD_raw,extent(Missouri_shp))
