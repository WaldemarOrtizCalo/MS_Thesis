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

#      Functions                                                            ####

#      Data                                                                 ####

# NLCD Raster

NLCD_raw <- raster("1.DataManagement\\RawData\\RasterData\\NLCD_2016_Land_Cover_L48_20190424.img")

# Deer Data 

df_deer <- read_csv("1.DataManagement/CleanData/deer_all_clean.csv")

# Missouri Shapefiles



###############################################################################