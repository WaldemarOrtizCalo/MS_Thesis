#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-05-05 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(raster)
library(tidyverse)
library(sf)
library(mapview)
library(pbapply)
library(doParallel)
library(stringr)
library(landscapemetrics)
library(foreach)
library(terra)

###############################################################################
# Road Layers


library(sf)
library(raster)
library(stars)

template <- raster("1.DataManagement\\CovRasters\\base_layers\\north_nlcd.tif")
rds <- st_read("1.DataManagement/CleanData/Missouri_Roads.shp")

rds_rast<-st_rasterize(rds, template = template)
a##################