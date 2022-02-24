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
#      Functions                                                            ####

#      Data                                                                 ####
#        [Deer]                                                             ####

df_deer <- read_csv("1.DataManagement/CleanData/deer_all_clean.csv")

#        [NLCD]                                                             ####

NLCD_Missouri <- raster("1.DataManagement\\CleanData\\NLCD_Missouri.tif")

###############################################################################
#   [Delineating Study Area MCPs]                                           ####
#      [North]                                                              ####

# Subsetting only North locations
df_deer_north <- df_deer %>% 
  filter(site == "North")
  
# Making a SpatialPoints Object for MCP
spatialpoints_deer_north <-SpatialPoints(coords = cbind(df_deer_north$x,df_deer_north$y),
                                         proj4string = crs(NLCD_Missouri))

# MCP 
mcp_deer_north <- mcp(xy = spatialpoints_deer_north,
                      percent = 95,
                      unin = "m",
                      unout = "km2")

#      [South]                                                              ####

# Subsetting only South locations
df_deer_sorth <- df_deer %>% filter(site == "South")

#      [Southeast]                                                     ####

# Subsetting only Southeast locations
df_deer_southeast <- df_deer %>% filter(site == "Southeast")
