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

# Masking and Cropping

available_north_NLCD <- mask(NLCD_Missouri,mcp_deer_north) %>% crop(mcp_deer_north)

# Visual Inspection 

mapview(available_north_NLCD)

#      [South]                                                              ####

# Subsetting only South locations
df_deer_south <- df_deer %>% 
  filter(site == "South")

# Making a SpatialPoints Object for MCP
spatialpoints_deer_south <-SpatialPoints(coords = cbind(df_deer_south$x,df_deer_south$y),
                                         proj4string = crs(NLCD_Missouri))

# MCP 
mcp_deer_south <- mcp(xy = spatialpoints_deer_south,
                      percent = 95,
                      unin = "m",
                      unout = "km2")

# Masking and Cropping

available_south_NLCD<- mask(NLCD_Missouri,mcp_deer_south) %>% 
  crop(mcp_deer_south)

# Visual Inspection 

#mapview(available_south_NLCD)

#      [Southeast]                                                          ####

# Subsetting only Southeast locations
df_deer_southeast <- df_deer %>% 
  filter(site == "Southeast")

# Making a SpatialPoints Object for MCP
spatialpoints_deer_southeast <-SpatialPoints(coords = cbind(df_deer_southeast$x,df_deer_southeast$y),
                                         proj4string = crs(NLCD_Missouri))

# MCP 
mcp_deer_southeast <- mcp(xy = spatialpoints_deer_southeast,
                      percent = 95,
                      unin = "m",
                      unout = "km2")

# Masking and Cropping

available_southeast_NLCD<- mask(NLCD_Missouri,mcp_deer_southeast) %>% 
  crop(mcp_deer_southeast)

# Visual Inspection 

#mapview(available_southeast_NLCD)

###############################################################################

#### Generating Available Points

# Sample Code
samp2011 <- sampleRandom(NLCD_Missouri, size = 20, na.rm = TRUE, sp = TRUE)
