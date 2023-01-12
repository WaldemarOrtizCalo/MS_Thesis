#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2023-01-12 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(terra)
library(tidyverse)

#      Functions                                                            ####

#      Data                                                                 ####
nlcd_list <- list.files("1.DataManagement/CovRasters/base_layers",
                        pattern = "nlcd.tif",
                        full.names = T) %>% 
  str_subset("aux", negate = T)

north_classes <- c("water",
                   "developed",
                   "barren",
                   "forest",
                   "shrub",
                   "grassland",
                   "cropland",
                   "wetland")

south_classes <- c("water",
                   "developed",
                   "barren",
                   "deciduous_forest",
                   "evergreen_forest",
                   "mixed_forest",
                   "shrub",
                   "grassland",
                   "cropland",
                   "wetland")

southeast_classes <- c("water",
                   "developed",
                   "barren",
                   "deciduous_forest",
                   "evergreen_forest",
                   "mixed_forest",
                   "shrub",
                   "grassland",
                   "cropland",
                   "wetland")

###############################################################################

north <- rast(nlcd_list[[1]]) %>% 
  freq() %>% 
  mutate(prop = round(count/sum(.$count),digits = 3)) %>% 
  mutate(layer = north_classes)

south <- rast(nlcd_list[[2]]) %>% 
  freq() %>% 
  mutate(prop = round(count/sum(.$count),digits = 3)) %>% 
  mutate(layer = south_classes)

southeast <- rast(nlcd_list[[3]]) %>% 
  freq() %>% 
  mutate(prop = round(count/sum(.$count),digits = 3)) %>% 
  mutate(layer = southeast_classes)

###############################################################################

