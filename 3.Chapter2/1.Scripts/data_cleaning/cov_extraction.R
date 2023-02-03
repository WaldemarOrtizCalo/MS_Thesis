#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2023-02-02 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(ctmm)
library(tidyverse)
library(sf)
library(terra)

#      Functions                                                            ####

#      Data                                                                 ####

# Deer Locations
locs_deer <- read_csv("1.DataManagement/ch2_data/clean/deer_mortalitylocs.csv")

# Environmental Layers 
covlayers_north <- list.files("1.DataManagement/CovRasters/base_layers", 
                              pattern = "north",
                              full.names = T) %>% str_subset(".xml", negate = T)

covlayers_south <- list.files("1.DataManagement/CovRasters/base_layers", 
                              pattern = "south",
                              full.names = T) %>% str_subset(".xml", negate = T) %>% str_subset("southeast", negate = T)

###############################################################################
