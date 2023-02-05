#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2023-02-04 

# Purpose: Creating layers that were not created before in Chapter 1

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(tidyverse)
library(sf)
library(terra)

#      Functions                                                            ####

#      Data                                                                 ####

# Environmental Layers 
covlayers_north <- list.files("1.DataManagement/CovRasters/base_layers", 
                              pattern = "north",
                              full.names = T) %>% str_subset(".xml", negate = T) %>% rast()


covlayers_south <- list.files("1.DataManagement/CovRasters/base_layers", 
                              pattern = "south",
                              full.names = T) %>% 
  str_subset(".xml", negate = T) %>% 
  str_subset("southeast", negate = T) %>% rast()

###############################################################################
#   North                                                                   ####

input <- covlayers_north[["north_dem"]]

algos <- c("slope","TRI","aspect")

for(i in 1:length(algos)){
  output <- terrain(input,
                    v = algos[i],
                    neighbors = 8,
                    filename = paste0("1.DataManagement/CovRasters/base_layers/north_",algos[i],".tif"),
                    overwrite = T)
  
  print(i/length(algos))
}

###############################################################################
#   South                                                                   ####

input <- covlayers_south[["south_dem"]]

algos <- c("slope","TRI","aspect")

for(i in 1:length(algos)){
  output <- terrain(input,
                    v = algos[i],
                    neighbors = 8,
                    filename = paste0("1.DataManagement/CovRasters/base_layers/south",algos[i],".tif"),
                    overwrite = T)
  
  print(i/length(algos))
}

###############################################################################