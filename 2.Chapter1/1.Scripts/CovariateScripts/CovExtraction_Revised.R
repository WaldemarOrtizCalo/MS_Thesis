#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-09-15 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(tidyverse)
library(sf)
library(terra)

#      Functions                                                            ####

#      Data                                                                 ####
#        [Location Data]                                                    ####
#        [Covariate Layers]                                                 ####

# North
covs_north <- list.files(path = "1.DataManagement/CovRasters/cov_layers_final/north",
                         full.names = T,
                         pattern = ".tif") %>% 
  str_subset(pattern = "aux",negate = T) %>%
  rast()

# South
covs_south <- list.files(path = "1.DataManagement/CovRasters/cov_layers_final/south",
                         full.names = T,
                         pattern = ".tif") %>% 
  str_subset(pattern = "aux",negate = T) %>%
  rast()

# Southeast
covs_southeast <- list.files(path = "1.DataManagement/CovRasters/cov_layers_final/southeast",
                             full.names = T,
                             pattern = ".tif") %>% 
  str_subset(pattern = "aux",negate = T) %>%
  rast()

# Fixed Layer Names
names(covs_southeast[[1]])<- "southeast_contag"
names(covs_southeast[[13]])<- "southeast_shdi"

###############################################################################