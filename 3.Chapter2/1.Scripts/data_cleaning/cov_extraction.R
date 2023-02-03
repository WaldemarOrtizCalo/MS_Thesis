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
library(move)

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
#   North                                                                   ####
#      Data Import                                                          ####

# Deer Locations
locs <- locs_deer %>% filter(site == "North")

# Covariate Layers/Stack 
cov_layers <- rast(covlayers_north)

#      Home Range Calculation                                               ####
#        Filtering ids and periods with greater than 20 locs                ####

valid_homeranges <- locs %>% 
  group_by(individual.local.identifier,ordered_int_id) %>% 
  summarize(n_locs = n()) %>% 
  filter(n_locs > 20)

# Dev 
i <- 1

# HR metadata 
indiv_id <- valid_homeranges[[i,1]]
interval_id <- valid_homeranges[[i,2]]

# Filtering locations
locs_hr <- locs %>% 
  filter(individual.local.identifier == indiv_id & ordered_int_id == interval_id) %>% 
  mutate(HR_id = paste0(indiv_id,"_",interval_id),.after = 1)

telemetry_object <- move(x=locs_hr$location.long, 
                         y=locs_hr$location.lat, time=as.POSIXct(locs_hr$timestamp, format="%Y-%m-%d %H:%M:%OS", tz="UTC"), 
                         proj=CRS("+init=epsg:5070"),
                         data=locs_hr, 
                         animal=locs_hr$HR_id) %>% 
  as.telemetry()

# Creating CTMM fit objects for the smoothing
M.IID <- ctmm.fit(telemetry_object) 
GUESS <- ctmm.guess(telemetry_object,interactive=FALSE) 
M.OUF <- ctmm.fit(telemetry_object,GUESS) 

# Home Range Polygons
KDE <- akde(telemetry_object,M.IID) %>% as.sf()
AKDE <- akde(telemetry_object,M.OUF) %>% as.sf()

# Exporting
writeShapefile(object = KDE,
               folder = "1.DataManagement/ch2_data/clean/homerange_polygons/north/KDE",
               file=paste0(paste0(indiv_id,"_",interval_id),"_kde"),
               overwrite = T)

writeShapefile(object = AKDE,
               folder = "1.DataManagement/ch2_data/clean/homerange_polygons/north/AKDE",
               file=paste0(paste0(indiv_id,"_",interval_id),"_akde"),
               overwrite = T)

###############################################################################
#   South                                                                   ####
###############################################################################