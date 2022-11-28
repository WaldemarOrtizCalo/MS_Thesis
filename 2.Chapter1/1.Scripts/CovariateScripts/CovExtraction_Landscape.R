#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-11-21 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(tidyverse)
library(sf)
library(terra)
library(adehabitatHR)

#      Functions                                                            ####

#      Data                                                                 ####

#        Deer Locations                                                     ####

deer_all <- read_csv("1.DataManagement/CleanData/deer_all_revised.csv") 

deer_all %>% group_by(sex,age, .add = T) %>% 
  summarise(n = n())

###############################################################################
#   North                                                                   ####
#      Data Import                                                          ####
#        Deer locations                                                     ####

deer <- deer_all %>% filter(site == "North")

deer_male <- deer %>% filter(sex == "M")

deer_female <- deer %>% filter(sex == "F")

#        Covariate Rasters                                                  ####

cov_rasts <- list.files("1.DataManagement/CovRasters_Landscape/north",
                        full.names = T) %>% 
  str_subset("patches",negate = T) %>% 
  str_subset("patcharea",negate = T) %>% rast()

#      Creating Sampling Polygons                                           ####






spdf <- data_aggregated
coordinates(spdf) <- ~location.long + location.lat
proj4string(spdf) <- CRS("+init=epsg:5070")

kde <- kernelUD(spdf, h = "href")
kde_UD <- getverticeshr(kde, 95)%>% 
  st_as_sf() %>% 
  st_write("1.DataManagement/HomeRangePolygons/north/AggregatedPolygons/kde_north.shp",
           append=FALSE)



###############################################################################