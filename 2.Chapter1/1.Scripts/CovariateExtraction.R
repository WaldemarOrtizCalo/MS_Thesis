#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-03-12 

# Purpose:This script is to extract covariates. 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(sf)
library(raster)
library(tidyverse)
library(adehabitatHR)
library(mapview)
library(FedData)
library(lubridate)
library(foreach)
library(stringr)


#      Functions                                                            ####

#      Data                                                                 ####
#        [Deer Data]                                                        ####
df_deer <- read_csv("1.DataManagement/CleanData/deer_all_clean.csv")

#        [NLCD Data]                                                        ####
Missouri_NLCD <- raster("1.DataManagement\\CleanData\\NLCD_Missouri.tif")

#        [DEM Data]                                                         ####
Missouri_topo <- stackOpen("1.DataManagement/CleanData/Topo_Missouri.stk")

#        [Road Data]                                                        ####
Missouri_roads <- st_read("1.DataManagement\\CleanData\\Missouri_Roads.shp")

#        [County Boundaries Data]                                           ####
Missouri_boundaries <- st_read("1.DataManagement\\CleanData\\shp_Missouri.shp")


###############################################################################
#   [Testing with one indiv]                                               ####
#      [Creating ltraj object]                                             ####

# Subsetting a deer
indiv <- df_deer %>% filter(id == "N15001")

# Setting Coordinates 
coordinates(indiv) <- c("x","y")

# Setting projection system
proj4string(indiv) <- CRS("+init=epsg:5070")

# Creating ltraj
ltraj <- as.ltraj(xy = coordinates(indiv),
                  date = indiv$t,
                  id= indiv$id, 
                  typeII=TRUE)

# Plotting out data to visually inspect
plot(ltraj)

mapview(indiv)

#      [Adding Buffer Size Column]                                         ####

ltraj_track <-ltraj[[1]] %>% 
  transform(dt = dt/3600) %>% 
  mutate(sampling_buffer = (dist/dt)+(2*sd((dist/dt),na.rm = T))) 

# Creating sf object
sf_object <- st_as_sf(indiv[1,])

#Creating buffer
buff <- st_buffer(sf_object,
                ltraj_track[,"sampling_buffer"][1])

# Sampling Buffer
sampleused <- st_sample(buff,5,multipoint,type = "random")

available <- sampleused %>% as("Spatial")

indivsub <- indiv[1,]

cbind(indivsub,available)

mapview(sampleused)+mapview(buff)
